{------------------------------------------------------------------------------ 
 - Copyright 2025 ContinuousC                                                 - 
 -                                                                            - 
 - Licensed under the Apache License,  Version 2.0  (the "License");  you may - 
 - not use this file except in compliance with the License. You may  obtain a - 
 - copy of the License at http://www.apache.org/licenses/LICENSE-2.0          - 
 -                                                                            - 
 - Unless  required  by  applicable  law  or agreed  to in  writing, software - 
 - distributed under the License is distributed on an "AS IS"  BASIS, WITHOUT - 
 - WARRANTIES OR CONDITIONS OF ANY KIND, either express  or implied.  See the - 
 - License for the  specific language  governing permissions  and limitations - 
 - under the License.                                                         - 
 ------------------------------------------------------------------------------}

{-# LANGUAGE RecordWildCards, KindSignatures #-} 

import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Control.Monad (when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (State, StateT, execState, evalStateT, gets, modify)
import qualified Data.Map.Strict as M
import qualified Data.Set as S

data Version = Version
  { vMajor :: Int
  , vMinor :: Int
  , vPatch :: Int
  , vPre   :: Prerelease
  } deriving (Show, Eq, Ord)

data Prerelease = PDevelopment | PTesting Int | PAcceptance Int | PRelease
  deriving (Show, Eq, Ord)
data Environment = EDevelopment | ETesting | EAcceptance | ERelease
  deriving (Show, Eq, Ord)
data Bump = BPatch | BMinor | BMajor
  deriving (Show, Eq, Ord)

data Artifact = Artifact
  { aName    :: String
  , aVersion :: Version
  , aDeps    :: [Artifact]
  } deriving Show

type Updates = M.Map String Version
type Cached m r = StateT (M.Map String r) m r
type Update = State Updates

showVersion :: Version -> String
showVersion Version {..} = show vMajor <> "." <> show vMinor <> "." <> show vPatch
                           <> showPrerelease vPre

showPrerelease :: Prerelease -> String
showPrerelease  PDevelopment   = "-dev"
showPrerelease (PTesting n)    = "-tst" <> show n
showPrerelease (PAcceptance n) = "-acc" <> show n
showPrerelease  PRelease       = ""

showArtifact :: Artifact -> String
showArtifact Artifact {..} = intercalate "\n" (us : deps)
  where
    us = "- " <> aName <> " ( version = " <> showVersion aVersion <> " )"
    deps = map (indent . showArtifact) aDeps
    indent = intercalate "\n" . map ("   " <>) . lines

showTree :: [Artifact] -> String
showTree = intercalate "\n" . map showArtifact

showBumpEnv :: Maybe Bump -> Environment -> String
showBumpEnv bump env = maybe "" (\b -> showBump b <> " ") bump <> showEnvironment env

showBump :: Bump -> String
showBump BPatch = "patch"
showBump BMinor = "minor"
showBump BMajor = "major"

showEnvironment :: Environment -> String
showEnvironment EDevelopment = "development"
showEnvironment ETesting     = "testing"
showEnvironment EAcceptance  = "acceptance"
showEnvironment ERelease     = "release"

showUpdates :: Updates -> String
showUpdates = intercalate "\n" . map showUpdate . M.toList

showUpdate :: (String, Version) -> String
showUpdate (k,v) = "- " <> k <> ": " <> showVersion v

applyUpdates :: Updates -> [Artifact] -> [Artifact]
applyUpdates updates = map update
  where
    update Artifact{..} = Artifact { aVersion = case aName `M.lookup` updates of
                                                  Just v -> v
                                                  Nothing -> aVersion
                                   , aDeps = applyUpdates updates aDeps
                                   , .. }

bumpTree :: Maybe Bump -> Environment -> S.Set String -> [Artifact] -> Updates
bumpTree bump env sel as = execState run M.empty
  where
    deps = treeDeps sel as
    run = do
      runTree (bumpArtifact bump env sel deps) as
      runTree (ensureMaxEnv env sel deps) as -- fixes invalid input
      runTree (ensureEditable env sel deps) as

treeDeps :: S.Set String -> [Artifact] -> S.Set String
treeDeps sel = tdeps False
  where
    tdeps isSel = foldl S.union S.empty . map (deps isSel)
    deps isSel Artifact{..}
      | aName `S.member` sel = tdeps True aDeps
      | isSel                = S.insert aName (tdeps isSel aDeps)
      | otherwise            = tdeps isSel aDeps

runTree :: (Artifact -> Cached Update r) -> [Artifact] -> Update ()
runTree f as = evalStateT (mapM_ f as) M.empty

cached :: String -> Cached Update r -> Cached Update r
cached name f = do
  c <- gets $ M.lookup name
  case c of
    Just r -> return r
    Nothing -> do
      r <- f
      modify $ M.insert name r
      return r

bumpArtifact :: Maybe Bump -> Environment -> S.Set String -> S.Set String
             -> Artifact -> Cached Update ()
bumpArtifact bump env sel deps a@Artifact{..} = cached aName $ do
  mapM_ (bumpArtifact bump env sel deps) aDeps
  version <- lift $ updatedVersion a
  if aName `S.member` sel
    then maybe (return ()) (lift . modify . M.insert aName) (bumpVersion bump env aVersion)
    else when (aName `S.member` deps)
         $ maybe (return ()) (lift . modify . M.insert aName) (bumpDepVersion env version)

ensureEditable :: Environment -> S.Set String -> S.Set String
               -> Artifact -> Cached Update Bool
ensureEditable env sel deps Artifact{..} = cached aName $ do
  isUpdated <- lift $ gets (M.member aName)
  hasEdits <- or <$> mapM (ensureEditable env sel deps) aDeps
  let minEnv = if aName `S.member` sel || aName `S.member` deps
               then env else EDevelopment
  if hasEdits && not isUpdated
    then maybe (return ()) (lift . modify . M.insert aName) (bumpPrerelease minEnv aVersion)
         >> return True
    else return $ hasEdits || isUpdated

ensureMaxEnv :: Environment -> S.Set String -> S.Set String
             -> Artifact -> Cached Update Environment
ensureMaxEnv env sel deps a@Artifact{..} = cached aName $ do
  maxEnv <- foldl min ERelease <$> mapM (ensureMaxEnv env sel deps)  aDeps
  version <- lift $ updatedVersion a
  let ourEnv = environment (vPre version)
      minEnv = if aName `S.member` sel || aName `S.member` deps
               then env else EDevelopment
  if ourEnv > maxEnv
    then maybe (return ()) (lift . modify . M.insert aName) (bumpPrerelease minEnv version)
         >> return minEnv
    else return ourEnv

updatedVersion :: Artifact -> Update Version
updatedVersion Artifact{..} = fromMaybe aVersion <$> gets (M.lookup aName)

bumpVersion :: Maybe Bump -> Environment -> Version -> Maybe Version
bumpVersion (Just BPatch) env Version{..} =
  Just Version { vPatch = vPatch + 1
               , vPre = prerelease env
               , .. }
bumpVersion (Just BMinor) env Version{..} =
  Just Version { vMinor = vMinor + 1
               , vPatch = 0
               , vPre = prerelease env
               , .. }
bumpVersion (Just BMajor) env Version{..} =
  Just Version { vMajor = vMajor + 1
               , vMinor = 0
               , vPatch = 0
               , vPre = prerelease env
               , .. }
bumpVersion Nothing env v = bumpPrerelease env v

bumpDepVersion :: Environment -> Version -> Maybe Version
bumpDepVersion minEnv v@Version{..}
  | environment vPre < minEnv = Just $ ensurePrerelease minEnv v
  | otherwise = Nothing

bumpPrerelease :: Environment -> Version -> Maybe Version
bumpPrerelease env Version{ vPre = PRelease, ..} =
  Just Version { vPatch = vPatch + 1
               , vPre = prerelease env
               , .. }
bumpPrerelease EDevelopment Version{..}
  | vPre == PDevelopment = Nothing
  | otherwise =
      Just Version { vPre = PDevelopment
                   , .. }
bumpPrerelease ETesting Version{..} =
  Just Version { vPre = PTesting (
                   case vPre of
                     PTesting n ->  n + 1
                     _ -> 0 )
               , .. }
bumpPrerelease EAcceptance Version{..} =
  Just Version { vPre = PAcceptance (
                   case vPre of
                     PAcceptance n ->  n + 1
                     _ -> 0 )
               , .. }
bumpPrerelease ERelease Version{..} =
  Just Version { vPre = PRelease
               , .. }

ensurePrerelease :: Environment -> Version -> Version
ensurePrerelease env Version{..} =
  Version { vPatch = if vPre == PRelease && env /= ERelease
                     then vPatch + 1
                     else vPatch
          , vPre = prerelease env
          , .. }

prerelease :: Environment -> Prerelease
prerelease EDevelopment = PDevelopment
prerelease ETesting     = PTesting 0
prerelease EAcceptance  = PAcceptance 0
prerelease ERelease     = PRelease

environment :: Prerelease -> Environment
environment  PDevelopment   = EDevelopment
environment (PTesting _)    = ETesting
environment (PAcceptance _) = EAcceptance
environment  PRelease       = ERelease


----- Test -----

main :: IO ()
main = do

  let bump = bumpTree versionBump env sel
      versionBump = Just BMajor
      env = EAcceptance
      sel = S.fromList ["continuousc-chart", "relation-graph-wasm"]

      updates = bump testTree
      testTree' = applyUpdates updates testTree
      updates' = bump testTree'
      testTree'' = applyUpdates updates' testTree'

  putStrLn   "Original:"
  putStrLn $ showTree testTree

  putStrLn $ "\nBump: " <> showBumpEnv versionBump env
  mapM_ (putStrLn . ("- " <>)) sel

  putStrLn   "\nUpdates:"
  putStrLn $ showUpdates updates

  putStrLn   "\nApplied:"
  putStrLn $ showTree testTree'

  putStrLn $ "\nBump: " <> showBumpEnv versionBump env 
  mapM_ (putStrLn . ("- " <>)) sel

  putStrLn   "\nUpdates:"
  putStrLn $ showUpdates updates'

  putStrLn   "\nApplied:"
  putStrLn $ showTree testTree''


testTree :: [Artifact]
testTree = [ chart, k8sDiscoveryChart ]
  where
    chart = Artifact "continuousc-chart" (Version 0 0 3 PRelease) [frontend, engine]
    frontend = Artifact "continuousc-frontend-image" (Version 0 0 2 PRelease) [wasm]
    engine = Artifact "relation-graph-engine" (Version 0 1 1 PRelease) []
    wasm = Artifact "relation-graph-wasm" (Version 0 1 6 (PAcceptance 12)) []
    k8sDiscoveryChart = Artifact "k8s-discovery-chart" (Version 0 0 1 (PTesting 1)) [k8sDiscovery]
    k8sDiscovery = Artifact "k8s-discovery-image" (Version 0 0 1 PRelease) [wasm]
