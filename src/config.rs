/******************************************************************************
 * Copyright 2025 ContinuousC                                                 *
 *                                                                            *
 * Licensed under the Apache License,  Version 2.0  (the "License");  you may *
 * not use this file except in compliance with the License. You may  obtain a *
 * copy of the License at http://www.apache.org/licenses/LICENSE-2.0          *
 *                                                                            *
 * Unless  required  by  applicable  law  or agreed  to in  writing, software *
 * distributed under the License is distributed on an "AS IS"  BASIS, WITHOUT *
 * WARRANTIES OR CONDITIONS OF ANY KIND, either express  or implied.  See the *
 * License for the  specific language  governing permissions  and limitations *
 * under the License.                                                         *
 ******************************************************************************/

use std::{
    collections::{BTreeMap, BTreeSet},
    convert::Infallible,
    fmt::Display,
    path::{Path, PathBuf},
    str::FromStr,
};

use parse_display_derive::Display;
use serde::{de::DeserializeOwned, Deserialize, Serialize};
use serde_with::DeserializeFromStr;

use crate::{
    bump::Updates,
    error::{EditError, EditErrorKind, EditResult, Error, Result},
    git::{get_version_tags, is_dirty, is_tagged, TagState},
    version::{Version, VersionReq},
    VC_VERSION,
};

#[derive(clap::Args, Clone)]
pub(crate) struct RepoArgs {
    /// Specify repositories explicitly, instead of looking for a
    /// config file in ancestor directories.
    #[clap(long = "repo", short)]
    repos: Vec<PathBuf>,
}

#[derive(Debug)]
pub(crate) struct Repos(pub(crate) BTreeMap<PathBuf, Config>);

#[derive(Debug)]
pub(crate) struct Artifacts<'a>(BTreeMap<&'a ArtifactId, ArtifactWithInfo<'a>>);

#[derive(Deserialize, Debug)]
pub(crate) struct Config {
    pub(crate) api: Option<semver::VersionReq>,
    #[serde(default)]
    pub(crate) include: BTreeSet<PathBuf>,
    #[serde(default = "default_tag_separator")]
    pub(crate) tag_separator: String,
    #[serde(default)]
    pub(crate) artifacts: BTreeMap<ArtifactId, Artifact>,
    #[serde(default)]
    pub(crate) workspaces: BTreeMap<WorkspaceId, Workspace>,
}

fn default_tag_separator() -> String {
    String::from("_v")
}

#[derive(Deserialize, Display, Eq, Ord, PartialEq, PartialOrd, Clone, Debug)]
pub(crate) struct ArtifactId(String);

#[derive(Deserialize, Display, PartialEq, Eq, PartialOrd, Ord, Clone, Debug)]
pub(crate) struct WorkspaceId(String);

impl FromStr for ArtifactId {
    type Err = Infallible;
    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        Ok(Self(s.to_string()))
    }
}

#[derive(Deserialize, Debug)]
pub(crate) struct Workspace {
    pub(crate) r#type: WorkspaceType,
    pub(crate) root: PathBuf,
    #[serde(default)]
    pub(crate) pre_commit: Vec<PreCommitHook>,
}

#[derive(Deserialize, Debug)]
pub(crate) struct Artifact {
    pub(crate) r#type: ArtifactType,
    pub(crate) source: Source,
    #[serde(default)]
    pub(crate) pre_commit: Vec<PreCommitHook>,
    #[serde(default)]
    pub(crate) pre_tag: Vec<PreTagHook>,
    #[serde(default)]
    pub(crate) post_tag: Vec<PostTagHook>,
}

#[derive(Deserialize, Debug)]
pub(crate) struct PreCommitHook {
    #[serde(flatten)]
    pub(crate) command: HookCmd,
    #[serde(default)]
    pub(crate) files: Vec<PathBuf>,
}

#[derive(Deserialize, Debug)]
pub(crate) struct PreTagHook {
    #[serde(flatten)]
    pub(crate) command: HookCmd,
}

#[derive(Deserialize, Debug)]
pub(crate) struct PostTagHook {
    #[serde(flatten)]
    pub(crate) command: HookCmd,
}

#[derive(Deserialize, Debug)]
pub(crate) struct HookCmd {
    pub(crate) run: String,
    #[serde(default)]
    pub(crate) working_dir: Option<WorkingDir>,
}

#[derive(Deserialize, Clone, Debug)]
#[serde(rename_all = "snake_case")]
pub(crate) enum WorkingDir {
    Repo,
    Source,
    Workspace,
    Custom(PathBuf),
}

#[derive(Debug)]
pub(crate) struct ArtifactInfo<'a> {
    pub(crate) version: Version,
    pub(crate) dependencies: BTreeMap<&'a ArtifactId, Option<Version>>,
}

#[derive(Debug)]
pub(crate) struct ArtifactWithInfo<'a> {
    pub(crate) repo: &'a Path,
    pub(crate) git: PathBuf,
    pub(crate) root: PathBuf,
    pub(crate) artifact: &'a Artifact,
    pub(crate) workspace: Option<&'a Workspace>,
    pub(crate) info: ArtifactInfo<'a>,
    pub(crate) tag: String,
    pub(crate) tag_prefix: String,
    pub(crate) tags: BTreeSet<Version>,
    pub(crate) dirty: bool,
    pub(crate) tagged: TagState,
}

#[derive(Deserialize, Display, PartialEq, Eq, Clone, Copy, Debug)]
#[serde(rename_all = "snake_case")]
pub(crate) enum WorkspaceType {
    Cargo,
}

#[derive(Deserialize, Display, PartialEq, Eq, Clone, Copy, Debug)]
#[serde(rename_all = "snake_case")]
pub(crate) enum ArtifactType {
    Helm,
    Docker,
    Cargo,
    Npm,
}

#[derive(Deserialize, Display, PartialEq, Eq, Clone, Copy, Debug)]
#[serde(rename_all = "snake_case")]
pub(crate) enum SourceType {
    Helm,
    Npm,
    Cargo,
}

#[derive(Deserialize, Debug)]
#[serde(tag = "type", rename_all = "snake_case")]
pub(crate) enum Source {
    Helm(HelmChart),
    Npm(NpmPackage),
    Cargo(CargoPackage),
}

#[derive(Deserialize, Debug)]
pub(crate) struct HelmChart {
    pub(crate) root: PathBuf,
    #[serde(default)]
    pub(crate) paths: Vec<PathBuf>,
    #[serde(default)]
    pub(crate) dependencies: Vec<HelmDependency>,
    #[serde(default)]
    pub(crate) values: Vec<HelmValue>,
}

#[derive(Deserialize, Debug)]
pub(crate) struct HelmDependency {
    pub(crate) name: String,
    pub(crate) artifact: ArtifactId,
}

#[derive(Deserialize, Debug)]
pub(crate) struct HelmValue {
    pub(crate) path: String,
    pub(crate) artifact: ArtifactId,
}

#[derive(Deserialize, Debug)]
pub(crate) struct NpmPackage {
    pub(crate) root: PathBuf,
    #[serde(default)]
    pub(crate) paths: Vec<PathBuf>,
    #[serde(default)]
    pub(crate) dependencies: Vec<NpmDependency>,
}

#[derive(Deserialize, Debug)]
pub(crate) struct NpmDependency {
    pub(crate) name: String,
    pub(crate) artifact: ArtifactId,
}

#[derive(Deserialize, Debug)]
pub(crate) struct CargoPackage {
    pub(crate) root: PathBuf,
    #[serde(default)]
    pub(crate) workspace: Option<WorkspaceId>,
    #[serde(default)]
    pub(crate) paths: Vec<PathBuf>,
    #[serde(default)]
    pub(crate) dependencies: Vec<CargoDependency>,
}

#[derive(Deserialize, PartialEq, Eq, PartialOrd, Ord, Clone, Debug)]
pub(crate) struct CrateName(String);

#[derive(Deserialize, Debug)]
pub(crate) struct CargoDependency {
    pub(crate) name: CrateName,
    pub(crate) artifact: ArtifactId,
}

impl Repos {
    pub(crate) fn load(args: &RepoArgs) -> Result<Self> {
        let vc_version = semver::Version::parse(VC_VERSION).unwrap();
        if args.repos.is_empty() {
            log::debug!("searching config file in current directory and its ancestors");
            let path = std::env::current_dir().map_err(Error::GetCwd)?;
            let (path, file) = path
                .ancestors()
                .filter_map(|p| Some((p, Self::find_config(p)?)))
                .last()
                .ok_or(Error::MissingAncestorConfig)?;
            let mut repos = BTreeMap::new();
            Self::load_config(&vc_version, path, &file, &mut repos)?;
            Ok(Self(repos))
        } else {
            let mut repos = BTreeMap::new();
            args.repos.iter().try_for_each(|path| {
                let file = Self::find_config(path)
                    .ok_or_else(|| Error::MissingConfig(path.to_path_buf()))?;
                Self::load_config(&vc_version, path, &file, &mut repos)
            })?;
            Ok(Self(repos))
        }
    }

    fn find_config(path: &Path) -> Option<PathBuf> {
        let path = path.join(".vc.yaml");
        path.exists().then_some(path)
    }

    fn load_config(
        vc_version: &semver::Version,
        path: &Path,
        file: &Path,
        repos: &mut BTreeMap<PathBuf, Config>,
    ) -> Result<()> {
        log::debug!("loading config from {}", file.display());
        let data = std::fs::read(file).map_err(|e| Error::ReadFile(file.to_path_buf(), e))?;
        let config = serde_yaml::from_slice::<Config>(&data)
            .map_err(|e| Error::DecodeYaml(file.to_path_buf(), e))?;
        if let Some(req) = &config.api {
            req.matches(vc_version)
                .then_some(())
                .ok_or_else(|| Error::IncompatibleConfigVersion(file.to_path_buf(), req.clone()))?;
        } else {
            log::warn!("Missing \"api\" version key for {}", file.display());
        }
        let includes = config.include.clone();
        repos.insert(path.to_path_buf(), config);
        includes.iter().try_for_each(|inc_path| {
            let inc_path = path.join(inc_path);
            let inc_file = Self::find_config(&inc_path)
                .ok_or_else(|| Error::MissingConfig(inc_path.to_path_buf()))?;
            if !repos.contains_key(&inc_path) {
                Self::load_config(vc_version, &inc_path, &inc_file, repos)?;
            }
            Ok(())
        })
    }

    pub(crate) fn artifacts(&self) -> Result<Artifacts<'_>> {
        let artifacts = self
            .0
            .iter()
            .flat_map(|(repo_path, repo)| {
                repo.artifacts.iter().map(|(artifact_id, artifact)| {
                    Ok((
                        artifact_id,
                        artifact.with_info(repo_path, artifact_id, repo)?,
                    ))
                })
            })
            .collect::<Result<BTreeMap<_, _>>>()?;
        Ok(Artifacts(artifacts))
    }
}

impl<'a> Artifacts<'a> {
    pub(crate) fn keys(&self) -> impl Iterator<Item = &ArtifactId> {
        self.0.keys().copied()
    }

    pub(crate) fn iter(&self) -> impl Iterator<Item = (&'a ArtifactId, &ArtifactWithInfo<'a>)> {
        self.0.iter().map(|(k, v)| (*k, v))
    }

    pub(crate) fn get(&self, id: &ArtifactId) -> Result<&ArtifactWithInfo<'a>> {
        self.try_get(id)
            .ok_or_else(|| Error::MissingArtifact(id.clone()))
    }

    pub(crate) fn try_get(&self, id: &ArtifactId) -> Option<&ArtifactWithInfo<'a>> {
        self.0.get(id)
    }

    pub(crate) fn reverse_deps(&self) -> BTreeMap<&ArtifactId, BTreeSet<&ArtifactId>> {
        self.0
            .iter()
            .flat_map(|(artifact_id, artifact_info)| {
                artifact_info
                    .artifact
                    .dependencies()
                    .into_iter()
                    .map(|dep_id| (dep_id, *artifact_id))
            })
            .fold(
                BTreeMap::<_, BTreeSet<_>>::new(),
                |mut req, (dep_id, req_id)| {
                    req.entry(dep_id).or_default().insert(req_id);
                    req
                },
            )
    }

    pub(crate) fn root_ids(&self) -> impl Iterator<Item = &ArtifactId> {
        let required_by = self.reverse_deps();
        self.0
            .keys()
            .copied()
            .filter(move |artifact_id| !required_by.contains_key(artifact_id))
    }

    pub(crate) fn roots(&self) -> impl Iterator<Item = (&ArtifactId, &ArtifactWithInfo)> {
        let required_by = self.reverse_deps();
        self.0.iter().filter_map(move |(artifact_id, artifact)| {
            (!required_by.contains_key(*artifact_id)).then_some((*artifact_id, artifact))
        })
    }
}

impl Artifact {
    pub(crate) fn workspace(&self) -> Option<&WorkspaceId> {
        match &self.source {
            Source::Helm(_) => None,
            Source::Npm(_) => None,
            Source::Cargo(s) => s.workspace.as_ref(),
        }
    }

    pub(crate) fn dependencies(&self) -> Vec<&ArtifactId> {
        match &self.source {
            Source::Helm(s) => s.dependencies(),
            Source::Npm(s) => s.dependencies(),
            Source::Cargo(s) => s.dependencies(),
        }
    }

    pub(crate) fn with_info<'a>(
        &'a self,
        repo: &'a Path,
        artifact_id: &ArtifactId,
        config: &'a Config,
    ) -> Result<ArtifactWithInfo<'a>> {
        let info = self.load_info(repo)?;
        let dirty = is_dirty(repo, self.source.paths())?;

        let tag_prefix = format!("{artifact_id}{}", config.tag_separator);
        let tag = format!("{tag_prefix}{}", info.version);
        let tagged = is_tagged(&tag, repo, self.source.paths())?;
        let tags = get_version_tags(&tag_prefix, repo)?;

        let mut root = repo.to_path_buf();
        self.source.root().components().for_each(|c| match c {
            std::path::Component::CurDir => {}
            std::path::Component::ParentDir => {
                root.pop();
            }
            c => root.push(c),
        });

        let git = root
            .ancestors()
            .find(|p| p.join(".git").exists())
            .map(|p| p.to_path_buf())
            .ok_or_else(|| Error::MissingGitDir(artifact_id.clone()))?;

        let workspace = match &self.source {
            Source::Cargo(pkg) => match &pkg.workspace {
                Some(workspace_id) => {
                    let workspace = config
                        .workspaces
                        .get(workspace_id)
                        .ok_or_else(|| Error::MissingWorkspace(workspace_id.clone()))?;
                    (workspace.r#type == WorkspaceType::Cargo)
                        .then_some(())
                        .ok_or_else(|| {
                            Error::InvalidWorkspaceType(self.source.get_type(), workspace.r#type)
                        })?;
                    Some(workspace)
                }
                None => None,
            },
            _ => None,
        };

        Ok(ArtifactWithInfo {
            repo,
            root,
            git,
            artifact: self,
            info,
            dirty,
            tagged,
            tags,
            tag,
            tag_prefix,
            workspace,
        })
    }

    pub(crate) fn load_info(&self, repo: &Path) -> Result<ArtifactInfo> {
        match &self.source {
            Source::Helm(s) => s.load_info(repo),
            Source::Npm(s) => s.load_info(repo),
            Source::Cargo(s) => s.load_info(repo),
        }
    }

    fn updated_paths(&self, updated: bool, deps_updated: bool, repo: &Path) -> Vec<PathBuf> {
        match &self.source {
            Source::Helm(s) => s.updated_paths(updated, deps_updated, repo),
            Source::Npm(s) => s.updated_paths(updated, deps_updated, repo),
            Source::Cargo(s) => s.updated_paths(updated, deps_updated, repo),
        }
    }

    fn update(
        &self,
        new_version: Option<&Version>,
        dep_updates: Option<&Updates>,
        repo: &Path,
    ) -> Result<()> {
        match &self.source {
            Source::Helm(s) => s.update(new_version, dep_updates, repo),
            Source::Npm(s) => s.update(new_version, dep_updates, repo),
            Source::Cargo(s) => s.update(new_version, dep_updates, repo),
        }
    }
}

impl ArtifactWithInfo<'_> {
    pub(crate) fn update(
        &self,
        new_version: Option<&Version>,
        dep_updates: Option<&Updates>,
    ) -> Result<()> {
        self.artifact.update(new_version, dep_updates, self.repo)
    }

    pub(crate) fn deps_updated(&self, updates: &Updates) -> bool {
        self.artifact
            .dependencies()
            .iter()
            .any(|id| updates.contains_key(id))
    }

    pub(crate) fn commit_paths(&self, updated: bool, deps_updated: bool) -> Vec<PathBuf> {
        self.artifact
            .updated_paths(updated, deps_updated, self.repo)
    }
}

impl Source {
    pub(crate) fn get_type(&self) -> SourceType {
        match self {
            Source::Helm(_) => SourceType::Helm,
            Source::Npm(_) => SourceType::Npm,
            Source::Cargo(_) => SourceType::Cargo,
        }
    }

    fn root(&self) -> &Path {
        match self {
            Source::Helm(s) => &s.root,
            Source::Npm(s) => &s.root,
            Source::Cargo(s) => &s.root,
        }
    }

    fn paths(&self) -> impl Iterator<Item = &Path> {
        match self {
            Source::Helm(s) => std::iter::once(&s.root).chain(s.paths.iter()),
            Source::Npm(s) => std::iter::once(&s.root).chain(s.paths.iter()),
            Source::Cargo(s) => std::iter::once(&s.root).chain(s.paths.iter()),
        }
        .map(|p| p.as_path())
    }
}

impl HelmChart {
    fn dependencies(&self) -> Vec<&ArtifactId> {
        self.dependencies
            .iter()
            .map(|d| &d.artifact)
            .chain(self.values.iter().map(|v| &v.artifact))
            .collect()
    }

    fn load_info(&self, repo: &Path) -> Result<ArtifactInfo> {
        let root = repo.join(&self.root);
        let chart = read_yaml::<HelmChartChart>(&root.join("Chart.yaml"))?;
        let values = read_yaml::<HelmChartValues>(&root.join("values.yaml"))?;

        Ok(ArtifactInfo {
            version: chart.version.clone(),
            dependencies: self
                .dependencies
                .iter()
                .map(|d| (&d.artifact, chart.get_version(&d.name)))
                .chain(
                    self.values
                        .iter()
                        .map(|v| (&v.artifact, values.get_version(&v.path))),
                )
                .collect(),
        })
    }

    fn update(
        &self,
        new_version: Option<&Version>,
        dep_updates: Option<&Updates>,
        repo: &Path,
    ) -> Result<()> {
        let root = repo.join(&self.root);
        let dep_updated = dep_updates.is_some_and(|updates| {
            self.dependencies
                .iter()
                .any(|dep| updates.contains_key(&dep.artifact))
        });
        let val_updated = dep_updates.is_some_and(|updates| {
            self.values
                .iter()
                .any(|val| updates.contains_key(&val.artifact))
        });

        if new_version.is_some() || dep_updated {
            let chart_path = root.join("Chart.yaml");
            let mut chart = nondestructive::yaml::from_slice(
                std::fs::read(&chart_path).map_err(|e| Error::ReadFile(chart_path.clone(), e))?,
            )
            .map_err(|e| Error::DecodeYamlEdit(chart_path.clone(), e))?;

            if let Some(new_version) = new_version {
                chart
                    .as_mut()
                    .as_mapping_mut()
                    .ok_or_else(|| Error::MissingKeyStatic(chart_path.clone(), "version"))?
                    .get_mut("version")
                    .ok_or_else(|| Error::MissingKeyStatic(chart_path.clone(), "version"))?
                    .set_string(new_version.to_string());
            }

            if dep_updated {
                let updates = dep_updates.unwrap();
                let mut deps = chart.as_mut();
                let mut deps = deps
                    .as_mapping_mut()
                    .ok_or_else(|| Error::MissingKeyStatic(chart_path.clone(), "dependencies"))?;
                let mut deps = deps
                    .get_mut("dependencies")
                    .ok_or_else(|| Error::MissingKeyStatic(chart_path.clone(), "dependencies"))?;
                let mut deps = deps
                    .as_sequence_mut()
                    .ok_or_else(|| Error::MissingKeyStatic(chart_path.clone(), "dependencies"))?;

                // Missing SequenceMut::iter_mut()!?
                for i in 0.. {
                    let Some(mut dep) = deps.get_mut(i) else {
                        break;
                    };
                    let mut map = dep.as_mapping_mut().ok_or_else(|| {
                        Error::MissingKey(chart_path.clone(), format!("dependencies[{i}].name"))
                    })?;
                    let name = map
                        .as_ref()
                        .get("name")
                        .and_then(|v| v.as_str())
                        .ok_or_else(|| {
                            Error::MissingKey(chart_path.clone(), format!("dependencies[{i}].name"))
                        })?;
                    log::debug!("Checking dep {name}");
                    if let Some(version) = self
                        .dependencies
                        .iter()
                        .find(|d| d.name == name)
                        .and_then(|d| updates.get(&d.artifact))
                    {
                        log::debug!("Updating dep {name} to {version}");
                        map.get_mut("version")
                            .ok_or_else(|| {
                                Error::MissingKey(
                                    chart_path.clone(),
                                    format!("dependencies[{i}].version"),
                                )
                            })?
                            .set_string(version.to_string());
                    }
                }
            }

            std::fs::write(&chart_path, chart.to_string())
                .map_err(|e| Error::WriteFile(chart_path.clone(), e))?;
        }

        if val_updated {
            let values_path = root.join("values.yaml");
            let mut values = nondestructive::yaml::from_slice(
                std::fs::read(&values_path).map_err(|e| Error::ReadFile(values_path.clone(), e))?,
            )
            .map_err(|e| Error::DecodeYamlEdit(values_path.clone(), e))?;

            let updates = dep_updates.unwrap();
            self.values
                .iter()
                .filter_map(|v| Some((v, updates.get(&v.artifact)?)))
                .try_for_each(|(v, new_version)| {
                    v.path
                        .split('.')
                        .try_fold(values.as_mut(), |values, c| {
                            values.into_mapping_mut()?.get_into_mut(c)
                        })
                        .ok_or_else(|| Error::MissingKey(values_path.clone(), v.path.clone()))?
                        .set_string(new_version.to_string());
                    Ok(())
                })?;

            std::fs::write(&values_path, values.to_string())
                .map_err(|e| Error::WriteFile(values_path.clone(), e))?;
        }

        Ok(())
    }

    fn updated_paths(&self, updated: bool, deps_updated: bool, repo: &Path) -> Vec<PathBuf> {
        let root = repo.join(&self.root);
        updated
            .then(|| root.join("Chart.yaml"))
            .into_iter()
            .chain(deps_updated.then(|| root.join("values.yaml")))
            .collect()
    }
}

#[derive(Serialize, Deserialize)]
struct HelmChartChart {
    name: String,
    version: Version,
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    dependencies: Vec<HelmChartDependency>,
    #[serde(flatten)]
    rest: serde_yaml::Mapping,
}

#[derive(Serialize, Deserialize)]
struct HelmChartDependency {
    name: String,
    version: String,
    #[serde(flatten)]
    rest: serde_yaml::Mapping,
}

#[derive(Serialize, Deserialize)]
struct HelmChartValues(serde_yaml::Value);

impl HelmChartChart {
    fn get_version(&self, name: &str) -> Option<Version> {
        let dep = self.dependencies.iter().find(|dep| dep.name == name)?;
        Version::from_str(&dep.version).ok()
    }
}

impl HelmChartValues {
    fn get_version(&self, path: &str) -> Option<Version> {
        let v = path
            .split('.')
            .try_fold(&self.0, |v, p| v.as_mapping()?.get(p))?;
        Version::from_str(v.as_str()?).ok()
    }
}

impl NpmPackage {
    fn dependencies(&self) -> Vec<&ArtifactId> {
        self.dependencies.iter().map(|v| &v.artifact).collect()
    }

    fn load_info(&self, repo: &Path) -> Result<ArtifactInfo> {
        let pkg = read_json::<NpmPackageJson>(&repo.join(&self.root).join("package.json"))?;
        Ok(ArtifactInfo {
            version: pkg.version,
            dependencies: self
                .dependencies
                .iter()
                .map(|v| {
                    (
                        &v.artifact,
                        pkg.dependencies.get(&v.name).and_then(|v| v.version()),
                    )
                })
                .collect(),
        })
    }

    fn update(
        &self,
        new_version: Option<&Version>,
        dep_updates: Option<&Updates>,
        repo: &Path,
    ) -> Result<()> {
        let pkg_path = repo.join(&self.root).join("package.json");
        // let lock_path = repo.join("package-lock.json");

        /* Read package.json and package-lock.json */

        let mut pkg = read_json::<serde_json::Value>(&pkg_path)?;
        // let mut lock = read_json::<serde_json::Value>(&lock_path)?;

        /* Update package.json */

        let obj = pkg
            .as_object_mut()
            .ok_or_else(|| Error::MissingKeyStatic(pkg_path.clone(), "version"))?;

        if let Some(new_version) = new_version {
            *obj.get_mut("version")
                .ok_or_else(|| Error::MissingKeyStatic(pkg_path.clone(), "version"))? =
                serde_json::Value::String(new_version.to_string());
        }

        if let Some(updates) = dep_updates {
            for key in ["dependencies", "devDependencies"] {
                if let Some(deps) = obj.get_mut(key).and_then(|v| v.as_object_mut()) {
                    self.dependencies
                        .iter()
                        .filter_map(|dep| Some((dep, updates.get(&dep.artifact)?)))
                        .for_each(|(dep, new_version)| {
                            if let Some(v) = deps.get_mut(&dep.name) {
                                *v = serde_json::Value::String(format!("={new_version}"));
                            }
                        });
                }
            }
        }

        /* Update package-lock.json */

        // let pkg_name = obj
        //     .get("name")
        //     .and_then(|v| v.as_str())
        //     .ok_or_else(|| Error::MissingKeyStatic(pkg_path.clone(), "name"))?;
        // let pkg_updates = new_version
        //     .iter()
        //     .map(|v| (pkg_name, *v))
        //     .chain(self.dependencies.iter().filter_map(|dep| {
        //         let new_version = dep_updates?.get(&dep.artifact)?;
        //         Some((dep.name.as_str(), new_version))
        //     }))
        //     .collect::<BTreeMap<_, _>>();

        // lock.get_mut("packages")
        //     .and_then(|v| v.as_object_mut())
        //     .ok_or_else(|| Error::MissingKeyStatic(lock_path.clone(), "packages"))?
        //     .iter_mut()
        //     .try_for_each(|(path, v)| {
        //         let obj = v.as_object_mut().ok_or_else(|| {
        //             Error::MissingKey(lock_path.clone(), format!("packages.{path}"))
        //         })?;
        //         let name = obj.get("name").and_then(|v| v.as_str()).unwrap_or_else(|| {
        //             match path.rfind('/') {
        //                 Some(i) => match path[..i].rfind('/') {
        //                     Some(i) if path[(i + 1)..].starts_with('@') => &path[(i + 1)..],
        //                     _ => &path[(i + 1)..],
        //                 },
        //                 None => path,
        //             }
        //         });

        //         if let Some(new_version) = pkg_updates.get(name) {
        //             let version = obj.get_mut("version").ok_or_else(|| {
        //                 Error::MissingKey(lock_path.clone(), format!("packages.{path}.version"))
        //             })?;
        //             *version = serde_json::Value::String(new_version.to_string());
        //         }

        //         if let Some(deps) = obj.get_mut("dependencies").and_then(|v| v.as_object_mut()) {
        //             deps.iter_mut().try_for_each(|(name, version)| {
        //                 if let Some(new_version) = pkg_updates.get(name.as_str()) {
        //                     *version = serde_json::Value::String(new_version.to_string());
        //                 }
        //                 Ok(())
        //             })?
        //         }

        //         Ok(())
        //     })?;

        /* Write package.json and package-lock.json */

        let mut pkg = serde_json::to_string_pretty(&pkg).unwrap();
        pkg.push('\n');

        // let mut lock = serde_json::to_string_pretty(&lock).unwrap();
        // lock.push('\n');

        std::fs::write(&pkg_path, pkg).map_err(|e| Error::WriteFile(pkg_path.clone(), e))?;
        // std::fs::write(&lock_path, lock).map_err(|e| Error::WriteFile(lock_path.clone(), e))?;
        Ok(())
    }

    fn updated_paths(&self, _updated: bool, _deps_updated: bool, repo: &Path) -> Vec<PathBuf> {
        let pkg = repo.join(&self.root).join("package.json");
        vec![pkg]
    }
}

#[derive(Deserialize)]
struct NpmPackageJson {
    version: Version,
    #[serde(default)]
    dependencies: BTreeMap<String, NpmPackageJsonReq>,
    // #[serde(flatten)]
    // rest: serde_json::Map<String, serde_json::Value>,
}

#[derive(DeserializeFromStr)]
enum NpmPackageJsonReq {
    Version(VersionReq),
    File(#[allow(unused)] PathBuf),
}

impl FromStr for NpmPackageJsonReq {
    type Err = Error;
    fn from_str(s: &str) -> Result<Self> {
        if let Some(path) = s.strip_prefix("file:") {
            Ok(Self::File(PathBuf::from(path)))
        } else {
            Ok(Self::Version(VersionReq::from_str(s)?))
        }
    }
}

impl NpmPackageJsonReq {
    fn version(&self) -> Option<Version> {
        match self {
            NpmPackageJsonReq::Version(v) => v.as_exact().cloned(),
            _ => None,
        }
    }
}

impl CargoPackage {
    fn dependencies(&self) -> Vec<&ArtifactId> {
        self.dependencies.iter().map(|v| &v.artifact).collect()
    }

    fn load_info(&self, repo: &Path) -> Result<ArtifactInfo> {
        let pkg = read_toml::<CargoToml>(&repo.join(&self.root).join("Cargo.toml"))?;
        Ok(ArtifactInfo {
            version: pkg.package.version,
            dependencies: self
                .dependencies
                .iter()
                .map(|v| {
                    (
                        &v.artifact,
                        pkg.dependencies.get(&v.name).and_then(|d| d.version()),
                    )
                })
                .collect(),
        })
    }

    fn update(
        &self,
        new_version: Option<&Version>,
        dep_updates: Option<&Updates>,
        repo: &Path,
    ) -> Result<()> {
        fn with_key<'a, F: FnOnce(&mut toml_edit::Item) -> EditResult<'a, ()>>(
            table: &mut toml_edit::Table,
            key: &'a str,
            f: F,
        ) -> EditResult<'a, ()> {
            f(table
                .get_mut(key)
                .ok_or_else(|| EditError::new(EditErrorKind::MissingKey).at(key))?)
            .map_err(|e| e.at(key))
        }

        fn with_inline_key<'a, F: FnOnce(&mut toml_edit::Value) -> EditResult<'a, ()>>(
            table: &mut toml_edit::InlineTable,
            key: &'a str,
            f: F,
        ) -> EditResult<'a, ()> {
            f(table
                .get_mut(key)
                .ok_or_else(|| EditError::new(EditErrorKind::MissingKey).at(key))?)
            .map_err(|e| e.at(key))
        }

        fn get_table_mut(item: &mut toml_edit::Item) -> EditResult<'static, &mut toml_edit::Table> {
            match item {
                toml_edit::Item::Table(t) => Ok(t),
                _ => Err(EditError::new(EditErrorKind::UnexpectedType(
                    item.type_name(),
                ))),
            }
        }

        fn get_value_mut(item: &mut toml_edit::Item) -> EditResult<'static, &mut toml_edit::Value> {
            match item {
                toml_edit::Item::Value(v) => Ok(v),
                _ => Err(EditError::new(EditErrorKind::UnexpectedType(
                    item.type_name(),
                ))),
            }
        }

        fn set_string(item: &mut toml_edit::Value, new_value: String) -> EditResult<'static, ()> {
            let value = match item {
                toml_edit::Value::String(s) => Ok(s),
                _ => Err(EditError::new(EditErrorKind::UnexpectedType(
                    item.type_name(),
                ))),
            }?;
            let old_value = std::mem::replace(value, toml_edit::Formatted::new(new_value));
            value.decor_mut().clone_from(old_value.decor());
            Ok(())
        }

        /* Read Cargo.toml and Cargo.lock */

        let pkg_path = repo.join(&self.root).join("Cargo.toml");
        // let lock_path = repo.join("Cargo.lock");

        let mut pkg = std::fs::read_to_string(&pkg_path)
            .map_err(|e| Error::ReadFile(pkg_path.clone(), e))?
            .parse::<toml_edit::Document>()
            .map_err(|e| Error::DecodeTomlEdit(pkg_path.clone(), e))?;

        // let mut lock = std::fs::read_to_string(&lock_path)
        //     .map_err(|e| Error::ReadFile(lock_path.clone(), e))?
        //     .parse::<toml_edit::Document>()
        //     .map_err(|e| Error::DecodeTomlEdit(lock_path.clone(), e))?;

        /* Update Cargo.toml */

        if let Some(new_version) = new_version {
            with_key(pkg.as_table_mut(), "package", |v| {
                with_key(get_table_mut(v)?, "version", |v| {
                    set_string(get_value_mut(v)?, new_version.to_string())
                })
            })
            .map_err(|e| e.into_error(pkg_path.clone()))?;
        }

        if let Some(updates) = dep_updates {
            self.dependencies
                .iter()
                .filter_map(|dep| Some((dep, updates.get(&dep.artifact)?)))
                .try_for_each(|(dep, new_version)| {
                    with_key(pkg.as_table_mut(), "dependencies", |v| {
                        with_key(get_table_mut(v)?, &dep.name.0, |v| match v {
                            toml_edit::Item::Value(toml_edit::Value::InlineTable(t)) => {
                                with_inline_key(t, "version", |v| {
                                    set_string(v, format!("={new_version}"))
                                })
                            }
                            toml_edit::Item::Value(_) => {
                                set_string(get_value_mut(v)?, format!("={new_version}"))
                            }
                            v => Err(EditError::new(EditErrorKind::UnexpectedType(v.type_name()))),
                        })
                    })
                })
                .map_err(|e| e.into_error(pkg_path.clone()))?;
        }

        /* Update Cargo.lock */

        // let crate_name = pkg
        //     .as_table()
        //     .get("package")
        //     .ok_or_else(|| EditError::new(EditErrorKind::MissingKey))
        //     .and_then(|item| {
        //         item.get("name")
        //             .ok_or_else(|| EditError::new(EditErrorKind::MissingKey))
        //             .and_then(|v| {
        //                 v.as_str().ok_or_else(|| {
        //                     EditError::new(EditErrorKind::UnexpectedType(v.type_name()))
        //                 })
        //             })
        //             .map_err(|e| e.at("name"))
        //     })
        //     .map_err(|e| e.at("package").into_error(pkg_path.clone()))?;

        // let pkg_updates = new_version
        //     .map(|v| (crate_name, v))
        //     .into_iter()
        //     .chain(dep_updates.iter().flat_map(|updates| {
        //         self.dependencies
        //             .iter()
        //             .filter_map(|dep| Some((dep.name.0.as_str(), updates.get(&dep.artifact)?)))
        //     }))
        //     .collect::<BTreeMap<_, _>>();

        // with_key(&mut lock, "package", |item| match item {
        //     toml_edit::Item::ArrayOfTables(tables) => tables.iter_mut().try_for_each(|table| {
        //         let pkg_name = table
        //             .get("name")
        //             .and_then(|v| v.as_str())
        //             .ok_or_else(|| EditError::new(EditErrorKind::MissingKey).at("name"))?;
        //         if let Some((pkg_name, pkg_version)) = pkg_updates.get_key_value(pkg_name) {
        //             with_key(table, "version", |item| {
        //                 get_value_mut(item).and_then(|v| set_string(v, pkg_version.to_string()))
        //             })
        //             .map_err(|e| e.at(pkg_name))?;
        //         }
        //         Ok(())
        //     }),
        //     _ => Err(EditError::new(EditErrorKind::UnexpectedType(
        //         item.type_name(),
        //     ))),
        // })
        // .map_err(|e| e.into_error(lock_path.clone()))?;

        /* Write updated files. */

        std::fs::write(&pkg_path, pkg.to_string())
            .map_err(|e| Error::WriteFile(pkg_path.clone(), e))?;
        // std::fs::write(&lock_path, lock.to_string())
        //     .map_err(|e| Error::WriteFile(lock_path.clone(), e))?;
        Ok(())
    }

    fn updated_paths(&self, _updated: bool, _deps_updated: bool, repo: &Path) -> Vec<PathBuf> {
        let pkg = repo.join(&self.root).join("Cargo.toml");
        vec![pkg]
    }
}

#[derive(Deserialize)]
struct CargoToml {
    package: CargoTomlPackage,
    #[serde(default)]
    dependencies: BTreeMap<CrateName, CargoTomlDependency>,
}

#[derive(Deserialize)]
struct CargoTomlPackage {
    //name: String,
    version: Version,
}

#[derive(Deserialize)]
#[serde(untagged)]
enum CargoTomlDependency {
    Short(VersionReq),
    Long { version: Option<VersionReq> },
}

impl CargoTomlDependency {
    fn version(&self) -> Option<Version> {
        match self {
            CargoTomlDependency::Short(r) => r.as_exact().cloned(),
            CargoTomlDependency::Long { version } => {
                version.as_ref().and_then(|v| v.as_exact().cloned())
            }
        }
    }
}

impl HookCmd {
    pub(crate) fn working_dir_for_artifact(&self, artifact: &ArtifactWithInfo) -> Result<PathBuf> {
        match &self.working_dir {
            None | Some(WorkingDir::Source) => Ok(artifact.root.clone()),
            Some(WorkingDir::Repo) => Ok(artifact.repo.to_path_buf()),
            Some(WorkingDir::Workspace) => match &artifact.workspace {
                Some(ws) => Ok(artifact.repo.join(&ws.root)),
                None => Err(Error::InvalidWorkingDir(WorkingDir::Workspace)),
            },
            Some(WorkingDir::Custom(path)) => Ok(artifact.repo.join(path)),
        }
    }

    pub(crate) fn working_dir_for_workspace(
        &self,
        repo: &Path,
        workspace: &Workspace,
    ) -> Result<PathBuf> {
        match &self.working_dir {
            None | Some(WorkingDir::Workspace) => Ok(repo.join(&workspace.root)),
            Some(WorkingDir::Repo) => Ok(repo.to_path_buf()),
            Some(WorkingDir::Custom(path)) => Ok(repo.join(path)),
            Some(WorkingDir::Source) => Err(Error::InvalidWorkingDir(WorkingDir::Source)),
        }
    }
}

impl Display for WorkingDir {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            WorkingDir::Repo => write!(f, "repo"),
            WorkingDir::Source => write!(f, "source"),
            WorkingDir::Workspace => write!(f, "workspace"),
            WorkingDir::Custom(path) => write!(f, "custom: {}", path.display()),
        }
    }
}

fn read_yaml<T: DeserializeOwned>(path: &Path) -> Result<T> {
    let data = std::fs::read(path).map_err(|e| Error::ReadFile(path.to_path_buf(), e))?;
    let value =
        serde_yaml::from_slice::<T>(&data).map_err(|e| Error::DecodeYaml(path.to_path_buf(), e))?;
    Ok(value)
}

fn read_toml<T: DeserializeOwned>(path: &Path) -> Result<T> {
    let data = std::fs::read_to_string(path).map_err(|e| Error::ReadFile(path.to_path_buf(), e))?;
    let value = toml::from_str::<T>(&data).map_err(|e| Error::DecodeToml(path.to_path_buf(), e))?;
    Ok(value)
}

fn read_json<T: DeserializeOwned>(path: &Path) -> Result<T> {
    let data = std::fs::read(path).map_err(|e| Error::ReadFile(path.to_path_buf(), e))?;
    let value =
        serde_json::from_slice::<T>(&data).map_err(|e| Error::DecodeJson(path.to_path_buf(), e))?;
    Ok(value)
}
