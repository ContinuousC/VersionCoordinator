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

use std::{collections::BTreeMap, process::ExitCode};

use crate::config::{ArtifactId, ArtifactWithInfo, Artifacts, RepoArgs, Repos};
use crate::error::{Error, Problem, Result, VerifyResult};
use crate::git::{self, TagState};
use crate::version::Prerelease;

/// Tag artifact versions.
#[derive(clap::Args, Clone)]
pub(crate) struct TagArgs {
    #[clap(flatten)]
    repos: RepoArgs,
    /// Show what would be done, without actually modifying anything.
    #[clap(short = 'n', long)]
    dry_run: bool,
    /// Tag all taggable artifacts.
    #[clap(long, short = 'a')]
    all: bool,
    /// Artifacts to tag.
    #[clap(required_unless_present = "all")]
    artifacts: Vec<ArtifactId>,
}

pub(crate) fn tag(args: &TagArgs) -> Result<ExitCode> {
    let config = Repos::load(&args.repos)?;
    let artifacts = config.artifacts()?;
    let mut taggable = BTreeMap::new();

    if args.all {
        artifacts
            .roots()
            .try_for_each(|(root_id, root)| {
                check_taggable(root_id, root, &artifacts, &mut taggable)
            })
            .map_err(Error::Verify)?;
    } else {
        args.artifacts
            .iter()
            .try_for_each(|artifact_id| {
                let artifact = artifacts
                    .try_get(artifact_id)
                    .ok_or_else(|| Problem::MissingArtifact(artifact_id.clone()))?;
                check_taggable(artifact_id, artifact, &artifacts, &mut taggable)
            })
            .map_err(Error::Verify)?;
    }

    if args.dry_run {
        taggable
            .iter()
            .filter(|(_, artifact)| matches!(artifact.tagged, TagState::Untagged))
            .for_each(|(artifact_id, artifact)| {
                println!("{}: {}", artifact_id, artifact.tag,);
            });
    } else {
        taggable
            .iter()
            .filter(|(_, artifact)| matches!(artifact.tagged, TagState::Untagged))
            .try_for_each(|(_, artifact)| git::set_tag(artifact.repo, &artifact.tag))?;
    }

    Ok(ExitCode::SUCCESS)
}

fn check_taggable<'a>(
    artifact_id: &'a ArtifactId,
    artifact: &'a ArtifactWithInfo<'a>,
    artifacts: &'a Artifacts<'a>,
    taggable: &mut BTreeMap<&'a ArtifactId, &'a ArtifactWithInfo<'a>>,
) -> VerifyResult<()> {
    if taggable.insert(artifact_id, artifact).is_some() {
        Ok(())
    } else {
        (artifact.info.version.pre != Prerelease::Development)
            .then_some(())
            .ok_or_else(|| Problem::DevVersion(artifact_id.clone()))?;
        matches!(artifact.tagged, TagState::Untagged | TagState::Tagged)
            .then_some(())
            .ok_or_else(|| Problem::TagModified(artifact_id.clone()))?;
        (!artifact.dirty)
            .then_some(())
            .ok_or_else(|| Problem::Dirty(artifact_id.clone()))?;

        artifact
            .info
            .dependencies
            .iter()
            .try_for_each(|(&dependency_id, req_version)| {
                let req = req_version.as_ref().ok_or_else(|| {
                    Problem::MissingExactDepVersion(artifact_id.clone(), dependency_id.clone())
                })?;
                let dependency = artifacts.try_get(dependency_id).ok_or_else(|| {
                    Problem::MissingDependency(artifact_id.clone(), dependency_id.clone())
                })?;
                (req == &dependency.info.version)
                    .then_some(())
                    .ok_or_else(|| {
                        Problem::WrongVersion(
                            artifact_id.clone(),
                            dependency_id.clone(),
                            Box::new(req.clone()),
                            Box::new(dependency.info.version.clone()),
                        )
                    })?;
                check_taggable(dependency_id, dependency, artifacts, taggable)
            })
    }
}
