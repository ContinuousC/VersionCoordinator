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

use std::collections::BTreeSet;
use std::process::ExitCode;

use crate::config::{ArtifactId, ArtifactWithInfo, Artifacts, RepoArgs, Repos};
use crate::error::{Problem, Result};
use crate::git::TagState;
use crate::version::Prerelease;

/// Check properties of the artifact tree.
#[derive(clap::Args, Clone)]
pub(crate) struct VerifyArgs {
    #[clap(flatten)]
    repos: RepoArgs,
    /// Only verify specified artifact(s) and their dependencies.
    artifacts: Vec<ArtifactId>,
    /// Check that artifacts are clean (committed).
    #[clap(long, short)]
    clean: bool,
    /// Check that artifacts are tagged.
    #[clap(long, short = 'T')]
    tagged: bool,
    /// Check that artifacts are taggable.
    #[clap(long, short = 't')]
    taggable: bool,
}

pub(crate) fn verify(args: &VerifyArgs) -> Result<ExitCode> {
    let config = Repos::load(&args.repos)?;
    let artifacts = config.artifacts()?;

    let mut problems = Vec::new();
    let mut seen = BTreeSet::new();

    if args.artifacts.is_empty() {
        artifacts.iter().for_each(|(artifact_id, artifact)| {
            verify_artifact(
                artifact_id,
                artifact,
                &artifacts,
                args,
                &mut problems,
                &mut seen,
            )
        });
    } else {
        args.artifacts.iter().try_for_each(|artifact_id| {
            let artifact = artifacts.get(artifact_id)?;
            verify_artifact(
                artifact_id,
                artifact,
                &artifacts,
                args,
                &mut problems,
                &mut seen,
            );
            Result::Ok(())
        })?;
    }

    if problems.is_empty() {
        println!("Verification successful - no problems where found");
        Ok(ExitCode::SUCCESS)
    } else {
        problems.iter().for_each(|problem| {
            println!("{problem}");
        });
        Ok(ExitCode::FAILURE)
    }
}

fn verify_artifact<'a>(
    artifact_id: &'a ArtifactId,
    artifact: &ArtifactWithInfo<'a>,
    artifacts: &Artifacts<'a>,
    args: &VerifyArgs,
    problems: &mut Vec<Problem>,
    seen: &mut BTreeSet<&'a ArtifactId>,
) {
    if !seen.insert(artifact_id) {
        return;
    }

    if (args.clean || args.taggable) && artifact.dirty {
        (problems.push(Problem::Dirty(artifact_id.clone())));
    }

    if args.tagged && artifact.tagged == TagState::Untagged {
        problems.push(Problem::Untagged(artifact_id.clone()));
    }

    if (args.tagged || args.taggable) && artifact.tagged == TagState::Modified {
        problems.push(Problem::TagModified(artifact_id.clone()));
    }

    if args.taggable && artifact.info.version.pre == Prerelease::Development {
        problems.push(Problem::DevVersion(artifact_id.clone()));
    }

    for (dependency_id, req_version) in &artifact.info.dependencies {
        let Some(dependency) = artifacts.try_get(dependency_id) else {
            problems.push(Problem::MissingArtifact((*dependency_id).clone()));
            continue;
        };

        if let Some(req) = req_version {
            if req != &dependency.info.version {
                problems.push(Problem::WrongVersion(
                    artifact_id.clone(),
                    (*dependency_id).clone(),
                    Box::new(req.clone()),
                    Box::new(dependency.info.version.clone()),
                ));
            }
        } else {
            problems.push(Problem::MissingExactDepVersion(
                artifact_id.clone(),
                (*dependency_id).clone(),
            ));
        }

        verify_artifact(dependency_id, dependency, artifacts, args, problems, seen);
    }
}
