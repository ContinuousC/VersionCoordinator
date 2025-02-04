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

use crate::config::{ArtifactId, Artifacts, RepoArgs, Repos};
use crate::error::Result;
use crate::git::TagState;
use crate::version::Version;

/// Show the artifact tree.
#[derive(clap::Args, Clone)]
pub(crate) struct ShowArgs {
    #[clap(flatten)]
    repos: RepoArgs,
    /// Only show specified artifact(s).
    artifacts: Vec<ArtifactId>,
    /// Do not hide repeated trees.
    #[clap(short, long)]
    full: bool,
}

pub(crate) fn show_tree(args: &ShowArgs) -> Result<ExitCode> {
    let config = Repos::load(&args.repos)?;
    let artifacts = config.artifacts()?;
    let roots = if args.artifacts.is_empty() {
        artifacts.root_ids().collect::<BTreeSet<_>>()
    } else {
        args.artifacts
            .iter()
            .map(|id| {
                let _ = artifacts.get(id)?;
                Ok(id)
            })
            .collect::<Result<BTreeSet<_>>>()?
    };

    let mut shown = (!args.full).then(BTreeSet::new);
    roots
        .iter()
        .for_each(|root_id| show_tree_node(root_id, None, &artifacts, 0, &mut shown));

    Ok(ExitCode::SUCCESS)
}

fn show_tree_node<'a>(
    artifact_id: &'a ArtifactId,
    req_version: Option<&Version>,
    artifacts: &Artifacts<'a>,
    indent: usize,
    shown: &mut Option<BTreeSet<&'a ArtifactId>>,
) {
    (0..indent).for_each(|_| print!("   "));
    if let Some(artifact_info) = artifacts.try_get(artifact_id) {
        let show_deps = shown
            .as_mut()
            .map_or(true, |shown| shown.insert(artifact_id));

        print!("- {artifact_id}");
        if !artifact_info.info.dependencies.is_empty() && !show_deps {
            print!(" (*)")
        }
        print!(" ({}", &artifact_info.artifact.r#type);
        print!(", {}", artifact_info.root.display());
        print!(", version = {}", artifact_info.info.version);
        if let Some(req_version) = req_version {
            if req_version != &artifact_info.info.version {
                print!(", requested = \x1b[31m{}\x1b[0m", req_version);
            }
        }
        //if artifact_info.info.version.pre != Prerelease::Development {
        if artifact_info.tagged == TagState::Tagged {
            print!(", \x1b[32m{}\x1b[0m", artifact_info.tagged);
        } else {
            print!(", \x1b[33m{}\x1b[0m", artifact_info.tagged);
        }
        //}
        if artifact_info.dirty {
            print!(", \x1b[33mdirty\x1b[0m");
        } else {
            print!(", \x1b[32mclean\x1b[0m");
        }
        println!(")");

        if show_deps {
            artifact_info
                .info
                .dependencies
                .iter()
                .for_each(|(artifact_id, req_version)| {
                    show_tree_node(
                        artifact_id,
                        req_version.as_ref(),
                        artifacts,
                        indent + 1,
                        shown,
                    )
                })
        }
    } else {
        println!("- {artifact_id} (\x1b[31mnot found\x1b[0m)");
    }
}
