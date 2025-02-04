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

// trait VerifyExitStatus: Sized {
//     fn into_success<F, E>(self, err: F) -> std::result::Result<(), E>
//     where
//         F: FnOnce(Self) -> E;
// }

// impl VerifyExitStatus for ExitStatus {
//     fn into_success<F, E>(self, err: F) -> std::result::Result<(), E>
//     where
//         F: FnOnce(Self) -> E,
//     {
//         self.success().then_some(()).ok_or_else(|| err(self))
//     }
// }

use std::{collections::BTreeSet, path::Path, process::Command};

use parse_display_derive::Display;

use crate::{
    error::{Error, Result},
    version::Version,
};

#[derive(Display, PartialEq, Eq, Clone, Copy, Debug)]
#[display(style = "snake_case")]
pub(crate) enum TagState {
    /// The version tag was not found in the checked-out repo.
    Untagged,
    /// The version tag has been set and no relevant changes have been
    /// made since in the checked-out repo.
    Tagged,
    /// The version tag has been set, but the checked-out repo has
    /// changed since.
    Modified,
}

pub(crate) fn is_dirty<'a>(repo: &'a Path, paths: impl Iterator<Item = &'a Path>) -> Result<bool> {
    let output = Command::new("git")
        .current_dir(repo)
        .args(["diff", "--quiet", "HEAD", "--"])
        .args(paths)
        .output()
        .map_err(Error::Subprocess)?;
    match output.status.code() {
        Some(0) => Ok(false),
        Some(1) => Ok(true),
        _ => Err(Error::Git(output.status)),
    }
}

pub(crate) fn is_tagged<'a>(
    tag: &str,
    repo: &'a Path,
    paths: impl Iterator<Item = &'a Path>,
) -> Result<TagState> {
    let output = Command::new("git")
        .current_dir(repo)
        .args(["tag", "--list", tag])
        .output()
        .map_err(Error::Subprocess)?;
    output
        .status
        .success()
        .then_some(())
        .ok_or(Error::Git(output.status))?;

    if output.stdout.strip_prefix(tag.as_bytes()) == Some(b"\n") {
        let output = Command::new("git")
            .current_dir(repo)
            .args(["diff", "--quiet", tag, "HEAD", "--"])
            .args(paths)
            .output()
            .map_err(Error::Subprocess)?;
        match output.status.code() {
            Some(0) => Ok(TagState::Tagged),
            Some(1) => Ok(TagState::Modified),
            _ => Err(Error::Git(output.status)),
        }
    } else {
        Ok(TagState::Untagged)
    }
}

pub(crate) fn get_version_tags(prefix: &str, repo: &Path) -> Result<BTreeSet<Version>> {
    let output = Command::new("git")
        .current_dir(repo)
        .args(["tag", "--list", &format!("{prefix}*")])
        .output()
        .map_err(Error::Subprocess)?;
    output
        .status
        .success()
        .then_some(())
        .ok_or(Error::Git(output.status))?;
    Ok(std::str::from_utf8(&output.stdout)
        .map_err(Error::GitUnicode)?
        .lines()
        .filter_map(|s| match s.strip_prefix(prefix)?.parse() {
            Ok(v) => Some(v),
            Err(e) => {
                log::warn!("ingoring invalid version tag: {s}: {e}");
                None
            }
        })
        .collect())
}

pub(crate) fn set_tag(repo: &Path, tag: &str) -> Result<()> {
    let output = Command::new("git")
        .current_dir(repo)
        .args(["tag", "-a", tag, "-m", "tagged by version-coordinator"])
        .output()
        .map_err(Error::Subprocess)?;
    output
        .status
        .success()
        .then_some(())
        .ok_or(Error::Git(output.status))
}

pub(crate) fn commit<'a>(
    repo: &Path,
    paths: impl IntoIterator<Item = &'a Path>,
    msg: &str,
) -> Result<()> {
    let output = Command::new("git")
        .current_dir(repo)
        .args(["commit", "-m", msg, "--"])
        .args(paths)
        .output()
        .map_err(Error::Subprocess)?;
    output
        .status
        .success()
        .then_some(())
        .ok_or(Error::Git(output.status))
}

// pub(crate) fn save_commit_msg<'a>(repo: &Path, msg: &str) -> Result<()> {
//     let path = repo.join(".git/COMMIT_EDITMSG");
//     std::fs::write(&path, msg.as_bytes()).map_err(|e| Error::WriteFile(path, e))
// }
