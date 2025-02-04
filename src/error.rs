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

use std::{path::PathBuf, process::ExitStatus, str::Utf8Error};

use crate::{
    config::{ArtifactId, SourceType, WorkingDir, WorkspaceId, WorkspaceType},
    version::Version,
    VC_VERSION,
};

pub type Result<T> = std::result::Result<T, Error>;
pub type VerifyResult<T> = std::result::Result<T, Problem>;
pub type EditResult<'a, T> = std::result::Result<T, EditError<'a>>;

#[derive(thiserror::Error, Debug)]
pub enum Error {
    #[error("failed to find current working directory: {0}")]
    GetCwd(std::io::Error),
    #[error("no config file found in current directory or any of its ancestors")]
    MissingAncestorConfig,
    #[error("no config file found in {0}")]
    MissingConfig(PathBuf),
    #[error("failed to read file: {0}: {1}")]
    ReadFile(PathBuf, std::io::Error),
    #[error("failed to write file: {0}: {1}")]
    WriteFile(PathBuf, std::io::Error),
    #[error("failed to read file: {0}: {1}")]
    DecodeYaml(PathBuf, serde_yaml::Error),
    #[error("failed to read file: {0}: {1}")]
    DecodeYamlEdit(PathBuf, nondestructive::yaml::Error),
    #[error("failed to read file: {0}: {1}")]
    DecodeToml(PathBuf, toml::de::Error),
    #[error("failed to read file: {0}: {1}")]
    DecodeTomlEdit(PathBuf, toml_edit::TomlError),
    #[error("failed to read file: {0}: {1}")]
    DecodeJson(PathBuf, serde_json::Error),
    #[error(
        "{0}: requires vc version {1}, but we are version {}.\n\
		 Please update vc by running: cargo install --registry si version-coordinator",
        VC_VERSION
    )]
    IncompatibleConfigVersion(PathBuf, semver::VersionReq),
    #[error("failed to edit file: {0}: missing key {1}")]
    MissingKeyStatic(PathBuf, &'static str),
    #[error("failed to edit file: {0}: missing key {1}")]
    MissingKey(PathBuf, String),
    #[error("failed to edit file: {0}: {1}: {2}")]
    EditFile(PathBuf, String, EditErrorKind),
    #[error("failed to run subprocess: {0}")]
    Subprocess(std::io::Error),
    #[error("git returned non-zero: {0}")]
    Git(ExitStatus),
    #[error("invalid git output: {0}")]
    GitUnicode(Utf8Error),
    #[error("missing workspace: {0}")]
    MissingWorkspace(WorkspaceId),
    #[error("missing artifact: {0}")]
    MissingArtifact(ArtifactId),
    #[error("verification failed: {0}")]
    Verify(Problem),
    #[error("invalid version: {0}")]
    InvalidVersion(String),
    #[error("invalid pre-release: {0}")]
    InvalidPrerelease(String),
    #[error("invalid workspace type for source type {0}: {1}")]
    InvalidWorkspaceType(SourceType, WorkspaceType),
    // #[error("{0}: failed to run cargo update: {1}")]
    // CargoUpdate(ArtifactId, std::io::Error),
    // #[error("{0}: cargo update returned non-zero exit status: {1}")]
    // CargoUpdateStatus(ArtifactId, ExitStatus),
    // #[error("{0}: failed to run npm install: {1}")]
    // NpmInstall(ArtifactId, std::io::Error),
    // #[error("{0}: npm install returned non-zero exit status: {1}")]
    // NpmInstallStatus(ArtifactId, ExitStatus),
    #[error("invalid working dir: {0}")]
    InvalidWorkingDir(WorkingDir),
    #[error("missing git dir for {0}")]
    MissingGitDir(ArtifactId),
    #[error("hook returned non-zero exit status: {0}")]
    HookExitStatus(ExitStatus),
    #[error("failed to run hook: {0}")]
    RunHook(std::io::Error),
}

#[derive(thiserror::Error, Debug)]
pub(crate) enum Problem {
    #[error("missing artifact: {0}")]
    MissingArtifact(ArtifactId),
    #[error("{0}: missing dependency: {1}")]
    MissingDependency(ArtifactId, ArtifactId),
    #[error("{0}: missing (exact) version for {1}")]
    MissingExactDepVersion(ArtifactId, ArtifactId),
    #[error("{0}: found wrong version for {1} (required: {2}, actual: {3})")]
    WrongVersion(ArtifactId, ArtifactId, Box<Version>, Box<Version>),
    #[error("{0}: not committed")]
    Dirty(ArtifactId),
    #[error("{0}: untagged")]
    Untagged(ArtifactId),
    #[error("{0}: tagged and then modified")]
    TagModified(ArtifactId),
    #[error("{0}: has a development version")]
    DevVersion(ArtifactId),
}

pub(crate) struct EditError<'a>(Vec<&'a str>, EditErrorKind);

#[derive(thiserror::Error, Debug)]
pub(crate) enum EditErrorKind {
    #[error("missing key")]
    MissingKey,
    #[error("unexpected type: {0}")]
    UnexpectedType(&'static str),
}

impl<'a> EditError<'a> {
    pub(crate) fn new(kind: EditErrorKind) -> Self {
        Self(Vec::new(), kind)
    }

    pub(crate) fn into_error(mut self, path: PathBuf) -> Error {
        self.0.reverse();
        Error::EditFile(path, self.0.join("."), self.1)
    }

    pub(crate) fn at(mut self, key: &'a str) -> Self {
        self.0.push(key);
        self
    }
}
