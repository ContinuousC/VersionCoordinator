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

use std::{fmt::Display, str::FromStr};

use serde_with::{DeserializeFromStr, SerializeDisplay};

use crate::error::{Error, Result};

#[derive(SerializeDisplay, DeserializeFromStr, Eq, PartialEq, Clone, Debug)]
pub(crate) enum VersionReq {
    Exact(Version),
    NotExact(String),
}

#[derive(SerializeDisplay, DeserializeFromStr, Eq, PartialEq, PartialOrd, Ord, Clone, Debug)]
pub(crate) struct Version {
    pub main: MainVersion,
    pub pre: Prerelease,
}

#[derive(Eq, PartialEq, PartialOrd, Ord, Clone, Debug)]
pub(crate) struct MainVersion {
    pub major: u64,
    pub minor: u64,
    pub patch: u64,
}

#[derive(
    SerializeDisplay, DeserializeFromStr, Eq, PartialEq, PartialOrd, Ord, Clone, Copy, Debug,
)]
pub(crate) enum Prerelease {
    Development,
    Testing(u64),
    Acceptance(u64),
    Release,
}

#[derive(Eq, PartialEq, PartialOrd, Ord, Clone, Copy, Debug)]
pub(crate) enum Environment {
    Development,
    Testing,
    Acceptance,
    Release,
}

// pub(crate) struct VersionState(BTreeMap<MainVersion, MainVersionState>);

// pub(crate) enum MainVersionState {
//     Released,
//     Unreleased { testing: u32, acceptance: u32 },
// }

impl FromStr for VersionReq {
    type Err = Error;
    fn from_str(s: &str) -> Result<Self> {
        match s.strip_prefix('=').unwrap_or(s).parse().ok() {
            Some(v) => Ok(Self::Exact(v)),
            None => Ok(Self::NotExact(s.to_string())),
        }
    }
}

impl FromStr for Version {
    type Err = Error;
    fn from_str(s: &str) -> Result<Self> {
        let err = || Error::InvalidVersion(s.to_string());
        let (major, s) = s
            .split_once('.')
            .and_then(|(n, s)| Some((n.parse().ok()?, s)))
            .ok_or_else(err)?;
        let (minor, s) = s
            .split_once('.')
            .and_then(|(n, s)| Some((n.parse().ok()?, s)))
            .ok_or_else(err)?;
        let (n, s) = s.split_once('-').unwrap_or((s, ""));
        let patch = n.parse().map_err(|_| err())?;
        let pre = s.parse()?;
        Ok(Self {
            main: MainVersion {
                major,
                minor,
                patch,
            },
            pre,
        })
    }
}

impl FromStr for Prerelease {
    type Err = Error;
    fn from_str(s: &str) -> Result<Self> {
        let err = || Error::InvalidPrerelease(s.to_string());
        if s.is_empty() {
            Ok(Self::Release)
        } else if s == "dev" {
            Ok(Self::Development)
        } else if let Some(n) = s.strip_prefix("tst.") {
            Ok(Self::Testing(n.parse().map_err(|_| err())?))
        } else if let Some(n) = s.strip_prefix("acc.") {
            Ok(Self::Acceptance(n.parse().map_err(|_| err())?))
        } else {
            Err(err())
        }
    }
}

impl Display for VersionReq {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Exact(v) => write!(f, "={v}"),
            Self::NotExact(s) => write!(f, "{s}"),
        }
    }
}

impl Display for Version {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.main)?;
        if self.pre != Prerelease::Release {
            write!(f, "-{}", self.pre)?;
        }
        Ok(())
    }
}

impl Display for MainVersion {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}.{}.{}", self.major, self.minor, self.patch)
    }
}

impl Display for Prerelease {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Development => write!(f, "dev"),
            Self::Testing(n) => write!(f, "tst.{n}"),
            Self::Acceptance(n) => write!(f, "acc.{n}"),
            Self::Release => Ok(()),
        }
    }
}

impl Display for Environment {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Development => write!(f, "development"),
            Self::Testing => write!(f, "testing"),
            Self::Acceptance => write!(f, "acceptance"),
            Self::Release => write!(f, "release"),
        }
    }
}

impl VersionReq {
    pub(crate) fn as_exact(&self) -> Option<&Version> {
        match self {
            VersionReq::Exact(v) => Some(v),
            VersionReq::NotExact(_) => None,
        }
    }
}

impl Prerelease {
    pub(crate) fn as_testing(&self) -> Option<u64> {
        match self {
            Self::Testing(n) => Some(*n),
            _ => None,
        }
    }

    pub(crate) fn as_acceptance(&self) -> Option<u64> {
        match self {
            Self::Acceptance(n) => Some(*n),
            _ => None,
        }
    }

    pub(crate) fn environment(&self) -> Environment {
        match self {
            Prerelease::Development => Environment::Development,
            Prerelease::Testing(_) => Environment::Testing,
            Prerelease::Acceptance(_) => Environment::Acceptance,
            Prerelease::Release => Environment::Release,
        }
    }
}

impl Environment {
    pub(crate) fn prerelease1(&self) -> Prerelease {
        match self {
            Environment::Development => Prerelease::Development,
            Environment::Testing => Prerelease::Testing(1),
            Environment::Acceptance => Prerelease::Acceptance(1),
            Environment::Release => Prerelease::Release,
        }
    }
}
