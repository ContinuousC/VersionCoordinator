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

use std::collections::{BTreeMap, BTreeSet};
use std::fmt::Write;
use std::path::{Path, PathBuf};
use std::process::{Command, ExitCode};

use crate::config::{
    ArtifactId, ArtifactWithInfo, Artifacts, PostTagHook, PreCommitHook, PreTagHook, RepoArgs,
    Repos,
};
use crate::error::{Error, Problem, Result, VerifyResult};
use crate::git::{self, TagState};
use crate::version::{Environment, MainVersion, Prerelease, Version};

/// Update versions in the artifact tree.
#[derive(clap::Args, Clone)]
pub(crate) struct BumpArgs {
    #[clap(flatten)]
    repos: RepoArgs,
    /// Show what would be done, without actually modifying anything.
    #[clap(long, short = 'n')]
    dry_run: bool,
    /// Select all artifacts.
    #[clap(long, short = 'a')]
    all: bool,
    /// Include ancestors of selected artifacts.
    #[clap(long, short = 'P', conflicts_with = "all")]
    ancestors: bool,
    /// Commit changes.
    #[clap(long, short)]
    commit: bool,
    /// Tag taggable versions.
    #[clap(long, short, requires = "commit")]
    tag: bool,
    #[clap(flatten)]
    bump: BumpArg,
    #[clap(flatten)]
    env: EnvArg,
    /// Artifacts to modify.
    #[clap(required_unless_present = "all")]
    artifacts: Vec<ArtifactId>,
}

#[derive(clap::Args, Clone)]
#[group(multiple = false)]
struct BumpArg {
    /// Increase patch version.
    #[clap(long, short = 'p')]
    patch: bool,
    /// Increase minor version.
    #[clap(long, short = 'm')]
    minor: bool,
    /// Increase major version.
    #[clap(long, short = 'M')]
    major: bool,
}

impl BumpArg {
    fn to_bump(&self) -> Option<Bump> {
        if self.patch {
            Some(Bump::Patch)
        } else if self.minor {
            Some(Bump::Minor)
        } else if self.major {
            Some(Bump::Major)
        } else {
            None
        }
    }
}

#[derive(clap::Args, Clone)]
#[group(multiple = false)]
struct EnvArg {
    /// Bump to testing version.
    #[clap(long, short = 'T', visible_alias = "test", visible_alias = "tst")]
    testing: bool,
    /// Bump to acceptance version.
    #[clap(long, short = 'A', visible_alias = "accept", visible_alias = "acc")]
    acceptance: bool,
    /// Bump to release version.
    #[clap(long, short = 'R', visible_alias = "prod", visible_alias = "prd")]
    release: bool,
}

impl EnvArg {
    fn to_env(&self) -> Environment {
        if self.release {
            Environment::Release
        } else if self.acceptance {
            Environment::Acceptance
        } else if self.testing {
            Environment::Testing
        } else {
            Environment::Development
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub(crate) enum Bump {
    Patch,
    Minor,
    Major,
}

pub(crate) struct BumpContext<'a> {
    pub(crate) artifacts: &'a Artifacts<'a>,
    pub(crate) selected: &'a BTreeSet<&'a ArtifactId>,
    pub(crate) descendants: &'a BTreeSet<&'a ArtifactId>,
    pub(crate) bump: Option<Bump>,
    pub(crate) env: Environment,
}

pub(crate) type Updates<'a> = BTreeMap<&'a ArtifactId, Version>;

#[derive(Debug)]
pub(crate) struct UpdatePlan<'a>(Vec<UpdateStage<'a>>);

#[derive(Default, Debug)]
struct UpdateStage<'a> {
    updates: Updates<'a>,
    commits: Option<Vec<(&'a Path, CommitInfo<'a>)>>,
    tags: Option<BTreeMap<&'a ArtifactId, TagInfo<'a>>>,
}

#[derive(Default, Debug)]
struct CommitInfo<'a> {
    modified: Vec<PathBuf>,
    pre_commit_hooks: Vec<HookInfo<&'a PreCommitHook>>,
}

#[derive(Debug)]
struct TagInfo<'a> {
    repo: &'a Path,
    version: Version,
    tag: String,
    pre_tag_hooks: Vec<HookInfo<&'a PreTagHook>>,
    post_tag_hooks: Vec<HookInfo<&'a PostTagHook>>,
}

#[derive(Debug)]
struct HookInfo<T> {
    working_dir: PathBuf,
    hook: T,
}

pub(crate) fn bump(args: &BumpArgs) -> Result<ExitCode> {
    let config = Repos::load(&args.repos)?;
    let artifacts = config.artifacts()?;

    let selected = if args.all {
        artifacts.keys().collect::<BTreeSet<_>>()
    } else if args.ancestors {
        let selected = args.artifacts.iter().collect::<BTreeSet<_>>();
        get_ancestors(&artifacts, &selected)?
    } else {
        args.artifacts.iter().collect::<BTreeSet<_>>()
    };

    let descendants = get_strict_descendants(&artifacts, &selected)?;

    let updates = get_updates(&BumpContext {
        artifacts: &artifacts,
        selected: &selected,
        descendants: &descendants,
        bump: args.bump.to_bump(),
        env: args.env.to_env(),
    })?;

    verify_updates(&artifacts, &updates).map_err(Error::Verify)?;

    let update_plan = UpdatePlan::new(
        &artifacts,
        &updates,
        args.commit,
        args.tag.then_some((&selected, &descendants)),
    )?;

    if args.dry_run {
        show_updates(&artifacts, &update_plan);
    } else {
        run_updates(&artifacts, &update_plan)?;
    }

    Ok(ExitCode::SUCCESS)
}

fn get_ancestors<'a>(
    artifacts: &Artifacts<'a>,
    selected: &BTreeSet<&'a ArtifactId>,
) -> Result<BTreeSet<&'a ArtifactId>> {
    fn is_ancestor(
        artifacts: &Artifacts,
        selected: &BTreeSet<&ArtifactId>,
        id: &ArtifactId,
        artifact: &ArtifactWithInfo,
    ) -> Result<bool> {
        if selected.contains(id) {
            Ok(true)
        } else {
            for dep_id in artifact.info.dependencies.keys().copied() {
                let artifact = artifacts.get(dep_id)?;
                if is_ancestor(artifacts, selected, dep_id, artifact)? {
                    return Ok(true);
                }
            }
            Ok(false)
        }
    }

    artifacts
        .iter()
        .filter_map(
            |(id, artifact)| match is_ancestor(artifacts, selected, id, artifact) {
                Ok(r) => r.then_some(Ok(id)),
                Err(e) => Some(Err(e)),
            },
        )
        .collect()
}

fn get_strict_descendants<'a>(
    artifacts: &Artifacts<'a>,
    selected: &BTreeSet<&'a ArtifactId>,
) -> Result<BTreeSet<&'a ArtifactId>> {
    fn add_strict_descendants<'a>(
        artifacts: &Artifacts<'a>,
        selected: &BTreeSet<&ArtifactId>,
        ids: impl IntoIterator<Item = &'a ArtifactId>,
        result: &mut BTreeSet<&'a ArtifactId>,
    ) -> Result<()> {
        ids.into_iter().try_for_each(|id| {
            let artifact = artifacts.get(id)?;
            let unseen = selected.contains(id) || result.insert(id);
            if unseen {
                add_strict_descendants(
                    artifacts,
                    selected,
                    artifact.info.dependencies.keys().copied(),
                    result,
                )?
            }
            Ok(())
        })
    }
    let mut deps = BTreeSet::new();
    add_strict_descendants(artifacts, selected, selected.iter().copied(), &mut deps)?;
    Ok(deps)
}

fn get_updates<'a>(ctx: &BumpContext<'a>) -> Result<Updates<'a>> {
    let mut updates = BTreeMap::new();

    bump_versions(ctx, &mut updates)?;
    ensure_max_env(ctx, &mut updates)?;
    ensure_editable(ctx, &mut updates)?;

    Ok(updates)
}

fn bump_versions<'a>(ctx: &BumpContext<'a>, updates: &mut Updates<'a>) -> Result<()> {
    ctx.selected.iter().copied().try_for_each(|id| {
        let artifact = ctx.artifacts.get(id)?;
        let (version, tagged) = get_updated_version(id, artifact, updates);
        if let Some(new_version) = bump_version(ctx.bump, ctx.env, &artifact.tags, version, tagged)
        {
            updates.insert(id, new_version);
        }
        Ok(())
    })?;
    ctx.descendants.iter().copied().try_for_each(|id| {
        let artifact = ctx.artifacts.get(id)?;
        let (version, tagged) = get_updated_version(id, artifact, updates);
        if let Some(new_version) = min_env_version(ctx.env, &artifact.tags, version, tagged) {
            updates.insert(id, new_version);
        }
        Ok(())
    })
}

pub(crate) fn ensure_max_env<'a>(ctx: &BumpContext<'a>, updates: &mut Updates<'a>) -> Result<()> {
    fn run<'a>(
        ctx: &BumpContext<'a>,
        ids: impl Iterator<Item = &'a ArtifactId>,
        updates: &mut Updates<'a>,
        max_envs: &mut BTreeMap<&ArtifactId, Environment>,
    ) -> Result<Environment> {
        ids.map(|id| {
            if let Some(r) = max_envs.get(id) {
                Ok(*r)
            } else {
                let artifact = ctx.artifacts.get(id)?;
                let (version, tagged) = get_updated_version(id, artifact, updates);
                let version = version.clone();
                let deps = artifact.info.dependencies.keys().copied();
                let max_env = run(ctx, deps, updates, max_envs)?;
                let env = version.pre.environment();
                if env > max_env {
                    let min_env = if ctx.selected.contains(id) || ctx.descendants.contains(id) {
                        ctx.env
                    } else {
                        Environment::Development
                    };
                    if let Some(new_version) =
                        env_version(min_env, &artifact.tags, &version, tagged)
                    {
                        updates.insert(id, new_version);
                    }
                    Ok(min_env)
                } else {
                    Ok(env)
                }
            }
        })
        .try_fold(Environment::Release, |env, r| Ok(r?.min(env)))
    }

    let _ = run(ctx, ctx.artifacts.keys(), updates, &mut BTreeMap::new())?;
    Ok(())
}

pub(crate) fn ensure_editable<'a>(ctx: &BumpContext<'a>, updates: &mut Updates<'a>) -> Result<()> {
    fn run<'a>(
        ctx: &BumpContext<'a>,
        ids: impl Iterator<Item = &'a ArtifactId>,
        updates: &mut Updates<'a>,
        changed: &mut BTreeMap<&ArtifactId, bool>,
    ) -> Result<bool> {
        ids.map(|id| {
            if let Some(r) = changed.get(id) {
                Ok(*r)
            } else {
                let artifact = ctx.artifacts.get(id)?;
                let (version, _) = get_updated_version(id, artifact, updates);
                let version = version.clone();
                let deps = artifact.info.dependencies.keys().copied();
                let updated = version != artifact.info.version;
                let deps_updated = run(ctx, deps, updates, changed)?;
                if deps_updated && !updated {
                    let min_env = if ctx.selected.contains(id) || ctx.descendants.contains(id) {
                        ctx.env
                    } else {
                        Environment::Development
                    };
                    if let Some(new_version) =
                        env_version(min_env, &artifact.tags, &version, TagState::Modified)
                    {
                        updates.insert(id, new_version);
                    }
                }
                Ok(updated || deps_updated)
            }
        })
        .try_fold(false, |changed, r| Ok(r? || changed))
    }

    let _ = run(ctx, ctx.artifacts.keys(), updates, &mut BTreeMap::new())?;
    Ok(())
}

fn get_updated_version<'a>(
    id: &ArtifactId,
    artifact: &'a ArtifactWithInfo<'a>,
    updates: &'a Updates<'a>,
) -> (&'a Version, TagState) {
    match updates.get(id) {
        Some(version) => (version, TagState::Untagged),
        None => {
            let tagged = match (artifact.tagged, artifact.dirty) {
                (TagState::Tagged, true) => TagState::Modified,
                (tagged, _) => tagged,
            };
            (&artifact.info.version, tagged)
        }
    }
}

fn bump_version(
    bump: Option<Bump>,
    env: Environment,
    tags: &BTreeSet<Version>,
    version: &Version,
    tagged: TagState,
) -> Option<Version> {
    if let Some(bump) = bump {
        let new_version = match bump {
            Bump::Major => Version {
                main: MainVersion {
                    major: tags.iter().map(|v| v.main.major).max().map_or(0, |n| n + 1),
                    minor: 0,
                    patch: 0,
                },
                pre: env.prerelease1(),
            },
            Bump::Minor => Version {
                main: MainVersion {
                    major: version.main.major,
                    minor: tags
                        .iter()
                        .filter(|v| v.main.major == version.main.major)
                        .map(|v| v.main.minor)
                        .max()
                        .map_or(0, |n| n + 1),
                    patch: 0,
                },
                pre: env.prerelease1(),
            },
            Bump::Patch => Version {
                main: MainVersion {
                    major: version.main.major,
                    minor: version.main.minor,
                    patch: tags
                        .iter()
                        .filter(|v| {
                            v.main.major == version.main.major && v.main.minor == version.main.minor
                        })
                        .map(|v| v.main.patch)
                        .max()
                        .map_or(0, |n| n + 1),
                },
                pre: env.prerelease1(),
            },
        };
        (&new_version != version).then_some(new_version)
    } else {
        env_version(env, tags, version, tagged)
    }
}

fn min_env_version(
    env: Environment,
    tags: &BTreeSet<Version>,
    version: &Version,
    tagged: TagState,
) -> Option<Version> {
    if version.pre.environment() < env || tagged == TagState::Modified {
        env_version(env, tags, version, tagged)
    } else {
        None
    }
}

fn env_version(
    env: Environment,
    tags: &BTreeSet<Version>,
    version: &Version,
    tagged: TagState,
) -> Option<Version> {
    if version.pre.environment() == env && tagged != TagState::Modified {
        None
    } else {
        let main = if tags
            .iter()
            .any(|v| v.main == version.main && v.pre == Prerelease::Release)
        {
            MainVersion {
                major: version.main.major,
                minor: version.main.minor,
                patch: tags
                    .iter()
                    .filter(|v| {
                        v.main.major == version.main.major && v.main.minor == version.main.minor
                    })
                    .map(|v| v.main.patch)
                    .max()
                    .map_or(0, |n| n + 1),
            }
        } else {
            version.main.clone()
        };
        let new_version = match env {
            Environment::Development => Version {
                main,
                pre: Prerelease::Development,
            },
            Environment::Testing => Version {
                main,
                pre: Prerelease::Testing(
                    tags.iter()
                        .filter(|v| v.main == version.main)
                        .filter_map(|v| v.pre.as_testing())
                        .max()
                        .map_or(1, |n| n + 1),
                ),
            },
            Environment::Acceptance => Version {
                main,
                pre: Prerelease::Acceptance(
                    tags.iter()
                        .filter(|v| v.main == version.main)
                        .filter_map(|v| v.pre.as_acceptance())
                        .max()
                        .map_or(1, |n| n + 1),
                ),
            },
            Environment::Release => Version {
                main,
                pre: Prerelease::Release,
            },
        };
        (&new_version != version).then_some(new_version)
    }
}

fn get_commits<'a>(
    artifacts: &'a Artifacts,
    updates: &Updates<'a>,
    pre_commit_hooks: BTreeMap<&'a Path, Vec<HookInfo<&'a PreCommitHook>>>,
) -> Vec<(&'a Path, CommitInfo<'a>)> {
    struct GitTree<'a>(BTreeMap<&'a Path, GitTree<'a>>);

    let mut git_tree = GitTree(BTreeMap::new());

    artifacts.iter().for_each(|(_, artifact)| {
        let path = artifact.git.as_path();
        let mut node = &mut git_tree.0;
        loop {
            if let Some((p, _)) = node.iter().find(|(p, _)| path.starts_with(p)) {
                if *p == path {
                    break;
                } else {
                    node = &mut node.get_mut(*p).unwrap().0;
                }
            } else {
                // TODO: use extract_if when stabilized.
                let mut children = BTreeMap::new();
                node.split_off(path).into_iter().for_each(|(p, t)| {
                    if p.starts_with(path) {
                        children.insert(p, t);
                    } else {
                        node.insert(p, t);
                    }
                });
                node.insert(path, GitTree(children));
                break;
            }
        }
    });

    let mut info = artifacts
        .iter()
        .filter_map(|(id, artifact)| {
            let git = artifact.git.as_path();
            let updated = updates.contains_key(id);
            let deps_updated = artifact.deps_updated(updates);
            (updated || deps_updated).then(|| (git, artifact.commit_paths(updated, deps_updated)))
        })
        .chain(pre_commit_hooks.iter().flat_map(|(repo, hooks)| {
            hooks.iter().map(|hook| {
                (
                    *repo,
                    hook.hook
                        .files
                        .iter()
                        .map(|path| hook.working_dir.join(path))
                        .collect(),
                )
            })
        }))
        .fold(
            BTreeMap::<_, CommitInfo>::new(),
            |mut map, (repo, paths)| {
                map.entry(repo)
                    .or_default()
                    .modified
                    .extend(paths.into_iter().map(|p| {
                        let p = p.strip_prefix(repo).expect("failed to strip path prefix");
                        let mut r = PathBuf::new();
                        p.components()
                            .try_for_each(|c| match c {
                                std::path::Component::CurDir => Some(()),
                                std::path::Component::ParentDir => r.pop().then_some(()),
                                std::path::Component::Normal(_) => {
                                    r.push(c);
                                    Some(())
                                }
                                _ => None,
                            })
                            .expect("failed to normalize path");
                        r
                    }));
                map
            },
        );

    pre_commit_hooks.into_iter().for_each(|(repo, hooks)| {
        info.entry(repo)
            .or_insert_with(CommitInfo::default)
            .pre_commit_hooks
            .extend(hooks)
    });

    fn add_commits<'a>(
        repo: &'a Path,
        node: &GitTree<'a>,
        info: &mut BTreeMap<&Path, CommitInfo<'a>>,
        commits: &mut Vec<(&'a Path, CommitInfo<'a>)>,
    ) -> bool {
        let mut commit = info.remove(repo).unwrap_or_default();
        commit.modified.extend(
            node.0
                .iter()
                .filter(|(p, t)| add_commits(p, t, info, commits))
                .map(|(p, _)| p.strip_prefix(repo).unwrap().to_path_buf())
                .collect::<Vec<_>>(),
        );

        let modified = !commit.modified.is_empty();
        modified.then(|| commits.push((repo, commit)));
        modified
    }

    let mut commits = Vec::new();
    git_tree.0.iter().for_each(|(path, node)| {
        let _ = add_commits(path, node, &mut info, &mut commits);
    });
    assert!(info.is_empty());
    commits
}

fn get_tags<'a>(
    artifacts: &'a Artifacts,
    updates: &Updates,
    selected: &BTreeSet<&'a ArtifactId>,
    descendants: &BTreeSet<&'a ArtifactId>,
) -> Vec<(&'a ArtifactId, Version, &'a Path, String)> {
    selected
        .iter()
        .chain(descendants.iter())
        .filter_map(|id| {
            let artifact = artifacts.try_get(id)?;
            let version = if let Some(version) = updates.get(id) {
                version
            } else {
                (artifact.tagged == TagState::Untagged).then_some(())?;
                &artifact.info.version
            };
            (version.pre != Prerelease::Development).then_some(())?;
            let tag = format!("{}{}", artifact.tag_prefix, version);
            Some((*id, version.clone(), artifact.root.as_path(), tag))
        })
        .collect::<Vec<_>>()
}

pub(crate) fn verify_updates(artifacts: &Artifacts, updates: &Updates) -> VerifyResult<()> {
    artifacts
        .iter()
        .filter(|(id, artifact)| updates.contains_key(id) || artifact.deps_updated(updates))
        .try_for_each(|(id, artifact)| {
            (!artifact.dirty)
                .then_some(())
                .ok_or_else(|| Problem::Dirty((*id).clone()))
        })
}

impl<'a> UpdatePlan<'a> {
    pub(crate) fn new(
        artifacts: &'a Artifacts,
        updates: &'a Updates<'a>,
        commit: bool,
        tag: Option<(&BTreeSet<&'a ArtifactId>, &BTreeSet<&'a ArtifactId>)>,
    ) -> Result<Self> {
        let mut plan = Vec::new();
        let mut missing_tags = tag
            .map(|(selected, descendants)| {
                get_tags(artifacts, updates, selected, descendants)
                    .into_iter()
                    .map(|(id, a, b, c)| (id, (a, b, c)))
                    .collect::<BTreeMap<_, _>>()
            })
            .unwrap_or_default();
        let mut todo = artifacts
            .iter()
            .filter_map(|(artifact_id, artifact)| {
                let version = updates.get(artifact_id);
                (version.is_some() || missing_tags.contains_key(artifact_id)).then_some((
                    artifact_id,
                    artifact,
                    version,
                ))
            })
            .map(|(artifact_id, artifact, version)| {
                let deps = artifact
                    .info
                    .dependencies
                    .keys()
                    .copied()
                    .collect::<BTreeSet<_>>();
                (artifact_id, (version, deps))
            })
            .collect::<BTreeMap<_, _>>();

        while !todo.is_empty() {
            let todo1 = todo.keys().copied().collect::<BTreeSet<_>>();
            let mut updates = BTreeMap::new();
            let mut tags = BTreeMap::new();

            todo.retain(|id, (update, deps)| {
                if deps.is_disjoint(&todo1) {
                    if let Some(version) = update {
                        updates.insert(*id, version.clone());
                    }
                    if let Some((version, path, tag)) = missing_tags.remove(id) {
                        tags.insert(
                            *id,
                            TagInfo {
                                repo: path,
                                version,
                                tag,
                                pre_tag_hooks: Vec::new(),
                                post_tag_hooks: Vec::new(),
                            },
                        );
                    }
                    false
                } else {
                    true
                }
            });

            let workspaces = updates
                .keys()
                .filter_map(|artifact_id| {
                    let artifact = artifacts.get(artifact_id).unwrap();
                    let workspace_id = artifact.artifact.workspace()?;
                    let workspace = artifact.workspace?;
                    Some((
                        workspace_id,
                        (artifact.git.as_path(), artifact.repo, workspace),
                    ))
                })
                .collect::<BTreeMap<_, _>>();

            let commits = commit
                .then(|| {
                    let pre_commit_hooks = updates
                        .keys()
                        .flat_map(|artifact_id| {
                            let artifact = artifacts.get(artifact_id).unwrap();
                            artifact.artifact.pre_commit.iter().map(|hook| {
                                let working_dir =
                                    hook.command.working_dir_for_artifact(artifact)?;
                                Ok((artifact.git.as_path(), HookInfo { working_dir, hook }))
                            })
                        })
                        .chain(workspaces.values().flat_map(|(git, repo, workspace)| {
                            workspace.pre_commit.iter().map(|hook| {
                                let working_dir =
                                    hook.command.working_dir_for_workspace(repo, workspace)?;
                                Ok((*git, HookInfo { working_dir, hook }))
                            })
                        }))
                        .try_fold(BTreeMap::<_, Vec<_>>::new(), |mut map, res| {
                            let (repo, hook) = res?;
                            map.entry(repo).or_default().push(hook);
                            Ok(map)
                        })?;
                    Ok(get_commits(artifacts, &updates, pre_commit_hooks))
                })
                .transpose()?;

            let tags = tag
                .is_some()
                .then(|| {
                    tags.iter_mut().try_for_each(|(artifact_id, info)| {
                        let artifact = artifacts.get(artifact_id).unwrap();
                        artifact.artifact.pre_tag.iter().try_for_each(|hook| {
                            let working_dir = hook.command.working_dir_for_artifact(artifact)?;
                            info.pre_tag_hooks.push(HookInfo { working_dir, hook });
                            Ok(())
                        })?;
                        artifact.artifact.post_tag.iter().try_for_each(|hook| {
                            let working_dir = hook.command.working_dir_for_artifact(artifact)?;
                            info.post_tag_hooks.push(HookInfo { working_dir, hook });
                            Ok(())
                        })
                    })?;
                    Ok(tags)
                })
                .transpose()?;

            plan.push(UpdateStage {
                updates,
                commits,
                tags,
            });
        }

        Ok(Self(plan))
    }
}

pub(crate) fn run_updates(artifacts: &Artifacts, updates: &UpdatePlan) -> Result<()> {
    updates.0.iter().enumerate().try_for_each(|(i, stage)| {
        log::info!("stage {i}: running updates");

        let _updated = artifacts
            .iter()
            .filter_map(|(id, artifact)| {
                let new_version = stage.updates.get(id);
                let dep_updates = artifact
                    .deps_updated(&stage.updates)
                    .then_some(&stage.updates);
                (new_version.is_some() || dep_updates.is_some()).then(|| {
                    artifact.update(new_version, dep_updates)?;
                    Ok((id, artifact))
                })
            })
            .collect::<Result<BTreeMap<_, _>>>()?;

        if let Some(commits) = &stage.commits {
            let commit_msg = get_commit_msg(artifacts, &stage.updates);
            commits.iter().try_for_each(|(repo, commit)| {
                log::info!("stage {i}: running pre-commit hooks");
                commit.pre_commit_hooks.iter().try_for_each(|info| {
                    log::info!(
                        "stage {i}: running pre-commit hook: {}: {}",
                        info.working_dir.display(),
                        info.hook.command.run
                    );
                    let exit_status = Command::new("/bin/bash")
                        .arg("-c")
                        .arg(&info.hook.command.run)
                        .current_dir(&info.working_dir)
                        .status()
                        .map_err(Error::RunHook)?;
                    exit_status
                        .success()
                        .then_some(())
                        .ok_or_else(|| Error::HookExitStatus(exit_status))
                })?;

                log::info!("stage {i}: running commit");
                git::commit(
                    repo,
                    commit.modified.iter().map(|p| p.as_path()),
                    &commit_msg,
                )
            })?;
        }

        if let Some(tags) = &stage.tags {
            tags.iter().try_for_each(|(_, tag)| {
                log::info!("stage {i}: running pre-tag hooks");
                tag.pre_tag_hooks.iter().try_for_each(|info| {
                    let exit_status = Command::new("/bin/bash")
                        .arg("-c")
                        .arg(&info.hook.command.run)
                        .env("VC_ARTIFACT_TAG", &tag.tag)
                        .env("VC_ARTIFACT_VERSION", tag.version.to_string())
                        .current_dir(&info.working_dir)
                        .status()
                        .map_err(Error::RunHook)?;
                    exit_status
                        .success()
                        .then_some(())
                        .ok_or_else(|| Error::HookExitStatus(exit_status))
                })?;

                log::info!("stage {i}: setting tag");
                git::set_tag(tag.repo, &tag.tag)?;

                log::info!("stage {i}: running post-tag hooks");
                tag.post_tag_hooks.iter().try_for_each(|info| {
                    let exit_status = Command::new("/bin/bash")
                        .arg("-c")
                        .arg(&info.hook.command.run)
                        .env("VC_ARTIFACT_TAG", &tag.tag)
                        .env("VC_ARTIFACT_VERSION", tag.version.to_string())
                        .current_dir(&info.working_dir)
                        .status()
                        .map_err(Error::RunHook)?;
                    exit_status
                        .success()
                        .then_some(())
                        .ok_or_else(|| Error::HookExitStatus(exit_status))
                })
            })?;
        }

        Ok(())
    })
}

// fn save_commit_msgs(
//     commits: Vec<(&Path, Vec<PathBuf>)>,
//     artifacts: &Artifacts,
//     updates: &Updates,
// ) -> Result<()> {
//     let commit_msg = get_commit_msg(artifacts, updates);
//     commits
//         .iter()
//         .try_for_each(|(repo, _)| git::save_commit_msg(repo, &commit_msg))
// }

fn get_commit_msg(artifacts: &Artifacts, updates: &Updates) -> String {
    let mut commit_msg = String::new();
    writeln!(commit_msg, "Versions bumped by version-coordinator.").unwrap();
    writeln!(commit_msg).unwrap();
    updates.iter().for_each(|(id, new_version)| {
        let artifact = artifacts.get(id).unwrap();
        writeln!(
            commit_msg,
            "- {id}: {} -> {}",
            artifact.info.version, new_version
        )
        .unwrap();
    });
    commit_msg
}

pub(crate) fn show_updates(artifacts: &Artifacts, updates: &UpdatePlan) {
    updates.0.iter().enumerate().for_each(|(i, stage)| {
        println!("\n*** Stage {i} ***");

        if !stage.updates.is_empty() {
            println!("\nUpdates:");
            stage.updates.iter().for_each(|(id, new_version)| {
                let artifact = artifacts.get(id).unwrap();
                println!("- {id}: {} -> {}", artifact.info.version, new_version)
            });
        }

        if let Some(commits) = &stage.commits {
            if !commits.is_empty() {
                if commits
                    .iter()
                    .any(|(_, commit)| !commit.pre_commit_hooks.is_empty())
                {
                    println!("\nPre-commit hooks:");
                    commits
                        .iter()
                        .flat_map(|(_, commit)| &commit.pre_commit_hooks)
                        .for_each(|info| {
                            println!(
                                "- {}: {}",
                                info.working_dir.display(),
                                info.hook.command.run
                            )
                        });
                }

                println!("\nCommits:");
                commits.iter().for_each(|(repo, info)| {
                    println!("- {}:", repo.display());
                    info.modified.iter().for_each(|path| {
                        println!("   - {}", path.display());
                    });
                });
            }
        }

        if let Some(tags) = &stage.tags {
            if tags.values().any(|tag| !tag.pre_tag_hooks.is_empty()) {
                println!("\nPre-tag hooks:");
                tags.values()
                    .flat_map(|tag| &tag.pre_tag_hooks)
                    .for_each(|info| {
                        println!(
                            "- {}: {}",
                            info.working_dir.display(),
                            info.hook.command.run
                        )
                    });
            }

            if !tags.is_empty() {
                println!("\nTags:");
                tags.values().for_each(|tag| {
                    println!("- {}: {}", tag.repo.display(), tag.tag);
                });
            }

            if tags.values().any(|tag| !tag.post_tag_hooks.is_empty()) {
                println!("\nPost-tag hooks:");
                tags.values()
                    .flat_map(|tag| &tag.post_tag_hooks)
                    .for_each(|info| {
                        println!(
                            "- {}: {}",
                            info.working_dir.display(),
                            info.hook.command.run
                        )
                    });
            }
        }
    });
}
