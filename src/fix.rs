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
    process::ExitCode,
};

use crate::{
    bump::{
        ensure_editable, ensure_max_env, run_updates, show_updates, verify_updates, BumpContext,
        UpdatePlan,
    },
    config::{RepoArgs, Repos},
    error::{Error, Result},
    version::Environment,
};

/// Fix invalid version relationships in the artifact tree.
#[derive(clap::Args, Clone)]
pub(crate) struct FixArgs {
    #[clap(flatten)]
    repos: RepoArgs,
    /// Show what would be done, without actually modifying anything.
    #[clap(long, short = 'n')]
    dry_run: bool,
    /// Commit changes.
    #[clap(long, short)]
    commit: bool,
}

pub(crate) fn fix(args: &FixArgs) -> Result<ExitCode> {
    let config = Repos::load(&args.repos)?;
    let artifacts = config.artifacts()?;

    let selected = BTreeSet::new();
    let descendants = BTreeSet::new();

    let ctx = BumpContext {
        artifacts: &artifacts,
        selected: &selected,
        descendants: &descendants,
        bump: None,
        env: Environment::Development,
    };

    let updates = {
        let mut updates = BTreeMap::new();
        ensure_max_env(&ctx, &mut updates)?;
        ensure_editable(&ctx, &mut updates)?;
        updates
    };

    let update_plan = UpdatePlan::new(&artifacts, &updates, args.commit, None)?;

    verify_updates(&artifacts, &updates).map_err(Error::Verify)?;

    if args.dry_run {
        show_updates(&artifacts, &update_plan);
    } else {
        run_updates(&artifacts, &update_plan)?;
    }

    Ok(ExitCode::SUCCESS)
}
