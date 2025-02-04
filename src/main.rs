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

mod bump;
mod config;
mod error;
mod fix;
mod git;
mod show;
mod tag;
mod verify;
mod version;

use std::process::ExitCode;

use clap::{CommandFactory, Parser};

use clap_complete::Generator;
use error::Result;
use log::LevelFilter;

static VC_VERSION: &str = env!("CARGO_PKG_VERSION");

#[derive(Parser)]
#[clap(author, version, about, name = "vc", bin_name = "vc")]
struct Args {
    #[clap(subcommand)]
    command: Subcommand,
}

#[derive(clap::Subcommand, Clone)]
enum Subcommand {
    Show(show::ShowArgs),
    Verify(verify::VerifyArgs),
    Bump(bump::BumpArgs),
    Fix(fix::FixArgs),
    Tag(tag::TagArgs),
    #[clap(hide = true)]
    Completion {
        shell: clap_complete::Shell,
    },
    #[clap(hide = true)]
    ManPages,
}

fn main() -> ExitCode {
    env_logger::builder()
        .filter_level(LevelFilter::Info)
        .parse_default_env()
        .init();
    let args = Args::parse();
    match run(&args) {
        Ok(c) => c,
        Err(e) => {
            log::error!("{e}");
            ExitCode::from(255)
        }
    }
}

fn run(args: &Args) -> Result<ExitCode> {
    match &args.command {
        Subcommand::Show(args) => show::show_tree(args),
        Subcommand::Verify(args) => verify::verify(args),
        Subcommand::Bump(args) => bump::bump(args),
        Subcommand::Fix(args) => fix::fix(args),
        Subcommand::Tag(args) => tag::tag(args),
        Subcommand::Completion { shell } => {
            let mut command = Args::command();
            command.build();
            shell.generate(&command, &mut std::io::stdout());
            Ok(ExitCode::SUCCESS)
        }
        Subcommand::ManPages => {
            fn render_page(cmd: &clap::Command, prefix: &str) {
                cmd.get_subcommands()
                    .filter(|cmd| cmd.get_name() != "help" && !cmd.is_hide_set())
                    .for_each(|sub| {
                        render_page(sub, &format!("{prefix}{}-", cmd.get_name()));
                    });

                let path = format!("{prefix}{}.1", cmd.get_name());
                clap_mangen::Man::new(cmd.clone().name(unsafe {
                    // Ugly hack to get clap_mangen to show the right binary name.
                    std::mem::transmute::<&str, &'static str>(cmd.get_bin_name().unwrap())
                }))
                .render(&mut std::fs::File::create(path).unwrap())
                .unwrap();
            }

            let mut command = Args::command();
            command.build();
            render_page(&command, "");

            Ok(ExitCode::SUCCESS)
        }
    }
}
