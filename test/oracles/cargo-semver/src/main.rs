// SPDX-FileCopyrightText: 2026 Alexandra de Wit
// SPDX-License-Identifier: MIT

use semver::Version;
use std::cmp::Ordering;
use std::process::ExitCode;

fn main() -> ExitCode {
    let arguments: Vec<String> = std::env::args().skip(1).collect();
    match arguments.as_slice() {
        [mode, left, right] if mode == "compare" => {
            match (Version::parse(left), Version::parse(right)) {
                (Ok(a), Ok(b)) => println!(
                    "{}",
                    match a.cmp_precedence(&b) {
                        Ordering::Less => -1,
                        Ordering::Equal => 0,
                        Ordering::Greater => 1,
                    }
                ),
                _ => println!("invalid"),
            }
        }
        [mode, versions @ ..] if mode == "sort" => {
            let mut accepted: Vec<(Version, &str)> = versions
                .iter()
                .filter_map(|raw| Version::parse(raw).ok().map(|key| (key, raw.as_str())))
                .collect();
            accepted.sort_by(|(a, _), (b, _)| a.cmp_precedence(b));
            for (_, raw) in accepted {
                println!("{raw}");
            }
        }
        _ => {
            eprintln!("usage: cargo-semver-oracle compare VERSION VERSION | sort [VERSION ...]");
            return ExitCode::from(64);
        }
    }
    ExitCode::SUCCESS
}
