// SPDX-FileCopyrightText: 2026 Alexandra de Wit
//
// SPDX-License-Identifier: MIT

#[cfg(unix)]
fn platform() -> &'static str { ecluse_e2e_unix::value() }
#[cfg(windows)]
fn platform() -> &'static str { ecluse_e2e_windows::value() }
#[cfg(not(any(unix, windows)))]
fn platform() -> &'static str { "other" }

#[cfg(feature = "optional")]
fn optional() -> &'static str { ecluse_e2e_extra::value() }
#[cfg(not(feature = "optional"))]
fn optional() -> &'static str { "disabled" }

pub fn value() -> String {
    format!("{}|{}|{}|{}|{}|{}|{}", renamed_leaf::value(),
        ecluse_e2e_left::value(), ecluse_e2e_right::value(), optional(),
        platform(), env!("ECLUSE_OWNED_BUILD"), ecluse_e2e_macro::marker!())
}
