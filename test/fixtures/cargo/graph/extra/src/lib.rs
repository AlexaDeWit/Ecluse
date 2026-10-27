// SPDX-FileCopyrightText: 2026 Alexandra de Wit
//
// SPDX-License-Identifier: MIT

pub fn value() -> &'static str {
    if cfg!(feature = "default-marker") { "wrong-default" }
    else if cfg!(feature = "extra") { "optional-extra" }
    else { "optional-bare" }
}
