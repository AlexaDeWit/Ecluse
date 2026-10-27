// SPDX-FileCopyrightText: 2026 Alexandra de Wit
//
// SPDX-License-Identifier: MIT

pub fn value() -> &'static str {
    if cfg!(feature = "extra") { "leaf-extra" }
    else if cfg!(feature = "base") { "leaf-base" }
    else { "leaf-bare" }
}
