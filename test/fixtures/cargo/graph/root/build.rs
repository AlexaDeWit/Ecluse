// SPDX-FileCopyrightText: 2026 Alexandra de Wit
//
// SPDX-License-Identifier: MIT

fn main() {
    println!("cargo:rustc-env=ECLUSE_OWNED_BUILD={}", ecluse_e2e_build::value());
}
