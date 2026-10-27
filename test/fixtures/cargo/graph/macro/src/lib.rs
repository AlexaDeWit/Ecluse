// SPDX-FileCopyrightText: 2026 Alexandra de Wit
//
// SPDX-License-Identifier: MIT

extern crate proc_macro;

#[proc_macro]
pub fn marker(_: proc_macro::TokenStream) -> proc_macro::TokenStream {
    "\"owned-macro\"".parse().unwrap()
}
