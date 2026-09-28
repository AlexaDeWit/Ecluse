// SPDX-FileCopyrightText: 2026 Alexandra de Wit
//
// SPDX-License-Identifier: MIT

/*
 * Upstream's lexer at 537a43a7 under renamed symbols, so the fuzz harness links it beside the
 * vendored lexer.
 */
#define lex_json      upstream_lex_json
#define handle_number upstream_handle_number
#define handle_string upstream_handle_string
#include "lexer-537a43a7.c"
