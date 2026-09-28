# Vendored json-stream

This folder holds the library modules and the C lexer of json-stream 0.4.6.1, which Écluse builds
as its internal `ecluse-json-stream` library. The registry readers in `ecluse-core` walk this lexer's
tokens, and the other JSON reads use its parser.

## Upstream

- Project: [json-stream on Hackage](https://hackage.haskell.org/package/json-stream), written by
  Ondřej Palkovský.
- Source repository: <https://github.com/ondrap/json-stream>.
- These files come from the fork <https://github.com/AlexaDeWit/json-stream.git> at commit
  `520e25758baa5b2665b45eee71ecf8e6a9759868`. That commit is upstream `537a43a7` plus one lexer
  commit, which reads each lexer result strictly and keeps the string loop's state in locals.

## Licence

json-stream is licensed under the BSD-3-Clause licence. [`LICENSE`](LICENSE) is upstream's licence
file, unchanged, and it covers every file in this folder except this README. Each vendored source file
names BSD-3-Clause and its copyright holders in an SPDX header. Écluse's own code stays under the MIT
licence in the repository root.

## Comparing with upstream

Run this from the repository root:

```bash
bash scripts/json-stream-vendor-diff.sh
```

The script fetches the fork commit, strips the SPDX header from each vendored file, and diffs the
rest. It prints nothing and exits 0 while every vendored file matches the commit.

## Changes from upstream

- Every vendored source file gains an SPDX header before its first line. The header names
  BSD-3-Clause, Ondřej Palkovský as the copyright holder, and Alexandra de Wit on `CLexer.hs` and
  `lexer.c`, the two files the fork commit changes. The rest of each file is byte-identical.
- `json-stream.cabal` is not vendored. The `ecluse-json-stream` stanza in `ecluse.cabal` builds the
  same modules and C source with upstream's language and warning settings. It exposes the modules
  upstream keeps internal, so the registry readers can walk the lexer's tokens. It also turns off
  the unused-imports warning, because `Parser.hs` imports `liftA2`, which base 4.18 and later
  export from the Prelude.
- Only the files the library builds from are vendored. `Data/JsonStream/Conduit.hs` (behind
  upstream's `conduit` flag), `c_lib/unescape_string.c` (used only with text before 2.0), the tests,
  the benchmarks and the package metadata stay upstream.
