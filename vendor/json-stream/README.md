# Vendored json-stream

This folder holds json-stream's library modules. Écluse builds its internal
`ecluse-json-stream` library with a Haskell lexer. The C sources remain as a reference for the
lexer fuzz harness. They are not linked into this library.

- Upstream: <https://github.com/ondrap/json-stream>, published on Hackage as
  [json-stream](https://hackage.haskell.org/package/json-stream). Its author is Ondrej Palkovsky.
- The tree was taken from upstream commit `537a43a775e64f50dc63c373193323de98619799`. Git history
  tracks every change since then.
- Licence: BSD-3-Clause, in upstream's unchanged [`LICENSE`](LICENSE). [`REUSE.toml`](../../REUSE.toml)
  records each vendored file's licence and copyright. This README is Écluse's own, under the MIT
  licence.
- [`json-stream.freeze`](json-stream.freeze) names the upstream release the tree derives from, so
  advisories against json-stream still reach Écluse's dependency scanners.

CI compares every path here, with its mode and SHA-256 digest, against
[`../json-stream.sha256`](../json-stream.sha256).
After an intended change, record it with `task vendor-pin`. To compare the tree with the upstream
commit, run `task vendor-diff`.
