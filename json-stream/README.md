# Maintained json-stream fork

Écluse maintains this private `ecluse-json-stream` Cabal library. Its Haskell lexer and registry
integration have diverged from upstream. The public parser contract still follows json-stream.

- Origin: [Ondrej Palkovsky's json-stream](https://github.com/ondrap/json-stream), release 0.4.6.1,
  at commit `537a43a775e64f50dc63c373193323de98619799`.
- Licence: BSD-3-Clause. The original [LICENSE](LICENSE) remains unchanged. Source headers retain
  upstream attribution and identify Écluse's modifications.
- Advisory tracking: [upstream.freeze](upstream.freeze) retains the upstream dependency identity
  for vulnerability scanning. It does not imply that the implementation is unchanged.

String decoding also derives from aeson-2.0's staged-streams Unicode decoder, as recorded in the
inherited source. The maintained version uses local ST storage and checked input offsets.

The supported modules are `Data.JsonStream.Parser` and `Data.JsonStream.Lexer`. Cursor constructors,
decoding internals, and compatibility adapters remain private. The registry driver owns body and
input-piece limits. Tokens and cursors can retain input slices until their callers release them.

The historical [C lexer reference](../test/fuzz/json-lexer/reference/) is test-only and is not linked
into the application. Recorded C traces and independent decoder results check inherited behaviour.

`task json-stream-diff` compares the maintained source and historical C reference with the recorded
upstream commit. Differences produce exit status 1. `task json-reference-check` verifies only the
frozen C reference. Source formatting, lint, Stan and SPDX checks include the maintained library.
