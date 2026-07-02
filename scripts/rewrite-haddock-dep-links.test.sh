#!/usr/bin/env bash
#
# Deterministic unit test for rewrite-haddock-dep-links.sh. Exercises the link
# shapes Haddock emits -- a GHC boot library, a cabal-store Hackage dependency,
# and links that must be left untouched -- against the rewriter's --filter mode,
# so the mapping is locked against regression without a full docs build. Run via
# `make test-scripts` (folded into `make check`).
set -euo pipefail

here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
script="$here/rewrite-haddock-dep-links.sh"

fail=0

# Assert the rewriter turns $input into exactly $expected.
check() {
  local name="$1" input="$2" expected="$3" got
  got="$(printf '%s\n' "$input" | bash "$script" --filter)"
  if [ "$got" = "$expected" ]; then
    printf 'ok   - %s\n' "$name"
  else
    printf 'FAIL - %s\n' "$name"
    printf '       in:   %s\n' "$input"
    printf '       want: %s\n' "$expected"
    printf '       got:  %s\n' "$got"
    fail=1
  fi
}

# --- GHC boot libraries (pkgroot prefix, with the Nix store hash) ---

check "boot lib, GHC 9.10.3 abbreviated-hash dir" \
  'href="${pkgroot}/../../../../n36vfz6wsiv35jxmfybic39cpqq38vyi-ghc-9.10.3-doc/share/doc/ghc/html/libraries/base-4.20.2.0-4d66/Data-Bool.html#t:Bool"' \
  'href="https://hackage.haskell.org/package/base/docs/Data-Bool.html#t:Bool"'

check "boot lib, single-component version" \
  'href="${pkgroot}/../../../../n36vfz6wsiv35jxmfybic39cpqq38vyi-ghc-9.10.3-doc/share/doc/ghc/html/libraries/containers-0.7-e888/Data-Map.html#t:Map"' \
  'href="https://hackage.haskell.org/package/containers/docs/Data-Map.html#t:Map"'

check "boot lib, no hash (legacy GHC dir layout)" \
  'href="${pkgroot}/../../../../1nf74pkvqh2222f62f6a5gmy24miipxp-ghc-9.6.6-doc/share/doc/ghc/html/libraries/base-4.18.2.1/Data-Bool.html#t:Bool"' \
  'href="https://hackage.haskell.org/package/base/docs/Data-Bool.html#t:Bool"'

# All-digit abbreviated hash: the version group must NOT swallow it (regression for
# #468 -- real on GHC 9.10.3: process-1.6.26.1-2190, haskeline-0.8.2.1-2435,
# hpc-0.7.0.2-9331; an all-letter hash masked the bug, and the build guard cannot
# catch it because the mis-rewrite is a well-formed-looking Hackage URL).

check "boot lib, all-digit hash, contrived (version not folded)" \
  'href="${pkgroot}/../../../../n36vfz6wsiv35jxmfybic39cpqq38vyi-ghc-9.10.3-doc/share/doc/ghc/html/libraries/base-4.20.2.0-1234/Data-Bool.html#t:Bool"' \
  'href="https://hackage.haskell.org/package/base/docs/Data-Bool.html#t:Bool"'

check "boot lib, all-digit hash, real (process-1.6.26.1-2190)" \
  'href="${pkgroot}/../../../../n36vfz6wsiv35jxmfybic39cpqq38vyi-ghc-9.10.3-doc/share/doc/ghc/html/libraries/process-1.6.26.1-2190/System-Process.html#t:CreateProcess"' \
  'href="https://hackage.haskell.org/package/process/docs/System-Process.html#t:CreateProcess"'

# --- cabal-store Hackage dependencies (file:// prefix, CI runner path) ---

check "store dep, simple name (CI runner path)" \
  'href="file:///home/runner/work/Ecluse/Ecluse/.cabal/store/ghc-9.10.3/relude-1.2.2.2-2efbd1f48c44e2bba58392e4f621ea5c8b5be81616ff1ae27399bf05bcd58019/share/doc/html/Relude-String-Reexport.html#t:Text"' \
  'href="https://hackage.haskell.org/package/relude/docs/Relude-String-Reexport.html#t:Text"'

check "store dep, hyphenated package name" \
  'href="file:///home/runner/work/Ecluse/Ecluse/.cabal/store/ghc-9.10.3/amazonka-core-2.0-95e330b7970574d1561990f20459e0c7d4c91604c33e33a6f9fa13123e0cc7d8/share/doc/html/Amazonka-Core.html#t:Service"' \
  'href="https://hackage.haskell.org/package/amazonka-core/docs/Amazonka-Core.html#t:Service"'

check "store dep, hash starting with a digit (version not eaten)" \
  'href="file:///home/runner/work/Ecluse/Ecluse/.cabal/store/ghc-9.10.3/hs-opentelemetry-api-1.0.0.0-1ce0b35f1b5a6c6cb03b210dc4392e8965db621d1bd9c03b8e20a13e0bba0df5/share/doc/html/OpenTelemetry-Trace.html#t:Tracer"' \
  'href="https://hackage.haskell.org/package/hs-opentelemetry-api/docs/OpenTelemetry-Trace.html#t:Tracer"'

check "store dep, hyperlinked-source (src/) page" \
  'href="file:///home/runner/work/Ecluse/Ecluse/.cabal/store/ghc-9.10.3/relude-1.2.2.2-2efbd1f48c44e2bba58392e4f621ea5c8b5be81616ff1ae27399bf05bcd58019/share/doc/html/src/Relude.Foldable.Fold.html#elem"' \
  'href="https://hackage.haskell.org/package/relude/docs/src/Relude.Foldable.Fold.html#elem"'

check "store dep, all-digit unit-id hash (version not folded)" \
  'href="file:///home/runner/work/Ecluse/Ecluse/.cabal/store/ghc-9.10.3/relude-1.2.2.2-1234567890123456789012345678901234567890123456789012345678901234/share/doc/html/Relude-String-Reexport.html#t:Text"' \
  'href="https://hackage.haskell.org/package/relude/docs/Relude-String-Reexport.html#t:Text"'

# --- Generic Nix store documentation ---

check "nix-store dep, simple name" \
  'href="/nix/store/n36vfz6wsiv35jxmfybic39cpqq38vyi-relude-1.2.1.0-doc/share/doc/html/Relude.html#t:Text"' \
  'href="https://hackage.haskell.org/package/relude/docs/Relude.html#t:Text"'

check "nix-store dep, hyphenated name" \
  'href="/nix/store/p4873mabc-amazonka-core-2.0-doc/share/doc/html/Amazonka-Prelude.html#t:Text"' \
  'href="https://hackage.haskell.org/package/amazonka-core/docs/Amazonka-Prelude.html#t:Text"'

# --- JSON search index: the href is escaped (\"); the escape must survive ---

check "doc-index.json escaped href" \
  '"display":"<a href=\"${pkgroot}/../../../../n36vfz6wsiv35jxmfybic39cpqq38vyi-ghc-9.10.3-doc/share/doc/ghc/html/libraries/base-4.20.2.0-4d66/System-IO.html#t:IO\">IO</a>"' \
  '"display":"<a href=\"https://hackage.haskell.org/package/base/docs/System-IO.html#t:IO\">IO</a>"'

# --- Links that must be left untouched ---

check "external template link (fonts) untouched" \
  'href="https://fonts.googleapis.com/css?family=PT+Sans:400,400i,700"' \
  'href="https://fonts.googleapis.com/css?family=PT+Sans:400,400i,700"'

check "relative intra-doc link untouched" \
  'href="Ecluse-Core-Version.html#t:Version"' \
  'href="Ecluse-Core-Version.html#t:Version"'

check "already-canonical Hackage link untouched" \
  'href="https://hackage.haskell.org/package/base/docs/Data-Bool.html#t:Bool"' \
  'href="https://hackage.haskell.org/package/base/docs/Data-Bool.html#t:Bool"'

# --- Belt-and-braces: no build-host path may survive a rewrite ---

leaky='href="file:///home/runner/work/Ecluse/Ecluse/.cabal/store/ghc-9.10.3/vector-0.13.2.0-8df2f058464a632dc4a649fac1c2693cdcdccf163ecebd58ccd51f174db45b1d/share/doc/html/Data-Vector.html#t:Vector"'
rewritten="$(printf '%s\n' "$leaky" | bash "$script" --filter)"
if printf '%s' "$rewritten" | grep -qE '/home/|file://|\$\{pkgroot\}|/nix/store/'; then
  printf 'FAIL - %s\n' "build-host path survived the rewrite"
  printf '       got:  %s\n' "$rewritten"
  fail=1
else
  printf 'ok   - %s\n' "no build-host path survives the rewrite"
fi

if [ "$fail" -ne 0 ]; then
  echo "rewrite-haddock-dep-links: TESTS FAILED" >&2
  exit 1
fi
echo "rewrite-haddock-dep-links: all tests passed"
