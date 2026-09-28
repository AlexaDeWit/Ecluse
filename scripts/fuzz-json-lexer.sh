#!/usr/bin/env bash
# Build test/fuzz/json-lexer/lexer_fuzz.c with libFuzzer, AddressSanitizer and
# UndefinedBehaviorSanitizer, then fuzz the vendored lexer against upstream's.
# usage: scripts/fuzz-json-lexer.sh [libFuzzer options], for example -max_total_time=600.
# FUZZ_OUT holds the binary and the growing corpus (default dist-fuzz/json-lexer).
# FUZZ_KNOWN=report drops the suppression of the known signed overflow in handle_number.
set -euo pipefail

root="$(git rev-parse --show-toplevel)"
fuzz="$root/test/fuzz/json-lexer"
out="${FUZZ_OUT:-$root/dist-fuzz/json-lexer}"
mkdir -p "$out/corpus"

# clang and libFuzzer come from the flake's pinned nixpkgs. -fno-wrapv undoes the wrapping
# that nixpkgs' hardening flags imply, so UndefinedBehaviorSanitizer sees signed overflow.
nix shell --inputs-from "$root" nixpkgs#clang_19 nixpkgs#llvmPackages_19.compiler-rt --command \
  clang -g -O1 -fsanitize=fuzzer,address,undefined -fno-sanitize-recover=all \
  -fsanitize-recover=signed-integer-overflow -fno-wrapv \
  -I "$root/vendor/json-stream/c_lib" \
  "$fuzz/lexer_fuzz.c" "$root/vendor/json-stream/c_lib/lexer.c" "$fuzz/upstream_lexer.c" \
  -o "$out/lexer_fuzz"

suppressions="suppressions=$fuzz/ubsan.supp:"
if [[ "${FUZZ_KNOWN:-}" == report ]]; then
  suppressions=""
fi
export UBSAN_OPTIONS="${suppressions}halt_on_error=1:print_stacktrace=1"
exec "$out/lexer_fuzz" -dict="$fuzz/json.dict" -artifact_prefix="$out/" "$out/corpus" "$fuzz/seeds" "$@"
