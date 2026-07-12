#!/usr/bin/env bash
#
# Enforce a supply-chain invariant: every container image any test tier pulls,
# runs, or builds FROM must be nailed to an immutable @sha256: digest, never a
# mutable tag. Écluse is itself a supply-chain policy proxy; a floating tag can be
# re-pointed at a poisoned image between the pin and the pull, and an immutable
# digest cannot, so the test harness must never trust a tag. This guard fails the
# build if any image reference at an actual pull/run/build construct lacks a
# @sha256:<64 hex> digest, stopping a future tag (or a dependency-bot PR that
# rewrites a digest back to a tag) from silently regressing the pins.
#
# The guard is deliberately keyed on the *constructs that actually pull an image*,
# not on a blanket colon scan, so it never trips on the many innocuous colons in
# the harness (host:port endpoints like `ministack:4566`, bind-mount specs like
# `/certs:ro`, or Haddock `@...@` inline code in comments). The constructs are:
#
#   Haskell harness (.hs):
#     - `dockerRun <name> <net> "<IMAGE>"`  -- the detached e2e `docker run` builder;
#       the third argument, when a string literal, is the image reference. When it is
#       a bound variable (the product image from ECLUSE_E2E_IMAGE, or `collectorImage`)
#       there is no literal here and the reference is checked at its binding instead.
#     - `<name>Image = "<IMAGE>"`           -- an image-reference binding (e.g. the OTLP
#       collector image the e2e and integration telemetry tiers build FROM / run).
#     - `"FROM <IMAGE>..."`                 -- a `FROM` line inside a `fromDockerfile`
#       string literal. A `"FROM "` that continues into a variable carries no literal
#       here and is checked at that variable's binding.
#
#   GitHub Actions workflow (.yml/.yaml):
#     - the image-reference arguments passed to `scripts/docker-prepull.sh`, whether on
#       the same line as the script or on the following folded-scalar continuation lines.
#
# The product image build (`flake.nix .#dockerImage`) is a separate, Nix-pinned
# surface and is intentionally out of this guard's remit.
#
# Comments are stripped from Haskell before scanning (both `--` line comments and
# `{- -}` block comments), so an example or a prose mention of a construct in a doc
# comment cannot cause a false failure.
#
# Usage:
#   scripts/check-image-digest-pinning.sh                 # scan the default harness set
#   scripts/check-image-digest-pinning.sh FILE [FILE ...] # scan the given files (tests)
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

# The construct patterns (see the header). Kept as variables so the `[[ =~ ]]` sites
# stay readable; each capturing group is the image reference to validate.
readonly h1_re='dockerRun[[:space:]]+[^[:space:]"]+[[:space:]]+[^[:space:]"]+[[:space:]]+"([^"]+)"'
readonly h2_re='^[[:space:]]*([A-Za-z0-9_.]*Image)[[:space:]]*=[[:space:]]*"([^"]+)"'
readonly h3_re='"FROM ([^"[:space:]\]+)'
# A digest pin: @sha256: then exactly 64 lowercase-hex, bounded so a short (or
# longer) hex run is not silently accepted as 64.
readonly digest_re='@sha256:[0-9a-f]{64}([^0-9a-f]|$)'

violations=0

# Record one unpinned reference and bump the counter.
report() {
  printf 'not digest-pinned: %s:%s\n    %s\n' "$1" "$2" "$3" >&2
  violations=$((violations + 1))
}

# Validate one captured image reference; a reference missing its @sha256: digest is
# reported as a violation.
check_ref() {
  local file="$1" lineno="$2" ref="$3"
  if [[ ! "$ref" =~ $digest_re ]]; then
    report "$file" "$lineno" "$ref"
  fi
}

# Scan a Haskell harness source for the three image-pull constructs, first stripping
# comments so a construct mentioned in prose or a doc example is never matched.
scan_haskell() {
  local file="$1"
  local lineno=0 in_block=0 raw code before after
  # The block-comment delimiters, held in variables so the pattern-match sites can
  # quote them. A bare brace in a `[[ == ]]` glob reads as a literal to the linter;
  # quoted, each delimiter matches literally while the surrounding `*` stays a glob.
  local open='{-' close='-}'
  while IFS= read -r raw || [[ -n "$raw" ]]; do
    lineno=$((lineno + 1))
    code="$raw"
    # Close an open `{- -}` block comment; keep only the tail after `-}`.
    if [[ "$in_block" -eq 1 ]]; then
      if [[ "$code" == *"$close"* ]]; then
        code="${code#*"$close"}"
        in_block=0
      else
        continue
      fi
    fi
    # Remove each complete inline `{- -}` block comment on this line.
    while [[ "$code" == *"$open"* && "$code" == *"$close"* ]]; do
      before="${code%%"$open"*}"
      after="${code#*"$open"}"
      after="${after#*"$close"}"
      code="$before$after"
    done
    # Open a `{- -}` block comment that does not close on this line; keep the head.
    if [[ "$code" == *"$open"* ]]; then
      code="${code%%"$open"*}"
      in_block=1
    fi
    # Drop a `--` line comment.
    code="${code%%--*}"

    if [[ "$code" =~ $h1_re ]]; then
      check_ref "$file" "$lineno" "${BASH_REMATCH[1]}"
    fi
    if [[ "$code" =~ $h2_re ]]; then
      check_ref "$file" "$lineno" "${BASH_REMATCH[2]}"
    fi
    if [[ "$code" =~ $h3_re ]]; then
      check_ref "$file" "$lineno" "${BASH_REMATCH[1]}"
    fi
  done <"$file"
}

# Scan a workflow for the image-reference arguments to `scripts/docker-prepull.sh`.
# The refs may sit on the same line as the script or on the following folded-scalar
# continuation lines (one bare token per line); the run block ends at a blank line or
# a dedented mapping/list key, at which point argument collection stops.
scan_workflow() {
  local file="$1"
  local lineno=0 in_prepull=0 raw seen w
  local -a words fields
  while IFS= read -r raw || [[ -n "$raw" ]]; do
    lineno=$((lineno + 1))
    if [[ "$raw" == *docker-prepull.sh* ]]; then
      in_prepull=1
      # Any tokens after the script name on this same line are image arguments.
      read -ra words <<<"$raw"
      seen=0
      for w in "${words[@]}"; do
        if [[ "$seen" -eq 1 ]]; then
          check_ref "$file" "$lineno" "$w"
        fi
        if [[ "$w" == *docker-prepull.sh ]]; then
          seen=1
        fi
      done
      continue
    fi
    if [[ "$in_prepull" -eq 1 ]]; then
      read -ra fields <<<"$raw"
      if [[ "${#fields[@]}" -eq 0 ]]; then
        in_prepull=0
      elif [[ "${#fields[@]}" -eq 1 && "${fields[0]}" != -* ]]; then
        check_ref "$file" "$lineno" "${fields[0]}"
      else
        in_prepull=0
      fi
    fi
  done <"$file"
}

# Dispatch a file to the scanner for its type.
scan_file() {
  local file="$1"
  case "$file" in
    *.hs) scan_haskell "$file" ;;
    *.yml | *.yaml) scan_workflow "$file" ;;
    *) printf 'guard: unrecognised file type, skipping: %s\n' "$file" >&2 ;;
  esac
}

main() {
  local -a targets
  if [[ "$#" -gt 0 ]]; then
    targets=("$@")
  else
    targets=(
      "$repo_root/test/e2e/Ecluse/E2E/Harness/Docker.hs"
      "$repo_root/test/integration/Ecluse/Integration/Ministack.hs"
      "$repo_root/test/integration/Ecluse/TelemetryMetricsSpec.hs"
      "$repo_root/test/integration/Ecluse/TelemetryTracingSpec.hs"
      "$repo_root/.github/workflows/ci.yml"
    )
  fi

  local f
  for f in "${targets[@]}"; do
    if [[ ! -f "$f" ]]; then
      printf 'guard: target not found: %s\n' "$f" >&2
      violations=$((violations + 1))
      continue
    fi
    scan_file "$f"
  done

  if [[ "$violations" -gt 0 ]]; then
    printf '\nimage-digest-pinning: %s unpinned image reference(s) found.\n' "$violations" >&2
    printf 'Every container image a test tier pulls, runs, or builds FROM must carry a\n' >&2
    printf '@sha256:<64 hex> digest (a mutable tag can be re-pointed at a poisoned image).\n' >&2
    exit 1
  fi
  printf 'image-digest-pinning: all scanned image references are @sha256: digest-pinned.\n'
}

main "$@"
