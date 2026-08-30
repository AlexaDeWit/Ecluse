#!/usr/bin/env bash
# Deterministic unit test for release-version.sh, the single parse behind the cabal
# field and the git tag. A version accepted here becomes an immutable tag, so the
# refusals carry more weight than the accepts. Run via `task test-scripts`.
set -euo pipefail

here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
# shellcheck source=scripts/release-version.sh
. "$here/release-version.sh"

fail=0

# Assert an accepted version and both encodings. $1 input, $2 tag, $3 cabal version.
accepts() {
  local input="$1" want_tag="$2" want_cabal="$3"
  release_tag="" release_cabal_version=""
  if ! release_version_parse "$input"; then
    printf 'FAIL - %-16s rejected, expected accept\n' "$input"
    fail=1
    return
  fi
  if [ "$release_tag" = "$want_tag" ] && [ "$release_cabal_version" = "$want_cabal" ]; then
    printf 'ok   - %-16s -> tag %s, cabal %s\n' "$input" "$release_tag" "$release_cabal_version"
  else
    printf 'FAIL - %-16s -> tag %s, cabal %s (want tag %s, cabal %s)\n' \
      "$input" "$release_tag" "$release_cabal_version" "$want_tag" "$want_cabal"
    fail=1
  fi
}

# Assert a refusal. $1 input, $2 why it must not parse.
rejects() {
  local input="$1" why="$2"
  release_tag="stale" release_cabal_version="stale"
  if release_version_parse "$input" 2>/dev/null; then
    printf 'FAIL - %-16s accepted, expected reject (%s)\n' "$input" "$why"
    fail=1
  elif [ -n "$release_tag" ] || [ -n "$release_cabal_version" ]; then
    printf 'FAIL - %-16s rejected but left stale outputs\n' "$input"
    fail=1
  else
    printf 'ok   - %-16s rejected (%s)\n' "$input" "$why"
  fi
}

accepts "0.2.0" "v0.2.0" "0.2.0"
accepts "v0.2.0" "v0.2.0" "0.2.0"
accepts "10.20.30" "v10.20.30" "10.20.30"
# A candidate tags vX.Y.Z-rc.N while the cabal field stays X.Y.Z, so the two encodings
# diverge here and nowhere else. This is the case a single string would get wrong.
accepts "0.2.0-rc.1" "v0.2.0-rc.1" "0.2.0"
accepts "v0.2.0-rc.12" "v0.2.0-rc.12" "0.2.0"

rejects "" "empty"
rejects "0.2" "two components"
rejects "0.2.0.1" "four components"
rejects "v" "prefix only"
rejects "vv0.2.0" "doubled prefix"
rejects "0.2.0-rc" "rc without a number"
rejects "0.2.0-rc.1.2" "rc with two numbers"
rejects "0.2.0-beta.1" "unsupported prerelease name"
rejects "0.2.0 " "trailing space"
rejects "1.0.0-rc.1-rc.2" "two rc suffixes"
rejects "a.b.c" "non-numeric"
rejects "0.2.0;whoami" "shell metacharacter"
rejects "01.02.03" "leading zeros"
rejects "V0.2.0" "uppercase prefix"
rejects " 0.2.0" "leading space"
# The classic anchor bypass: bash's $ can match before a trailing newline, so a second
# line could ride along into a tag string.
rejects "$(printf '0.2.0\nrm -rf /')" "embedded newline"

exit "$fail"
