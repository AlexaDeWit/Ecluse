#!/usr/bin/env bash
# Check parser and workflow gate contracts through `task test-scripts`.
set -euo pipefail

here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
# shellcheck source=scripts/release-version.sh
. "$here/release-version.sh"

work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT
mkdir -p "$work/bin"
cat > "$work/bin/cabal" <<'STUB'
#!/usr/bin/env bash
set -euo pipefail
[ "$*" = 'info ./ecluse.cabal' ] || exit 2
[ "${TEST_CABAL_FAILURE:-0}" = 0 ] || exit 1
printf '* ecluse-%s\n' "$TEST_CABAL_VERSION"
STUB
chmod +x "$work/bin/cabal"

# Exercise the workflow's Task entry point while keeping Cabal reads deterministic.
gate() {
  : > "$work/output"
  PATH="$work/bin:$PATH" INPUT_TAG="$1" REF_NAME="$2" TEST_CABAL_VERSION="$3" \
    GITHUB_OUTPUT="$work/output" task --dir "$here/.." release-validate-version > "$work/task.log" 2>&1
}

gate_accepts() {
  local input="$1" ref="$2" cabal="$3" tag="$4"
  printf 'image-tag=%s\ngit-tag=%s\n' "${tag#v}" "$tag" > "$work/expected"
  if gate "$input" "$ref" "$cabal" && cmp -s "$work/output" "$work/expected"; then
    printf 'ok   - gate accepts %q / %q\n' "$input" "$ref"
  else
    printf 'FAIL - gate output for %q / %q\n' "$input" "$ref"
    cat "$work/task.log" "$work/output"
    fail=1
  fi
}

gate_rejects() {
  local input="$1" ref="$2" cabal="$3"
  if gate "$input" "$ref" "$cabal"; then
    printf 'FAIL - gate accepted %q / %q\n' "$input" "$ref"
    fail=1
  elif [ -s "$work/output" ]; then
    printf 'FAIL - gate emitted outputs after rejecting %q / %q\n' "$input" "$ref"
    cat "$work/output"
    fail=1
  else
    printf 'ok   - gate rejects %q / %q without outputs\n' "$input" "$ref"
  fi
}

fail=0

# Assert an accepted version and both encodings. $1 input, $2 tag, $3 cabal version.
accepts() {
  local input="$1" want_tag="$2" want_cabal="$3"
  gate_accepts "$input" ignored-ref "$want_cabal" "$want_tag"
  gate_accepts "" "$input" "$want_cabal" "$want_tag"
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
  gate_rejects "$input" "" 0.2.0
  gate_rejects "" "$input" 0.2.0
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

for github_actions in false true; do
  export GITHUB_ACTIONS="$github_actions"
  printf 'GITHUB_ACTIONS=%s\n' "$GITHUB_ACTIONS"
  accepts "0.2.0" "v0.2.0" "0.2.0"
  accepts "v0.2.0" "v0.2.0" "0.2.0"
  accepts "10.20.30" "v10.20.30" "10.20.30"
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
  rejects "$(printf '0.2.0\nrm -rf /')" "embedded newline"

  rejects "0.2.0-rc.01" "leading zero in candidate number"
  rejects "0.2.0+build.1" "build metadata"
  rejects $'0.2.0\ngit-tag=v9.9.9' "output injection"

  gate_rejects "v0.2.0" "v0.2.0" 0.3.0
  gate_rejects "v0.2.0-rc.1" "v0.2.0" 0.3.0
  gate_rejects "invalid-dispatch" "v0.2.0" 0.2.0
  gate_accepts "v0.2.0-rc.1" "v9.9.9" 0.2.0 v0.2.0-rc.1
  TEST_CABAL_FAILURE=1 gate_rejects "v0.2.0" "" 0.2.0
done

exit "$fail"
