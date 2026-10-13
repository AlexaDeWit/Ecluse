#!/usr/bin/env bash
# SPDX-FileCopyrightText: 2026 Alexandra de Wit
# SPDX-License-Identifier: MIT

# Shared controls inspect the actual include tree without executing target commands.
fail() {
  printf '%s\n' "$*" >&2
  return 1
}

expect_var() {
  local file="$1" name="$2" expected="$3"
  grep -Fqx "  $name: \"$expected\"" "$file" || fail "resolved variable differs: $name in $file"
}

verify_paths() {
  local commands_file="$1" expected_root="$2" expected_repository="$3" quoted path normalised
  local quoted_pattern="['\"]/[^'\"]*['\"]"
  local -a quoted_paths=()
  mapfile -t quoted_paths < <(grep -oE "$quoted_pattern" "$commands_file")
  [ "${#quoted_paths[@]}" -gt 0 ] || fail "no resolved paths: $commands_file" || return 1
  for quoted in "${quoted_paths[@]}"; do
    path="${quoted:1:${#quoted}-2}"
    normalised="$(realpath -m -- "$path")"
    case "$normalised" in
      "$expected_root"|"$expected_root"/*) ;;
      "$expected_repository/scripts/process-capture.sh"|"$expected_repository/scripts/process-status.sh") ;;
      *) fail "resolved path escapes diagnostic root: $path"; return 1 ;;
    esac
  done
}

wiring_preview() {
  local repository_root="$1" report_root="$2" preview_target="$3" preview_ref="$4"
  shift 4
  local preview="$report_root/repository"
  mkdir -p "$preview" "$report_root/bin"
  cp "$repository_root/Taskfile.yml" "$preview/Taskfile.yml"
  cp -a "$repository_root/taskfiles" "$preview/taskfiles"
  cat > "$report_root/bin/uname" <<'PLATFORM'
#!/usr/bin/env bash
set -euo pipefail
test "${1:-}" = -m
printf 'aarch64\n'
PLATFORM
  cat > "$report_root/bin/blocked" <<'BLOCKED'
#!/usr/bin/env bash
printf '%s\n' "$0" >> "$WIRING_BLOCK_LOG"
exit 125
BLOCKED
  chmod +x "$report_root/bin/uname" "$report_root/bin/blocked"
  for executable in mkdir git cabal ghc curl cp tar patch timeout gzip lscpu sha256sum find bash; do
    ln -s blocked "$report_root/bin/$executable"
  done
  # These shims use the real Bash directly because Bash itself must be blocked as a target command.
  sed -i "1c#!$(command -v bash)" "$report_root/bin/uname" "$report_root/bin/blocked"
  env -u GHCRTS PATH="$report_root/bin:$PATH" WIRING_BLOCK_LOG="$report_root/blocked-command.log" GITHUB_REF="$preview_ref" GITHUB_RUN_ID=static-preview GITHUB_RUN_ATTEMPT=1 \
    task --dir "$preview" --dry "$preview_target" "$@" > "$report_root/included-run-dry.txt" 2>&1
  test ! -e "$report_root/blocked-command.log" || fail 'a target command executed during the preview'
}
