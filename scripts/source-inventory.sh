#!/usr/bin/env bash
# For hs or sh: list emits NUL paths, manifest prints the content checksum file,
# and run passes the selected paths to the supplied tool command.
set -euo pipefail

mode="${1:?expected list, manifest, or run}"
extension="${2:?expected hs or sh}"
shift 2
case "$extension" in
  hs | sh) ;;
  *) echo "unsupported source extension: $extension" >&2; exit 1 ;;
esac

# Git stops at ignored directories and untracked nested repositories.
source_files() {
  git ls-files --cached --others --exclude-standard -z |
    while IFS= read -r -d '' path; do
      case "$path" in
        scratchpad/* | .agents/* | .claude/* | dist-*/* | coverage/* | _site/*) continue ;;
        *."$extension")
          if [[ -f "$path" && ! -L "$path" ]]; then
            printf '%s\0' "$path"
          fi
          ;;
      esac
    done | sort -zu
}

case "$mode" in
  list) source_files ;;
  manifest)
    mkdir -p .task/source-inventory
    manifest=".task/source-inventory/$extension.sha256"
    temporary="$(mktemp "$manifest.XXXXXX")"
    trap 'rm -f "$temporary"' EXIT
    source_files | xargs -0 -r sha256sum --zero -- | sha256sum > "$temporary"
    if ! cmp -s "$temporary" "$manifest"; then
      mv "$temporary" "$manifest"
    fi
    printf '%s\n' "$manifest"
    ;;
  run)
    [[ $# -gt 0 ]] || { echo 'expected a tool command' >&2; exit 1; }
    source_files | xargs -0 -r "$@"
    ;;
  *) echo "unsupported inventory mode: $mode" >&2; exit 1 ;;
esac
