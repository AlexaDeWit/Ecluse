#!/usr/bin/env bash
# Build the release image while a command runs. Print the build log, then return the first failure.
#   image-build-alongside.sh <archive-path-file> <command...>
set -euo pipefail
archive_path="${1:?archive path file}"
shift
here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "$here/process-group.sh"
work="$(mktemp -d)"
child_pid=
trap '[ -z "$child_pid" ] || stop_child 143; rm -rf "$work"' EXIT
trap 'exit 130' INT
trap 'exit 143' TERM

set -m
nix build --print-build-logs --option max-silent-time 1200 .#dockerImage --no-link --print-out-paths \
  < /dev/null > "$work/archive-path" 2> "$work/build.log" &
child_pid=$!
set +m

status=0
"$@" || status=$?
image_status=0
wait "$child_pid" || image_status=$?
child_pid=
cat "$work/build.log" >&2
[ "$status" -ne 0 ] || status="$image_status"
[ "$status" -ne 0 ] || cp "$work/archive-path" "$archive_path"
exit "$status"
