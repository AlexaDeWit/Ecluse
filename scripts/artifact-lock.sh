#!/usr/bin/env bash
# Hold one artifact lock for the whole command lifetime.
set -euo pipefail
root="${1:?set ECLTEST_ARTIFACT_DIR for staged commands}"
shift
mkdir -p "$root"
exec 8>"$root/.lock"
flock 8
export ECLTEST_ARTIFACT_DIR="$root"
exec "$@"
