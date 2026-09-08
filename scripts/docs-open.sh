#!/usr/bin/env bash
# Open the generated Haddock index when a desktop opener is available.
set -euo pipefail

if ! html="$(find dist-newstyle -path '*/doc/html/ecluse/index.html' -print -quit)" || [ -z "$html" ]; then
  echo "docs: no Haddock index found under dist-newstyle/ (did Haddock run?)" >&2
  exit 1
fi

echo "Haddock: $html"
if command -v xdg-open >/dev/null 2>&1; then
  xdg-open "$html" >/dev/null 2>&1 &
fi
