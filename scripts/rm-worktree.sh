#!/usr/bin/env bash
#
# Retire one agent worktree: remove the checkout, then reap the HLS cache it warmed.
# The counterpart to scripts/new-worktree.sh. See AGENTS.md -> "Build and tooling".
#
# `new-worktree` creates a checkout AND warms a ~1 GB HLS index outside it (under the
# hie-bios cache). A bare `git worktree remove` reclaims only the first of those, so
# retiring a slice by hand leaves the gigabyte behind. Removing both together is what
# keeps the two sides symmetric, and is why retiring a slice should go through here.
#
# The branch is deliberately kept. Retiring a worktree is a disk-space decision, not a
# decision to discard work: the commits stay reachable through the branch ref, and the
# slice can be checked out again with `task new-worktree`. Delete the branch separately
# once its PR has merged.
#
# Reaping delegates to scripts/reap-hls-caches.sh rather than re-spelling the rule, so
# the "which cache belongs to which checkout" logic can never drift between the two.
#
# Usage: scripts/rm-worktree.sh <branch> [dir]
#   <branch>  branch whose worktree should be retired (the branch itself is kept)
#   [dir]     worktree directory  (default: .agents/worktrees/<branch-slug>)
#
# Set FORCE=1 to remove a worktree that still has modified or untracked files. Without
# it, git refuses, which is the check that stops uncommitted work being thrown away.
set -euo pipefail

branch="${1:?usage: scripts/rm-worktree.sh <branch> [dir]}"
slug="${branch//\//-}"
dir="${2:-.agents/worktrees/$slug}"

here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

if [ ! -d "$dir" ]; then
  echo "rm-worktree: no worktree at '$dir'; reaping any cache it left behind anyway"
else
  force=()
  [ "${FORCE:-0}" = "1" ] && force=(--force)

  if ! git worktree remove "${force[@]}" "$dir"; then
    echo "rm-worktree: refused to remove '$dir'" >&2
    echo "rm-worktree:   it has modified or untracked files. Commit or push them first," >&2
    echo "rm-worktree:   or re-run with FORCE=1 to discard them." >&2
    exit 1
  fi
  echo "rm-worktree: removed '$dir' (branch '$branch' kept)"
fi

bash "$here/reap-hls-caches.sh"
