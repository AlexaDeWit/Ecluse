#!/usr/bin/env bash
#
# Reap the HLS build caches left behind by worktrees that no longer exist, and prune the
# stale worktree registrations that point at them. See AGENTS.md -> "Build and tooling".
#
# Why this exists: `task new-worktree` warms each agent worktree's HLS index, which
# cabal writes to its own directory under the hie-bios cache (~1 GB per worktree, since
# it holds a full dist-newstyle for this project). `git worktree remove` deletes the
# checkout but knows nothing about that cache, so every retired agent slice strands a
# gigabyte. Across a few dozen slices that silently becomes tens of gigabytes, which is
# exactly the disk the *live* worktrees need to breathe.
#
# The hie-bios cache is shared by every Haskell project on the machine, so "no live
# worktree owns this directory" is on its own an unsafe reason to delete: another
# project's cache looks identical from the outside. Each cabal cache instead records the
# source it was built from, in `cache/plan.json` under the local package's `pkg-src`
# path. We reap a directory only when that recorded path
#
#   1. lies inside THIS repository (so a foreign project is never a candidate), and
#   2. no longer exists on disk (so the worktree that owned it is genuinely gone).
#
# Anything we cannot attribute (an interrupted warm-up with no readable plan.json, or a
# worktree created outside the repo root via `new-worktree.sh`'s DIR argument) is
# reported and left alone. Leaving a stray gigabyte is always preferable to deleting a
# cache we did not positively identify as dead.
#
# Safe to run at any time, including while other worktrees have builds in flight: a live
# worktree's source directory exists, so its cache is never a candidate.
#
# Usage: scripts/reap-hls-caches.sh [--dry-run]
#   --dry-run   report what would be reaped, delete nothing
set -euo pipefail

dry_run=false
if [ "${1:-}" = "--dry-run" ]; then
  dry_run=true
elif [ -n "${1:-}" ]; then
  echo "reap-hls-caches: unknown argument '$1' (expected --dry-run)" >&2
  exit 1
fi

if ! command -v jq >/dev/null 2>&1; then
  echo "reap-hls-caches: jq is required; run this through 'task' inside the Nix dev shell" >&2
  exit 1
fi

cache_dir="${XDG_CACHE_HOME:-$HOME/.cache}/hie-bios"
if [ ! -d "$cache_dir" ]; then
  echo "reap-hls-caches: no cache at '$cache_dir'; nothing to do"
  exit 0
fi

# The main worktree is always the first entry, and is the root every agent worktree
# lives under. Scoping to it is what keeps a foreign project's cache off the table.
repo_root="$(git worktree list --porcelain | sed -n '1s|^worktree ||p')"
if [ -z "$repo_root" ]; then
  echo "reap-hls-caches: not inside a git worktree" >&2
  exit 1
fi

# Drop registrations whose checkout is already gone, so the live set below is accurate
# and `git worktree list` stops reporting them as prunable.
git worktree prune

reaped_mb=0
reaped=0
kept=0
skipped=0

for dist in "$cache_dir"/dist-*; do
  [ -d "$dist" ] || continue

  plan="$dist/cache/plan.json"
  if [ ! -r "$plan" ]; then
    echo "  skip   (no readable plan.json, cannot attribute)  $(basename "$dist")"
    skipped=$((skipped + 1))
    continue
  fi

  # The local package's source directory: the checkout this cache was built from.
  src="$(jq -r 'first(.["install-plan"][]? | select(.style == "local") | .["pkg-src"].path) // empty' "$plan" 2>/dev/null || true)"
  src="${src%/.}"
  src="${src%/}"

  if [ -z "$src" ]; then
    echo "  skip   (plan.json names no local source, cannot attribute)  $(basename "$dist")"
    skipped=$((skipped + 1))
    continue
  fi

  # Scope to this repository. A cache belonging to any other project is left untouched.
  case "$src" in
    "$repo_root" | "$repo_root"/*) ;;
    *)
      skipped=$((skipped + 1))
      continue
      ;;
  esac

  # The owning checkout still exists, so this cache is live.
  if [ -d "$src" ]; then
    kept=$((kept + 1))
    continue
  fi

  size_mb="$(du -sm "$dist" | cut -f1)"
  if [ "$dry_run" = true ]; then
    echo "  would reap  ${size_mb} MB  $(basename "$dist")"
  else
    rm -rf "$dist"
    echo "  reaped      ${size_mb} MB  $(basename "$dist")"
  fi
  reaped_mb=$((reaped_mb + size_mb))
  reaped=$((reaped + 1))
done

if [ "$dry_run" = true ]; then
  echo "reap-hls-caches: would reap $reaped orphaned cache(s), ${reaped_mb} MB; kept $kept live, skipped $skipped"
else
  echo "reap-hls-caches: reaped $reaped orphaned cache(s), ${reaped_mb} MB; kept $kept live, skipped $skipped"
fi
