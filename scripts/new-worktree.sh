#!/usr/bin/env bash
#
# Create a git worktree for one agent slice and warm its HLS index in the
# background, so the implementer's first agent-lsp navigation call lands on a hot index
# instead of paying a cold full-project typecheck mid-session. See AGENTS.md ->
# "Build & Tooling" and planning/orchestration-strategy.md -> "Subagents and
# isolation".
#
# Why warm at creation, not on first call: each worktree is a separate HLS
# workspace with its own (git-ignored) dist-newstyle / .hie. Dependencies already
# come warm from the shared Nix store, so the only per-worktree cost is
# typechecking this project's own modules. `make build` (== cabal build all
# --enable-tests) populates dist-newstyle with the interface files HLS reuses, so
# the heavy load is already on disk by the time the agent's MCP boots HLS. We
# delegate to `make build` rather than re-spelling the build so the warm-up can
# never drift from the canonical one.
#
# The build is backgrounded and its output redirected to a log OUTSIDE the
# worktree (so it never shows up as an untracked file there), so creation returns
# immediately. A build failure — e.g. a base that does not yet compile — warms
# what it can and never blocks creation.
#
# One worktree per agent is a hard rule; concurrency is capped at 2-3 and HLS is
# memory-hungry (~1-3 GB per load), so stagger creations rather than firing
# several cold builds at once.
#
# Usage: scripts/new-worktree.sh <branch> [base-ref] [dir]
#   <branch>    new branch to create for the worktree
#   [base-ref]  ref to branch from   (default: origin/main)
#   [dir]       worktree directory   (default: .claude/worktrees/<branch-slug>)
set -euo pipefail

branch="${1:?usage: scripts/new-worktree.sh <branch> [base-ref] [dir]}"
base="${2:-origin/main}"
slug="${branch//\//-}"
dir="${3:-.claude/worktrees/$slug}"

if [ -e "$dir" ]; then
  echo "new-worktree: '$dir' already exists; pick another DIR or remove it" >&2
  exit 1
fi

# Best-effort: make an origin/* base current. Never fatal — an offline run still
# branches from whatever ref resolves.
git fetch --quiet origin 2>/dev/null || true

git worktree add -b "$branch" "$dir" "$base"
echo "new-worktree: created '$dir' on '$branch' (from '$base')"

# Warm HLS's on-disk caches in the background. `env -u IN_NIX_SHELL` forces make
# to rebuild the dev shell from the worktree's own on-disk flake rather than
# trusting a (possibly stale) ambient agent shell — the same rule agents follow.
log="${TMPDIR:-/tmp}/ecluse-hls-warm-${slug}.log"
(
  cd "$dir"
  env -u IN_NIX_SHELL make build
) >"$log" 2>&1 &
warm_pid=$!

echo "new-worktree: warming HLS index in the background (pid $warm_pid)"
echo "new-worktree:   log:   $log"
echo "new-worktree:   enter: cd $dir"
