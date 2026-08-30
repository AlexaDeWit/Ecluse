#!/usr/bin/env bash
#
# Cut and push the release tag. The version argument is the operator's assertion of
# which release this is, and `task tag` derives the tag from ecluse.cabal, so the two
# must agree before anything happens. Every guardrail runs before the tag exists. The
# push is the point of no return: the Tag Integrity ruleset blocks tag deletion and
# update for everyone, so a pushed tag cannot be moved. See runbooks/release.md step 2.
#
# Usage: scripts/init-release.sh v0.2.0
#        DRY_RUN=1 runs every guardrail, then stops before creating or pushing.
set -euo pipefail

here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
# shellcheck source=scripts/release-version.sh
. "$here/release-version.sh"

version="${1:-}"

die() {
  printf 'init-release: %s\n' "$*" >&2
  exit 1
}

[ -n "$version" ] || die "pass a version, for example: task init-release VERSION=v0.2.0"
release_version_parse "$version" ||
  die "VERSION '$version' is not X.Y.Z or X.Y.Z-rc.N (a leading 'v' is optional)"
tag="$release_tag"
base="$release_cabal_version"

cabal_version="$(task version)" || die "could not read the version from ecluse.cabal"
[ "$base" = "$cabal_version" ] ||
  die "VERSION base '$base' does not match ecluse.cabal version '$cabal_version'. Bump the cabal field in a pull request first"

git diff-index --quiet HEAD -- || die "the working tree has uncommitted changes to tracked files"

branch="$(git rev-parse --abbrev-ref HEAD)"
[ "$branch" = "main" ] || die "on branch '$branch'. A release is cut from main"

git fetch --quiet origin main || die "could not fetch origin/main"
head_sha="$(git rev-parse HEAD)"
origin_sha="$(git rev-parse origin/main)"
[ "$head_sha" = "$origin_sha" ] ||
  die "HEAD ($head_sha) is not origin/main ($origin_sha). Pull before releasing"

# `CI gate` is the single required check and the branch-protection authority, so a red
# non-gating job (smoke, benchmarks) correctly does not block a release here.
repo="$(gh repo view --json nameWithOwner --jq .nameWithOwner)" || die "could not resolve the repository"
gate="$(gh api "repos/$repo/commits/$head_sha/check-runs" \
  --jq '[.check_runs[] | select(.name == "CI gate") | "\(.status)/\(.conclusion // "none")"] | last // "absent"')"
[ "$gate" = "completed/success" ] ||
  die "the 'CI gate' check on $head_sha is '$gate', not completed/success. Merged is not gate-green"

if git rev-parse -q --verify "refs/tags/$tag" >/dev/null; then
  die "tag $tag already exists locally. Delete it with 'git tag -d $tag' if it is stale"
fi
if git ls-remote --exit-code --tags origin "refs/tags/$tag" >/dev/null 2>&1; then
  die "tag $tag already exists on origin and is immutable. Ship the next version instead"
fi

cat <<PLAN

  tag      $tag
  commit   $head_sha
  subject  $(git log -1 --format=%s "$head_sha")
  cabal    $cabal_version
  CI gate  $gate

PLAN

if [ -n "${DRY_RUN:-}" ]; then
  echo "init-release: DRY_RUN set, nothing created or pushed"
  exit 0
fi

# Checked before the tag exists, so a headless run leaves nothing behind.
[ -t 0 ] || die "refusing to run without a terminal to confirm the push on. Use DRY_RUN=1 to rehearse"

task tag TAG="$tag"
git tag -v "$tag" || die "the tag signature did not verify. The ruleset refuses an unsigned tag"

printf '\nPushing %s starts the release and cannot be undone. Type the tag to confirm: ' "$tag"
read -r reply
if [ "$reply" != "$tag" ]; then
  git tag -d "$tag"
  die "confirmation did not match. Removed the local tag, nothing was pushed"
fi

git push origin "$tag"
printf '\npushed %s. The publish job now waits on the release environment gate.\n' "$tag"
