#!/usr/bin/env bash
#
# Cut and push the release tag for the version the operator states. Every guardrail runs
# before the tag exists, and the tag is pinned to the commit those guardrails cleared.
# The push is the point of no return: the Tag Integrity ruleset blocks tag deletion and
# update for everyone. See runbooks/release.md step 2.
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

# Removes the tag on any exit before the push, so an interrupt or an EOF at the prompt
# leaves nothing behind.
tag_created=""
cleanup_tag() {
  [ -n "$tag_created" ] || return 0
  git tag -d "$tag_created" >/dev/null 2>&1 || true
}
trap cleanup_tag EXIT

[ -n "$version" ] || die "pass a version, for example: task init-release VERSION=v0.2.0"
release_version_parse "$version" ||
  die "'$version' is not X.Y.Z or X.Y.Z-rc.N (a leading 'v' is optional)"
tag="$release_tag"
base="$release_cabal_version"

cabal_version="$(task version)" || die "could not read the version from ecluse.cabal"
[ "$base" = "$cabal_version" ] ||
  die "version base '$base' does not match ecluse.cabal version '$cabal_version'. Bump the cabal field in a pull request first"

git diff-index --quiet HEAD -- || die "the working tree has uncommitted changes to tracked files"

branch="$(git rev-parse --abbrev-ref HEAD)"
[ "$branch" = "main" ] || die "on branch '$branch'. A release is cut from main"

git fetch --quiet origin main || die "could not fetch origin/main"
head_sha="$(git rev-parse HEAD)"
[ "$head_sha" = "$(git rev-parse origin/main)" ] ||
  die "HEAD ($head_sha) is not origin/main. Pull before releasing"

# Derived from origin so the gate is read from the repository the tag is pushed to.
# `gh repo view` resolves a fork to its parent, which would split those two.
repo="$(git remote get-url origin | sed -E 's#^(git@github\.com:|https://github\.com/)##; s#\.git$##')"
[[ "$repo" == */* ]] || die "could not read an owner/name repository from origin"

# `CI gate` is the single required check, so a red non-gating job (smoke, benchmarks)
# correctly does not block. A re-run adds a check run rather than replacing one, so this
# takes the newest by id and filters server-side to stay inside one page.
gate="$(gh api "repos/$repo/commits/$head_sha/check-runs?check_name=CI+gate&per_page=100" \
  --jq '[.check_runs[]] | if length == 0 then "absent" else (max_by(.id) | "\(.status)/\(.conclusion // "none")") end')" ||
  die "could not read the CI gate status for $head_sha"
[ "$gate" = "completed/success" ] ||
  die "the 'CI gate' check on $head_sha is '$gate', not completed/success. Merged is not gate-green"

if git rev-parse -q --verify "refs/tags/$tag" >/dev/null; then
  die "tag $tag already exists locally. Delete it with 'git tag -d $tag' if it is stale"
fi
# --exit-code gives 0 for a hit and 2 for no match. Any other status is a transport or
# auth error, which must never read as "the tag is free".
ls_remote_rc=0
git ls-remote --exit-code --tags origin "refs/tags/$tag" >/dev/null 2>&1 || ls_remote_rc=$?
case "$ls_remote_rc" in
  0) die "tag $tag already exists on origin and is immutable. Ship the next version instead" ;;
  2) ;;
  *) die "could not check origin for $tag (git exit $ls_remote_rc). Refusing rather than assuming it is free" ;;
esac

cat <<PLAN

  tag      $tag
  commit   $head_sha
  subject  $(git log -1 --format=%s "$head_sha")
  cabal    $cabal_version
  CI gate  $gate

PLAN

if [ "${DRY_RUN:-}" = 1 ]; then
  echo "init-release: DRY_RUN=1, nothing created or pushed"
  exit 0
fi

# Checked before the tag exists, so a headless run leaves nothing behind.
[ -t 0 ] || die "refusing to run without a terminal to confirm the push on. Use DRY_RUN=1 to rehearse"

task tag RELEASE_TAG="$tag" RELEASE_COMMIT="$head_sha"
tag_created="$tag"
git tag -v "$tag" >/dev/null 2>&1 || die "the tag signature did not verify. The ruleset refuses an unsigned tag"

# HEAD can move while this runs, so confirm the tag sits on the commit the guardrails
# cleared rather than trusting that it did.
tagged="$(git rev-parse "$tag^{commit}")"
[ "$tagged" = "$head_sha" ] ||
  die "the tag landed on $tagged, not the verified commit $head_sha. Removed it, nothing was pushed"

printf '\nPushing %s starts the release and cannot be undone. Type the tag to confirm: ' "$tag"
reply=""
read -r reply || true
[ "$reply" = "$tag" ] || die "not confirmed. Removed the local tag, nothing was pushed"

git push origin "$tag"
tag_created="" # pushed, so the tag stays
printf '\npushed %s. The publish job now waits on the release environment gate.\n' "$tag"
