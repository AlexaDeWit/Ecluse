#!/usr/bin/env bash
#
# Pre-pull one or more pinned container images into the local Docker cache,
# retrying a transient registry failure with bounded exponential backoff, so a
# momentary Docker Hub blip does not fail a whole gating CI job on the first
# attempt. The unauthenticated GitHub-hosted runners share a heavily throttled
# Docker Hub IP pool whose auth-token endpoint intermittently times out ("context
# deadline exceeded"); the integration and end-to-end suites pull their pinned
# data-plane images (ministack, the OTLP collector, nginx, Verdaccio) at run time,
# so a single throttled pull inside a suite's setup hook can take the whole gate
# red. Warming the images here first, with retries, means the suite's own
# `docker run` / `docker build FROM` then finds them already cached.
#
# The image references are passed in by the caller (the CI e2e / build-test jobs)
# and MUST stay in sync with the pins in the test harness, which is the source of
# truth:
#   - test/e2e/Ecluse/E2E/Harness/Docker.hs
#   - test/integration/Ecluse/Integration/Ministack.hs (and the telemetry specs)
# Digest pins are copied verbatim; this helper never rewrites a digest to a
# floating tag, so it adds no new trust surface.
#
# This is best-effort cache-warming, not a gate: if an image still cannot be
# pulled after the whole backoff budget, the helper logs a clear warning and
# exits 0, leaving the suite's own pull to try once more (and to fail loudly with
# its own diagnostic if the outage has not cleared). It therefore only ever adds
# resilience; it never introduces a new way for the job to go red.
#
# Usage:
#   scripts/docker-prepull.sh IMAGE_REF [IMAGE_REF ...]
#
# Tunables (env):
#   PREPULL_ATTEMPTS             total attempts per image        (default 5)
#   PREPULL_BACKOFF_SECONDS      initial backoff, doubled each   (default 5)
#                                retry, capped at the ceiling below
#   PREPULL_BACKOFF_MAX_SECONDS  backoff ceiling in seconds      (default 60)
set -euo pipefail

readonly ATTEMPTS="${PREPULL_ATTEMPTS:-5}"
readonly BACKOFF_SECONDS="${PREPULL_BACKOFF_SECONDS:-5}"
readonly BACKOFF_MAX_SECONDS="${PREPULL_BACKOFF_MAX_SECONDS:-60}"

# True when a Docker daemon is reachable. With none, cache-warming is moot: the
# suite's own docker calls will surface the missing daemon, so we skip cleanly
# (exit 0) rather than burn the backoff budget on pulls that cannot succeed.
docker_ready() {
  command -v docker >/dev/null 2>&1 && docker info >/dev/null 2>&1
}

# Pull one image, retrying on failure with bounded exponential backoff. Returns 0
# once the image is cached, 1 once the per-image attempt budget is exhausted.
pull_with_retry() {
  local image="$1"
  local attempt=1
  local backoff="$BACKOFF_SECONDS"
  while true; do
    echo "docker-prepull: pulling ${image} (attempt ${attempt}/${ATTEMPTS})" >&2
    if docker pull "$image"; then
      echo "docker-prepull: cached ${image}" >&2
      return 0
    fi
    if [ "$attempt" -ge "$ATTEMPTS" ]; then
      return 1
    fi
    echo "docker-prepull: pull of ${image} failed; retrying in ${backoff}s" >&2
    sleep "$backoff"
    attempt=$((attempt + 1))
    backoff=$((backoff * 2))
    if [ "$backoff" -gt "$BACKOFF_MAX_SECONDS" ]; then
      backoff="$BACKOFF_MAX_SECONDS"
    fi
  done
}

main() {
  if [ "$#" -eq 0 ]; then
    echo "usage: $0 IMAGE_REF [IMAGE_REF ...]" >&2
    exit 2
  fi

  if ! docker_ready; then
    echo "docker-prepull: no reachable Docker daemon; skipping cache-warm" >&2
    return 0
  fi

  local -a failed=()
  local image
  for image in "$@"; do
    if ! pull_with_retry "$image"; then
      failed+=("$image")
    fi
  done

  if [ "${#failed[@]}" -gt 0 ]; then
    echo "docker-prepull: WARNING: could not pre-pull after ${ATTEMPTS} attempts:" >&2
    printf 'docker-prepull:   %s\n' "${failed[@]}" >&2
    echo "docker-prepull: the suite will attempt these pulls again; a persistent registry outage will fail it there." >&2
  fi
  return 0
}

main "$@"
