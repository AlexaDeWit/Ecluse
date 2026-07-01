---
id: S46
title: Docker Hub org account + repo-scoped publish token (retire account-wide personal PAT)
milestone: M8, Release hardening
status: completed
depends-on: []
test-tier: []
arch-refs:
  - docs/architecture/release-supply-chain.md#supply-chain-attestations
pr: null
---

# S46, Docker Hub org account + repo-scoped publish token

> Milestone **M8** · depends on:, (independent; release-ops change) · tier: n/a (publishing infrastructure, not a cabal suite)

**Goal & Resolution.** Move image publishing off a **personal** Docker Hub account with an
**account-wide** `Read & Write` personal access token (PAT).

Originally, this slice planned to migrate to a Docker Hub Organization Account to utilize repo-scoped Organization Access Tokens (OATs). However, as evaluated in the alternatives, **we chose to abandon Docker Hub in favor of GitHub Container Registry (GHCR)**.

This completely eliminated the need for static credentials, allowing the workflow to use the ephemeral, OIDC-backed `GITHUB_TOKEN` (`packages: write`) to push images to `ghcr.io`. 

The Docker Hub namespace (`docker.io/alexadewit/ecluse`) is retained only as an empty repository for typo-squatting defense. It may be used as a mirror in the future if Docker Hub implements native OIDC federation for GitHub Actions.

**Acceptance criteria (As implemented for GHCR).**
- [x] Docker Hub PATs and static secrets are completely removed from the environment.
- [x] The `release.yml` workflow authenticates to `ghcr.io` using the ephemeral `GITHUB_TOKEN`.
- [x] `release.yml`'s `IMAGE` env and any namespace-bearing steps point at `ghcr.io/alexadewit/ecluse`.
- [x] README verify recipe and `release-supply-chain.md` are updated to the new namespace.
- [x] `gh attestation verify` successfully verifies the image hosted on GHCR.

**File scope.**
- `.github/workflows/release.yml`
- `README.md`
- `docs/architecture/release-supply-chain.md`

**Test tier.** None (publishing infrastructure), validated by a dispatched `rc`
run of `release.yml`, like the rest of the release path. The PR gate is unaffected.

**Notes / risks.** The namespace change is a **breaking change for any existing
puller**, trivial now (pre-MVP), much costlier after GA, which is the argument
for doing it before launch. The org plan is a **recurring cost**; GHCR is the
zero-cost alternative that also deletes the static-secret weakness, so the billing
decision and the registry-choice decision are coupled and should be made together.
No product code is touched; this is release-ops only, off the PR gate. The current
posture is a **deliberate, bounded accepted risk** for the pre-MVP window; see
the "Authentication (Docker Hub)" guarantees above, not an oversight.
