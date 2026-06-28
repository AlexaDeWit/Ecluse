---
id: S46
title: Docker Hub org account + repo-scoped publish token (retire account-wide personal PAT)
milestone: M8, Release hardening
status: not-started
depends-on: []
test-tier: []
arch-refs:
  - docs/architecture/release-supply-chain.md#supply-chain-attestations
pr: null
---

# S46, Docker Hub org account + repo-scoped publish token

> Milestone **M8** · depends on:, (independent; release-ops change) · tier: n/a (publishing infrastructure, not a cabal suite)

**Goal.** Move image publishing off a **personal** Docker Hub account with an
**account-wide** `Read & Write` personal access token (PAT) onto an
**organization** account whose publish credential is an **Organization Access
Token (OAT) scoped to the single `ecluse` repository**, owned by a dedicated
machine account. This closes the one residual weakness in the otherwise
attestation-anchored release path: the current PAT's permission is a *level*
(`Read & Write`), not a *target*, so it can push/pull **every repo the personal
account owns**, per-repository scoping simply is not offered on a personal
account. An OAT collapses that blast radius to exactly one repo and brings RBAC,
which is the posture a supply-chain tool should publish from.

**Why this is acceptable to defer (and what's already true).** The risk is
**accepted while pre-MVP**, before Écluse fronts a real build, a publish-token
compromise can't silently poison a consumer, because trust is anchored **off the
registry**: images are digest-pinned and carry keyless SLSA provenance + SBOM
attestations (GitHub OIDC → Fulcio/Rekor), so a thief can push bytes but cannot
forge the release workflow's signing identity, and `gh attestation verify`
rejects anything it didn't build. Two further facts bound the current exposure:

- Docker Hub **token auth cannot reach repository administration**, even a
  `Read, Write & Delete` token "does not allow you to modify account settings as
  password authentication would." Changing tag immutability / visibility,
  deleting the repo, and editing settings require a real authenticated session,
  **not** a token. So the immutable-tag / no-`latest` posture is **not** removable
  with a stolen PAT.
- The token is already `Read & Write` (no `Delete`) and lives on the **protected
  `release` GitHub Environment** (required reviewers), fed via `--password-stdin`.

The remaining live exposure is therefore narrow but real: **account-wide reach**
(every repo the personal account owns) and the ability to push a *new* plausible
tag (e.g. a forged `0.1.0-rc.99`), both of which an OAT + machine account close.
**Hardening becomes critical before GA**, when the proxy starts fronting real
builds and the cost of a forged tag stops being theoretical.

**Why a dedicated slice (not done inline).** It is an **account-level migration
with a paid-plan dependency** (OATs require a Docker Team/Business plan) and a
**namespace change** (`alexadewit/ecluse` → `<org>/ecluse`) that ripples through
the workflow, the README verify recipe, and the docs. It is cheap to do *now*
(pre-MVP, ~zero downstream pullers) and expensive after GA, but it is a discrete
ops change with an external billing decision, so it is parked here to be done
deliberately rather than mid-feature.

**Acceptance criteria.**
- [ ] A Docker Hub **organization** exists on a plan that offers **Organization
  Access Tokens** (Team/Business); the cost is accepted as an explicit decision.
- [ ] The `ecluse` image lives under the org namespace, published by a **dedicated
  machine account** that is an org member with access to **only** the `ecluse`
  repo (so even an account-wide credential is effectively repo-scoped).
- [ ] The publish credential is an **OAT scoped to the single repo** at the
  minimum permission for an immutable-tag push (write, **not** delete), replacing
  the personal account-wide PAT.
- [ ] Secrets (`DOCKERHUB_USERNAME` / `DOCKERHUB_TOKEN`) are rotated to the new
  identity and remain on the **protected `release` Environment** (required
  reviewers); `--password-stdin`, never argv., _AGENTS.md (CI & Security)_
- [ ] `release.yml`'s `IMAGE` env and any namespace-bearing steps point at
  `docker.io/<org>/ecluse`; the attestation `subject-name` follows.
- [ ] README verify recipe, `nix build …#dockerImage` reference, and
  `release-supply-chain.md` are updated to the new namespace; the doc's
  "Authentication (Docker Hub)" notes the OAT scope and a **rotation cadence**.
- [ ] A dispatched `rc` release exercises the full build → push → attest chain
  under the new identity and verifies green (`gh attestation verify`).

**Alternative to weigh at implementation, GHCR (OIDC-native).** Publishing to
**GitHub Container Registry** instead of (or alongside) Docker Hub removes the
**entire long-lived-secret class**: GHCR authenticates with the ephemeral,
per-run `GITHUB_TOKEN` (`packages: write`), repo-scoped, no stored PAT/OAT at all, and it unifies the trust story, since the attestations already run on GitHub
OIDC. Trade-off is discoverability (`docker pull` reflexively targets Docker Hub)
and anonymous-pull/ratelimit differences. The common resolutions: **GHCR as the
OIDC-native primary with Docker Hub as a mirror**, or GHCR outright. This slice
should record an explicit **GHCR-vs-org-Docker-Hub (vs both)** decision rather
than defaulting to Docker Hub; "we publish with zero long-lived registry
credentials" is a posture worth wanting for a supply-chain tool.

**File scope.**
- `.github/workflows/release.yml`, `IMAGE` env, push step, attestation
  `subject-name`; **or** a GHCR login/push (`packages: write`, no stored secret)
  if that path is chosen.
- `README.md`, image namespace in the verify recipe and the `nix build` ref.
- `docs/architecture/release-supply-chain.md`, "Authentication (Docker Hub)":
  OAT scope, machine account, rotation cadence; record the GHCR decision.
- `USAGE.md`, any pull/image reference, if present.

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
