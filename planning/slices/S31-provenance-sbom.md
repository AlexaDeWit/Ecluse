---
id: S31
title: SLSA provenance + SBOM attestation
milestone: M8 — Release hardening
status: not-started
depends-on: []
test-tier: []
arch-refs:
  - CONTRIBUTING.md#releases--container-image
  - AGENTS.md
pr: null
---

# S31 — SLSA provenance + SBOM attestation

> Milestone **M8** · depends on: — (independent; CI work) · tier: n/a (workflow)

**Goal.** Complete the release supply-chain posture (CI roadmap slice 3): add SLSA
build provenance and an SBOM attestation to the tag-triggered release workflow,
complementing the existing cosign keyless image signing.

**Acceptance criteria.**
- [ ] SLSA build provenance via `actions/attest-build-provenance` for the released
  image; verifiable with `gh attestation verify`. — _CONTRIBUTING.md#releases--container-image_
- [ ] An SBOM is generated and attested for the image.
- [ ] All actions SHA-pinned (Dependabot-bumped); the release workflow stays
  injection-free; provenance/SBOM run in the tag-triggered `release.yml`, **not** the
  PR gate. — _AGENTS.md (CI & Security)_
- [ ] Docs updated (CONTRIBUTING → Releases) describing how to verify provenance + SBOM.

**File fence.**
- `.github/workflows/release.yml` — provenance + SBOM steps (additive).
- `CONTRIBUTING.md` — verification docs.
- (note: a `worktree-provenance` worktree already exists on `worktree-provenance` — coordinate so this slice and that branch don't diverge.)

**Test tier.** None (workflow) — validated by a dry-run / next tagged release; the PR
gate is unaffected.

**Notes / risks.** Independent of the product code — can run as a parallel track at
any point. Coordinate with the **existing `worktree-provenance` worktree** (it may
already hold WIP for this); reconcile before opening the PR rather than duplicating.
Keep it off the PR gate (release-only), matching the cosign signing posture.
