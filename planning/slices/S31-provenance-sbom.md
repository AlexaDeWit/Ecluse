---
id: S31
title: SLSA provenance + SBOM attestation
milestone: M8 — Release hardening
status: in-review
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
stored as **immutable OCI referrers**. No separate cosign image signature: cosign
can't store attestations immutably (only `sign` has a referrer mode), and the
provenance attestation already binds the digest to the builder identity.

**Acceptance criteria.**
- [x] SLSA build provenance via `actions/attest-build-provenance` for the released
  image; verifiable with `gh attestation verify`. — _CONTRIBUTING.md#releases--container-image_
- [x] An SBOM (`sbomnix` over `.#ecluse-bin`) is generated and attested via
  `actions/attest-sbom`.
- [x] All actions SHA-pinned (Dependabot-bumped); the release workflow stays
  injection-free; provenance/SBOM run in the tag-triggered `release.yml`, **not** the
  PR gate. — _AGENTS.md (CI & Security)_
- [x] Docs updated (CONTRIBUTING → Releases) describing how to verify provenance + SBOM.

**File fence.**
- `.github/workflows/release.yml` — digest resolve, registry login, provenance + SBOM attest steps.
- `Makefile` / `flake.nix` — keep `make sbom` (sbomnix); drop the cosign sign/attest path.
- `README.md` / `CONTRIBUTING.md` / `AGENTS.md` — `gh attestation verify` recipe + how it's produced.

**Test tier.** None (workflow) — validated by a dry-run / next tagged release; the PR
gate is unaffected.

**Notes / risks.** Independent of the product code; release-only, off the PR gate.
First shipped via cosign (PR #9), then reworked to the GitHub attest-actions because
cosign can't store attestations as immutable referrers and the repo enforces
immutable tags. Unproven in CI until a tagged/dispatched run — validate with an `rc`
dispatch before the first real `vX.Y.Z`.
