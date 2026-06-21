---
id: S33
title: Packument merge across upstreams
milestone: M3 — Request pipeline
status: not-started
depends-on: [S07]
test-tier: [unit]
arch-refs:
  - docs/architecture/registry-model.md#packument-merge-across-upstreams
  - docs/architecture/rules-engine.md#applying-verdicts-to-a-packument
  - docs/architecture.md#request-lifecycle
pr: null
---

# S33 — Packument merge across upstreams

> Milestone **M3** · depends on: [S07](S07-npm-projection.md) · tier: unit

**Goal.** The pure, ecosystem-agnostic transform that combines several upstream
packuments into the one document Écluse serves: a fold over
`[(Provenance, PackageInfo)]` producing a single merged `PackageInfo` plus the
divergences it detected. This is **core domain logic over `PackageInfo`**, above
the `RegistryClient` seam — *not* npm-adapter code — so it is written once and
reused by every ecosystem. The packument lifecycle is now a **merge**, not a
private-hit short-circuit (see
[registry-model.md#packument-merge-across-upstreams](../../docs/architecture/registry-model.md#packument-merge-across-upstreams)
for *why*: a short-circuit hides not-yet-mirrored public versions and silently
breaks demand-driven mirroring of partially-mirrored packages).

**Acceptance criteria.**
- [ ] **Fold over upstreams.** `mergePackuments :: [(Provenance, PackageInfo)] -> MergeResult`
  with `Provenance = Trusted | Gated`. The single-input case is the degenerate
  identity, so 0/1-upstream deployments fall out for free. — _registry-model.md#packument-merge-across-upstreams_
- [ ] **Trust split is the caller's, applied before merge.** `Trusted`
  (private-provenance) versions enter the union **as-is**; `Gated`
  (public-provenance) versions are the already-rule-filtered set (S09). Merge does
  not itself run rules — it unions what it is handed. — _rules-engine.md#applying-verdicts-to-a-packument_
- [ ] **Collision → private (higher-precedence) wins**, and a **divergence**
  (same version key, differing artifact integrity) is **detected and reported** in
  `MergeResult` (a supply-chain signal — log/metric in S14/S26), never silently
  dropped. — _registry-model.md#packument-merge-across-upstreams_
- [ ] **Reconcile dist-tags / time / latest over the union.** `dist-tags.latest`
  → highest surviving version across all sources (`compareVersions`); per-source
  tags pointing at an absent version are dropped; `time` is the union restricted to
  surviving versions. — _rules-engine.md#applying-verdicts-to-a-packument_
- [ ] **Provenance is a merge-time parameter**, not (necessarily) a persisted
  `PackageDetails` field; if threaded through for observability, keep it out of
  equality/identity. (Decide in implementation; note the choice.) — _domain-model.md_

**File fence.**
- `src/Ecluse/Package/Merge.hs` — `Provenance`, `MergeResult` (merged
  `PackageInfo` + detected divergences), `mergePackuments`.
- `test/unit/Ecluse/Package/MergeSpec.hs` — properties: union completeness; private
  wins on collision; divergence detected iff integrity differs; `latest` always a
  surviving key across the union; single-input is identity; merge is associative /
  order-independent except the documented precedence tiebreak.

**Test tier.** Unit — `hedgehog` over hand-built `PackageInfo` values (no network,
no adapter); this is the home of the cross-source coherence invariants the
synthesized-packument schema cannot express (see
[api-surface.md](../../docs/architecture/api-surface.md#the-synthesized-packument-schema--the-trust-boundary)).

**Notes / risks.** Keep this module ecosystem-agnostic — it must never import the
npm adapter. Divergence detection compares `Artifact` integrity hashes already in
`PackageDetails`; do not fetch. Whether a divergent version is *dropped*
(fail-closed) or *served with private winning* is a policy call — surface the
divergence in `MergeResult` and let S14 apply policy, so this slice stays pure.
