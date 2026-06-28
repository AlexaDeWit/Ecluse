---
id: S33
title: Packument merge across upstreams
milestone: M3, Request pipeline
status: merged
depends-on: [S07]
test-tier: [unit]
arch-refs:
  - docs/architecture/registry-model.md#packument-merge-across-upstreams
  - docs/architecture/rules-engine.md#applying-verdicts-to-a-packument
  - docs/architecture.md#request-lifecycle
pr: 91
---

# S33, Packument merge across upstreams

> Milestone **M3** · depends on: [S07](S07-npm-projection.md) · tier: unit

**Goal.** The pure, ecosystem-agnostic transform that reasons over several upstream
packuments to decide the one document Écluse serves: a fold over
`[(Provenance, PackageInfo)]` producing a **`MergePlan`**, the *decision surface*
(which versions survive, the `SourceId` each came from, the reconciled
`dist-tags`/`time`, and the detected divergences), **not** a re-serialised
`PackageInfo` (the typed model is lossy; the serve layer replays the plan onto the
raw upstream `Value`s so unmodeled keys survive). This is **core domain logic over
`PackageInfo`**, above the `RegistryClient` handle, *not* npm-adapter code, so it
is written once and reused by every ecosystem. The packument lifecycle is now a **merge**, not a
private-hit short-circuit (see
[registry-model.md#packument-merge-across-upstreams](../../docs/architecture/registry-model.md#packument-merge-across-upstreams)
for *why*: a short-circuit hides not-yet-mirrored public versions and silently
breaks demand-driven mirroring of partially-mirrored packages).

**Acceptance criteria.**
- [x] **Fold over upstreams.** `mergePackuments :: [(Provenance, PackageInfo)] -> Maybe MergePlan`
  with `Provenance = TrustedSource | GatedSource` (suffixed `*Source` to avoid
  colliding with `Ecluse.Package`'s `Trusted`). An empty input list is `Nothing`; the
  single-input case is the degenerate identity, so 0/1-upstream deployments fall out
  for free., _registry-model.md#packument-merge-across-upstreams_
- [x] **Trust split is the caller's, applied before merge.** `Trusted`
  (private-provenance) versions enter the union **as-is**; `Gated`
  (public-provenance) versions are the already-rule-filtered set (S09). Merge does
  not itself run rules, it unions what it is handed., _rules-engine.md#applying-verdicts-to-a-packument_
- [x] **Collision → private (higher-precedence) wins**, and a **divergence**
  (same version key, differing artifact integrity) is **detected and reported** in
  the `MergePlan` (a supply-chain signal, log/metric in S14/S26), never silently
  dropped. Divergence detection is order-independent (an `IntegrityFingerprint` over
  the sorted `(alg, digest)` multiset)., _registry-model.md#packument-merge-across-upstreams_
- [x] **Reconcile dist-tags / time / latest over the union.** `dist-tags.latest`
  resolved by `Ecluse.Version.selectLatest` (keep-unless-denied, stable-preferring,
  unparseable-safe) over the union; per-source tags pointing at an absent version are
  dropped; `time` is the union restricted to surviving versions. Same-key collisions
  on tags and times resolve **by provenance** (trusted wins), so the plan is
  independent of caller input order., _rules-engine.md#applying-verdicts-to-a-packument_
- [x] **Provenance is a merge-time parameter**, not a persisted `PackageDetails`
  field, and is kept out of equality/identity. *As built:* each survivor records the
  `SourceId` (the 0-based input index) of the source that won it, so the serve layer
  takes that version's object from the right raw `Value`. `SourceId = Int` is used
  rather than keying on `Provenance`, which is ambiguous once several inputs share a
  provenance (the multi-source case)., _domain-model.md_

**File scope.**
- `src/Ecluse/Package/Merge.hs`, `Provenance`, `SourceId`, `MergePlan` (the
  decision surface: survivors→`SourceId`, reconciled `dist-tags`/`time`, divergences),
  `Divergence`, `IntegrityFingerprint`, `mergePackuments`.
- `test/unit/Ecluse/Package/MergeSpec.hs`, properties: union completeness; private
  wins on collision; divergence detected iff integrity differs; `latest` always a
  surviving key across the union; single-input is identity; merge is associative /
  order-independent except the documented precedence tiebreak.

**Test tier.** Unit, `hedgehog` over hand-built `PackageInfo` values (no network,
no adapter); this is the home of the cross-source coherence invariants the
synthesized-packument schema cannot express (see
[api-surface.md](../../docs/architecture/api-surface.md#the-synthesized-packument-schema--the-trust-boundary)).

**Notes / risks.** Keep this module ecosystem-agnostic, it must never import the
npm adapter. Divergence detection compares `Artifact` integrity hashes already in
`PackageDetails`; do not fetch. Whether a divergent version is *dropped*
(fail-closed) or *served with private winning* is a policy call, surface the
divergence in the `MergePlan` and let S14 apply policy, so this slice stays pure.

**Served output is lossless (PR #23).** This slice decides, over the domain model,*which* versions/tags survive and *which* integrity divergences exist; the **served**
document must still relay unmodeled upstream fields unchanged
(`additionalProperties` passthrough), so S14 applies these decisions structurally to
the raw upstream `Value`(s) rather than re-serialising a lossy typed model (see
[S09](S09-npm-rewrite-filter.md) and
[api-surface.md](../../docs/architecture/api-surface.md#the-synthesized-packument-schema--the-trust-boundary)).
