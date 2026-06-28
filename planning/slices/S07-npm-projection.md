---
id: S07
title: npm projection → domain (PackageInfo/PackageDetails)
milestone: M1 — npm protocol adapter
status: merged
depends-on: [S06]
test-tier: [unit]
arch-refs:
  - docs/architecture/registry-model.md#registry-abstraction
  - docs/architecture/domain-model.md
  - docs/research/reverse-engineering/npm.md#12-what-écluse-must-replicate
pr: null
---

# S07 — npm projection → domain (`PackageInfo` / `PackageDetails`)

> Milestone **M1** · depends on: [S06](S06-npm-wire-decoders.md) · tier: unit

**Goal.** Project npm wire types into the ecosystem-agnostic domain model: the pure
`parsePackageInfo` / `parseVersionDetails` / `parseVersionList` fields of
`RegistryClient`. Introduce the packument-level domain type `PackageInfo` (the
view above `PackageDetails`). Nothing above the adapter sees npm wire data.

**Acceptance criteria.**
- [ ] `PackageInfo` domain type introduced (name, dist-tags, the per-version
  `PackageDetails` map, the `time`/publish-age data, surviving package-level
  metadata) — placed to avoid an import cycle with `PackageDetails` (in
  `Ecluse.Package` or a sibling; no `.hs-boot`). — _registry-model.md, domain-model.md_
- [ ] `parsePackageInfo :: RegistryResponse -> Either ParseError PackageInfo`,
  `parseVersionDetails :: RegistryResponse -> Version -> Either ParseError PackageDetails`,
  `parseVersionList :: RegistryResponse -> Either ParseError [Version]` — all pure. —
  _registry-model.md#registry-abstraction_
- [ ] Signal mapping: npm `hasInstallScript` (or derived from `scripts`) →
  `CodeExecSignal`; `deprecated` → `Availability`; `dist` → `NonEmpty Artifact`
  with algorithm-tagged `Hash`es (SRI + SHA-1); `_npmUser` → `pkgPublisher`;
  `time[version]` → `pkgPublishedAt`; names parsed into `Ecosystem`/`Scope`/canonical. —
  _domain-model.md, npm.md#12-what-écluse-must-replicate_
- [ ] Unknown/unfetched signals map to the explicit-unknown cases
  (`CodeExecUnknown`/`TrustUnknown`/`Nothing`) rather than fabricated values.

**File scope.**
- `src/Ecluse/Package.hs` (or new `src/Ecluse/Packument.hs`) — `PackageInfo` type (+ exports).
- `src/Ecluse/Registry/Npm/Project.hs` — the three `parse*` functions.
- `ecluse.cabal` — register module(s).
- `test/unit/Ecluse/Registry/Npm/ProjectSpec.hs` — projection of the S06 fixtures into domain values; signal-mapping table.

**Test tier.** Unit — assert projected domain values for known fixtures (incl. scoped
names, deprecated, install-script, missing-time).

**Notes / risks.** `PackageInfo` placement is the one design choice to settle (it is
referenced by S02's handle and S09/S14) — prefer defining it in `Ecluse.Package`
alongside `PackageDetails` to keep the import graph acyclic; **escalate** if that
forces an awkward cycle. The `hasInstallScript` derivation must match npm.md exactly
(`scripts` ∋ {preinstall, install, postinstall}).

**Integrity feeds divergence (PR #23).** Carry __both__ `dist.shasum` and
`dist.integrity` into the `Artifact` hashes: the cross-upstream merge ([S33](S33-packument-merge.md))
flags a same-version integrity **divergence** between the private and public
upstreams as a supply-chain signal, so neither hash may be dropped in projection.

**As-built notes.**
- **`PackageInfo` lives in `Ecluse.Package`** (alongside `PackageDetails`), the
  preferred placement, no sibling module or `.hs-boot` was needed; the import graph
  stays acyclic. Its fields are `infoName` / `infoVersions` (a `Map Text
  PackageDetails`) / `infoDistTags` / `infoPublishedAt`.
- **A thin projection-local wire wrapper recovers `_npmUser`.** S06's
  `Wire.VersionManifest` intentionally does not model the per-version publisher, so
  the projection decodes each version object once more through its own
  `WirePackument`/`VersionEntry` types, which pair a `VersionManifest` with a
  `Maybe Wire.Person` read straight from `_npmUser` — **reusing S06's `Wire.Person`
  decoder** rather than adding a new one. `_npmUser` → `pkgPublisher`, both
  `dist.shasum` (SHA-1) and `dist.integrity` (SRI) survive into `artHashes`, and
  `time[version]` → `pkgPublishedAt`. Both signal-mapping requirements above are met
  as written; this wrapper is the mechanism, not a deviation.
