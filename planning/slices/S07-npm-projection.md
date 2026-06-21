---
id: S07
title: npm projection → domain (PackageInfo/PackageDetails)
milestone: M1 — npm protocol adapter
status: not-started
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

**File fence.**
- `src/Ecluse/Package.hs` (or new `src/Ecluse/Packument.hs`) — `PackageInfo` type (+ exports).
- `src/Ecluse/Registry/Npm/Project.hs` — the three `parse*` functions.
- `ecluse.cabal` — register module(s).
- `test/unit/Ecluse/Registry/Npm/ProjectSpec.hs` — projection of the S06 fixtures into domain values; signal-mapping table.

**Test tier.** Unit — assert projected domain values for known fixtures (incl. scoped
names, deprecated, install-script, missing-time).

**Notes / risks.** `PackageInfo` placement is the one design choice to settle (it is
referenced by S02's seam and S09/S14) — prefer defining it in `Ecluse.Package`
alongside `PackageDetails` to keep the import graph acyclic; **escalate** if that
forces an awkward cycle. The `hasInstallScript` derivation must match npm.md exactly
(`scripts` ∋ {preinstall, install, postinstall}).
