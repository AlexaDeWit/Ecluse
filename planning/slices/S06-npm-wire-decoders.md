---
id: S06
title: npm wire types + lenient decoders
milestone: M1 — npm protocol adapter
status: in-progress
depends-on: []
test-tier: [unit, smoke]
arch-refs:
  - docs/research/reverse-engineering/npm.md#11-type-model
  - docs/research/reverse-engineering/npm.md#4-package-metadata--the-packument-full
  - docs/research/reverse-engineering/npm.md#5-abbreviated-packument
  - docs/research/reverse-engineering/npm.md#7-the-dist-object
pr: null
---

# S06 — npm wire types + lenient decoders

> Milestone **M1** · depends on: — (root) · tier: unit, smoke

**Goal.** Model the npm registry wire JSON and decode it leniently: the full and
abbreviated packument, the per-version manifest, and the `dist` object. Lenient on
input (ignore unknown keys; tolerate string-or-object `license`/`bugs`/`repository`/
`person`; tolerate the bare-string per-version 404 body), faithful on the fields the
rules and serving need.

**Acceptance criteria.**
- [ ] `aeson` decoders for `Packument` (full), `AbbreviatedPackument`,
  `VersionManifest`, `Dist`, and the shared scalars (`Person`, `Repository`, `Bugs`,
  `License`) per the type model. — _npm.md#11-type-model_
- [ ] **Lenient input**: unknown keys ignored; `license`/`bugs`/`repository`/person
  accept both string and object forms; `ErrorResponse` tolerates `{error|message}`
  **or** a bare JSON string (the per-version 404). — _npm.md#errors, npm.md#11-type-model_
- [ ] Captures the rule-decisive fields: abbreviated `hasInstallScript`,
  `deprecated`, `dist.{tarball,shasum,integrity}`, and the `time` map (full only) for
  publish age. — _npm.md#5-abbreviated-packument, npm.md#8-version--availability-resolution_
- [ ] Round-trips representative real bodies (fixtures captured from the npm.md
  probes: `is-odd`, `core-js`, a scoped `@babel/...`, `request` (deprecated)).

**File fence.**
- `src/Ecluse/Registry/Npm/Wire.hs` — wire types + `FromJSON` (+ `ToJSON` where serving needs it; output is S09/S14).
- `ecluse.cabal` — add `aeson`; register the module; add fixtures to `extra-source-files` if used by unit tests.
- `test/unit/Ecluse/Registry/Npm/WireSpec.hs` — decode fixtures; lenient-form and bare-string-404 cases.
- `test/unit/fixtures/npm/*.json` — captured bodies.
- `test/smoke/Ecluse/RegistryProtocolSpec.hs` — extend: live decode of an abbreviated packument confirms the model still matches reality (non-gating).

**Test tier.** Unit (fixtures, gating) + smoke (live npm, non-gating, surfaces protocol drift).

**Notes / risks.** This is pure and dependency-free (root) — a Wave-1 candidate. Do
**not** project into domain types here (that is S07) — keep wire and domain
separate so the lenient/faithful boundary is clean (parse, don't validate; the
adapter is the boundary). `hasInstallScript` is abbreviated-only; record the
`scripts` map too so S07 can derive it when only the full form is present.
