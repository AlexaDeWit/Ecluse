---
id: S35
title: OpenAPI contract drift controls
milestone: M2 — Web front door
status: not-started
depends-on: [S34]
test-tier: [unit]
arch-refs:
  - docs/architecture/api-surface.md#contract-drift-controls
  - docs/architecture/api-surface.md#source-of-truth-the-route-enumeration--mounts
pr: null
---

# S35 — OpenAPI contract drift controls

> Milestone **M2** · depends on: [S34](S34-capability-manifest.md) · tier: unit
>
> **Enhancement / fast-follow.** Hardens the S34 manifest against divergence from
> the server's actual routes and behaviour. **Not on the launch critical path** —
> sequence after S34. Purely additive: introduces **no** change to S34's manifest
> output, only guards around it. (S34 now **statically generates and publishes** the
> spec — it is not served — so these guards bind the generated artifact to the live
> server, not a served endpoint.)

**Goal.** Make it structurally hard for the
[capability manifest](../../docs/architecture/api-surface.md) to drift from the
server without a test failing — across **both** drift axes (schema; path/operation)
— plus PR-visible change detection. See
[api-surface.md → Contract drift controls](../../docs/architecture/api-surface.md#contract-drift-controls).

**Acceptance criteria.**
- [ ] **Schema backstop**: a `hedgehog` property per owned type runs
  `validateToJSON` (`Data.OpenApi.Schema.Validation`) — generate → encode →
  validate against that type's schema. Explicitly covers the **hand-written
  synthesized-packument** partial schema as well as the autodocodec-derived ones. —
  _api-surface.md#contract-drift-controls_
- [ ] **Route ↔ operation exhaustiveness**: a test that pattern-matches **every**
  `Route` constructor and asserts a corresponding manifest operation (and the
  reverse, 1:1), so adding a `Route` without documenting it fails to compile or
  fails the test. — _api-surface.md#source-of-truth-the-route-enumeration--mounts_
- [ ] **Live status contract**: `hspec-wai` drives the real `Application` and asserts
  each documented operation's status, boundaries included (`Search` → `501`, unknown →
  `404`, a denial → `403`). This ties the **statically generated** spec to the
  server's live behaviour — the manifest is not served, but the statuses it documents
  must match what the routes actually return. — _api-surface.md#contract-drift-controls_
- [ ] **Golden snapshot**: the spec emitted by **S34's build-time generator** from a
  **fixed canonical config** is committed and compared in CI; a mismatch fails until
  regenerated, so every contract change is a reviewed diff. This is the **same file
  S34 publishes** — the published spec doubles as the golden. —
  _api-surface.md#contract-drift-controls_
- [ ] _(Optional)_ an `openapi-diff` CI step classifying breaking vs additive
  changes against the committed golden; note explicitly if deferred.

**File scope.**
- `test/unit/Ecluse/App/ManifestDriftSpec.hs` — the `validateToJSON` properties, the
  `Route` / operation exhaustiveness test, and the `hspec-wai` status contract.
  (Module home follows the `ecluse-core` / `ecluse` split, as in S34.)
- `test/golden/openapi.json` — the committed canonical-config spec; it is **S34's
  generator output** (not a separately produced snapshot), so the published artifact
  and the golden are one file.
- `Makefile` / CI — a `make openapi-golden` target that re-runs **S34's generator**
  and a CI check comparing its output to the committed golden, wired in as a **`gate`
  dependency** (per [CONTRIBUTING → CI](../../CONTRIBUTING.md), never a new required
  status).

**Test tier.** Unit — the properties, exhaustiveness, and `hspec-wai` contract; the
golden comparison is a CI step over the generated artifact.

**Notes / risks.** Generate the golden from a **fixed config**, never a live
deployment, or it churns on per-environment base URLs. `hspec-golden` is an option
for the snapshot compare, but a raw file diff is enough. This slice adds guarantees
*around* the manifest; if any criterion would require changing S34's output,
**escalate** rather than fold the change in here.
