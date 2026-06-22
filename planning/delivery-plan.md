# Écluse Delivery Plan

The dependency-ordered DAG of PR-sized slices that takes **Écluse** from its
current state — the pure functional core — to a launch-ready, releasable proxy
and through its designed fast-follows.

This is **Phase 0** of [`orchestration-strategy.md`](orchestration-strategy.md):
the architecture is frozen ([`../docs/architecture.md`](../docs/architecture.md)),
and this plan decomposes it into reviewable work. The architect signs off on this
breakdown **before any code is written**.

This file is the **index**. The authoritative, mutable detail for each slice —
including its live status — lives in one file per slice under [`slices/`](slices/).
Per-slice files are deliberate: parallel agents (and their status updates) touch
**disjoint files**, so concurrent work never collides on a shared table.

---

## How to read and use this plan

- **One slice = one PR.** Each slice is a single coherent, reviewable-in-a-sitting
  capability with a limited file scope, the test tier(s) it owes, acceptance
  criteria traced to architecture sections, and explicit dependencies.
- **Status lives in the slice file, and the slice's own PR keeps it current.** Each
  [`slices/`](slices/)`SNN-*.md` carries a `status:` field in its frontmatter —
  `not-started → in-progress → in-review → merged`. The slice's implementation PR
  advances it (to `merged`, which becomes true the moment that PR lands) and
  records any as-built delta, with the `planning/slices/` file inside the slice's
  file scope; the
  [inter-wave pass](orchestration-strategy.md#inter-wave-quality--alignment-pass)
  then reconciles every merged slice — and the architecture doc it derives from —
  against what actually shipped. The git history of those files *is* the milestone
  log. This index deliberately does **not** duplicate per-slice status (that would
  reintroduce the merge conflict the per-slice split avoids); the
  [**In flight**](#in-flight) section below is the single-writer (team-lead) pointer
  to what is being worked right now.
- **Depth is proximity-proportional.** Near-term slices (M0–M3) are detailed to
  implementation depth now. Later slices (M4–M8) carry goal / criteria / scope /
  deps and are deepened as their dependencies land and their worktrees are rebased
  onto the new base — integration drift is surfaced then, not at PR time.
- **Definition of done** for every slice is the checklist in
  [`orchestration-strategy.md`](orchestration-strategy.md#definition-of-done): all
  acceptance criteria met with test evidence, independent review (Stage A + B)
  passed, the local gate green, Semgrep clean, the CI `gate` green on the PR, docs
  updated in the same PR, GPG-signed Conventional Commits.

---

## Current state (the baseline this plan builds on)

**Built and tested — the pure functional core:**

- `Ecluse.Ecosystem` — the ecosystem tag.
- `Ecluse.Version` — version identity + per-ecosystem ordering (semver / PEP 440 /
  `Gem::Version`), parse-don't-validate.
- `Ecluse.Package` — the full ecosystem-agnostic domain model (`PackageName`,
  `CodeExecSignal`, `Trust`, `Availability`, `Artifact`, `Dependency`, `Person`,
  `PackageDetails`).
- `Ecluse.Rules` + `Ecluse.Rules.Types` — the **pure** rule tier (the three
  launch rules), deny-by-default.
- Mature CI/release infrastructure: the unified `gate`, coverage → Codecov,
  Nix-store cache, lean CI shell, reproducible OCI image + keyless provenance/SBOM attestations.

**Design-only (no code yet) — what this plan delivers:** the imperative shell
(`Env`/`App`), the three handles (`RegistryClient`, `MirrorQueue`,
`CredentialProvider`), the config loader, the npm adapter, the web layer, the
request pipeline, the AWS backends + mirror worker, the effectful/CVE tier,
observability, and the GCP backends.

> **One alignment item folded in:** `Ecluse.Rules.evalRules` currently selects by
> *list order* (deny short-circuits, first allow wins). The architecture specifies
> *precedence-field*-based, order-independent selection plus an `Unavailable`
> fourth outcome. Slice **S05** brings the code to the end-state design.

---

## Milestones

| # | Theme | Outcome |
|---|---|---|
| **M0** | Shell, handles & foundations | The imperative shell + the three handle interfaces with in-memory doubles; config loader; logging; rules-precedence alignment. Unblocks every downstream track. |
| **M1** | npm protocol adapter | The npm `RegistryClient`: wire decoders, projection to the domain model, data-plane fetch/publish, URL rewrite + packument filtering. |
| **M2** | Web front door | The raw-WAI `Application`: pure router, error/denial model, meta-routes, middleware, bounded-memory streaming, conditional-GET/ETag, metadata cache, and the capability manifest (`/openapi.json`). |
| **M3** | Request pipeline (**walking skeleton**) | The thin end-to-end path: multi-upstream packument merge, credential forward/strip, packument + tarball serving, demand-driven mirror enqueue (against in-memory cloud doubles). |
| **M4** | AWS cloud backends & worker | `CredentialProvider` (CodeArtifact / static), SQS `MirrorQueue`, the mirror worker, the AWS composition root. **AWS launch-ready.** |
| **M5** | Effectful rules & CVE | The effectful tier (timeout / retry / circuit-breaker, `Unavailable`), the OSV local-sync in-memory advisory index, and the CVE rules — `DenyIfCVE` (block affected) and `AllowIfRemediatesCve` (fast-track fixes past the quarantine). |
| **M6** | Observability | Opt-in, vendor-neutral OpenTelemetry/OTLP: tracing, the `ecluse.*` metrics catalog, JSONL `dd` log correlation. |
| **M7** | GCP backends | The Pub/Sub de-risking spike → Pub/Sub `MirrorQueue`, the ADC credential leaf, GCP wiring. **Scheduled after AWS launch.** |
| **M8** | Release hardening | SLSA build provenance + SBOM attestation; the launch docs & deployment runbook. |
| **M9** | Performance benchmarking | The benchmark harness + micro / pipeline / load benchmarks, wired into CI as an **informational, non-gating** trend compared against prior baselines. **Never blocks a merge** — a correctness change may knowingly accept a regression. Off the launch critical path. |

---

## The DAG (slice index)

Every slice links to its detail file. **Depends on** lists the slice IDs that must
be **merged** before it can start. Tier = the test suite(s) it owes
(`U`=unit, `I`=integration, `S`=smoke, `B`=bench — informational, non-gating).

### M0 — Shell, handles & foundations

| ID | Slice | Depends on | Tier |
|----|-------|------------|------|
| [S01](slices/S01-app-env-scaffold.md) | App/Env scaffold + composition root | S02 | U |
| [S02](slices/S02-handle-interfaces.md) | Handle interfaces + in-memory doubles | — | U |
| [S03](slices/S03-config-loader.md) | Config model & fail-fast loader | S02, S05 | U |
| [S04](slices/S04-logging-katip.md) | `katip` logging scaffold (json/console) | S01 | U |
| [S05](slices/S05-rules-precedence.md) | Rules precedence alignment | — | U |
| [S36](slices/S36-security-guards.md) | Outbound SSRF + input-validation + response-bound guards — _security gate ([issue #11](https://github.com/AlexaDeWit/Ecluse/issues/11))_ | — | U, I |

### M1 — npm protocol adapter

| ID | Slice | Depends on | Tier |
|----|-------|------------|------|
| [S06](slices/S06-npm-wire-decoders.md) | npm wire types + lenient decoders | — | U, S |
| [S07](slices/S07-npm-projection.md) | npm projection → domain (`PackageInfo`/`PackageDetails`) | S06 | U |
| [S08](slices/S08-npm-data-plane.md) | npm data plane: fetch + publish | S02, S07 | U, S |
| [S09](slices/S09-npm-rewrite-filter.md) | URL rewrite + packument filtering | S05, S07 | U |

### M2 — Web front door

| ID | Slice | Depends on | Tier |
|----|-------|------------|------|
| [S10](slices/S10-router.md) | Pure router (`classify`/`Route`) | — | U |
| [S11](slices/S11-response-model.md) | Error model + denial responses | S05, S10 | U |
| [S12](slices/S12-wai-app-middleware.md) | WAI app + meta-routes + middleware + dispatch | S01, S10, S11 | U |
| [S13](slices/S13-streaming-cache.md) | Streaming + conditional-GET/ETag + metadata cache | S12 | U |
| [S34](slices/S34-capability-manifest.md) | Capability manifest (OpenAPI) + `/openapi.json` + docs render — _not on the launch critical path_ | S03, S12, S14 | U |
| [S35](slices/S35-openapi-drift-controls.md) | OpenAPI contract drift controls (`validateToJSON`, route↔op exhaustiveness, golden snapshot) — _enhancement; fast-follow after S34_ | S34 | U |

### M3 — Request pipeline (walking skeleton)

| ID | Slice | Depends on | Tier |
|----|-------|------------|------|
| [S33](slices/S33-packument-merge.md) | Packument merge across upstreams (core, pure) | S07 | U |
| [S14](slices/S14-packument-path.md) | Packument path end-to-end (**skeleton closes**) | S08, S09, S13, S33 | U |
| [S15](slices/S15-tarball-path.md) | Tarball path + demand-driven mirror enqueue | S14 | U |

### M4 — AWS cloud backends & worker

| ID | Slice | Depends on | Tier |
|----|-------|------------|------|
| [S16](slices/S16-credential-wrapper.md) | `CredentialProvider` generic wrapper + static leaf | S02 | U |
| [S17](slices/S17-codeartifact-leaf.md) | CodeArtifact `mintToken` leaf | S16 | S |
| [S18](slices/S18-sqs-queue.md) | SQS `MirrorQueue` backend | S02 | I |
| [S19](slices/S19-mirror-worker.md) | Mirror worker (fetch → verify → publish → ack) | S08, S16, S18 | U, I |
| [S20](slices/S20-aws-composition.md) | AWS composition root + config wiring (**launch-ready**) | S03, S15, S17, S18, S19 | I |

### M5 — Effectful rules & CVE

| ID | Slice | Depends on | Tier |
|----|-------|------------|------|
| [S21](slices/S21-effectful-tier.md) | Effectful rule tier (`Unavailable`, timeout/retry/breaker) | S05, S14 | U |
| [S22](slices/S22-cve-sync.md) | `CVELookup` handle + OSV local-sync index | S01, S21 | U, I |
| [S23](slices/S23-deny-if-cve.md) | CVE rules — `DenyIfCVE` + `AllowIfRemediatesCve` | S03, S22 | U |

### M6 — Observability

| ID | Slice | Depends on | Tier |
|----|-------|------------|------|
| [S24](slices/S24-otel-substrate.md) | OTel substrate + telemetry config (off by default) | S01, S03 | U |
| [S25](slices/S25-tracing-spans.md) | WAI/http-client + domain spans | S12, S19, S24 | U, I |
| [S26](slices/S26-metrics-logs.md) | `ecluse.*` metrics + JSONL `dd` correlation | S04, S24 | U, I |

### M7 — GCP backends (after AWS launch)

| ID | Slice | Depends on | Tier |
|----|-------|------------|------|
| [S27](slices/S27-gcp-spike.md) | Pub/Sub de-risking spike (decision gate) | S18 | I |
| [S28](slices/S28-pubsub-queue.md) | Pub/Sub `MirrorQueue` backend | S02, S27 | I |
| [S29](slices/S29-adc-credential.md) | Artifact Registry / ADC credential leaf | S16 | S |
| [S30](slices/S30-gcp-composition.md) | GCP composition wiring | S03, S28, S29 | I |

### M8 — Release hardening

| ID | Slice | Depends on | Tier |
|----|-------|------------|------|
| [S31](slices/S31-provenance-sbom.md) | SLSA provenance + SBOM attestation | — | — |
| [S32](slices/S32-launch-docs.md) | Launch docs & deployment runbook | S20 | — |

### M9 — Performance benchmarking (informational; never gates)

| ID | Slice | Depends on | Tier |
|----|-------|------------|------|
| [S37](slices/S37-benchmark-harness.md) | Benchmark harness + pure-core micro-benchmarks | — | B |
| [S38](slices/S38-pipeline-benchmarks.md) | End-to-end pipeline benchmarks (in-process) | S14, S33, S37 | B |
| [S39](slices/S39-load-and-memory.md) | Macro load + bounded-memory streaming observations | S15, S37 | B |

---

## Parallelization — ~3 slices in flight

Concurrency is capped at **2–3 slices** so evaluation quality stays high
([orchestration-strategy → Subagents and isolation](orchestration-strategy.md#subagents-and-isolation)).
After every merge the team lead rebases the dependent worktrees onto the new base
and re-runs their gate.

The three vertical tracks (foundations, adapter, web) run against the handles in
parallel, then converge at M3:

- **Wave 1 — independent roots (no deps):** `S02` (handles), `S06` (npm decoders),
  `S10` (router). _S05 (rules precedence) is also dependency-free and is the
  natural next pull as a slot frees._
- **Wave 2 (in flight):** `S01` (Env, needs S02), `S05` (rules precedence, root),
  `S07` (npm projection, needs S06). _`S11` (responses) moves to Wave 3 — it needs
  `S05`, which is pulled forward into Wave 2._
- **Between waves — quality & alignment pass.** Once a wave's PRs are all merged,
  and before the next wave is dispatched, run the codebase-wide
  [quality & alignment pass](orchestration-strategy.md#inter-wave-quality--alignment-pass)
  (structural / Haddock / performance — e.g. needless `String`↔`Text`↔`ByteString`
  conversions). **Wave 3 does not start until Wave 2 (S01, S05, S07) is merged and
  this pass is done.**
- **Wave 3:** `S03` + `S04` (config/logging), `S08` (npm fetch/publish), `S11`
  (responses), `S12` (WAI app). `S03` also depends on `S05` (the rule model its
  default-policy merge layers over). `S16` (credential wrapper) can pull in here —
  it depends only on the handle (S02) and de-risks M4 early.
- **Converge:** `S09` → `S13`, with `S33` (pure cross-upstream merge, needs only
  `S07`) → `S14` (**walking skeleton closes**) → `S15`.
- **Then:** M4 (AWS) and M5 (CVE) layer on; M6/M8 run as independent parallel
  tracks; **M7 (GCP) is scheduled after the AWS launch** (S20) — its spike (S27)
  is the gate on committing the GCP backends. `S34` (capability manifest) is a
  **fast-follow** after `S14` (so it documents a packument the server actually
  serves) and does not gate the launch path; `S35` (manifest drift controls) is a
  further enhancement layered on `S34`, also off the launch path.
- **Performance benchmarking (M9) is an independent, informational track, off the
  critical path.** `S37` depends on nothing pending — it runs against the
  already-merged pure core and is the natural companion to the inter-wave quality
  pass (which then *measures* regressions instead of eyeballing them); `S38` joins
  once the walking skeleton (`S14` / `S33`) lands, `S39` once the tarball path
  (`S15`) is up. **None gate** — they inform.

### Critical path to AWS launch

`S02 → S01 → S12 → S13 → S14 → S15 → S20`, with `S06→S07→S08→S09`, `S07→S33`, and
`S16→{S17,S18}→S19` feeding the join at S20.

---

## In flight

_Wave 1 (S02, S06, S10) and Wave 2 (S01, S05, S07) merged; the inter-wave
[quality & alignment pass](orchestration-strategy.md#inter-wave-quality--alignment-pass)
ran (PR #45 — `LoweredHostSet` + a HADDOCK §11 sweep). **Wave 3 in flight:** S11
(responses) merged; S03 (config) and S08 (npm data plane) in review; S04 (logging),
S16 (credential wrapper), and S12 (WAI app, needs S11) are the next pulls._

---

## Operating cadence (summary)

Full contract in [`orchestration-strategy.md`](orchestration-strategy.md). In
brief:

- **Roles.** The repo owner is the **principal architect** (owns design, reviews
  and merges every PR). The lead agent is the **team lead**: decomposes, dispatches
  implementation subagents, evaluates, reproduces the gate, hands review-ready PRs
  back. **The team lead never merges and never pushes to `main`.**
- **One worktree per agent**, each on its own branch, is a hard rule — including
  for this planning work. Implementer agents keep changes within their slice's file scope,
  touching other files only with strong justification.
- **The per-PR loop:** BUILD (implementer, TDD, self-runs the local gate) →
  EVALUATE (a fresh reviewer agent: Stage A requirements/traceability, Stage B
  quality/security/test-quality) → GATE (reproduce CI locally, push, confirm the
  real `gate` green) → HAND OFF to the architect.
- **Escalate, don't guess.** Any agent that is stuck, unsure, or facing
  ambiguous / missing / contradictory spec stops and surfaces it. No fabricated
  values or API behaviour, no silently-weakened tests, no `.semgrepignore` without
  the architect's approval, no sprawl beyond the slice's file scope without strong justification, no leftover
  `TODO`/`undefined`/stub passed off as done.
- **Reproduce the gate before handoff:**
  `make check && make test-integration && make docs-site && make nix-check`;
  Semgrep clean; GPG-signed Conventional Commits; SHA-pinned, injection-free
  workflows.

---

## Explicitly out of scope (this plan and the launch)

Tracked here so reviewers see the boundaries; each is an architecture decision
([architecture → Out of Scope](../docs/architecture.md#out-of-scope-for-now)),
not an omission:

- Package **hosting/storage** (delegated to the configured registries); mirroring
  to raw object storage (writes go through `publishArtifact`, no blob handle).
- **PyPI / RubyGems adapters** — the domain model, `RegistryClient`, and hosting
  model are built to accommodate them, but only the **npm** adapter ships.
- **Search** (`/-/v1/search`) — returns `501` at launch (documented as such in the
  [capability manifest](../docs/architecture/api-surface.md)).
- **Re-specifying upstream registry protocols** in the capability manifest — Écluse
  documents *its coverage* (and what is unsupported), not npm's full
  packument / registry contract.
- **On-disk artifact caching** (the mirror retry window is acceptable).
- **Cloud IAM validation at the proxy edge** (a gateway concern).
- **Post-mirror CVE re-scan** of already-mirrored versions — CVE gating is
  point-in-time at ingestion; the re-scan is a deferred follow-on
  ([rules-engine → Point-in-time gating](../docs/architecture/rules-engine.md#point-in-time-gating--a-known-limitation)).
- **Web UI / admin API.**
- **Performance *gating*.** The benchmarks (M9) are informational only — they
  trend allocations / time / latency against prior baselines and comment on
  regressions, but **never block a merge**: correctness may knowingly cost
  performance. Actual optimization is driven by what they reveal, not promised here.
