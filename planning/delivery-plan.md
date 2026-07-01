# Écluse Delivery Plan

The dependency-ordered DAG of PR-sized slices that takes **Écluse** from its
functional core, through the merged walking skeleton, to a launch-ready, releasable
proxy and through its designed fast-follows.

This is **Phase 0** of [`orchestration-strategy.md`](orchestration-strategy.md):
the architecture is frozen ([`../docs/architecture.md`](../docs/architecture.md)),
and this plan decomposes it into reviewable work. The architect signs off on this
breakdown **before any code is written**.

This file is the **index**. The authoritative, mutable detail for each slice,including its live status, lives in one file per slice under [`slices/`](slices/).
Per-slice files are deliberate: parallel agents (and their status updates) touch
**disjoint files**, so concurrent work never collides on a shared table.

---

## How to read and use this plan

- **One slice = one PR.** Each slice is a single coherent, reviewable-in-a-sitting
  capability with a limited file scope, the test tier(s) it owes, acceptance
  criteria traced to architecture sections, and explicit dependencies.
- **Status lives in the slice file, and the slice's own PR keeps it current.** Each
  [`slices/`](slices/)`SNN-*.md` carries a `status:` field in its frontmatter,  `not-started → in-progress → in-review → merged`. The slice's implementation PR
  advances it (to `merged`, which becomes true the moment that PR lands) and
  records any as-built delta, with the `planning/slices/` file inside the slice's
  file scope; the
  [inter-wave pass](orchestration-strategy.md#inter-wave-quality--alignment-pass)
  then reconciles every merged slice, and the architecture doc it derives from,  against what actually shipped. The git history of those files *is* the milestone
  log. This index deliberately does **not** duplicate per-slice status (that would
  reintroduce the merge conflict the per-slice split avoids); the
  [**In flight**](#in-flight) section below is the single-writer (team-lead) pointer
  to what is being worked right now.
- **Depth is proximity-proportional.** Near-term slices (M0–M3) are detailed to
  implementation depth now. Later slices (M4–M8) carry goal / criteria / scope /
  deps and are deepened as their dependencies land and their worktrees are rebased
  onto the new base, integration drift is surfaced then, not at PR time.
- **Definition of done** for every slice is the checklist in
  [`orchestration-strategy.md`](orchestration-strategy.md#definition-of-done): all
  acceptance criteria met with test evidence, independent review (Stage A + B)
  passed, the local gate green, Semgrep clean, the CI `gate` green on the PR, docs
  updated in the same PR, GPG-signed Conventional Commits.

---

## Current state (the baseline this plan builds on)

**Built and tested, the pure functional core:**

- `Ecluse.Ecosystem`, the ecosystem tag.
- `Ecluse.Version`, version identity + per-ecosystem ordering (semver / PEP 440 /
  `Gem::Version`), parse-don't-validate.
- `Ecluse.Package`, the full ecosystem-agnostic domain model (`PackageName`,
  `CodeExecSignal`, `Trust`, `Availability`, `Artifact`, `Dependency`, `Person`,
  `PackageDetails`).
- `Ecluse.Rules` + `Ecluse.Rules.Types`, the **pure** rule tier, deny-by-default
  with precedence-field, order-independent selection (S05).
- Mature CI/release infrastructure: the unified `gate`, coverage → Codecov,
  Nix-store cache, lean CI shell, reproducible OCI image + keyless provenance/SBOM
  attestations (S31).

**Built and merged, the walking skeleton (the packument path, end to end):**

- **M0**, the imperative shell (`Env`/`App`), the three handles with in-memory
  doubles, the config loader, `katip` logging, rules-precedence alignment, and the
  SSRF / input-validation / response-bound guards (S01–S05, S36).
- **M1**, the npm adapter: wire decoders, projection to the domain model, the
  `http-client` data plane (fetch/publish), and URL rewrite + packument filtering
  (S06–S09).
- **M2 (core)**, the raw-WAI `Application`: pure router, error/denial model,
  meta-routes + middleware + dispatch, bounded-memory streaming, conditional-GET /
  ETag, and the metadata cache (S10–S13). _The capability manifest (S34) is the
  remaining M2 work, off the launch critical path._
- **M3 (core)**, the cross-upstream packument merge (S33) and the packument path
  end to end (S14): `GET /{pkg}` flows router → parallel multi-upstream fetch →
  gate-public / trust-private → merge + filter → serve, under the default
  `passthrough` credential strategy.
- Pulled in early: **S16** (the `CredentialProvider` generic wrapper + static leaf,
  M4) and **S31** (SLSA provenance + SBOM attestation, M8).

**Merged onto the skeleton, M3 + M4 closed and M5 / M6 partially landed:**

- **M3 (complete)**, the tarball path + demand-driven mirror enqueue (S15): the
  bounded-memory artifact stream and the enqueue-on-miss that hands a mirror request
  to the `MirrorQueue` double.
- **M4 (complete, AWS launch-ready)**, the AWS credential leaf (CodeArtifact
  `mintToken`, S17) and the SQS `MirrorQueue` backend (S18), each behind its handle;
  egress / SSRF hardening, per-context resolved-IP recheck on the untrusted public /
  artifact path, a disallow-by-default `dist.tarball`-host policy, and behaviour-level
  metadata protection (S40); the mirror worker (S19), fetch → verify → publish → ack;
  and the **AWS composition root (S20,
  [#292](https://github.com/AlexaDeWit/Ecluse/pull/292))** that ties the backends into
  the single config-driven composition root, making Écluse a deployable AWS-backed npm
  proxy. _Off the launch critical path, still open under M4: the first-party publish
  path (S52) and the `service` read strategy (S44; `delegated-cache`/S45 superseded)._
- **M5 (partial)**, the effectful rule tier: `Unavailable`, with per-source
  timeout / retry / circuit-breaker (S21). _Remaining: the CVE tier (S22 / S23)._
- **M6 (complete)**, opt-in, vendor-neutral OpenTelemetry: the substrate + telemetry
  config (S24); WAI/http-client + domain spans (S25,
  [#293](https://github.com/AlexaDeWit/Ecluse/pull/293)); and the `ecluse.*` metrics
  catalogue + bounded-label guard + JSONL `dd` log correlation (S26, across
  [#296](https://github.com/AlexaDeWit/Ecluse/pull/296) /
  [#312](https://github.com/AlexaDeWit/Ecluse/pull/312) /
  [#331](https://github.com/AlexaDeWit/Ecluse/pull/331) /
  [#341](https://github.com/AlexaDeWit/Ecluse/pull/341)), with OTLP export failures routed to
  a throttled `katip` warning. _Deferred follow-ons: the Prometheus `/metrics` scrape exporter
  ([#288](https://github.com/AlexaDeWit/Ecluse/issues/288)) and the advisory-sync span / metrics
  (land with the CVE tier, S22)._

> **Refactors layered on the merged slices** (each landed without its own DAG slice,
> is reflected in the **architecture** docs, and is cross-referenced from the
> affected slice file): the agnostic `FilterPlan` extraction (#107 / #119);
> per-source metadata-cache keying with a cached raw document (#111 / #113); the
> injected route classifier, with npm path grammar moved into the adapter
> (#106 / #116); the uncached trusted-leg fix for per-client authority (#115 /
> #117); the **per-mount error renderer + mandatory path-mounting** refactor
> (#122 / #133), which introduced `MountBinding` (`bindingPrefix :: NonEmpty Text`,
> so a root mount is unrepresentable) and moved npm's `{"error": …}` body out of the
> agnostic serve layer into the adapter renderer; and the **Unified Multicall Binary (CLI router)** refactor (e.g. `ecluse serve`, `ecluse pilot`, `ecluse dredger`), ensuring shared config constraints and `ECLUSE_PUBLISH_SCOPES` protection.

**Remaining, what this plan still delivers:** on top of the launch-ready AWS base
(**M4**, closed by S20) and the now-complete observability stack (**M6**), the
first-party publish path (S52) and the non-default credential strategies
(`service`, S43–S44; `delegated-cache`/S45 superseded); the CVE tier (S22 / S23) on top of the merged
effectful tier (**M5**); the GCP backends (M7); the launch docs + release-hardening tail
(M8); the capability manifest (S34; S35 dropped); and the informational benchmark track (M9).

> **Base-hardening before S15.** The config / mount / credential / Reader-context
> generalization decided across the **base-hardening track** (D1–D6) is now
> landed ahead of S15, bringing the merged code into line with the
> already-rendered architecture: ecosystem-keyed mounts with a derived prefix; the
> `MountRegistries` role record; process-global credential providers a mount
> *references*; and the `ReaderT RequestCtx IO` request hot path. Its **decision
> outcomes** are reflected in the affected slice files (S01, S03, S14, S15, S20,
> S43); per the architect it is **not** a DAG entry of its own.

---

## Milestones

| # | Theme | Outcome |
|---|---|---|
| **M0** | Shell, handles & foundations | The imperative shell + the three handle interfaces with in-memory doubles; config loader; logging; rules-precedence alignment. Unblocks every downstream track. |
| **M1** | npm protocol adapter | The npm `RegistryClient`: wire decoders, projection to the domain model, data-plane fetch/publish, URL rewrite + packument filtering. |
| **M2** | Web front door | The raw-WAI `Application`: pure router, error/denial model, meta-routes, middleware, bounded-memory streaming, conditional-GET/ETag, and the metadata cache. (The OpenAPI capability manifest is **statically generated and published to the docs site**, not a served meta-route; see S34.) |
| **M3** | Request pipeline (**walking skeleton**) | The thin end-to-end path: multi-upstream packument merge, the `passthrough` credential default (forward/strip) and the per-mount [credential-strategy](../docs/architecture/access-model.md) framework, packument + tarball serving, demand-driven mirror enqueue (against in-memory cloud doubles). |
| **M4** | AWS cloud backends & worker | `CredentialProvider` (CodeArtifact / static), the mirror-target write, and private-upstream reads under the `service` [strategy](../docs/architecture/access-model.md), SQS `MirrorQueue`, the mirror worker, the AWS composition root. **AWS launch-ready.** |
| **M5** | Effectful rules & CVE | The effectful tier (timeout / retry / circuit-breaker, `Unavailable`), the OSV local-sync in-memory advisory index, and the CVE rules, `DenyIfCVE` (block affected) and `AllowIfRemediatesCve` (fast-track fixes past the quarantine). |
| **M6** | Observability | Opt-in, vendor-neutral OpenTelemetry/OTLP: tracing, the `ecluse.*` metrics catalog, JSONL `dd` log correlation. |
| **M7** | GCP backends | The Pub/Sub de-risking spike → Pub/Sub `MirrorQueue`, the ADC credential leaf, GCP wiring. **Scheduled after AWS launch.** |
| **M8** | Release hardening | SLSA build provenance + SBOM attestation; the launch docs & deployment runbook. |
| **M9** | Benchmarking & load testing | **Two layers, one capability:** (A) deterministic **work-per-request** micro / allocation benches over the pure core; (B) **throughput & latency under load** driving the real `Application` over in-memory doubles, with the mandatory traffic scenarios (merge-in-loop, private cache hit, worker mirroring). **Inform-only, no SLO, never gates** (the workflow reds only on a literal benchmark failure). Off the launch critical path. |
| **M10** | Azure backends | The Service-Bus-vs-Storage-Queues de-risking spike → Azure `MirrorQueue`, the Entra ID credential leaf, Azure composition wiring. **Lowest priority, furthest-out; after AWS *and* GCP.** |

---

## The DAG (slice index)

Every slice links to its detail file. **Depends on** lists the slice IDs that must
be **merged** before it can start. Tier = the test suite(s) it owes
(`U`=unit, `I`=integration, `S`=smoke, `B`=bench, informational, non-gating).
`S` and `B` never gate: a smoke test detects drift against a live service and
**never discharges an acceptance criterion on its own**, so any behaviour a slice
owes also owes a deterministic `U`/`I` test (see [Testing Strategy](../docs/testing.md) →
*What gates, and what doesn't*).

### M0, Shell, handles & foundations

| ID | Slice | Depends on | Tier |
|----|-------|------------|------|
| [S01](slices/S01-app-env-scaffold.md) | App/Env scaffold + composition root | S02 | U |
| [S02](slices/S02-handle-interfaces.md) | Handle interfaces + in-memory doubles | - | U |
| [S03](slices/S03-config-loader.md) | Config model & fail-fast loader | S02, S05 | U |
| [S04](slices/S04-logging-katip.md) | `katip` logging scaffold (json/console) | S01 | U |
| [S05](slices/S05-rules-precedence.md) | Rules precedence alignment | - | U |
| [S36](slices/S36-security-guards.md) | Outbound SSRF + input-validation + response-bound guards, _security gate ([issue #11](https://github.com/AlexaDeWit/Ecluse/issues/11))_ | - | U, I |

### M1, npm protocol adapter

| ID | Slice | Depends on | Tier |
|----|-------|------------|------|
| [S06](slices/S06-npm-wire-decoders.md) | npm wire types + lenient decoders | - | U, S |
| [S07](slices/S07-npm-projection.md) | npm projection → domain (`PackageInfo`/`PackageDetails`) | S06 | U |
| [S08](slices/S08-npm-data-plane.md) | npm data plane: fetch + publish | S02, S07 | U, S |
| [S09](slices/S09-npm-rewrite-filter.md) | URL rewrite + packument filtering | S05, S07 | U |

### M2, Web front door

| ID | Slice | Depends on | Tier |
|----|-------|------------|------|
| [S10](slices/S10-router.md) | Pure router (`classify`/`Route`) | - | U |
| [S11](slices/S11-response-model.md) | Error model + denial responses | S05, S10 | U |
| [S12](slices/S12-wai-app-middleware.md) | WAI app + meta-routes + middleware + dispatch | S01, S10, S11 | U |
| [S13](slices/S13-streaming-cache.md) | Streaming + conditional-GET/ETag + metadata cache | S12 | U |
| [S34](slices/S34-capability-manifest.md) | Capability manifest (OpenAPI), static generation + docs publish (no served endpoint), _not on the launch critical path_ | S03, S12, S14 | U |
| [S35](slices/S35-openapi-drift-controls.md) | ~~OpenAPI contract drift controls~~, **dropped** (can't classify breaking-vs-additive without a semantic OpenAPI differ, deferred; the manifest's `ManifestSpec` unit tests in #427 are the accepted breaking-change guarantor) | S34 | - |

### M3, Request pipeline (walking skeleton)

| ID | Slice | Depends on | Tier |
|----|-------|------------|------|
| [S33](slices/S33-packument-merge.md) | Packument merge across upstreams (core, pure) | S07 | U |
| [S14](slices/S14-packument-path.md) | Packument path end-to-end (**skeleton closes**) | S08, S09, S13, S33 | U |
| [S15](slices/S15-tarball-path.md) | Tarball path + demand-driven mirror enqueue | S14 | U |
| [S43](slices/S43-credential-strategy.md) | Credential strategy + edge authentication, _access-model framework; off the launch critical path_ | S03, S12, S14 | U |

### M4, AWS cloud backends & worker

| ID | Slice | Depends on | Tier |
|----|-------|------------|------|
| [S16](slices/S16-credential-wrapper.md) | `CredentialProvider` generic wrapper + static leaf | S02 | U |
| [S17](slices/S17-codeartifact-leaf.md) | CodeArtifact `mintToken` leaf | S16 | S |
| [S18](slices/S18-sqs-queue.md) | SQS `MirrorQueue` backend | S02 | I |
| [S19](slices/S19-mirror-worker.md) | Mirror worker (fetch → verify → publish → ack) | S08, S16, S18 | U, I |
| [S20](slices/S20-aws-composition.md) | AWS composition root + config wiring (**launch-ready**); reserves the publication-target role | S03, S15, S17, S18, S19 | I |
| [S52](slices/S52-publish-path.md) | First-party publish path → publication target (`PUT /{pkg}`, scope-allowlist guard), _[#163](https://github.com/AlexaDeWit/Ecluse/issues/163)_ | S03, S08, S12, S20 | U, I |
| [S44](slices/S44-service-credential-reads.md) | Service-credential read path (`service` strategy; private leg read per-request, **not** cached), _access-model; off the launch critical path_ | S43, S16, S13 | U, I |
| [S45](slices/S45-delegated-cache-probe.md) | Delegated-cache authorisation probe, **superseded** (Écluse forbids a shared private cache), _access-model; off the launch critical path_ | S44 | U, I |
| [S40](slices/S40-egress-ssrf-hardening.md) | Egress / SSRF hardening, resolved-IP recheck, disallow-by-default tarball-host policy, operator egress docs, _follow-on to [S36](slices/S36-security-guards.md); [issue #11](https://github.com/AlexaDeWit/Ecluse/issues/11)_ | S08, S15 | U, I |

### M5, Effectful rules & CVE

| ID | Slice | Depends on | Tier |
|----|-------|------------|------|
| [S21](slices/S21-effectful-tier.md) | Effectful rule tier (`Unavailable`, timeout/retry/breaker) | S05, S14 | U |
| [S22](slices/S22-cve-sync.md) | `CVELookup` handle + SQLite `osv.db` polling | S01, S21 | U, I |
| [S23](slices/S23-deny-if-cve.md) | CVE rules, `DenyIfCVE` + `AllowIfRemediatesCve` | S03, S22 | U |

### M6, Observability

| ID | Slice | Depends on | Tier |
|----|-------|------------|------|
| [S24](slices/S24-otel-substrate.md) | OTel substrate + telemetry config (off by default) | S01, S03 | U |
| [S25](slices/S25-tracing-spans.md) | WAI/http-client + domain spans | S12, S19, S24 | U, I |
| [S26](slices/S26-metrics-logs.md) | `ecluse.*` metrics + JSONL `dd` correlation | S04, S24 | U, I |

### M7, GCP backends (after AWS launch)

| ID | Slice | Depends on | Tier |
|----|-------|------------|------|
| [S27](slices/S27-gcp-spike.md) | Pub/Sub de-risking spike (decision gate) | S18 | I |
| [S28](slices/S28-pubsub-queue.md) | Pub/Sub `MirrorQueue` backend | S02, S27 | I |
| [S29](slices/S29-adc-credential.md) | Artifact Registry / ADC credential leaf | S16 | S |
| [S30](slices/S30-gcp-composition.md) | GCP composition wiring | S03, S28, S29 | I |

### M8, Release hardening

| ID | Slice | Depends on | Tier |
|----|-------|------------|------|
| [S31](slices/S31-provenance-sbom.md) | SLSA provenance + SBOM attestation | - | - |
| [S32](slices/S32-launch-docs.md) | Launch docs & deployment runbook | S20 | - |
| [S41](slices/S41-image-runtime-smoke.md) | Image runtime smoke, distroless `docker run` + real proxied fetch (`dlopen`/NSS verification), _defence-in-depth; low priority_ | S20 | - |
| [S42](slices/S42-spdx-license-headers.md) | Per-file SPDX license headers (REUSE-style) + new-file lint, _housekeeping; tree-wide sweep, run at a quiet point_ | - | - |
| [S46](slices/S46-dockerhub-org-account.md) | Docker Hub org account + repo-scoped publish token (retire account-wide personal PAT), _accepted risk pre-MVP; harden before GA_ | - | - |
| [S53](slices/S53-e2e-ecosystem.md) | End-to-end testing ecosystem, whole-system through the real composition root, real `npm` CLI on the public surface, server↔worker round-trip; real Verdaccio + scriptable upstream stub, _new **non-gating** `e2e` tier (pre-merge + nightly); built now on `runServices`, rebased onto S20 when it lands; [#271](https://github.com/AlexaDeWit/Ecluse/issues/271)_ | S15, S19 | E2E |
| [S54](slices/S54-bounded-memory-streaming.md) | Bounded-memory streaming, residency-invariant **gating correctness** test (memory independent of artifact size; `maxBodyBytes` cap), _split out of the M9 re-cut as a correctness test, not a perf bench; **last, lowest criticality**_ | S15 | I |
| [S55](slices/S55-revocation-denylist.md) | Revocation denylist, `DenyByIdentity` hard-deny rule (operator yank-before-public-yank), paired with operator purge of Registry B, _post-mirror revocation enabler; pure rule; not launch-critical_ | S05 | U |

### M9, Benchmarking & load testing (informational; never gates)

Re-cut 2026-06-27 (architect-approved): one capability, two layers; **inform-only, no
SLO**; the workflow reds only on a literal benchmark failure. Measured on the **shared
public** runner with **local deep-dives** for trustworthy absolutes; `workflow_dispatch`
for the first iteration. See [`docs/architecture/performance.md`](../docs/architecture/performance.md).

| ID | Slice | Depends on | Tier |
|----|-------|------------|------|
| [S37](slices/S37-benchmark-harness.md) | Harness + work-per-request micro-benches + CI baseline | - | B |
| [S38](slices/S38-pipeline-benchmarks.md) | Load benchmarks + mandatory traffic scenarios | S37 | B |
| [S39](slices/S39-load-and-memory.md) | ~~Macro load + bounded-memory~~, **superseded** (load folded into S38; bounded-memory lifted out as a gating correctness item) | - | - |

### M10, Azure backends (lowest priority; after AWS & GCP)

The furthest-out track, a third cloud behind the same two handles. Gated on its own
de-risking spike (the queue decision), sharper than GCP's; the credential and registry
arms are low-risk. See [Cloud Backends → Azure backends](../docs/architecture/cloud-backends.md#azure-backends-designed-for-furthest-out).

| ID | Slice | Depends on | Tier |
|----|-------|------------|------|
| [S47](slices/S47-azure-spike.md) | Azure queue de-risking spike, **Service Bus (REST) vs Storage Queues (Azurite)** decision gate | S18 | I |
| [S48](slices/S48-azure-queue.md) | Azure `MirrorQueue` backend (per S47) | S02, S47 | I |
| [S49](slices/S49-entra-credential.md) | Entra ID credential leaf (Managed Identity / Workload Identity Federation) | S16 | S |
| [S50](slices/S50-azure-composition.md) | Azure composition wiring | S03, S48, S49 | I |

---

## Parallelization, ~3 slices in flight

Concurrency is capped at **2–3 slices** so evaluation quality stays high
([orchestration-strategy → Subagents and isolation](orchestration-strategy.md#subagents-and-isolation)).
After every merge the team lead rebases the dependent worktrees onto the new base
and re-runs their gate.

The three vertical tracks (foundations, adapter, web) run against the handles in
parallel, then converge at M3. _**Waves 1–3 and the M3 convergence (through S14)
are merged**; the sequence below is the historical record of how the build was
ordered. The live pointer to current work is [In flight](#in-flight)._

- **Wave 1, independent roots (no deps):** `S02` (handles), `S06` (npm decoders),
  `S10` (router). _S05 (rules precedence) is also dependency-free and is the
  natural next pull as a slot frees._
- **Wave 2:** `S01` (Env, needs S02), `S05` (rules precedence, root),
  `S07` (npm projection, needs S06). _`S11` (responses) moves to Wave 3, it needs
  `S05`, which is pulled forward into Wave 2._
- **Between waves, quality & alignment pass.** Once a wave's PRs are all merged,
  and before the next wave is dispatched, run the codebase-wide
  [quality & alignment pass](orchestration-strategy.md#inter-wave-quality--alignment-pass)
  (structural / Haddock / performance; e.g. needless `String`↔`Text`↔`ByteString`
  conversions). **Wave 3 does not start until Wave 2 (S01, S05, S07) is merged and
  this pass is done.**
- **Wave 3:** `S03` + `S04` (config/logging), `S08` (npm fetch/publish), `S11`
  (responses), `S12` (WAI app). `S03` also depends on `S05` (the rule model its
  default-policy merge layers over). `S16` (credential wrapper) can pull in here,  it depends only on the handle (S02) and de-risks M4 early.
- **Converge:** `S09` → `S13`, with `S33` (pure cross-upstream merge, needs only
  `S07`) → `S14` (**walking skeleton closes**) → `S15`.
- **Then:** M4 (AWS) and M5 (CVE) layer on; M6/M8 run as independent parallel
  tracks; **M7 (GCP) is scheduled after the AWS launch** (S20), its spike (S27)
  is the gate on committing the GCP backends. `S34` (capability manifest) is a
  **fast-follow** after `S14` (so it documents a packument the server actually
  serves) and does not gate the launch path; it **concludes the capability-manifest
  epic** (the once-planned `S35` drift controls are **dropped**; see the S35 slice).
- **Performance benchmarking (M9) is an independent, informational track, off the
  critical path.** `S37` depends on nothing pending, it runs against the
  already-merged pure core and is the natural companion to the inter-wave quality
  pass (which then *measures* regressions instead of eyeballing them); `S38` joins
  once the walking skeleton (`S14` / `S33`) lands, `S39` once the tarball path
  (`S15`) is up. **None gate**, they inform.
- **Azure (M10) is the furthest-out track of all**, sequenced **after GCP** and the
  lowest priority in the queue. Gated on its own spike (`S47`: Service Bus-over-REST,
  smoke-only, vs Storage Queues on Azurite, sharper than GCP's emulator gap); the
  Entra credential leaf (`S49`) and Azure Artifacts publish are low-risk. Purely
  additive behind the two handles.

### Critical path to AWS launch

`S02 → S01 → S12 → S13 → S14 → S15 → S20`, with `S06→S07→S08→S09`, `S07→S33`, and
`S16→{S17,S18}→S19` feeding the join at S20. _**Fully merged through S20**
([#292](https://github.com/AlexaDeWit/Ecluse/pull/292)), the AWS launch path is live
end to end: the tarball path (S15), the `S16→{S17,S18}→S19` worker feed, and the
composition join (S20) are all in. See [In flight](#in-flight) for what now leads._

---

## In flight

_**M4 and M6 are closed.** The AWS launch path is live (S20,
[#292](https://github.com/AlexaDeWit/Ecluse/pull/292)), the tarball path (S15), the
`S16→{S17,S18}→S19` worker feed, the effectful rule tier (S21), and egress / SSRF hardening
(S40) tied into the single config-driven composition root. The opt-in observability stack is
complete: the OTel substrate (S24), tracing (S25,
[#293](https://github.com/AlexaDeWit/Ecluse/pull/293)), the `ecluse.*` metrics catalogue +
JSONL `dd` correlation (S26,
[#296](https://github.com/AlexaDeWit/Ecluse/pull/296) /
[#312](https://github.com/AlexaDeWit/Ecluse/pull/312) /
[#331](https://github.com/AlexaDeWit/Ecluse/pull/331)), and OTLP export failures surfaced as
a throttled `katip` warning ([#341](https://github.com/AlexaDeWit/Ecluse/pull/341)). The
non-gating `e2e` tier (S53) boots the whole system, including telemetry, through the real
composition root._

_**Post-wave corrective / baseline pass, done.** The shared host-authority parser
([#329](https://github.com/AlexaDeWit/Ecluse/pull/329)); the bounded in-memory `MirrorQueue`
backend ([#298](https://github.com/AlexaDeWit/Ecluse/pull/298) /
[#313](https://github.com/AlexaDeWit/Ecluse/pull/313)); the integrity vocabulary centralization
+ SHA-384 as a first-class algorithm ([#314](https://github.com/AlexaDeWit/Ecluse/pull/314) /
[#321](https://github.com/AlexaDeWit/Ecluse/pull/321) /
[#334](https://github.com/AlexaDeWit/Ecluse/pull/334)); the shared `ecluse-test-support` library
([#336](https://github.com/AlexaDeWit/Ecluse/pull/336) /
[#338](https://github.com/AlexaDeWit/Ecluse/pull/338)); the `WireVocab` named-enum vocabulary
with the PyPI / RubyGems version-wire arms ([#337](https://github.com/AlexaDeWit/Ecluse/pull/337));
the helper dedupe ([#339](https://github.com/AlexaDeWit/Ecluse/pull/339)); and the
retired-terminology cleanup ([#333](https://github.com/AlexaDeWit/Ecluse/pull/333))._

_**Latest, the first-party publish path (S52, M4) is merged**
([#379](https://github.com/AlexaDeWit/Ecluse/pull/379), closes
[#163](https://github.com/AlexaDeWit/Ecluse/issues/163)): `npm publish` (`PUT /{pkg}`) is
accepted at a mount, gated by the anti-shadowing publish-scope allow-list **before any
upstream write**, then relayed to the publication target with the publisher's forwarded
credential, under the application-wide **credential-redirect invariant** (`redirectCount = 0`
at `withToken`; see the [S52 as-built notes](slices/S52-publish-path.md)).
**E2E coverage of the publish flow is the one item in flight**; nothing else is open._

_**The forward queue**, per the architect's sequencing: the **CVE tier** (S22 / S23, **M5**)
and the **credential-strategy framework** (S43 → S44, the non-default `service` read
strategy; S45/`delegated-cache` superseded), then the pre-release tail, configuration re-evaluation,
the API / contract stability pass, the audit, and the 0.1.0 cut. **M9, benchmarking & load
testing** (S37 → S38) remains an independent, informational, off-critical-path track (re-cut
architect-approved 2026-06-27; see the
[M9 slice index](#m9--benchmarking--load-testing-informational-never-gates)), not yet
started._

---

## Operating cadence (summary)

Full contract in [`orchestration-strategy.md`](orchestration-strategy.md). In
brief:

- **Roles.** The repo owner is the **principal architect** (owns design, reviews
  and merges every PR). The lead agent is the **team lead**: decomposes, dispatches
  implementation subagents, evaluates, reproduces the gate, hands review-ready PRs
  back. **The team lead never merges and never pushes to `main`.**
- **One worktree per agent**, each on its own branch, is a hard rule, including
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
- **Fast local check, then CI is the gate:** `make check` before pushing, the fast
  pre-push tier (build, test, fourmolu/hlint, Semgrep), the gate minus its Docker
  and Haddock tiers; Semgrep clean is the hard floor. Open the draft PR and confirm
  the real `gate` green, don't reproduce the whole gate locally. GPG-signed
  Conventional Commits; SHA-pinned, injection-free workflows.

---

## Explicitly out of scope (this plan and the launch)

Tracked here so reviewers see the boundaries; each is an architecture decision
([architecture → Out of Scope](../docs/architecture.md#out-of-scope-for-now)),
not an omission:

- Package **hosting/storage** (delegated to the configured registries); mirroring
  to raw object storage (writes go through `publishArtifact`, no blob handle).
- **PyPI / RubyGems adapters**, the domain model, `RegistryClient`, and hosting
  model are built to accommodate them, but only the **npm** adapter ships.
- **Search** (`/-/v1/search`), returns `501` at launch (documented as such in the
  [capability manifest](../docs/architecture/api-surface.md)).
- **Re-specifying upstream registry protocols** in the capability manifest, Écluse
  documents *its coverage* (and what is unsupported), not npm's full
  packument / registry contract.
- **On-disk artifact caching** (the mirror retry window is acceptable).
- **Cloud IAM validation at the proxy edge** (a gateway concern).
- **Post-mirror CVE re-scan** of already-mirrored versions, CVE gating is
  point-in-time at ingestion; the re-scan is a deferred follow-on
  ([rules-engine → Point-in-time gating](../docs/architecture/rules-engine.md#point-in-time-gating--a-known-limitation)).
- **Web UI / admin API.**
- **Performance *gating*.** The benchmarks (M9) are informational only, they
  trend allocations / time / latency against prior baselines and comment on
  regressions, but **never block a merge**: correctness may knowingly cost
  performance. Actual optimization is driven by what they reveal, not promised here.
