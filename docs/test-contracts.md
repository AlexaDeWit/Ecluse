# Test contract coverage

This map records the assertion-preserving extraction for [#1657](https://github.com/AlexaDeWit/Ecluse/issues/1657).
The suite boundary follows the real collaborator. A shared contract does not move an emulator test into the unit tier.
Case IDs identify Hspec examples, each of which can contain several assertions.
`Contract.*` below abbreviates `Ecluse.Test.Contract.*` under `test/contracts/`.
E2E execution reports audit those IDs against the fixed execution manifest.
Lower-tier catalogues validate registration independently of fixture availability.
Performance runners keep their native sampling and verdict paths.

`fullEcosystemContract` requires proxying, installation, policy, advisories, mirroring,
publication, Dredger, and telemetry. `fullPackageStoreContract` requires reads, writes,
inventory, deletion, integrity, and recovery. These lists are normative for each subject.
Driver capability flags cannot shrink them. Independent obligations detect a family omitted
from registration, while actual Hspec execution proves the registered laws ran.
Each missing required family must fail by name, and unrelated families continue.
npm, PyPI, Cargo, and RubyGems run the full composition.

## Metadata and serving

### Metadata transport

Original: core/test/unit/Ecluse/Core/Registry/Metadata/FetchSpec.hs.
Replacement: test/contracts/Ecluse/Test/Contract/Metadata/Fetch.hs.
Both npm and PyPI run the same body with their original raw documents and builders.
The original adapter-completeness example and native fixture registration remain in FetchSpec.
Each prefix below expands under core.metadata.fetch for npm, PyPI, Cargo and RubyGems.
RubyGems' caller-bound framing replaces held-name-mismatch with held-origin-mismatch.

| Case suffix | Preserved assertions |
| --- | --- |
| status-200, status-201, status-299, status-401, status-403, status-404, status-301, status-304, status-400, status-408, status-410, status-429, status-500, status-503, status-599 | Full and selected result classification, fetch/decode tracing sequence, complete body lengths and digest on success |
| full-charge | Selected reads charge zero, full reads charge every byte |
| oversized-auth-401, oversized-auth-403 | Authorisation classification precedes oversized error-body parsing |
| exact-byte-bound | Whole-source digest at exact maxMetadataBytes |
| gzip-bytes | Decompressed byte count and digest |
| gzip-charge | Decompressed byte charges |
| gzip-bound | Compressed negative control remains below 1024, inflated body exceeds bound |
| decode-span-empty-url, decode-span-refused-connection | Request failure opens no decode span |
| empty-url-fault | Typed FetchUrlUnformable EmptyBaseUrl |
| transport-fault | Typed transport refusal |
| no-redirect | HTTP 302 and exact single captured request path |
| held-parity | Held three-chunk/full HTTP equality, selected equality, version presence and byte charge |
| held-byte-bound | Full and selected BodyTooLarge MetadataBodyLimit 64 |
| held-malformed | Full and selected undecodable |
| held-name-mismatch | Full and selected self-reported other identity |
| held-version-bound | Full TooManyVersions 2 1 |

The full and selected held-read versus response assertions remain inside the contract's bothWays helper for every held case. These were not moved into a provider callback.
The fetch obligations define the required cases for each ecosystem, including Cargo record framing.

### Metadata projection

The projection contract distinguishes `SingleDocument`, `RecordStream` and `CallerBoundRecords` input.
npm and PyPI retain their exact whole-document errors for malformed, absent, and conflicting identities.
Cargo full reads report invalid entries for unusable native rows.
Selected reads exclude unusable rows from matching versions without retaining invalid-entry diagnostics.
Quarantine cannot turn a transport or resource fault into success.
The native Cargo tests keep duplicate-version and EOF scanning controls beside their independent wire reader.

Originals: core/test/unit/Ecluse/Core/Registry/Npm/MetadataSpec.hs and PyPI/MetadataSpec.hs.
Replacement: test/contracts/Ecluse/Test/Contract/Metadata/Projection.hs.
Each suffix expands under core.metadata.projection for npm, PyPI, Cargo and RubyGems.
RubyGems runs the caller-bound variant, with name-binding and origin-binding in place of missing-name and name-mismatch.

| Case suffix | Original examples and assertions |
| --- | --- |
| malformed | Both native full and selected "reports an undecodable body" examples |
| missing-name | Native full and npm selected "reports an absent top-level name as undecodable". PyPI selected counterpart added |
| name-mismatch | Native full and selected self-reported different-name errors remain MetadataNameMismatch, not decode errors |
| version-bound | Native full and selected version/release/file count examples remain TooManyVersions 2 1. Both selected present versions now run |
| artifact-bound | npm full artifact-count breach and exact ceiling. PyPI counterpart added alongside its unchanged file-grouping bound test |
| missing-version | Both native missing-release examples remain Right Nothing |
| selected-parity | Exact domain PackageDetails equality between the production full read and the production selected read, nonempty expected two-version map. Native rich/corpus and grouping parity remain |

Ten npm and eight PyPI duplicate example bodies are removed. Fourteen shared projection examples run.
Native raw-document identity and exact filename observations remain in both specs.
PyPI keeps the full read's artifact count, distinct-release count, selected file count over unselected and unusable entries, and absence of a selected-artifact ceiling.
PyPI keeps API-version refusal, duplicate-key order, original array entry keys, multiple wheels/sdists, normalised version names and release timestamps.
npm keeps packed/tree/reference-reader properties, damaged/raw duplicate wire inputs, source document retention, optional containers, rich version projection, real express corpus and nesting limits.
Independent readers in both ReaderSpec and StreamingSpec trees are unchanged.

### Serving

Replacement: test/contracts/Ecluse/Test/Contract/Serving.hs.
Drivers: test/integration/Ecluse/Server/Pipeline/ServingDriver.hs.
Registrations: npm PackumentIntegrationSpec, and the PyPI, Cargo, and RubyGems AdapterIntegrationSpec modules.
Each suffix expands to integration.serving.npm, integration.serving.pypi, integration.serving.cargo, and integration.serving.rubygems.

| Case suffix | Assertions and original home |
| --- | --- |
| default-denial | Listing and artifact denial without allow rules, added paired coverage |
| identity-precedence | Explicit identity permits a young release, explicit deny defeats allow on listing and artifact, added paired coverage |
| authorised-union | npm, PyPI, Cargo and RubyGems combine trusted-private and gated-public versions. The private origin alone holds one version and its own copy of a shared one, so the listing and that copy's digest show each origin's share. |
| quarantine-private-authority | Shared private young-release authority with old public survivor and rejected young public version. npm native latest-tag assertion remains in its existing gate case |
| private-divergence | Shared private digest and exact bytes on same-version divergence. npm native latest-tag/divergence cases remain |
| private-availability | An unavailable private origin (HTTP 429, 500 or 503, a closed connection) refuses listing and artifact with 503, and an invalid private reply (HTTP 302 or 400) refuses them with 502. A private 404 serves the gated public release, a private 401 or 403 refuses, and a public 503 serves the private release alone. A private 404 beside a public 404 answers listing and artifact with 404, and beside a public 503 with 503. New in every provider |
| public-integrity | npm "filters a hashless public version out of the served listing" moves here, with PyPI counterpart. A SHA-512 public floor then filters the SHA-256 public release from the listing and refuses its artifact with 403, while the SHA-256 trusted-private release stays listed under the default trusted floor, so the floor itself runs for Cargo and RubyGems, whose parsers refuse a hashless row |
| trusted-integrity | npm "drops a hashless trusted-private version from the listing by default" moves here, with PyPI counterpart. A SHA-512 trusted floor then drops a SHA-256 private release that the default floor lists, so the floor itself runs for Cargo and RubyGems, whose parsers refuse a hashless row |
| artifact-integrity | PyPI exact distribution bytes and advertised sha256 checks move here, with npm counterpart. Require fixture bytes, advertised digest and digest of actual response bytes |
| artifact-head | Successful artifact HEAD returns 200, an empty body and a Content-Length equal to the artifact's length, and the origins receive the artifact request only as HEAD. The PyPI instance is new. |
| conditional-coherence | npm 200 HEAD and matching conditional HEAD examples move here. Require GET 200, HEAD 200, empty HEAD body, matching nonempty ETag, exact Content-Length, GET and HEAD 304 with empty bodies |
| metadata-gzip | A gzip-accepting GET gets `Content-Encoding: gzip`, its exact Content-Length, and bytes that inflate to the identity GET's body. Both responses carry `Vary: Accept-Encoding` |
| metadata-gzip-validators | The gzip ETag is weak and differs from the identity ETag. A gzip HEAD states the gzip GET's length and ETag with no body. Only the selected coding's ETag answers 304, on GET and HEAD |
| metadata-gzip-refusal | A request that refuses both gzip and identity gets 406 with `Vary: Accept-Encoding` |
| metadata-gzip-opt-out | With native gzip off, gzip-accepting and `*;q=0` requests get the identity response unchanged, with no `Vary` |
| metadata-gzip-policy | Proxies over one pair of origins replay a gzip validator issued under an admitting policy. The same policy answers 304, a policy that denies every release answers 403, and a policy that denies one release answers 200 with the remaining listing under a new validator |
| artifact-uncoded | Artifact GET and HEAD under `Accept-Encoding: gzip` match the plain requests in status, headers and bytes, with no Content-Encoding |

Seven native duplicate example bodies are removed. Sixty-eight shared serving examples run.
Native npm tags, weaker/stronger floors, empty digests, retention, mutable cache representations, private authorisation, edge authorisation, origin failures and HEAD refusal cases remain.
Native PyPI media type, multi-file identity, exact mount-local URLs, canonical sidecar advertisement and legacy-alias removal with adversarial retained-field controls, trailing slash, project-name canonicalization, content negotiation, helpful refusals, indexed-private credential routing, foreign-host refusal and mutable integrity-floor validators remain.
Native deprecation/yank policy tests from the current baseline remain unchanged.
Native OriginIntegrationSpec and TarballIntegrationSpec retain their assertions under fixed integration case IDs. All pipeline core tests remain unchanged.

### Contract boundaries

The shared contract library imports core/test support only. Runtime/application resource and HTTP wiring stay in the integration driver.
The native driver uses existing serveDepsFor, newTestEnvWithQueue, testWithApplication, requestAt, package fixture builders and digest helpers.
Its independent Aeson readers inspect native wire version and artifact digest fields, never the production metadata projector.

## Backend laws

| Original assertions | Canonical case IDs | Evidence retained or strengthened |
| --- | --- | --- |
| Core Queue.MemorySpec enqueue/receive/ack round trip and every payload field | queue/memory/payload | Same production memory handle, exact job, every field, acknowledgement and empty follow-up poll |
| Core Queue.MemorySpec terminal drop | queue/memory/terminal-drop | Same production terminal action and empty follow-up poll |
| New memory profile distinction | queue/memory/destructive-receive | Receipt itself removes a job without acknowledgement, first delivery count |
| SqsIntegrationSpec enqueue/receive/ack | queue/sqs/payload | Same canonical body as memory, including jobs that carry a trace context and a scoped package, real emulator and production SQS handle |
| SqsIntegrationSpec unacknowledged redelivery and ApproximateReceiveCount | queue/sqs/redelivery | Exact payload on both deliveries and counts 1 then 2, acknowledgement cleanup |
| Core CredentialSpec static token, repeated token and expiry | credential/static/permanent/token and credential/static/expiring/token | Two independent expected reads, expiry and secret preservation, token Show redaction |
| Runtime Credential.CodeArtifactSpec token and expiry | credential/codeartifact-stub/token | Same shared token law over real mint request/parser with HTTP stub, no claim of live mint coverage |
| ProtocolSpec backend name, delete ceiling, refill, completion, alphabet, absent cursor | maintenance/protocol/facts/identity-ceiling, maintenance/protocol/facts/completion-refill, maintenance/protocol/facts/inventory-capability | Explicit Verdaccio protocol profile, no calls to infer capabilities |
| CodeArtifactSpec corresponding facts plus present cursor | maintenance/codeartifact/facts/identity-ceiling, maintenance/codeartifact/facts/completion-refill, maintenance/codeartifact/facts/inventory-capability | Production handle built with real SDK environment, no claim of actual remote API coverage |
| ProtocolSpec consent granted/classification and consent withheld/classification | maintenance/protocol/consented/consent and maintenance/protocol/withheld/consent | Exact verdict and provider reason retained |
| CodeArtifactSpec granted consent/classification | maintenance/codeartifact/consented/consent | Shared law over the production handle and typed control plane, original request sequencing cases retained |
| ProtocolSpec served versions | maintenance/protocol/inventory/versions | Exact versions, presence and revision. The old check omitted revision |
| Protocol exact package listing and new shared CodeArtifact inventory | maintenance/protocol/inventory/packages, maintenance/codeartifact/inventory/packages, maintenance/codeartifact/inventory/versions | Full inventory against independent names/versions, pagination remains native too. The CodeArtifact pages carry a revised published version and an archived one, so presence and revision both reach the law |
| Additional successful deletion law | maintenance/protocol/delete/removed and maintenance/codeartifact/delete/removed | Exact submitted version identities, order and one VersionRemoved per version. CodeArtifact crosses its 100-version ceiling |
| Runtime Cve.SyncSpec refused artifact retains last-good generation | advisory/local/last-good | Full byte, ETag, provenance, install age, remediation and temporary-file snapshot before/after exact epoch refusal |
| Additional real S3 conformance | advisory/s3/last-good | Same canonical body through a direct operator write (an unconditional SDK PutObject, its ETag the producer identity), real S3 fetch and syncStep |

### Full package-store subjects

Verdaccio npm, CodeArtifact npm, CodeArtifact PyPI and CodeArtifact Cargo each run the same ten cases through `fullPackageStoreContract`.
Each ID below has the prefix `package-store/<subject>/`.
The subjects are `verdaccio`, `codeartifact`, `codeartifact-pypi` and `codeartifact-cargo`.
The CodeArtifact profiles run in the `backend-conformance` CI job and use the `codeartifact` backend identity.
The six family names come from an independent completion definition, so removing a family
from the provider catalogue cannot make its absence pass.

| Family | Case suffixes | Required observation |
|---|---|---|
| `reads` | `present`, `absent` | Read both stored versions. Read absence, then fail one package-document read with a 503 and require an error before the next read succeeds. |
| `writes` | `complete`, `idempotent` | Publish a complete release beside a sibling and preserve it on retry. |
| `inventory` | `exact` | List exactly the published package and versions. |
| `deletion` | `selected` | Remove one version while its sibling stays readable and listed. Its artifacts answer 404 at their native locations, and the sibling's still serve their bytes. |
| `integrity` | `bytes-and-digests` | Preserve all artifact bytes and their verified digests. |
| `recovery` | `publish-retry`, `reopen`, `delete-retry` | Report failed work, finish a later attempt, and read completed work from a fresh handle. |

All profiles use production handles through deterministic service boundaries.
The PyPI profile uploads two wheels and one source distribution per version, with distinct consumer metadata.
Its native fixture decodes submitted multipart fields and retains actual uploaded bytes.
Independent format constants check the CodeArtifact requests.
Fresh handles retain only the remote fixture state.
The CodeArtifact result does not claim live AWS operations.
The npm E2E subject supplies separate Verdaccio service evidence.

All other original assertions remain in their native specs. This includes memory FIFO and overflow accounting, SQS visibility extension and active lease renewal, terminal redrive and dead-letter capture budget, unreachable endpoint classification, secret representation, refresh/expiry/cancellation policy, provider mint mapping, maintenance raw JSON preservation, pagination tokens, request credentials, per-version refusals, uncertain/unreached outcomes, upstream classification, cursor persistence and bounds, advisory HEAD/GET race, republication identity, EPSS policy, last-good age, cancellation, and the compile/publish/serve integration.

Consent gating, first-party protection, dry-run, cap charging and sweep recovery remain once in Core.Registry.SweepSpec and its generic sweep modules. These are caller policy, not backend guarantees. S3 package storage remains planned and unimplemented. CodeArtifact control-plane doubles remain unit evidence, not emulator proof.
No automatic test verifies a real AWS token mint or the Datadog SaaS.
Credential tests use local HTTP stubs. Telemetry tests use the local Collector and process logs.

### Negative controls and lifecycle

CodeArtifact registration checks reject a missing PyPI or Cargo profile before fixture acquisition.
The npm and PyPI integrity controls alter successful production reads to change bytes, digests or consumer metadata.
A PyPI control also removes distribution files.
Each control requires an assertion failure and proves that the altered read ran.

Ecluse.Test.Poll.runUntil replaces discarded timeout/poll results in Integration.WorkerLoop. It throws TestContractEscape on progress timeout or early normal worker exit, propagates worker errors, and cancels the worker after progress. Ecluse.Test.PollSpec covers all four outcomes including cleanup.

Ecluse.Test.Env.withEnvOverrides hoists Collector save/apply/restore, saves unique keys, preserves override order and restores absent values. Collector, Ministack and withAmbientAws use it. Existing withEnvVars clear/apply/clear semantics stay intact. EnvSpec checks success, typed action failure, assignment failure, cancellation and duplicate override order. Environment callers remain serial. Boot refusal timing and compiler-output isolation are
separate concerns tracked by #1349, #1182, and #1329.

## Smoke and measurement

The smoke and reader-retention suites run the shared `Contract.RegistrySmoke` and `Contract.Retention` laws under the case IDs below.
Genuine unlabelled native smoke examples remain outside these laws.
The live version-ordering smoke fetches the `smokeNames` packages in `bench/corpus/pins.json`, which list npm, PyPI, RubyGems and Cargo names.

| Original assertion | Replacement case IDs | Preserved conditions |
|---|---|---|
| npm live trusted package identity, non-empty versions, default version ceiling | `live/npm/react`, `live/npm/types-node`, `live/npm/lodash`, `live/npm/typescript` through `Contract.RegistrySmoke` | The same public packages, production full read, default limits, and transport-only pending classification. |
| PyPI live index identity and wheel evidence | `live/pypi/sampleproject` through `Contract.RegistrySmoke`, followed by its native wheel checks | `sampleproject` 4.0.0, exact filename, package/version identity, SHA-256 presence, status 200, non-empty bytes, integrity verification, 65,536-byte wheel bound. |
| RubyGems live index identity and gem evidence | `live/rubygems/rake` through `Contract.RegistrySmoke`, followed by its native archive checks | `rake` 13.2.1, no invalid rows, exact filename, package/version identity, one advertised SHA-256, status 200, non-empty bytes, integrity verification, archive coordinate and checksum checks, 1 MiB gem bound. |
| Cargo live discovery, sparse index and crate evidence | `live/cargo/serde` through `Contract.RegistrySmoke` | crates.io discovery, `serde` 1.0.200, no invalid rows, exact filename, package/version identity, one advertised SHA-256, status 200, non-empty bytes, integrity verification, 1 MiB crate bound. |
| npm repeated release keys, members, dependencies, and selected containers | `retention/npm/{selected,full}/{keys,members,dependencies}` and `retention/npm/selected/containers` through `Contract.Retention` | Original generators, 2,000 and 16,000 repeats, original read operations, chunking, GC sampling, and strict 256 KiB growth allowance. |
| PyPI repeated containers/members, unselected releases, and past-limit files | `retention/pypi/selected/{containers,members,other-releases}`, `retention/pypi/full/{members,past-limit}` through `Contract.Retention` | Original generators and read modes. Each selected input still proves exactly one kept file before sampling. The past-limit case still uses `maxArtifactCount = 2`. |
| Benchmark and allocation provider registration | `Ecluse.Test.EcosystemBench.checkBenchmarkCoverage`, enforced by `ecosystemBenches` | Reviewed native obligations and concrete providers must match the production adapter registry. Missing, duplicate, and unshipped providers fail before corpus loading. Empty corpora fail. |
| Load read and stress registration | `readLoadContract` with `LoadReadDriver`, checked by `Ecluse.BenchLoad.Scenarios.checkFixtureCoverage` before `runDriver` | Each ecosystem owes the shared routine read and stress laws. `requiredLoadLaws` supplies independent requirements, and `loadCoverageGaps` names omitted scenarios. Existing native extensions, floor checks, and verdicts remain. |

These measurement paths retain their native sampling and verdict logic:

| Tier | Existing canonical path | Evidence preserved |
|---|---|---|
| Residency | `MemoryModel.Probe` and `MemoryModelResidencySpec` | Adapter coverage, authenticated captures, isolated child processes, retained shapes, peak limits, charge envelopes, regression limits, and adverse results. |
| Read evaluation | `Registry/MetadataResidencySpec.hs` | Weak-head versus fully forced allowance, weak references, document release, and held typed facts. |
| Allocation | `acceptance/app/Main.hs` over `EcosystemBench` | Four measured legs, fresh timed input allocation, forcing boundaries, corpus/advisory setup, per-capture criteria, refusals, unavailable rows, and report verdicts. |
| Benchmarks | `bench/Main.hs` and `Ecluse/Core/*Bench.hs` over `EcosystemBench` | Native operations and synthetic checks, committed captures, independent expected output fixtures, calibration, and benchmark group order. |
| Load | `BenchLoad.Harness`, native fixtures, `Scenarios`, and `Verdict` | Process isolation, quotas, affinity, RTS flags, upstream latency, request patterns, concurrency-one passes, success floors, refused/unavailable rows, and gating semantics. |
| Live native protocol | npm, PyPI and RubyGems registry smoke specs | npm abbreviated parsing, bounded abbreviated fetch, raw SHA-1/SRI syntax, PyPI exact wheel observations, and RubyGems archive coordinate and checksum checks remain native. |

Existing corpus bytes, expected outputs, remaining floor values, and RTS settings stay fixed.
Provider coverage runs outside measured operations. New PyPI stress rows add samples without changing that boundary.

The `ReadLoadFoundation` scope requires the shared routine read and stress laws for npm, PyPI, Cargo, and RubyGems.
The 5% private-copy and default restart variants remain available through explicit selection.
`FullLoad` adds `MirrorWorker` as a separate requirement.
Active npm, PyPI, Cargo, and RubyGems fixtures and receipts use `FullLoad`, including their native worker rows.
PyPI also requires its three managed-read comparisons.
RubyGems replies use no assembled-response retention, so RubyGems omits the assembled-hit, cache-fit and cache-evict laws.
It adds three per-gem info working sets and a compact specification row.
The new PyPI ceiling, herd, warm-under-cold and ramp rows use the shared law bodies.
Their pass floors use three branch observations per pod shape, recorded in `bench/load/floors.json`.
The existing floors and measured settings stay fixed.

The allocation, live acceptance, benchmark and regular load workflows produce per-ecosystem
performance receipts after native measurement. Each receipt binds actual observations and
their outcomes to independent requirements, recorded conditions, run identity and artifact hashes.
The allocation receipts gate inside `ci.yml`. The timed workflows retain their current cadence.
Benchmark and load workflows stay outside the main CI gate and the functional subject jobs.
Selected load runs and the GC-thrash probe write diagnostic-only records.
They remain usable without claiming the missing full read-load rows.
Live upstream unavailability also records a diagnostic result without a completion claim.
The load receipt requires native `floorEnforcement` to be `held` with no reasons.
An unheld or missing status produces diagnostic-only evidence and keeps the native verdict.

## Supported and deferred profiles

| Profile or capability | State and obligation |
|---|---|
| npm read, mirror, and first-party publication | Supported. Keep public/private authority, exact package bytes, fresh-client parity, native installation observations, and origin evidence. |
| PyPI read and advisory decisions | Supported. Keep Simple JSON, wheel and source distribution bytes, selected-read limits, exact remediation semantics, and client isolation. |
| Shared multi-ecosystem proxy | Supported with one mount per ecosystem. Keep the npm, pip, and Cargo mixed-client scenario and mount-local policy, credentials, jobs, and URLs. |
| Memory queue with an in-process worker | Supported with destructive receive. Split roles over the memory queue remain boot refusals. |
| SQS queue and S3 advisory transport | Supported with native lease, redelivery, and advisory transport checks. S3 advisory storage grants no package-store capability. |
| PyPI publication and mirroring | Supported. Keep native multipart publication, every supported wheel and source distribution, retained companions, and durable complete-version evidence. |
| Cargo sparse reads, mirroring, and publication | Cargo 1.74+ through sparse HTTPS. Keep native discovery, records, dependencies, source identity, checksums, and original-source durable state. Prove archive requests traverse Écluse with real clients. |
| Cargo merged registry and authentication | One registry replaces crates.io. Preserve native credential-provider retry on an edge `401`, local discovery and its `auth-required` rule, private authorisation, first-party private-only reads, build-metadata version identity, and read/upload authority checks. |
| Cargo CodeArtifact maintenance | Physical deletion through deterministic native fixtures. A generic registry's yank API does not provide deletion. No live AWS fidelity claim. |
| Hosted incremental PyPI copying | Supported under [#1639](https://github.com/AlexaDeWit/Ecluse/issues/1639). Contracts require a complete authorised served view or explicit unavailability, retry/restart, and offline completion. |
| Filesystem or S3 package storage | Planned under [#1239](https://github.com/AlexaDeWit/Ecluse/issues/1239). Future contracts require committed completion visibility, interrupted writes, failed markers, restart, concurrent publication, and direct-read/listing invisibility. |
| PyPI scope allow, install-time execution denial, and external-dependency denial | Refused because the adapter blocks those rule types. Existing boot and capability tests keep the reasons explicit. |
| RubyGems Compact Index reads, mirroring, and publication | Bundler 2.6.9 and 4.0.23. Keep `/versions` discovery, on-demand `/info` metadata, archive SHA-256 values, complete platform mirrors with original-source durable state, and native publication. See the [protocol reference](https://ecluse-proxy.com/docs/protocol-support/#rubygems). |
| RubyGems CodeArtifact maintenance | Inventory and physical deletion through deterministic native fixtures. A native yank is not deletion. No live AWS fidelity claim. |
| RubyGems install-time execution denial, yank, external-dependency, scope allow, and deprecation rules | Refused because the adapter blocks those rule types. Existing boot and capability tests keep the reasons explicit. |
| Cargo install-time execution denial, external-dependency, scope allow, and deprecation rules | Refused because the adapter blocks those rule types. Existing boot and capability tests keep the reasons explicit. |
| Missing external service in smoke | Unavailable evidence, never a passing conformance result. Deterministic mirrors remain gating. |
| Missing image, client, ownership, or Docker in required E2E | Failed setup. Pending or absent cases cannot satisfy the execution audit. |

## Reproduction

Run commands inside `env -u IN_NIX_SHELL nix develop`.
Use `task test` for core/runtime/application contract checks, `task test-residency` for unchanged heap probes, and `task test-smoke` for live drift checks.
Use `task perf-allocation` for the allocation gate and the documented benchmark/load targets for measured runs.

Each test entrypoint runs its fixed declaration list. It accepts no custom selection arguments.
Separate manifest tools print fixed coverage metadata without Docker or credentials:

```bash
cabal run ecluse-e2e-manifest
cabal run ecluse-integration-manifest
```

Both print JSON to stdout. The audit tools accept the plan and report paths:

```bash
cabal run ecluse-e2e-audit -- PLAN REPORT...
cabal run ecluse-integration-audit -- PLAN REPORT
```

Run `task test-e2e` for fresh artifacts, all five fixed subjects, and the full audit.
The task prints its artifact directory. Set `ECLTEST_ARTIFACT_DIR` to that path for later runs or auditing.
For staged preparation, choose a fresh directory explicitly:

```bash
mkdir -p scratchpad/e2e
export ECLTEST_ARTIFACT_DIR="$(mktemp -d scratchpad/e2e/run.XXXXXXXXXX)"
task test-e2e-prepare
task test-e2e-load
task test-e2e-npm
task test-e2e-pypi
task test-e2e-cargo
task test-e2e-rubygems
task test-e2e-shared-system
task test-e2e-audit
```

Preparation cannot overwrite an existing artifact directory. Commands sharing that directory hold the same lock.
Rerun a fixed subject with its named task. It reuses the prepared image and immutable plan.
Before checking prerequisites, each attempt invalidates its previous published report and preserves it under `reports/attempts/`.
Every nonempty current report publishes to `reports/<subject>.json`, including failed results.
Publication preserves the command's failure status. Failed reports cannot satisfy completion.
Attempt reports, logs, and status remain available for diagnosis.
The full audit requires every group and case exactly once, with matching execution identities.
`ECLTEST_SEED` defaults to 1. `ECLTEST_TIMEOUT` accepts positive seconds. Neither changes the declaration list.

For a local cross-tier completion audit, give all tiers the same `ECLTEST_SOURCE`
and `ECLTEST_RUN_ID`. Set `ECLTEST_ARCHITECTURE` to `arm64` or `amd64` for the host.
Run `task test-unit-core-receipt`, `task test-unit-runtime-receipt`, and `task test-integration`.
Then run the staged E2E commands above.
Set `ECLTEST_CORE_UNIT_ARTIFACT_DIR` to `scratchpad/unit/core`.
Set `ECLTEST_RUNTIME_UNIT_ARTIFACT_DIR` to `scratchpad/unit/runtime`.
Set `ECLTEST_INTEGRATION_ARTIFACT_DIR` to `scratchpad/integration/$ECLTEST_RUN_ID`.
Run `task test-e2e-npm-completion`, `task test-e2e-pypi-completion`, `task test-e2e-cargo-completion`, or `task test-e2e-rubygems-completion` for the subject.
The completion audit reads the actual reports from all four tiers.
It checks required laws and execution identities against their compiled plans.

The `task test-integration` wrapper supplies container ownership and daemon exclusion controls.
Unit declaration audits run through `task test`. Smoke and residency retain their existing commands.
Integration coverage runs and audits the complete manifest.
`COVERAGE_FLAGS` cannot select cases, bypass execution, or change the report path.

Run `task test-conformance-verdaccio` and `task test-conformance-codeartifact` for the fixed
package-store subjects. Each runs its own manifest, full contract, and audit with an
execution identity. A missing report, failed law, or incomplete subject fails the task.
CodeArtifact uses the production adapter with deterministic control-plane doubles.
Its result does not claim live AWS service coverage.

## Fixed E2E entrypoints

Read each suite's declaration list to see the contracts and concrete fixtures it runs.
The Cabal components are `ecluse-e2e-npm`, `ecluse-e2e-pypi`, `ecluse-e2e-cargo`, `ecluse-e2e-rubygems`, and `ecluse-e2e-shared-system`.

| Subject entrypoint | Composition | Concrete coverage |
|---|---|---|
| [Npm.hs](../test/e2e/Ecluse/E2E/Suites/Npm.hs) | `fullEcosystemContract npmDriver` | Serving, installation, advisory, mirroring, publication, Dredger and telemetry laws. |
| [PyPI.hs](../test/e2e/Ecluse/E2E/Suites/PyPI.hs) | `fullEcosystemContract pypiDriver` | The same required families, with native wheel, sdist, multipart and CodeArtifact fixtures. |
| [Cargo.hs](../test/e2e/Ecluse/E2E/Suites/Cargo.hs) | `fullEcosystemContract cargoDriver` | The same required families, with real sparse clients, native discovery, records, archives, publication, durable state, and compatible maintenance. |
| [RubyGems.hs](../test/e2e/Ecluse/E2E/Suites/RubyGems.hs) | `fullEcosystemContract rubyDriver` and its registration contract | The same required families, with Bundler clients, Compact Index discovery, per-gem metadata, platform archives, publication, durable state, and CodeArtifact maintenance. |
| [SharedSystem.hs](../test/e2e/Ecluse/E2E/Suites/SharedSystem.hs) | Cross-ecosystem and core capability cases | The mixed-client case. |

Each ecosystem driver binds concrete providers for every required family.
PyPI mirror evidence includes whole-version copies, restart recovery, cross-target file selection and retained metadata.
Cross-target checks prove selected files and installed contents. They do not execute foreign architecture code.

The fixed declarations and independent requirements own each subject's required cases.
Unit, integration, residency, allocation, load and smoke evidence remain independent requirements.

Backend completion also requires the PyPI CodeArtifact full-store instance in the existing backend subject.
Its deterministic control-plane tests do not establish live AWS service guarantees.

Native cases also cover partial copies, public outages and exact installed files, including executable scripts.
Publication maps the required `authorisation-refusal` law to `pypi/publish/scope-refusal`.
Its `non-upload-refusal` law sends a raw `remove_pkg`, because no PyPI client sends one.
The write telemetry group supplies `pypi/telemetry/domain-spans` and `pypi/telemetry/private-fetch`.
All ecosystem completion audits use `FullEcosystem` and require actual execution reports.
An absent or unbound driver still produces a named failure under the shared contract.

Cargo's shared installation cases use the `cargo/installation` prefix.
Its [native provider obligations](testing.md#cargo-native-provider) retain graph, rename, feature, target, MSRV, lockfile, and asset-routing observations.
Native mirror cases require retained source yank and time despite target changes, plus restart and outage controls.
Missing or corrupt durable state cannot pass by proving only target archive presence.
Authentication and publication negatives preserve caller authority and distinct approved read and upload roots.

[Fixtures/Contracts.hs](../test/e2e/Ecluse/E2E/Fixtures/Contracts.hs) owns concrete resource acquisition and declaration metadata.
The shared `ecluse-e2e-test-support` library holds the fixture and provider modules.
Metadata checks the actual declarations and reports. It does not choose test combinations at runtime.

## MAL cases

The independent obligations and concrete declarations require these MAL cases.

| Case ID | MAL evidence |
| --- | --- |
| `npm/install/mal-forced-source-required` | Actual check-config and proxy invocations refuse forced activation without a source and name the rule and advisories.url. |
| `npm/install/mal-manual-source-required` | A manual rule under skip reaches the same exact diagnostic requirements in both invocations. |
| `npm/advisories/mal-denial` | Pilot compilation, observed npm generation sync, fresh npm refusal, direct 403, exact structured rule/MAL identifiers, and no mirror publication after the mirror window. |
| `pypi/advisories/mal-denial` | Pilot compilation and PyPI sync, affected wheel omitted while the good wheel remains, fresh pip refusal, direct 403, and exact structured identifiers. |
| `npm/dredger/mal-revocation` | Accepted replacement generation, trusted GET 200 before deletion, one exact MAL deletion line per store and the exact tally, complete mirror snapshot, private-cache inventory, fresh install refusal, direct 403, no refill, and retained fix bytes. |
| `pypi/dredger/mal-revocation` | The same lifecycle through the CodeArtifact mirror and cache fixture, with a fresh pip refusal. |

## Partial-fix cases

Each ecosystem's partial-fix source holds two advisories for one package. The young release fixes
the severe one (CVSS 9.8). A minor one (CVSS 4.2, EPSS 0.005) still affects that release.

| Case ID | Partial-fix evidence |
| --- | --- |
| `npm/advisories/partial-fix` | Under the shipped default, npm installs the young release. Under `remainingAdvisories: abstain`, the listing keeps the old release alone, and npm and a direct read refuse the young one by default. The structured denial record names the minor advisory that still affects it. |
| `pypi/advisories/partial-fix` | The same pair of policies for the Flask releases through pip. |

Both clients run the same `Install.Advisory` law through native `AdvisoryDriver` observations.
Each policy acquires an independent data plane and publishes its source through Pilot.
Scored npm sessions are storeless. The npm MAL session observes a real mirror store.
The structured log parser is shared.
The shared sweep lifecycle checks the exact deletion lines and closing tally the fixture names.
Its MAL case adds the second retaining store and the trusted-read observation before deletion.
The native drivers supply transport and client operations, not alternative business assertions.

## Mirrored yank cases

After a pinned install, the worker copies the project's greatest release to a native target that lists every file as not yanked.
The target cannot carry a yank, so a yank on the mirrored listing comes from the durable mirror record.

| Case ID | Mirrored yank evidence |
| --- | --- |
| `pypi/mirror-native/yanked-greatest` | The public source yanks every file of the greatest release. With public down, a fresh proxy lists only the mirrored files, each with the source's yank reason. With public up, the listing marks only those files yanked, and an unpinned install with a fresh cache selects the release below. |
| `pypi/mirror-native/yank-negative-control` | The same lifecycle without the source yank. Both listings offer every file, and the unpinned install selects the mirrored greatest release. |

## Canonical E2E module index

These modules live under `test/contracts/Ecluse/Test/Contract/` and take observations from the native E2E providers.
The ecosystem spec modules retain native observations and scenario providers.
Fixed suite lists bind their contract functions to concrete fixtures.

| Canonical module | Registered cases | Native evidence retained |
|---|---|---|
| `Install` | The shared npm and PyPI installation families, plus the retained original install IDs | Startup rule text, a `403` for each denied npm tarball, lifecycle-script sentinel, wheel filename/hash and native installed-result observations. An admitted artifact HEAD advertises the length of the artifact's GET bytes, and the public upstream logs only HEAD reads of it meanwhile. |
| `Install.Advisory` | Four shared laws per ecosystem. PyPI canonical-fix stays native. | Exact expected artifact membership in native listings before remediation installation, advertised digests, exact denial text, MAL source and npm mirror observations, and remediation controls. The EPSS denial reads an advisory with no CVSS vector, and its control admits the release a severe advisory below the EPSS threshold affects. The CVSS denial's control admits the release an advisory below the CVSS threshold affects. |
| `Install.Outage` | `pypi/install/outage` | Fresh proxy/client, public-route fault controls and request evidence. |
| `Serving` | `pypi/install/sdist`, `npm/install/head-no-effect`, and paired successful artifact HEAD laws | Source distribution digest and exact bytes, a HEAD length equal to the published tarball's and no-mirror observation. The integration providers use the same artifact laws, and their artifact HEAD also requires the fixture's exact length and an origin that receives only HEAD. |
| `Mirror` | All seven `npm/mirror/*` cases and `npm/install/latest-preservation` | Fresh public/mirror installations, exact native graphs/values, binaries, locks, origin evidence and independent negative control. Native tag observations remain provider-owned. |
| `Publication` | The three `npm/publish/*` cases | First-party identity, expected version present in destination inventory, successful fresh installation, out-of-scope publication refusal, and a refused `npm deprecate` that leaves the store's versions, tags and deprecation markers unchanged. |
| `Telemetry` | The retained six npm cases and paired PyPI read/service laws | Scenario-correlated spans, metrics, readiness and logs from the local Collector. The mirror job span links to the enqueue span that shares the rule evaluation's trace. With telemetry off, a running collector receives no span or metric and the proxy logs no export error. Datadog tag/log checks do not contact its SaaS. |
| `Sweep.Identity` | Seven `npm/dredger/*` identity cases below | Independent mirror/cache inventories, consent, dry-run and first-party observations. |
| `Sweep.Advisory` | The `advisory-revocation` and `mal-revocation` cases for npm and PyPI | Generation swap, exact audit, revoked/fixed installs, stored and served artifact observations. |
| `Sweep.Recovery` | Five recovery cases below | Residual deletion, refill, absent source bytes and copies arriving after cleanup. |
| `Sweep.Rollout` | Three rollout cases below | Durable queue, independently started roles, stale policy, retained bytes and sentinel drain. |

`Sweep.Types` owns the fixture and operation records. `Sweep` owns shared inventory, listing and refusal assertions.
Each Dredger provider keeps five isolated lifecycle brackets: identity, scored advisory, MAL advisory, recovery and rollout.
npm adds a sixth bracket for the skipped-admission outage.
Each provider's brackets remain inside its single schedulable `npm/dredger` or `pypi/dredger` group.
The detailed tables below preserve all 49 original E2E observations and descriptions.
Native deprecation/yank handling, npm publication metadata and tamper-refusal assertions remain in their ecosystem specs.

The new shared installation family runs these five IDs for each of `npm` and `pypi`:
`installation/accepted-install`, `installation/default-denial`, `installation/artifact-integrity`,
`installation/artifact-head`, and `installation/artifact-identity-coding`, with the ecosystem prefix.

Admission preserves the native version or distribution observation until the contract compares it with the expected result.
Refusal requires a failed client and an empty installation tree, including npm's lock file or pip's complete target directory.

The npm advisory counterparts are `npm/advisories/deny-cve`, `npm/advisories/deny-epss`,
`npm/advisories/mal-denial`, `npm/advisories/exact-fix`, and `npm/advisories/partial-fix`.
The PyPI full contract also runs `pypi/telemetry/signals`,
`pypi/telemetry/disabled`, `pypi/telemetry/export-failure`, and `pypi/telemetry/datadog`.
The six former telemetry IDs map as follows. Each case keeps its original observations.

| Former ID | Current ID |
|---|---|
| `telemetry/signals` | `npm/telemetry/signals` |
| `telemetry/domain-spans` | `npm/telemetry/domain-spans` |
| `telemetry/private-fetch` | `npm/telemetry/private-fetch` |
| `telemetry/disabled` | `npm/telemetry/disabled` |
| `telemetry/export-failure` | `npm/telemetry/export-failure` |
| `telemetry/datadog` | `npm/telemetry/datadog` |

`npm/telemetry/domain-spans` and `npm/telemetry/private-fetch` remain required by the full contract.
`pypi/advisories/canonical-fix` keeps its ID in the native advisory group.
Each new ID runs beside the original native case IDs and their negative controls.

## E2E assertion identities

The following original example descriptions retain their assertions and negative controls.
The six telemetry IDs gained the `npm/` prefix as mapped above.
The native modules retain scenario providers. The fixed suite declarations register their contract calls.

### `test/e2e/Ecluse/E2E/Npm/InstallE2ESpec.hs`

| Case ID | Original assertion |
|---|---|
| `npm/install/default-denial` | uses the shipped default to block install scripts and never mirrors the package |
| `npm/install/external-dependency-denial` | uses the shipped default to deny a version whose dependency names a git source, and never mirrors it |
| `npm/install/lifecycle-script-control` | runs no package lifecycle script during a harness install (defence in depth) |
| `npm/install/deprecation-relay` | relays a deprecated version's notice to the client, before and after the worker mirrors the version |
| `npm/install/deprecation-negative-control` | rejects the deprecation warning assertion for a version without the member |
| `npm/install/integrity-refusal` | refuses to mirror an artifact whose bytes fail the integrity gate |
| `npm/install/head-no-effect` | answers HEAD on a tarball with its size but no body, and enqueues no mirror |
| `npm/install/metadata-preservation` | mirrors supported installation metadata and omits unknown and public-registry fields |
| `npm/install/latest-preservation` | keeps the upstream latest on the mirror when an older version is mirrored after it |
| `npm/install/publication-unconfigured` | answers a publish with 405 when no publication target is configured |

### `test/e2e/Ecluse/E2E/Npm/MirrorInstallE2ESpec.hs`

| Case ID | Original assertion |
|---|---|
| `npm/mirror/dependency-graph` | resolves the dependency, its own dependency, and the peer, as the install from public did |
| `npm/mirror/module-exports` | loads every package's export through the root, as the install from public did |
| `npm/mirror/linked-executable` | links the root's executable, which prints the root's export, as the install from public did |
| `npm/mirror/origin-evidence` | takes every packument and artifact from the mirror while public answers nothing |
| `npm/mirror/installed-tree` | leaves the tree the install from public left, apart from the proxy's address |
| `npm/mirror/empty-store-control` | fails the same install when no private store holds the graph |

### `test/e2e/Ecluse/E2E/Npm/PublishE2ESpec.hs`

| Case ID | Original assertion |
|---|---|
| `npm/publish/publish-install` | publishes an in-scope package, then installs it back through the private leg |
| `npm/publish/scope-refusal` | refuses an out-of-scope publish before any upstream write (anti-shadowing guard) |
| `npm/publish/non-upload-refusal` | refuses `npm deprecate` with the `501` text and leaves the store's document unchanged |

### `test/e2e/Ecluse/E2E/Npm/DredgerE2ESpec.hs`

| Case ID | Original assertion | Canonical home and retained evidence |
|---|---|---|
| `npm/dredger/inventory` | walks the seeded store through listPackagesIn, including scoped base-name buckets | `Sweep.Identity`: Exact seeded snapshot, listing matches names, empty and matching prefix inventories, absent prefix empty. |
| `npm/dredger/admission-only` | keeps every version an identity deny limited to admission names, and states the phase at boot | `Sweep.Identity`: Successful role exit, admission-only phase at boot, exact examined/deleted/kept/guard tally, both complete snapshots unchanged. |
| `npm/dredger/denied-versions` | deletes every denied version and preserves all other versions, including first-party versions | `Sweep.Identity`: Successful exit and exact fixture version, deletion reason, no-generation audit and cycle tally. Denied inventory empty, retained snapshot/listing exact, cache unchanged. |
| `npm/dredger/dry-run` | previews distinct real inventories without writes and preserves first-party versions | `Sweep.Identity`: Distinct cache versions, first-party publication and listing readiness. Successful preview exit, both store diagnostics, one store-labelled preview line per copy in each store, and the exact cycle tally over both inventories, with first-party versions guard-skipped. Partial-evidence and non-authority messages. No first-party deletion and both snapshots unchanged. |
| `npm/dredger/consent-refusal` | refuses missing consent, names the key, and leaves every version intact | `Sweep.Identity`: Non-success exit, exact native consent key diagnostic, no sweep events, both snapshots unchanged. |
| `npm/dredger/first-party` | protects every first-party version even when an identity deny names one | `Sweep.Identity`: Successful exit, exact zero-work/guard-skipped tally across both stores, exact protected versions and unchanged mirror snapshot. |
| `npm/dredger/two-stores` | deletes shared and cache-only versions while preserving each store's other contents | `Sweep.Identity`: Cache-only publication and original bounded presence probes: present in cache, absent in mirror. Successful exit. Each snapshot loses only the condemned name. Both name inventories empty. |
| `npm/dredger/advisory-revocation` | revokes the version a new advisory generation condemns and keeps its fix installable | `Sweep.Advisory`: Every fixture release and unaffected package installed, with exact mirrored versions/listing and initial snapshot. Successful generation publication, second swap witness and successful sweep. Exact deletion and cycle tally, with unpinned generation. Snapshot removes only the vulnerable version. Vulnerable install fails and artifact returns 403. Bounded no-refill window, fixed version retained and fixed install succeeds. Stored and served artifact bytes remain nonempty with HTTP 200. |
| `npm/dredger/denied-install` | serves the sibling and refuses the denied version once both stores lose it | `Sweep.Recovery`: Exact initial cache versions and vulnerable bytes. Successful sweep, sibling-only inventories, removed cache artifact 404, metadata and sibling artifact 200. Denied artifact 403 and fresh install failure. Sibling fresh install succeeds with exact installed version. Both sibling-only inventories remain unchanged. |
| `npm/dredger/delete-recovery` | keeps the cache copy its backend refused and removes it on a later run | `Sweep.Recovery`: Exact cache target, version and refused-503 audit. Mirror empty while cache retains version and bytes. Later successful sweep removes the cache copy. Subsequent direct and client reads remain denied. Both inventories empty. |
| `npm/dredger/refill` | mirrors the version again when the policy lifts and the public source still holds bytes | `Sweep.Recovery`: Successful removal and both inventories empty. Relaxed fresh install succeeds with exact version. Bounded mirror inventory and nonempty stored artifact. |
| `npm/dredger/missing-source` | leaves both stores empty when the policy lifts and no source bytes remain | `Sweep.Recovery`: Successful removal and both inventories empty. Public-artifact fault, relaxed artifact 404 and fresh install failure. Original bounded write window, both stores remain empty. |
| `npm/dredger/late-copy` | removes the copies each store received after the cycle that reported them gone | `Sweep.Recovery`: Successful initial cleanup and empty stores. Direct publication into both stores, exact inventory and listing readiness. Later successful rediscovery sweep leaves both stores empty. |
| `npm/dredger/old-worker` | removes the copies an old worker mirrored after a stricter proxy started | `Sweep.Rollout`: Shared durable queue, successful old admissions and first-party publication. Target absent before worker starts, strict public reads denied. Old worker publishes target and unrelated release, target reaches both stores. Private-origin fresh install succeeds with exact version and nonempty served bytes. Successful sweep removes target from both stores and preserves unrelated and first-party versions. Next reads fail. Permissive drain publishes sentinel, never denied release. Successful rescan retains the first-party version. |
| `npm/dredger/old-dredger-refill` | restores a version an old Dredger removed while the newer roles permitted it | `Sweep.Rollout`: Initial cache bytes and mirror HTTP 200. Old policy removes both copies. Converged permissive sweep alone restores nothing. Fresh install succeeds with exact version and mirrored inventory. Restored artifact returns HTTP 200 with identical bytes. Served artifact returns HTTP 200. Exact byte equality strengthens the original SHA-512 equality. |
| `npm/dredger/old-dredger-loss` | leaves the version lost when an old Dredger removed the only remaining bytes | `Sweep.Rollout`: With public bytes absent, retained artifact initially returns 200. Old policy removes both copies. Converged sweep restores nothing. Artifact returns 404 rather than policy refusal. Fresh install fails. Bounded window leaves both stores empty. |

### `test/e2e/Ecluse/E2E/PyPI/InstallE2ESpec.hs`

| Case ID | Original assertion |
|---|---|
| `pypi/install/wheel` | installs the compatible non-yanked wheel with its advertised hash |
| `pypi/install/metadata-companion` | installs and imports through the metadata companion, fetched in identity coding from a gzipping upstream |
| `pypi/install/tampered-companion` | refuses a metadata companion whose bytes miss the advertised digest |
| `pypi/install/yanked-pin` | relays the yank reason when pip installs an exact pin |
| `pypi/install/yank-negative-control` | rejects the yank warning assertion for a release without the member |
| `pypi/install/sdist` | serves the source distribution beside the wheel under the shipped policy |
| `pypi/install/outage` | fails the same install through a second proxy while both public upstreams are down |

### `test/e2e/Ecluse/E2E/PyPI/AdvisoriesE2ESpec.hs`

| Case ID | Original assertion |
|---|---|
| `pypi/advisories/exact-fix` | admits the recent exact fix while the remediation-disabled control quarantines it |
| `pypi/advisories/canonical-fix` | admits a recent release with a canonicalised advisory fix and preserves refusal controls |
| `pypi/advisories/deny-cve` | denies the old affected Flask release by CVE |
| `pypi/advisories/deny-epss` | denies the old affected Flask release by EPSS |

### `test/e2e/Ecluse/E2E/TelemetryE2ESpec.hs`

| Case ID | Original assertion |
|---|---|
| `telemetry/signals` | exports ecluse.* metrics and a span to the collector on a real npm request |
| `telemetry/domain-spans` | emits the rule-eval, mirror-enqueue, and mirror-job domain spans to the collector on a mirror round-trip |
| `telemetry/private-fetch` | serves a mirrored artifact from the private leg, and the collector receives that upstream fetch |
| `telemetry/disabled` | starts, serves a real install, and logs JSONL to stdout -- no collector needed |
| `telemetry/export-failure` | surfaces a throttled export-failure warning yet keeps serving -- an absent collector degrades visibly, no crash |
| `telemetry/datadog` | carries the Datadog unified-service tags to the collector and the dd object onto the logs |

### `test/e2e/Ecluse/E2E/MixedEcosystemE2ESpec.hs`

| Case ID | Original assertion |
|---|---|
| `mixed/alternating-clients` | serves npm, pip and Cargo installs before a second npm install |

## Integration registration and native traceability

The integration runner registers complete lifecycle groups with stable case IDs.
Canonical cases execute shared serving, queue, or advisory contracts. Native cases retain
their assertions and use fixed `integration/<group>/native-NNN` IDs.
The generated manifest is the current inventory: `cabal run ecluse-integration-manifest`.
A parameterised example appends explicit fixture dimensions, such as HTTP status or first-party ownership.
The PyPI cases retain source authority, companion integrity, credential confinement, and private mirror observations.
Canonical `core-metadata` advertisement replaces its former omission control. The two legacy-alias controls remain.
Descriptions never determine the IDs. Keep the numeric ID when changing a description or reordering examples.

The pure expected catalogue and independent obligation snapshot live under `test/contracts/Ecluse/Test/Contract/Integration/`.
Preparation builds the expected plan from that snapshot.
The integration entrypoint discovers actual case IDs from its fixed Hspec specs before acquiring fixtures.
Removing a case, profile, or required capability fails catalogue validation.
Mutation checks exercise every registered case and capability without constructing integration specs.
The shared runner compares actual Hspec result IDs against each group's declared case set.
A deleted action cannot satisfy the catalogue merely because its metadata remains.

The runner registers each pipeline spec directly. The former `PipelineIntegrationSpec`
wrapper was removed because it repeated examples without adding hooks or assertions.

| Group | Native module |
| --- | --- |
| `integration/aws-composition` | `Ecluse.AwsEndToEndIntegrationSpec` |
| `integration/cargo` | `Ecluse.Core.Registry.Cargo.AdapterIntegrationSpec` |
| `integration/pypi` | `Ecluse.Core.Registry.PyPI.AdapterIntegrationSpec` |
| `integration/rubygems` | `Ecluse.Core.Registry.RubyGems.AdapterIntegrationSpec` |
| `integration/mirror-view` | `Ecluse.Core.Server.MirrorViewIntegrationSpec` |
| `integration/egress` | `Ecluse.Core.Security.EgressIntegrationSpec` |
| `integration/origin` | `Ecluse.Core.Server.Pipeline.OriginIntegrationSpec` |
| `integration/npm-metadata` | `Ecluse.Core.Server.Pipeline.PackumentIntegrationSpec` |
| `integration/publication` | `Ecluse.Core.Server.Pipeline.PublishIntegrationSpec` |
| `integration/pipeline-faults` | `Ecluse.Core.Server.Pipeline.SharedIntegrationSpec` |
| `integration/npm-artifact` | `Ecluse.Core.Server.Pipeline.TarballIntegrationSpec` |
| `integration/worker` | `Ecluse.Core.Worker.LoopIntegrationSpec` |
| `integration/pilot` | `Ecluse.PilotIntegrationSpec` |
| `integration/advisory-sync` | `Ecluse.Runtime.Cve.SyncIntegrationSpec` |
| `integration/mirror-state-s3-internal` | `Ecluse.Runtime.Mirror.State.S3.InternalIntegrationSpec` |
| `integration/mirror-state-s3` | `Ecluse.Runtime.Mirror.State.S3IntegrationSpec` |
| `integration/advisory-export` | `Ecluse.Runtime.Pilot.ExportIntegrationSpec` |
| `integration/queue-sqs` | `Ecluse.Runtime.Queue.SqsIntegrationSpec` |
| `integration/server-shutdown` | `Ecluse.Runtime.ServerIntegrationSpec` |
| `integration/telemetry-metrics` | `Ecluse.Runtime.Telemetry.InstrumentsIntegrationSpec` |
| `integration/telemetry-scrape` | `Ecluse.Runtime.Telemetry.ScrapeIntegrationSpec` |
| `integration/telemetry-traces` | `Ecluse.Runtime.Telemetry.TracingIntegrationSpec` |

The manifest and independent obligations enumerate the current IDs and expanded fixture cases.
Each ID labels its Hspec example in the module listed above.
That example retains all of its prior assertions, negative controls, and resource hooks.
The independent obligation file enumerates every expanded parameter case.

Read the explicit contract list in [integration/Spec.hs](../test/integration/Spec.hs).
Run `cabal run ecluse-integration-manifest` to inspect the fixed manifest without Docker.
Run `task test-integration` for the complete tier.
Coverage remains one full integration invocation with the same Cabal instrumentation and four total uploads.

## Unit registration validation

Core and runtime retain `hspec-discover` and their existing dependency boundaries.
The core `Ecluse.Test.Contract.UnitCatalogue` and runtime `Ecluse.Runtime.Test.Contract.UnitCatalogue` hold separate obligations.
Together, they require 100 canonical examples across nine actual spec roots.
Each suite's `SpecHook` reads the labelled declaration trees without executing hooks or examples.
A reporting run validates that catalogue and rejects missing, duplicate, and unknown IDs and missing capabilities.
Native unlabelled examples continue in their original suites.
Production adapter registration anchors ecosystem coverage.
The npm maintenance operations and CodeArtifact format anchor their supported maintenance profiles.
Add a provider's obligations separately from its actual registrations.
