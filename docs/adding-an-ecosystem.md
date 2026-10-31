# Adding an ecosystem

Use this guide to take a package ecosystem from a protocol assessment to complete support in Écluse.
npm, PyPI, Cargo, and RubyGems are the shipped examples. Their shared machinery covers more than metadata reads.
The [Cargo integration map](cargo-readiness.md) links its native contract and implementation responsibilities.

| Need | Authoritative reference |
|---|---|
| Domain signals and adapter boundary | [Registry model](architecture/registry-model.md#registry-abstraction) |
| Reader techniques and their limits | [Metadata readers and performance](ecosystem-metadata.md) |
| Test tiers, fixture paths, measurements, and CI duties | [Testing: onboarding an ecosystem](testing.md#onboarding-an-ecosystem) |
| Common assertions and native extensions | [Contract coverage map](test-contracts.md) |
| Configuration keys and defaults | [config/default.yaml](../config/default.yaml) |
| Supported operator behaviour | [Operator manual](../web/content/docs/) |

## 1. Establish the native contract

Read the registry and client specifications before choosing an existing adapter to copy.
Record the supported client versions, index protocol, registry operations, and backend capabilities in the implementation issue.
Follow relevant predecessor decisions before raising a design question.

Before writing adapter code, agree each rule's support and default with the repository owner.
Record its native meaning, evidence source, acquisition cost, and behaviour when evidence is unavailable in the implementation issue.
Choose whether to enable the rule by default, offer it as opt-in, or block it through `adapterRuleBlocks`.

Check whether the rule rejects routine ecosystem behaviour, such as source builds that execute code during installation.
A technically available signal does not make a rule practical.
Resolve this before adding evidence acquisition such as archive inspection.

Answer these questions with protocol evidence:

- How do names, namespaces, versions, and artifact filenames form identities?
- What metadata does installation require, including dependency, feature, and target information?
- Which facts support age, availability, install-time execution, and advisory rules?
- How do clients discover metadata, download, and publication endpoints?
- How do private credentials, caches, conditional requests, and lockfiles affect those requests?
- Do initial downloads verify the registry's advertised artifact digest, or does the client only record the received bytes?
- What proves a complete version, and what survives interruption or public-source outage?
- Which backend supplies inventory and deletion under the required authority and consent?

Keep protocol support separate from backend support. A store's missing operation does not invalidate unrelated uses of its ecosystem.
Refuse incompatible roles at boot or their capability boundary, using the existing capability checks.
The [cloud backend model](architecture/cloud-backends.md) owns that separation.

## 2. Define identity and registration

Start with pure identity parsing. Preserve raw version text while deriving the ordering key from the native grammar.
Do not assume two ecosystems with SemVer share all spelling, comparison, or advisory rules.
An unavailable ordering key remains unavailable. It does not invent an order or silently discard the published version.

| Change | Existing home | Worked example |
|---|---|---|
| Ecosystem tag, config spelling, mount prefix | [Ecosystem.hs](../core/src/Ecluse/Core/Ecosystem.hs) | Explicit constructors and wire table |
| Safe native package name | [Package.hs](../core/src/Ecluse/Core/Package.hs) and the adapter's `Project` module | npm scopes and PyPI normalisation |
| Version grammar and advisory spelling | [Version.hs](../core/src/Ecluse/Core/Version.hs) | Ecosystem-specific keys over preserved text |
| Adapter capabilities and typed rule blocks | [Adapter/Types.hs](../core/src/Ecluse/Core/Registry/Adapter/Types.hs) | [npm](../core/src/Ecluse/Core/Registry/Npm/Adapter.hs) and [PyPI](../core/src/Ecluse/Core/Registry/PyPI/Adapter.hs) |
| Adapter registration | [Adapter.hs](../core/src/Ecluse/Core/Registry/Adapter.hs) | Explicit dispatch, including unsupported tags |
| First-party names | `adapterFirstParty` and [Registry.FirstParty](../core/src/Ecluse/Core/Registry/FirstParty.hs) | [Cargo](../core/src/Ecluse/Core/Registry/Cargo/FirstParty.hs) names and prefixes over the shared grammar |
| Configuration and role validation | [Composition/Validate.hs](../src/Ecluse/Composition/Validate.hs) | Refuse missing active capabilities and blocked policies |

The first-party declaration reads each `firstParty` entry, names a refused entry, and decides which names the entries own.
Configuration and composition read it, so neither names the ecosystem.
Register new modules in the owning Cabal package. A tag alone does not establish supported behaviour.
Keep incomplete providers explicit in tests and documentation until their required behaviours pass.
Extend the default configuration reference and operator manual when the supported surface changes.

## 3. Build native requests and routes over shared transport

Put native paths, media types, and credential presentation in the adapter's modules.
[Registry.Request](../core/src/Ecluse/Core/Registry/Request.hs) owns URL failures, redirect refusal, and outbound request identity.
Its `metadataGet` shares compressed metadata negotiation. Its `artifactGet` preserves artifact bytes without decompression.
Use them where their HTTP contracts fit. A new protocol need not negotiate gzip merely because these adapters do.

The [credential mapping](../core/src/Ecluse/Core/Registry/Request.hs) names the header and native token encoding.
The common pipeline does not impose npm's bearer scheme or PyPI's basic scheme on another ecosystem.
Render a service credential through the `ServicePresentation` the codec and relay receive, so a store that declares its own mechanism applies to the new ecosystem too.
The Dredger reads a store through the codec's target read under the store's presentation, so the codec's read must present every request it forms, discovery included.
Keep caller authorisation separate from service credentials used for mirror or maintenance operations.
Never expand credential authority to satisfy a missing backend operation.

Declare native routes with [Server.Route](../core/src/Ecluse/Core/Server/Route.hs).
Derive routing and the published route descriptions from the same table.
Check metadata, artifacts, publication, discovery documents, errors, `GET`, and `HEAD` against the native client contract.
Identify any response action the current pipeline cannot express before adding adapter code.
Use `MetadataResponse` and `metadataReplies` in [Packument.Response](../core/src/Ecluse/Core/Server/Pipeline/Packument/Response.hs) for shared metadata status choices.
Keep native refusal bodies, denial wording, media types, and route descriptions in the adapter.

## 4. Supply reads, retained documents, and assembly

Supply a `PrepareRead` through `AdapterMetadata.metadataRead`.
A full listing read that keeps many small entries binds the origin's `ocChargeFullReadWorkspace` in a `PrepareRead` and pays the per-entry floor through `readFlooredWalk`, as `prepareNpmRead` and `preparePyPIRead` do. `staticRead` binds no workspace, so use it only for a read with no such floor.
Discovery-based adapters prepare the pure `EcosystemRead` under the selected origin's existing authority.
Prepare private reads after caller authentication and service reads after credential minting. Do not perform discovery at boot.
Declare `AdmittedIndexedArtifactLocation` when private artifact selection requires discovery or a native workspace reservation.
It requires metadata admission before the lookup. `IndexedArtifactLocation` retains static private lookups outside admission unless managed mirror state requires it.
[Metadata.Fetch](../core/src/Ecluse/Core/Registry/Metadata/Fetch.hs) owns the response lifetime, transport errors, tracing, full-read charge, source digest, and manifest.
The adapter supplies requests, full and selected walks, and their finishes.
Use the same production reader with held chunks for deterministic fixtures.

Choose framing from the protocol. npm and PyPI consume a single top-level JSON value.
A line-oriented index needs its own framing and reference acceptance, even if each record uses the shared JSON primitives.
Bound the full response, including data after the selected entry. Keep cancellation and progress failures visible.
The detailed implementation order is in [Metadata readers and performance](ecosystem-metadata.md#apply-the-techniques-in-this-order).

Keep installation fields in the native retained document.
Project only the facts consumed by policy, merging, admission, serving, and mirroring into `PackageDetails`.
Use [CachedDocument](../core/src/Ecluse/Core/Registry/CachedDocument.hs) for the pipeline's opaque document boundary.
New native representations require explicit boundary cases and corresponding retention coverage.

The shared merge chooses admitted source entries.
[ServedDocument](../core/src/Ecluse/Core/Registry/ServedDocument.hs) supplies exact entry selection, name checks, URL rebasing, and native encoding through `serialiseAcross`.
Adapters retain packed-render refusal, object, array, or record-stream assembly, native envelope fields, ordering, and document projections.
Never reconstruct installable metadata from policy facts alone.

Validate artifact locations through the existing [package filter](../core/src/Ecluse/Core/Package/Filter.hs).
Preserve the distinction between source digest claims and verified bytes.
Keep private-source authorisation, first-party precedence, and merge provenance under the existing registry contract.
Declare `AdapterArtifact.artifactPrivateAddressing` independently of `artifactHosts` in [Adapter.Capability](../core/src/Ecluse/Core/Registry/Adapter/Capability.hs).
`ConventionalArtifactPath` uses a native path. `IndexedArtifactLocation` requires an authoritative private metadata location.
The public host allowance does not select private addressing.

Prove that supported clients verify the registry's advertised archive digest on the first download.
Use an ordinary installation without an existing lockfile, then repeat with an incorrect registry checksum.
Frozen lockfile checks and mirror ingestion do not establish this check for a first download.
Choose a metadata protocol that carries strong archive checksums to the client.
Record its discovery, freshness and acquisition costs before writing the adapter.
Serve paths rely on client verification. The mirror worker verifies copied archive bytes.

## 5. Map policy and advisory evidence

Map native observations into [PackageDetails](../core/src/Ecluse/Core/Package.hs), including explicit unavailable signals.
An unknown value is not evidence that a package is harmless.
Do not branch on ecosystem names inside the rule engine.

Declare a typed `adapterRuleBlocks` entry when a rule has no suitable native meaning or its fact is inherently unavailable.
Give the reason. Boot rejects that rule in an active mount's resolved policy.
Keep temporary missing evidence distinct from a permanent rule block.
The [rule policy](../web/content/docs/configuration.md#rule-policy) owns the operator remedies.

Add the ecosystem's external advisory spelling in [Osv.Ecosystem](../core/src/Ecluse/Core/Osv/Ecosystem.hs).
Check export naming, package identity, version ranges, affected versions, and exact fixes against native examples.
Reuse Pilot acquisition, database publication, freshness, and rule evaluation.
Supply pinned advisory fixtures through the [existing fixture contract](testing.md#osv-advisory-fixtures).
Do not claim advisory coverage from a registered label alone.

## 6. Complete writes and maintenance

Use [PublishCodec](../core/src/Ecluse/Core/Registry/Publish/Internal.hs) for native mirror preparation, target reads, upload requests, and response classification.
The shared worker owns orchestration over the complete version.
[MirrorUpload](../core/src/Ecluse/Core/Registry/MirrorUpload.hs) carries checked membership and verified file facts, paired with required native metadata.
Do not add a dormant codec or publish helper without an active caller.

Declare `AdapterPublish.publishMirrorRequirements`, `publishUploadEndpoint` and `publishIdentity` in [Adapter.Capability](../core/src/Ecluse/Core/Registry/Adapter/Capability.hs).
`MirrorRequirements` selects cached or charged worker reads and native or durable version state.
It also carries the format's [MirrorStateFormat](../core/src/Ecluse/Core/Mirror/State/Format.hs), which every mirroring adapter declares for its `MirrorReadData` constructor.
The format says how mirror state stores, decodes, charges and weighs the retained read data, which project fields a record keeps once, and which files need a metadata companion.
Durable version state also carries the format's [MirrorDocuments](../core/src/Ecluse/Core/Mirror/Documents.hs): the restore, ordering, entry rekeying and listing assembly of managed documents.
Composition binds the state format to the mount's state store, mirror view and worker, and the documents to the mirror view. Mirror state names no ecosystem.
Pin a new format's record bytes, decode charges and weights in the [state codec spec](../core/test/unit/Ecluse/Core/Mirror/State/CodecSpec.hs).
Boot refuses a mirror target whose codec prepares native companions where the format requires none, or omits them where it requires them.
`UploadEndpointRequirement` declares whether both write targets upload at the read root or at a separate `uploadUrl`.
The configuration load reads it to require `uploadUrl` or refuse it.
`UploadIdentityRequirement` declares token-only or named identity for static credentials.
Composition reads it for its boot checks.
Neither switches on ecosystem names.
A store that asks for service credentials its own way gains one row for the ecosystem, for CodeArtifact in [Credential.CodeArtifact.Presentation](../runtime/src/Ecluse/Runtime/Credential/CodeArtifact/Presentation.hs).
A Basic row carries the store's username, and a named-identity target whose store's upload row names one needs no configured username.
Composition gains no branch for it.
Use `relayPublishRequest` in [Publish](../core/src/Ecluse/Core/Registry/Publish/Internal.hs) to retain native replies under metadata and progress limits.

| Responsibility | Reuse | Native work |
|---|---|---|
| Mirror input | Complete version inventory and verified file identities | Retained read data and required upload metadata |
| Copying and retries | Worker orchestration and existing authority limits | Native target reads and upload outcomes |
| Completion | Expected membership and complete-view checks | Document which target facts prove every file exists |
| Restart and outage | Existing [mirror state](../core/src/Ecluse/Core/Mirror/State.hs) where required | Preserve the source fields needed for later native reads, and declare the state format and how retained documents restore |
| First-party publish | [Publish pipeline](../core/src/Ecluse/Core/Server/Pipeline/Publish.hs), bounded body, identity guard | Body framing, identity claims, content type, allowed methods, reply semantics |
| Dredger | [Store maintenance interface](../core/src/Ecluse/Core/Registry/Maintenance.hs), policy, consent, deletion cap | Supported inventory and deletion operations from protocol or backend |
| Telemetry | Existing spans and observations around the shared paths | Native outcomes through the same observations |

A request for one file triggers the complete version, not that client's file subset.
A target version name, successful upload, conflict response, or one present file does not prove completion.
Preserve the complete authorised view during incremental uploads, or return explicit unavailability.
Completion must survive restart and permit completed private reads during public-source outage.
These settled requirements come from [#1133](https://github.com/AlexaDeWit/Ecluse/issues/1133) and [#1639](https://github.com/AlexaDeWit/Ecluse/issues/1639).
They do not promise atomic visibility to consumers that bypass Écluse.

npm is the one-tarball instance. PyPI includes every wheel, sdist, and declared metadata companion required by its contract.
Cargo pairs one verified archive with its retained original index row and source identity.
Native target publication cannot prove those source fields. See [Cargo integration](architecture/registry-model.md#cargo-integration).
Native archive extraction and publication formats stay separate when their meanings differ.
[MirrorUpload.mirrorReadDocument](../core/src/Ecluse/Core/Registry/MirrorUpload/Internal.hs) reconstructs retained native source documents without encoding or reparsing.
A new format adds its `MirrorReadData` constructor there, with its pairing, restriction and validation.
The [state codec](../core/src/Ecluse/Core/Mirror/State/Codec.hs) persists and restores native payloads through the declared state format.
A durable-state declaration does not add another protocol's source reconstruction or completion proof.
Neither the worker nor a completion mechanism gains Dredger's deletion authority.

## 7. Register evidence in the existing test system

Follow [Testing: onboarding an ecosystem](testing.md#onboarding-an-ecosystem) for the required paths, fixtures, and measurement providers.
Use the normal PR CI tiers. Do not create a second ecosystem-completion framework.

### Test infrastructure and E2E entry points

Start from these entry points when adding the ecosystem's test providers.
The [fixed E2E reference](test-contracts.md#fixed-e2e-entrypoints) owns the detailed coverage map.

| Part | Existing entry points | What the new ecosystem supplies |
|---|---|---|
| Common assertions | [Shared contracts](../test/contracts/Ecluse/Test/Contract/) | Native operations and observations for existing business laws |
| Native E2E drivers | [Drivers/Npm.hs](../test/e2e/Ecluse/E2E/Drivers/Npm.hs), [Drivers/PyPI.hs](../test/e2e/Ecluse/E2E/Drivers/PyPI.hs), [Drivers/Cargo.hs](../test/e2e/Ecluse/E2E/Drivers/Cargo.hs), [Drivers/RubyGems.hs](../test/e2e/Ecluse/E2E/Drivers/RubyGems.hs) | A concrete driver binding every required family to its native provider |
| Fixed subjects | [Suites/Npm.hs](../test/e2e/Ecluse/E2E/Suites/Npm.hs), [Suites/PyPI.hs](../test/e2e/Ecluse/E2E/Suites/PyPI.hs), [Suites/Cargo.hs](../test/e2e/Ecluse/E2E/Suites/Cargo.hs), [Suites/RubyGems.hs](../test/e2e/Ecluse/E2E/Suites/RubyGems.hs), [Suites/SharedSystem.hs](../test/e2e/Ecluse/E2E/Suites/SharedSystem.hs) | Direct contract declarations, a Cabal subject, and applicable mixed-client coverage |
| Fixture resources | [Fixtures/Contracts.hs](../test/e2e/Ecluse/E2E/Fixtures/Contracts.hs), [Harness.hs](../test/e2e/Ecluse/E2E/Harness.hs), [Harness/](../test/e2e/Ecluse/E2E/Harness/) | Native clients, upstreams, stores, resource acquisition, teardown, and declared isolation |
| Independent requirements and scheduling | [Requirements.hs](../test/e2e/Ecluse/E2E/Requirements.hs), [Catalogue.hs](../test/e2e/Ecluse/E2E/Catalogue.hs) | Required families, stable case IDs, profiles, prerequisites, and lifecycle groups |
| Plans, execution reports, and audits | [Reproduction procedure](test-contracts.md#reproduction), [Taskfile.yml](../Taskfile.yml), [CI](../.github/workflows/ci.yml) | Registration in the existing manifest, fixed subject tasks, and completion audits |
| Lower tiers and measurements | [Onboarding checklist](testing.md#onboarding-an-ecosystem) | Native unit/integration counterparts, independent readers, corpus, allocation, and residency providers |

The shared `ecluse-e2e-test-support` library holds fixture and provider modules.
Each fixture owns acquisition and teardown for its lifecycle group.
Preserve its declared service resources, serial cases, and Docker exclusivity when registering the new provider.
Missing tools, images, clients, or required ownership fail setup. They do not turn required cases into pending passes.
Real-client tests must observe installed contents and native effects through the prepared product image.
Keep deterministic backend fixtures distinct from claims about a live service.

Use the documented prepare, image-load, fixed-subject, and audit tasks in order, sharing one explicit `ECLTEST_ARTIFACT_DIR`.
The prepared image and plan remain immutable across those tasks.
For full completion, supply the matching unit and integration reports under the documented execution identity.
The [reproduction procedure](test-contracts.md#reproduction) owns the commands, prerequisites, and report paths.

Supply concrete operation records to the shared contracts in [test/contracts](../test/contracts/Ecluse/Test/Contract/).
Contracts own common assertions and scenario order.
Providers own clients, setup, teardown, native decoding, faults, and observations.
Reuse these setup helpers where their contracts fit:

- [PackageStore.Provider.withStoreSession](../test/contracts/Ecluse/Test/Contract/PackageStore/Provider.hs) keeps the native wire stub alive across reopened transport handles.
- [TestSupport.proxyApplication](../test/integration/Ecluse/Server/Pipeline/TestSupport.hs) mounts an ecosystem over supplied runtime and native dependencies.
- [Fixtures.Contracts.concreteMetadata](../test/e2e/Ecluse/E2E/Fixtures/Contracts.hs) shares fixture prerequisites and isolation while retaining native tools, resources, and capabilities.

Keep expected bytes and reference readers independent of production parsers and serialisers.
Retain native tests for obligations with no common meaning.
Record streams can quarantine malformed rows with explicit invalid-entry evidence.
That native behaviour must not weaken single-document protocols' exact error assertions.
Source-aware serving laws must exercise the actual native registry, through its real routes.

Separate pure captured rendering from runtime admission configuration.
The benchmark-only `MetadataRendering` record exposes assembly and serialisation without supplying fabricated charge factors.
Native record-stream references can decode to an array internally while retaining native line-framed output.
Production adapter registration still requires measured factors and independent provider coverage.

Declare obligations independently of drivers, capability flags, and actual registrations.
Register stable case IDs and lifecycle groups in the applicable catalogue and suite entrypoint.
Missing families, omitted registrations, duplicate IDs, and missing execution must fail by name.
Use the existing omission and wrong-observation controls.
The [contract coverage map](test-contracts.md) identifies the common laws and retained native evidence.

Add corpus captures and pins before recording expected outputs or calibration figures.
Register the existing benchmark, allocation, and residency providers instead of editing each benchmark group.
Use the existing load fixture contract for the ecosystem's future load coverage.

[CapturedArtifact](../bench/load/src/Ecluse/BenchLoad/CapturedArtifact.hs) holds neutral artifact coordinates and selection over projected facts.
Native providers retain parsing, version canonicalisation, route construction, and error wording.
[PatternProvider](../bench/load/Ecluse/BenchLoad/PatternScenario.hs) supplies capture authority and selected artifacts through `LoadReadDriver.lrdPatternProvider`.
The [npm provider](../bench/load/Ecluse/BenchLoad/Npm.hs) supplies selected replay from native pins.
The [PyPI provider](../bench/load/Ecluse/BenchLoad/PyPI.hs) keeps selected pattern replay unsupported and selects warm-read artifacts separately.
The [Cargo provider](../bench/load/Ecluse/BenchLoad/Cargo.hs) supplies sparse discovery and selected replay from native workload pins.
The [RubyGems provider](../bench/load/Ecluse/BenchLoad/RubyGems.hs) supplies per-gem `/info` replay and selected replay from native pins, without directory discovery.

Keep allocation, heap retention, native memory, complete-request throughput, and latency claims distinct.
The [metadata reference](ecosystem-metadata.md#lessons-from-earlier-changes) records lessons from earlier work.
The testing guide remains the authority for measurement duties and supported claims.

## 8. Establish completion

Call `fullEcosystemContract` with the concrete driver, as npm, PyPI, Cargo, and RubyGems do.
Its required families include proxying, installation, policy, advisories, mirroring, publication, Dredger, and telemetry.
A partial provider cannot reduce that requirement or describe missing work as a pass.
An intermediate read-only implementation remains incomplete ecosystem support.

Completion requires the lower-tier obligations and actual execution reports as well as the full contract.
Use the fixed ecosystem E2E tasks, plans, and audits described in [Testing](testing.md#shared-behaviour-contracts).
Cross-target installation checks prove file selection and installed contents, not execution of foreign architecture code.

Before handoff, reconcile the supported capability matrix, default configuration, operator manual, threat model, and CI evidence.
Document native limits and any backend combinations that refuse a role.
Show the contributor which shared paths the new adapter inherits and which native work remains.
Keep unmeasured performance effects explicit. Green CI proves its declared checks, not a separate throughput claim.
