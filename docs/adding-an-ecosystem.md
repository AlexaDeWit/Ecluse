# Adding an ecosystem

Use this guide to take a package ecosystem from a protocol assessment to complete support in Écluse.
npm and PyPI are the shipped examples. Their shared machinery covers more than metadata reads.
The [Cargo readiness plan](cargo-readiness.md) applies this guide without adding Rust support.

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

Answer these questions with protocol evidence:

- How do names, namespaces, versions, and artifact filenames form identities?
- What metadata does installation require, including dependency, feature, and target information?
- Which facts support age, availability, install-time execution, and advisory rules?
- How do clients discover metadata, download, and publication endpoints?
- How do private credentials, caches, conditional requests, and lockfiles affect those requests?
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
| Configuration and role validation | [Composition/Validate.hs](../src/Ecluse/Composition/Validate.hs) | Refuse missing active capabilities and blocked policies |

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
Keep caller authorisation separate from service credentials used for mirror or maintenance operations.
Never expand credential authority to satisfy a missing backend operation.

Declare native routes with [Server.Route](../core/src/Ecluse/Core/Server/Route.hs).
Derive routing and the published route descriptions from the same table.
Check metadata, artifacts, publication, discovery documents, errors, `GET`, and `HEAD` against the native client contract.
Identify any response action the current pipeline cannot express before adding adapter code.

## 4. Supply reads, retained documents, and assembly

Supply an `EcosystemRead` through `AdapterMetadata.metadataRead`.
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
[ServedDocument](../core/src/Ecluse/Core/Registry/ServedDocument.hs) supplies exact entry selection, name checks, URL rebasing, and `serialiseDocument`.
The latter shares packed-render refusal and native-encoding fallback.
Adapters retain their object or array assembly, native envelope fields, ordering, and document projections.
Never reconstruct installable metadata from policy facts alone.

Validate artifact locations through the existing [package filter](../core/src/Ecluse/Core/Package/Filter.hs).
Preserve the distinction between source digest claims and verified bytes.
Keep private-source authorisation, first-party precedence, and merge provenance under the existing registry contract.

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

| Responsibility | Reuse | Native work |
|---|---|---|
| Mirror input | Complete version inventory and verified file identities | Retained read data and required upload metadata |
| Copying and retries | Worker orchestration and existing authority limits | Native target reads and upload outcomes |
| Completion | Expected membership and complete-view checks | Document which target facts prove every file exists |
| Restart and outage | Existing [mirror state](../core/src/Ecluse/Core/Mirror/State.hs) where required | Preserve the source fields needed for later native reads |
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
Native archive extraction and publication formats stay separate when their meanings differ.
Neither the worker nor a completion mechanism gains Dredger's deletion authority.

## 7. Register evidence in the existing test system

Follow [Testing: onboarding an ecosystem](testing.md#onboarding-an-ecosystem) for the required paths, fixtures, and measurement providers.
Use the normal PR CI tiers. Do not create a second ecosystem-completion framework.

Supply concrete operation records to the shared contracts in [test/contracts](../test/contracts/Ecluse/Test/Contract/).
Contracts own common assertions and scenario order.
Providers own clients, setup, teardown, native decoding, faults, and observations.
Keep expected bytes and reference readers independent of production parsers and serialisers.
Retain native tests for obligations with no common meaning.

Declare obligations independently of drivers, capability flags, and actual registrations.
Register stable case IDs and lifecycle groups in the applicable catalogue and suite entrypoint.
Missing families, omitted registrations, duplicate IDs, and missing execution must fail by name.
Use the existing omission and wrong-observation controls.
The [contract coverage map](test-contracts.md) identifies the common laws and retained native evidence.

Add corpus captures and pins before recording expected outputs or calibration figures.
Register the existing benchmark, allocation, and residency providers instead of editing each benchmark group.
Use the existing load fixture contract for the ecosystem's future load coverage.
Keep allocation, heap retention, native memory, complete-request throughput, and latency claims distinct.
The [metadata reference](ecosystem-metadata.md#lessons-from-earlier-changes) records lessons from earlier work.
The testing guide remains the authority for measurement duties and supported claims.

## 8. Establish completion

Call `fullEcosystemContract` with the concrete driver, as npm and PyPI do.
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
