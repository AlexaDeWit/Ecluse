# Cargo readiness

Use this plan when starting Rust crates support. It applies [Adding an ecosystem](adding-an-ecosystem.md) to Cargo's registry protocol.
This is a protocol assessment and implementation map, not a claim that Écluse serves Cargo.
The current [adapter registry](../core/src/Ecluse/Core/Registry/Adapter.hs) registers npm and PyPI only.

The sources below were checked on 2026-10-25. Recheck their current contracts before implementation.
The proposed sequence identifies future work. It does not select every operator-facing design choice.

## Protocol fit

The [Cargo index specification](https://doc.rust-lang.org/cargo/reference/registry-index.html)
defines these facts. The right column states their consequence for the proposed adapter.

| Native fact | Required treatment |
|---|---|
| Sparse HTTP and Git are distinct index protocols | Declare the supported protocol before choosing routes or fixtures |
| Each index line describes one version | Add bounded record framing instead of reusing a single-document walk unchanged |
| `config.json` declares download and optional API locations | Serve discovery data whose endpoints retain Écluse's admission and publication boundaries |
| Entries carry SHA-256 `cksum` and `yanked` | Map integrity and availability without treating a yank as deletion |
| `pubtime` is optional | Preserve missing publication time as unavailable evidence |
| Entries retain dependency, feature, and Rust-version data | Define the served field contract before projecting policy facts |
| Index version uniqueness ignores SemVer build metadata | Test identity separately from ordering and raw spelling |
| Sparse clients use HTTP validators | Check filtered responses and policy changes through the existing conditional-response path |

The generic `EcosystemRead` accepts a body consumer, so the transport is not limited to one JSON object.
The existing `readJsonWalk` examples do assume one top-level value.
The future reader must state acceptance for blank lines, truncation, malformed records, duplicate versions, and unknown entry schemas.
Keep the response limit and cancellation around the whole body, including selected reads.
An independent native reader must define the expected result. Do not derive it from production framing.

Cargo's [Web API](https://doc.rust-lang.org/cargo/reference/registry-web-api.html) uses raw API tokens in `Authorization`.
Publication sends length-prefixed metadata and archive bytes to a path without the package name.
Reuse `CredentialMapping`, `PublishRequestBody`, byte admission, and the first-party identity guard.
Use [Publish.relayPublishRequest](../core/src/Ecluse/Core/Registry/Publish/Internal.hs) for bounded forwarding after native validation.
Add native body decoding and identity agreement. Neither npm JSON nor PyPI multipart framing fits this body.
Preserve the difference between publication metadata and index entries rather than treating one as the other's wire format.

[Registry authentication](https://doc.rust-lang.org/cargo/reference/registry-authentication.html) also involves client credential providers.
Include discovery and authenticated fetches in real-client coverage.
Do not infer that npm's bearer header or PyPI's basic header matches Cargo's token presentation.

## Evidence and client behaviour

Missing `pubtime` does not authorise inventing a publication date or weakening the age rule.
At Rust kickoff, choose an authoritative evidence source or document the existing unavailable-evidence result.
Adding an effectful lookup needs a concrete source, bounds, freshness, and failure behaviour in that plan.

Cargo can run [build scripts](https://doc.rust-lang.org/cargo/reference/build-scripts.html).
Its manifest can also declare a [procedural macro library](https://doc.rust-lang.org/cargo/reference/cargo-targets.html#the-proc-macro-field).
Index metadata alone does not establish the complete install-time execution signal.
Resolve that signal's meaning, available evidence, and any typed rule blocks before declaring a policy posture.
Do not copy PyPI's rule blocks or treat absent index fields as `NoCodeOnInstall`.
Bounded archive inspection, if selected, needs its own native metadata contract. PyPI's upload extractor is not automatically that contract.

[OSV covers `crates.io`](https://google.github.io/osv.dev/data/#covered-ecosystems).
Add that external spelling to the existing advisory mapping when adding the ecosystem tag.
Use Rust advisory fixtures to check name matching, version ranges, and exact-fix identities through Pilot and the rule engine.
Existing npm advisory spelling exceptions do not automatically apply to Cargo.

[Source replacement](https://doc.rust-lang.org/cargo/reference/source-replacement.html) assumes identical package contents and permits no extra packages absent from the original source.
Treat crates.io replacement and private registry use as separate client configurations in the Rust plan.
Check how dependency registry references, first-party names, and the proxy's private/public merge fit each configuration.
Do not promise the npm topology works unchanged with Cargo source replacement.

[Yanking does not stop existing lockfiles or direct downloads](https://doc.rust-lang.org/cargo/commands/cargo-yank.html#when-to-yank).
Retain artifact admission for pinned versions. Filtering an index is not the whole policy boundary.
The Web API's yank operation is not Dredger's required physical deletion operation.

## Backend capability fit

AWS documents [Cargo support](https://docs.aws.amazon.com/codeartifact/latest/ug/using-cargo.html)
and [sparse registry configuration](https://docs.aws.amazon.com/codeartifact/latest/ug/configure-use-cargo.html) in CodeArtifact.
This provides an existing backend candidate, not proof that Écluse's current backend integration supports the new ecosystem.
Check the SDK format mapping, namespace, endpoint, asset inventory, and delete behaviour when adding Cargo.
Keep inventory, deletion authority, consent, and upstream adoption checks in the existing backend capability.

A generic Cargo read or publish endpoint does not by itself prove complete store enumeration or physical version deletion.
Support Dredger through a backend that supplies those operations, and refuse incompatible configurations at their existing capability boundary.
Do not reject otherwise valid proxy or publication use because a native registry lacks maintenance verbs.
Do not add filesystem or S3 package storage as a prerequisite.

## Implementation sequence

Each stage below names the future changes and the machinery they reuse.
Stage completion is distinct from the full ecosystem completion claim.

| Stage | Native or integration work | Existing machinery inherited |
|---|---|---|
| 1. Contract and scope | Set the client versions, supported index protocol, configuration spelling, evidence sources, and backend matrix | This assessment, the registry model, and existing capability refusals |
| 2. Identity | Add the tag and native name parser. Extend version keys and advisory spelling | `Ecosystem`, `PackageName`, `Version`, `Osv.Ecosystem`, shared name safety checks |
| 3. Read and serve | Add request paths, bounded index framing, retained entries, discovery routes, native response contracts, and `CachedDoc` cases | `Registry.Request`, `Metadata.Fetch`, `Server.Route`, `Package.Filter`, merge plans, caches, admission, telemetry |
| 4. Rules and advisories | Add native signals, justified rule blocks, Rust advisory fixtures, and policy examples | Existing rule precedence, Pilot ingestion, freshness, and typed unavailable evidence |
| 5. Mirroring | Add checked native read data, its persistence codec where required, upload metadata, request encoding, and completion proof | `MirrorInventory`, verified bytes, `PublishCodec`, whole-version worker orchestration, mirror state |
| 6. First-party publication | Decode bounded length-prefixed identity claims and preserve native replies | Existing publish pipeline, caller credentials, target selection, first-party guard |
| 7. Maintenance | Extend the chosen backend's format and package handling. State unsupported protocol verbs | `StoreMaintenance`, Dredger policy, pacing, cursors, consent, deletion caps |
| 8. Completion | Register the Cargo driver, fixture graph, obligations, case IDs, CI subject, and measurement providers | Existing contract catalogue, completeness audits, benchmark groups, allocation and residency harnesses |
| 9. Operator contract | Add configuration examples, client setup, capability limits, and applicable threat-model changes | Default configuration reference, operator manual, existing threat model |

The future native modules belong under `Ecluse.Core.Registry.<Ecosystem>`.
The final module component and wire spelling belong to stage 1, rather than a dormant tag added during preparation.
Required shared integration points include these explicit sums and registrations:

- [Ecosystem and wire names](../core/src/Ecluse/Core/Ecosystem.hs), [version keys](../core/src/Ecluse/Core/Version.hs), and [adapter dispatch](../core/src/Ecluse/Core/Registry/Adapter.hs).
- [Cached documents](../core/src/Ecluse/Core/Registry/CachedDocument.hs), [mirror read data](../core/src/Ecluse/Core/Registry/MirrorUpload/Internal.hs), and any corresponding [state codec](../core/src/Ecluse/Core/Mirror/State/Codec.hs).
- [CodeArtifact maintenance](../runtime/src/Ecluse/Runtime/Maintenance/CodeArtifact.hs) and applicable configuration validation.
- [E2E drivers](../test/e2e/Ecluse/E2E/Drivers/), [contract catalogues](../test/contracts/Ecluse/Test/Contract/), [Cabal registration](../ecluse.cabal), and the [CI workflow](../.github/workflows/ci.yml).
- Corpus, advisory, benchmark, allocation, residency, and load providers in the [onboarding checklist](testing.md#onboarding-an-ecosystem).

Declare worker reads, state needs, and upload endpoints through `AdapterPublish.publishMirrorRequirements` in [Adapter.Capability](../core/src/Ecluse/Core/Registry/Adapter/Capability.hs).
Declare upload identity through `publishIdentity`, and private artifact addressing through `AdapterArtifact.artifactPrivateAddressing`, independently of public `artifactHosts`.
These declarations reuse composition checks. They do not add Cargo source reconstruction, persistence, publication, or completion semantics.
A `DurableVersionState` flag alone cannot make the native state codec support Cargo.
Supply those native contracts and their evidence before enabling the role.

These are integration changes, not evidence that every pipeline needs a Cargo branch.
Keep native formats in the adapter and backend operations in their backend.
No new general metadata framework or copied worker is required by this assessment.

## Future completion evidence

Supply the normal counterpart in every applicable tier from the onboarding checklist.
Native fixtures must include index framing and chunk boundaries, name/version identities, missing evidence, token handling, discovery URLs, and malformed uploads.
Real-client scenarios must cover dependency resolution and installed contents as well as direct artifact access.
Include locked yanked versions, private credentials, public outage, and restart where the relevant contract requires them.
Publication and mirroring need native target observations rather than a callback that reports a precomputed pass.
Maintenance evidence must establish actual deletion under the chosen backend contract, not just a successful yank.

Register independent full-contract obligations before claiming completeness.
An incomplete driver must leave named failures for missing required families.
Use the existing allocation and residency calibration rules when the future representation exists.
The present preparation changes neither their budgets nor their fixtures.

The remaining Rust design choices are the stage 1 protocol/client scope, identity spelling, evidence posture, and supported backend combinations.
Resolve those on the Rust implementation issue before code depends on them.
They do not block the current shared-code and documentation foundation.
