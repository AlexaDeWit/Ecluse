# Proposed native package storage

> Design for [#1239](https://github.com/AlexaDeWit/Ecluse/issues/1239), awaiting owner approval.
> Filesystem and S3 package backends are planned features in milestones 12 and 13.
> This proposal changes no shipped backend, configuration, or supported role.

## What makes a version visible?

A version becomes visible only when its completion marker references complete, durable metadata and artifact bytes.
This settled requirement applies to every supported role and ecosystem, including direct publication.
Metadata reads, package listings, version listings, artifact GET and HEAD, and companion reads must consult the committed marker.
The presence of artifact files or objects never proves completion.
A guessed artifact path must pass the same check.

Recommend one bounded, versioned JSON metadata document as the marker.
Its storage representation contains the following fields, independent of Haskell runtime serialisation:

- The ecosystem, canonical package identity, version, and schema version.
- The complete committed filename inventory, verified algorithm-tagged digests, byte lengths, and immutable storage references.
- Supported source metadata paired with the admitted version, including declared companion bytes or immutable references.

The codec rejects duplicate filenames, mismatched identities, unsupported schemas, invalid references, and exceeded bounds.
It preserves the existing [supported field contract](../../web/content/docs/protocol-support.md), rather than defining another ecosystem field set.
Committed filenames retain immutable digests and declarations, while the inventory may grow through conditional union.
Project envelopes and mutable npm tags remain separate from those immutable file identities.
An update may point to a version only after that version commits.

A separate marker is an alternative when metadata cannot carry the commit barrier.
It names immutable metadata by digest, so readers validate both objects.
Metadata-last is the recommendation because it needs one committed document lookup.
These are alternative designs, not two required mechanisms.

Missing markers mean absence.
Invalid markers, missing committed bytes, or failed storage operations mean explicit unavailability, never partial success.
Aggregated metadata must not silently omit a committed version whose required data cannot be read.

## How does publication survive interruption?

Publication uses the ecosystem's existing authorised scope, rather than inventing a whole-version completion signal.
[The settled PyPI protocol](https://github.com/AlexaDeWit/Ecluse/issues/1093#issuecomment-5502718228) accepts multipart, per-file, pathless uploads.
A direct upload commits its verified distribution and required metadata, then later uploads add filenames to the same version.
Each marker describes all committed files, never unfinished staging data.
A failed later upload preserves the prior committed inventory and its visibility.

Whole-version mirroring has a separate completion obligation.
The worker admits complete inventory through `MirrorInventory` and verifies files through `ExpectedMirrorFile` and `VerifiedArtifactBytes`.
Retain the paired source data represented by `MirrorReadData`.
One committed upload cannot acknowledge a mirror job whose admitted expected inventory contains more files or companions.
Expected sets may grow, as [ruled in #1133](https://github.com/AlexaDeWit/Ecluse/issues/1133#issuecomment-6116063124).
The [durable PyPI contract](cloud-backends.md#durable-pypi-read-data) requires the latest full expected set before mirror completion.
Managed serving reads also require that full expected view before exposing a mirrored version.
A valid marker proves its referenced files, but cannot prove that the worker omitted no expected files.

The native mechanism below is a proposal for that settled protocol, not a replacement publication protocol.
Each candidate owns immutable data keys, isolated from concurrent candidates.
Encode identity components injectively, so distinct valid identities never collide.
Never interpret raw package names, filenames, source URLs, or publisher-provided references as filesystem paths.
The marker codec accepts only backend-owned references within the selected store namespace.

```mermaid
flowchart LR
    A[Authorised file or mirror inventory] --> B[Stage and verify required bytes]
    B --> C[Read current marker]
    C --> D[Union compatible completed files]
    D --> E[Conditional create or replacement]
    E -->|Concurrent change| C
    E -->|Committed| F[Verify committed publication]
    F --> G[Direct upload succeeds]
    F --> H[Mirror checks full latest expected inventory]
```

The first marker uses conditional creation.
Later commits use compare-and-swap, which replaces only the marker revision the publisher read, or equivalent filesystem serialisation.
Before each attempt, reread and union the candidate with every committed filename.
After a concurrent change, repeat that union against the new revision so additions cannot erase another publisher's files.

An existing filename succeeds idempotently only when its digests and immutable declarations match.
Compare semantic content, excluding temporary candidate keys and transient download URLs.
Conflicting content returns a named file conflict without replacing the committed inventory.
Mutable project metadata follows the ecosystem contract outside that immutable comparison.
Every newly referenced byte must finish durably before the marker update.

| Interruption or race | Required outcome |
| --- | --- |
| Before new bytes finish | A first version stays absent. Later uploads leave the prior committed inventory visible. |
| After data completion, before marker commit | The prior marker remains authoritative. Restart may reuse verified durable candidates. |
| Marker commit fails or its acknowledgement disappears | Reread the marker. Matching file identities resolve success, conflicts refuse, and unresolved storage failure retries. |
| Concurrent equivalent uploads | One commit wins. The other publisher verifies matching file identities and succeeds idempotently. |
| Concurrent distinct filenames | Reread and union after conflict, retaining both completed files without lost additions. |
| Concurrent conflicting uploads for one filename | One commit wins. The loser reports an immutable-file conflict and preserves committed content. |
| Restart with staging data only | Staging never proves completion. Repeat verification before attempting commit. |
| One mirror upload commits | Acknowledge only after the target proves every file and companion in the latest admitted expected inventory. |

Orphan cleanup has separate authority and does not affect visibility.
The worker gains no deletion permission.
Initial delivery can retain unreachable candidates until independently authorised cleanup runs.
Age alone does not establish that a candidate has no active publisher.

## Which filesystem guarantees are required?

Bind operations to a trusted root directory handle and reject symbolic-link traversal beneath it.
Canonical-path checks alone do not protect later operations from path substitution.
Stage on the same filesystem, flush completed files, and flush directories containing newly created entries.
Serialise commits for each version across all writers, including separate processes, and reread the marker while holding that protection.
Union completed files, then write and flush temporary metadata.
Create the first marker atomically without replacement, or atomically replace the observed marker under that serialisation.
Flush its directory before acknowledging durable success.

An unprotected rename can overwrite concurrent additions.
Linux offers [`renameat2(RENAME_NOREPLACE)`](https://man7.org/linux/man-pages/man2/rename.2.html) for no-replace publication.
The [`fsync` contract](https://man7.org/linux/man-pages/man2/fsync.2.html) also requires flushing the directory to persist its new entry.
Later replacement needs atomic rename under the same cross-writer serialisation.
Readers see either complete marker revision, never partially written metadata.
A supported equivalent is acceptable.
Refuse this capability on platforms or filesystems without the required atomicity and durability.
An existence check followed by rename is not equivalent.

After an uncertain directory flush, marker presence alone does not establish durable success.
Retry must complete the durability operations or report storage unavailability.
A crash may lose an unacknowledged marker, but must never leave a durable marker pointing at unfinished data.

## Which S3 guarantees are required?

Upload immutable candidate objects and finish multipart uploads before creating the metadata marker with `If-None-Match: *`.
List-then-put cannot choose one winner.
For later additions, GET the current marker and retain its ETag as the revision token.
Union completed files and replace with `If-Match` for that ETag.
On `412`, reread and repeat the union, refusing conflicting existing filename identities.
On uncertain transport results, perform the same readback before deciding success.
On `404` during replacement, reread current state before attempting conditional creation, respecting concurrent deletion and refill authority.
Handle `409` through bounded retry and current-state inspection.
AWS requires restarting a conditional multipart upload after that conflict.
These outcomes follow the [conditional-write contract](https://docs.aws.amazon.com/AmazonS3/latest/userguide/conditional-writes.html).

Require conditional creation, conditional replacement, and strong GET and LIST consistency from the selected service.
[AWS documents those consistency guarantees](https://aws.amazon.com/s3/consistency/), but they do not establish another provider's behaviour.
Refuse only the incompatible capability or configuration, while preserving compliant providers and other supported roles.
Use the marker ETag only as a concurrency token.
Compute artifact digests independently rather than treating an ETag as an artifact checksum.

## Who may read and delete?

Process filesystem or S3 credentials grant storage access, not a caller's right to read a package.
The existing [credential contract](registry-model.md#credential-flow-and-authority) gives private read authority to the caller's upstream credential.
Native private reads need an approved equivalent before activation.

Construct native `MetadataClient` and artifact handles only after establishing the request's authorised mount, namespace, and package scope.
Bind that scope when the server constructs its private read capabilities, where `MetadataReads Private` currently enters `privateMetadataClient`.
Every metadata, listing, artifact, and companion operation must enforce the bound scope.
Keep these reads outside the shared anonymous cache.
Mirror publication instead uses its configured worker authority and admitted complete inventory.
Maintenance retains its independently scoped observation and deletion authority.

Use the existing `StoreMaintenance` consent checks and Dredger reassessment before each destructive batch.
Bind filesystem consent to a persistent store identifier within the opened root and its namespace.
Bind S3 consent to the configured service authority, bucket, namespace, and persistent store identifier.
The operator creates consent. Neither the worker nor Dredger creates or broadens it.
Keep consent separate from completion markers and sweep cursors.

Refuse the configured OSV bucket as a package store even when the object prefix differs.
Check configured store identity at boot or capability construction, without inventing remote alias discovery.

Deletion withdraws the observed marker before reclaiming that candidate's bytes.
It must not remove a replacement marker or bytes belonging to a concurrent publication.
The backend must serialise version deletion with publication, or condition deletion on the observed marker identity.
Preserve per-version outcomes and re-observation after uncertain deletion through `StoreMaintenance`.
Readers during deletion either obtain a complete usable view or report explicit unavailability.
The backend declares refill behaviour through `StoreFacts`, consistent with the existing immutable-publication contract.

## How does this fit the current capabilities?

Keep ecosystem encoding separate from concrete storage operations, as required by the [registry abstraction](registry-model.md#registry-abstraction).
`OriginClient` remains an HTTP adapter input rather than becoming a universal storage record.
The existing S3 mirror-state store records expected source evidence. It is not a package store or a package completion marker.

| Current boundary | Proposed extension and retained guarantee |
| --- | --- |
| [`MetadataClient`](../../core/src/Ecluse/Core/Registry/Metadata.hs) | Supply committed full and selected reads, with existing bounds and paired metadata. Add native typed faults where HTTP outcomes cannot represent failures. |
| [`MirrorPublish`](../../core/src/Ecluse/Core/Registry/Publish/Internal.hs) | Retain checked preparation. Add native per-file marker commits and additive inventory updates after verified data and companion completion. Retain existing HTTP publication rules. |
| [Worker completion](../../core/src/Ecluse/Core/Worker/Job.hs) | Run the fresh target probe and revalidate the latest required inventory after native commits. Never acknowledge a stale or incomplete set. |
| [Artifact capability](../../core/src/Ecluse/Core/Registry/Adapter/Capability.hs) | Add native bounded reads because current builders return HTTP requests. Enforce marker membership for GET, HEAD, and companions. |
| [`StoreMaintenance`](../../core/src/Ecluse/Core/Registry/Maintenance.hs) | Derive enumeration and manifest membership from markers. Preserve consent, guarded batches, deletion outcomes, refill facts, and optional cursors. |

Composition would select native handles only for approved roles and ecosystems.
Concrete filesystem and S3 adapters belong at the runtime edge.
The delivery PRs must update configuration, operator guidance, and the threat model when those interfaces become available.
No extra HTTP service is required by this proposal.

## What proves backend acceptance?

Each backend needs its own [full package-store contract](cloud-backends.md#onboarding-a-backend) and independently declared ecosystem obligations.
Passing a mirror-only subset must not claim full package-store completion.
Reuse existing metadata, mirror, and maintenance laws through production handles.
Add the following observations to deterministic specs and filesystem or emulated-service integration tests:

- Interrupt every publication phase, restart, and check all listing and direct-read forms from the visibility contract.
- Race equivalent uploads, distinct filenames, and conflicting filename identities, including lost acknowledgements and uncertain directory flushes.
- Commit a direct PyPI file, add another file, and confirm each revision references only complete durable bytes.
- Interrupt a later upload and retain the previous readable inventory, with no lost files after concurrent additions.
- Keep mirror completion pending until the full latest admitted expected inventory and companions pass proof.
- Reject forged references, symbolic links, identity collisions, duplicate filenames, invalid schemas, and exceeded metadata bounds.
- Refuse partial committed data, preserve caller isolation, and reject reads outside the authorised scope.
- Exercise consent withdrawal, root replacement, uncertain deletion, concurrent refill, and OSV bucket refusal despite a different prefix.
- Measure aggregation, staging memory, and storage operation counts under the existing admission and residency budgets.

Backend delivery must supply this runtime evidence. Documentation review does not prove these guarantees.

## What remains for owner approval?

Recommend metadata-last commits with conditional creation and additive inventory updates for direct publication and whole-version mirroring, plus maintenance support.
The owner must approve the native representation and delivery scope before backend work starts.
Per-file PyPI publication and growing expected sets are settled contracts, not new owner approval questions.

Native private reads require a caller-authorisation contract before activation, including the rule that produces the bound request scope.
Existing mirror configuration also requires a private upstream, so usable delivery must preserve that topology requirement.
Approval must identify its authorised readback path rather than silently enabling process-wide private access.
This unresolved native read mechanism does not justify blanket refusal of other compliant backend roles.
