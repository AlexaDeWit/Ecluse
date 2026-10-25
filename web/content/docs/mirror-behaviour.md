+++
title = "Mirror completion and recovery"
description = "A request can start mirroring before the copy is complete. Completion determines a managed version's visibility through Écluse."

weight = 33

[extra]
nav_title = "Mirror completion and recovery"
group = "Detailed behaviour"
kind = "Reference"
reference = "/docs/pypi-setup/#completion-outages-and-recovery"
+++

## A download triggers a version job {#trigger}

An admitted public artifact GET can enqueue mirroring. The job identifies the version. The worker reads current metadata and re-evaluates current policy before copying.

For PyPI, the job covers every supported wheel and source distribution in the admitted version, plus declared metadata companions.

## Partial copies do not form a complete release {#partial}

The registry can expose physical uploads one file at a time. Through Écluse, an incomplete managed PyPI version needs a validated complete public alternative or returns unavailable.

The durable state records expected identities and verified upload evidence. A duplicate-upload response alone does not prove completion.

Completion compares the current target inventory and digests with retained evidence, and verifies retained companions. It does not download every target archive again.

## Completed copies can survive a public outage {#outages}

After current private authorisation and identity checks, a complete version can serve its recorded files and companions without new public admission.

Retained expected identities also guide recovery when the public source later removes a file. Do not expire those records or their referenced companions.

Direct registry access is outside this visibility guarantee and can observe partial uploads.

{{ diagram(name="mirror-completion", alt="Mirror writes and completion, from the shared threat model.") }}

## npm metadata has a supported subset {#npm}

The mirror retains supported installation and runtime metadata. It replaces artifact coordinates and verified hashes, and removes source-registry signatures and attestations.

Existing copies are not rewritten automatically after a metadata improvement. Consult the [repair procedure](@/docs/how-it-works.md#npm-mirror-metadata) before deleting and recreating a copy.

## Cargo retains original-source metadata {#cargo}

Cargo completion pairs the retained admitted index row and source identity with a checksum-verified target archive.
Target publication can reset time or yank and omit fields. The target's native metadata cannot replace those retained facts.
Retained fields reflect first admission, without continuous yank synchronisation or target-yank mutation.

The crates.io replacement excludes unrecorded target versions from listings and direct archives.
Missing or corrupt state cannot provide an original-source managed read during public outage.
Ordinary private reads remain available through the separate alternate registry.
Current target authorisation and discovery supply archive addresses. Retained source records grant no new credential or host authority.

Boot performs no remote discovery. A new client's discovery can still fail while its configured trusted registry is unavailable.
Do not assume that a new client and an already configured client have identical outage behaviour.
