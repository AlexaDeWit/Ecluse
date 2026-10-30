+++
title = "Security model and limits"
description = "Écluse controls public package admission. The deployment supplies the network boundary, private registry authority, and permission to remove stored copies."

weight = 47

[extra]
nav_title = "Security model and limits"
group = "Understand \u00c9cluse"
kind = "Security"
reference = "/docs/threat-model/"
+++

## The operating assumptions {#assumptions}

- Package traffic from protected builds passes through Écluse.
- The private read repository combines only trusted sources.
- Caller credentials decide private package access.
- Each background role has only the authority its work requires.
- First-party declarations match the names your organisation owns.

A seven-day delay does not prove that a package is safe. Advisory quality, freshness, configured exceptions, and enforcement all affect the result.

## Read the rendered threat model {#threat-model}

The threat model contains the system diagrams, threats, controls, assumptions, and accepted risks. It remains the authoritative security record.

Each operation has its own view, including public admission, private downloads, mirror completion, advisory distribution, and cleanup.

[Open the threat model](@/docs/threat-model.md)

## Limits that matter when evaluating Écluse {#limits}

Trusted private reads do not repeat public admission. Removing a stored version requires authorised cleanup, and does not recall client-cached or installed bytes.

PyPI, Cargo, and RubyGems mirror completion applies to reads through Écluse. Direct registry reads can observe incomplete copies or changed target metadata.

Écluse hosts no package archives and does not inspect package code to detect malware.
Clients verify streamed archives against checksums from admitted metadata.
RubyGems requires the supported Bundler clients with checksum validation enabled.
The mirror worker verifies archive bytes before publication.
HEAD authorises and checks availability without reading or hashing an archive body.

Cargo build scripts, procedural macros and RubyGems native builds remain allowed.
Both adapters block the unsupported installation-execution rule at boot.
The server never runs the inspected code. A registry checksum proves agreement with that registry's assertion, not harmlessness.

Écluse checks neither Certificate Transparency nor certificate revocation. It trusts any certificate
that chains to a root in its CA bundle, so a compromised CA stays trusted until the bundle drops
that root. [Trusted certificate authorities](@/docs/deployment.md#trusted-certificate-authorities)
lists the roots the image leaves out, and the threat model records the
[residual](@/docs/threat-model.md#threat-121).

## Verify the software you deploy {#image}

Release images include provenance and SBOM attestations. Verify the base image before building your configured derivative, and pin it by digest.

[Read the image verification instructions](https://github.com/AlexaDeWit/Ecluse#verifying-the-image).
