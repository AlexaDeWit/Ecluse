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

[Open the threat model ↗](@/docs/threat-model.md)

## Limits that matter when evaluating Écluse {#limits}

Trusted private reads do not repeat public admission. Removing a stored version requires authorised cleanup, and does not recall client-cached or installed bytes.

PyPI mirror completion applies to reads through Écluse. Direct registry reads can observe partial uploads.

Écluse hosts no package archives and does not inspect package code to detect malware.

## Verify the software you deploy {#image}

Release images include provenance and SBOM attestations. Verify the base image before building your configured derivative, and pin it by digest.

[Read the image verification instructions](https://github.com/AlexaDeWit/Ecluse#verifying-the-image).
