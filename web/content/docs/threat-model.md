+++
title = "Threat model"
description = "Focused data-flow diagrams, the STRIDE threat register, and deployment assumptions for Écluse."
weight = 7
+++

Use these views to follow one operation through Écluse and see where its trust changes.
A process or store keeps the same identity across diagrams, so its threats apply wherever it appears.
The diagrams show the recommended deployment with separate first-party, mirror, and private read stores.
Optional roles and stores appear only in the views that use them.

The [Saerskriven model](https://github.com/AlexaDeWit/Ecluse/blob/main/threat-modelling/ecluse.yaml)
holds the diagrams, threats, controls, and assumptions. The documentation renders its images and register.
Threat numbers are lifetime identifiers, so gaps in the sequence are intentional.
Edge access control and storage-layer scanning remain operator responsibilities.

| Follow this operation | View |
| --- | --- |
| Understand the trusted stores and public entry point | [Registry topology and trust](#registry-topology-and-trust) |
| Resolve available versions | [Metadata listing and admission](#metadata-listing-and-admission) |
| Download a selected file | [Artifact downloads](#artifact-downloads) |
| Publish an internal package | [First-party publication](#first-party-publication) |
| Copy an admitted public version | [Mirror writes and completion](#mirror-writes-and-completion) |
| Read a complete managed PyPI version | [PyPI inventory and companions](#pypi-inventory-and-companions) |
| Build advisory evidence | [Advisory ingestion and publication](#advisory-ingestion-and-publication) |
| Load advisory evidence into each role | [Advisory distribution](#advisory-distribution) |
| Remove denied copies from trusted stores | [Revocation and cleanup](#revocation-and-cleanup) |

<a id="data-flow-diagram"></a>

## Registry topology and trust

The private endpoint C unions first-party store A and mirror B. Public packages enter through Écluse's admission gate.
The operator keeps C free of direct or transitive public connections, including after startup.

{{ diagram(name="registry-topology", alt="The client reaches the proxy across the operator boundary. The proxy reads C and anonymous public data. First-party A and mirror B supply C, with no public-to-C connection.") }}

Related threats: [registry collapse](#threat-10), [substitution across origins](#threat-11), [public aggregation bypass](#threat-15), and [upstream impersonation through a compromised CA](#threat-121).
[Deployment](@/docs/deployment.md#the-recommended-topology) explains the registry configuration.

## Metadata listing and admission

A listing combines the caller's authorised private view with admitted public files. First-party names use the private origin alone.
Response reuse cannot skip the private origin's authorisation.
A failed private read refuses the request, so a public namesake never replaces a private package during an outage.

{{ diagram(name="metadata-admission", alt="The client requests a listing. The proxy reads the private endpoint with caller credentials, public metadata anonymously, and advisory artifacts independently. It applies admission before merging and encoding.") }}

Related threats: [resource exhaustion](#threat-2), [private cache disclosure](#threat-9), [rejected files returning during assembly](#threat-116), [compressed response lengths](#threat-119), and [private-namespace shadowing during an outage](#threat-123).

## Artifact downloads

The download path prefers private content. Public fallback needs current admission and a permitted artifact location, and never follows a failed private read.
An explicit private authentication refusal stops the request. First-party names never fall back to public.

{{ diagram(name="artifact-downloads", alt="The client downloads through the proxy. The proxy prefers the authorised private source, gates a public fallback, and queues an admitted public artifact GET when mirroring is configured.") }}

Related threats: [caller credential disclosure](#threat-4), [SSRF](#threat-5), [first-party dependency confusion](#threat-103), and [private-namespace shadowing during an outage](#threat-123).
[How it works](@/docs/how-it-works.md#a-request-step-by-step) explains the npm and PyPI differences.

## First-party publication

Publication goes to A under the publisher's credential or the configured static publication token.
The proxy checks the namespace and upload identity before relaying a new release.

{{ diagram(name="first-party-publication", alt="The publisher sends a new npm release or PyPI upload to the proxy. Authentication, namespace, body, and filename checks precede the credential-bearing relay to first-party store A.") }}

Related threats: [package shadowing](#threat-6) and [misdirected publication credentials](#threat-29).

## Mirror writes and completion

A queued trigger requests work, but it does not preserve an earlier policy decision.
The worker checks current admission before new copying and proves completion from current target identities and retained companions.

{{ diagram(name="mirror-completion", alt="The queue supplies a version trigger. The worker obtains admitted public files, retains expected identities, verifies bytes, uploads to B, and reads B again before acknowledging completion.") }}

Related threats: [worker authority](#threat-7), [worker SSRF](#threat-14), [misdirected mirror credentials](#threat-22), and [advisory changes during transfer](#threat-110).

## PyPI inventory and companions

Physical uploads become visible one file at a time. Écluse serves a managed version only when it can prove a complete view.
The retained state also preserves declared metadata companions and admitted source fields.

{{ diagram(name="pypi-inventory", alt="After caller-authorised private metadata, the proxy reads retained expected identities and companions. It serves a proven complete version, uses a validated full public alternative, or reports unavailability.") }}

Related threats: [worker and state authority](#threat-7) and [rejected artifact entries](#threat-116).
[PyPI uploads and mirror state](@/docs/configuration.md#pypi-uploads-and-mirror-state) describes recovery from partial copies and the durability contract.

## Advisory ingestion and publication

Pilot turns OSV records and independent EPSS scores into per-ecosystem artifacts.
It reads existing publication metadata before a guarded conditional write. Pilot does not admit packages.

{{ diagram(name="advisory-publication", alt="OSV exports and EPSS scores cross from external sources into Pilot. Pilot reads the existing artifact revision and publication metadata, then conditionally publishes the new artifact to S3.") }}

Related threats: [pathological OSV data](#threat-20), [hostile OSV](#threat-101), [incorrect EPSS scores](#threat-111), [source credential disclosure](#threat-113), and [withdrawn records](#threat-115).

## Advisory distribution

The proxy, worker, and Dredger poll independently. They share the artifact contract and rules, but can hold different generations temporarily.
A rejected update does not refresh the age of the retained evidence.

{{ diagram(name="advisory-distribution", alt="The advisory store supplies the proxy, worker, and Dredger through separate read-only polling. Each validates and swaps its database, retaining last-good evidence only within its existing age limit.") }}

Related threats: [tampered databases](#threat-18), [cold-start availability](#threat-106), [stale evidence](#threat-112), and [package identity mismatch](#threat-114).

## Revocation and cleanup

Dredger applies revocation rules to actual inventories in B and C. Each target needs its own consent and authority.
First-party names stay protected, and A is not a deletion target. Repeated scans reconcile copies left after failures or older writes.

{{ diagram(name="revocation-cleanup", alt="Dredger reads advisories and each target's inventory and consent. It deletes named-denied versions from B, then eligible retained copies from C, under the deletion cap. Publication store A remains outside deletion.") }}

Related threats: [mass deletion](#threat-21), [Dredger authority](#threat-26), [private cleanup scope](#threat-107), [partial cleanup](#threat-108), and [policy rollout disagreement](#threat-109).
[The Dredger guide](@/docs/dredger.md) describes permissions, preview, and deletion controls.

## Threat register

Each threat carries one status:

- **Mitigated:** The code carries compensating controls. Linked assumptions state any operator responsibilities and residual conditions.
- **Accepted risk:** Écluse retains the risk deliberately under a recorded trust assumption or operator trade-off.
- **Open:** A required control has not landed. Planned work does not count as mitigation.
- **Transferred:** The operator retains responsibility for the defence. A backend-specific check can help without covering every supported configuration.

[Security posture](https://github.com/AlexaDeWit/Ecluse/blob/main/docs/architecture/security.md)
explains the invariants behind the controls.

{{ generated(name="threat-register") }}
