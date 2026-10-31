+++
title = "Private reads and credentials"
description = "A private repository is a trusted source. The caller still needs permission to read it."

weight = 41

[extra]
nav_title = "Private reads and credentials"
group = "Detailed behaviour"
kind = "Reference"
reference = "/docs/how-it-works/#a-request-step-by-step"
+++

## Private copies do not re-enter public admission {#trust}

Écluse reads private packages using the caller's credentials. Private metadata must meet the trusted integrity floor. Managed mirror versions must also meet their completion checks.

A conventional private npm artifact hit bypasses that metadata path and relies on the client's integrity verification.

Cargo reads private crates through `privateUpstream` like every other ecosystem. A crate there without a mirror record is trusted private content.

Changing a public admission rule does not immediately block an existing private copy. See [Admission, revocation, and retained copies](@/docs/revocation.md).

{{ diagram(name="metadata-admission", alt="Version discovery and admission, from the shared threat model.") }}

## The caller and worker have different authority {#credentials}

The caller's credential authorises private reads and, by default, publication. The mirror worker has its own write credential.

A worker token does not give the caller private access. An explicit private authentication refusal stops the request rather than falling back to a public source.

Cargo keeps its native refusal statuses, which the [private access policy](@/docs/configuration.md#first-party-namespaces) lists.
Cargo's discovery document is answered locally after edge authentication, so it never carries a private registry's answer.

When `server.authToken` is set, the shared edge token also travels to the private registry. That mode cannot preserve a different registry identity for each caller.

## A failing private registry stops the mount {#private-outage}

This matters on every mount that sets `privateUpstream`, because Écluse reads it on each listing and download. The private registry's answer decides what the client receives.

| Private registry answer | What Écluse does |
| --- | --- |
| A document for the requested name | Merges it with the admitted public versions. |
| `404` | Serves admitted public content. When the public registry also answers `404`, no registry holds the name and Écluse answers `404`. When the public registry is unavailable instead, Écluse answers `503`. A first-party name answers `404`. |
| `401` or `403` | Refuses with the statuses the [private access policy](@/docs/configuration.md#first-party-namespaces) lists. |
| A transport error, a timeout, a `5xx` or a `429` | Refuses with `503` for every name and serves no public content. When the private registry sent `Retry-After`, Écluse forwards it. |
| A redirect, any other status, a document for another name, or a document Écluse cannot decode or that exceeds its bound | Refuses with `502` for every name and serves no public content. |

Écluse does not follow a redirect from the private registry. These statuses apply to listings, metadata and artifacts on every ecosystem, and a first-party name keeps its own wording. RubyGems discovery reads the private `/versions` directory as a whole, and the [protocol reference](@/docs/protocol-support.md#rubygems) describes how a failure there refuses discovery.

A private package whose name you have not declared first-party can share its name with a public package. If Écluse served public content while the private read failed, the public package would replace the private one as soon as its version passed your rules, and builds would switch packages without notice. Declaring every private name cannot close that gap reliably, because most registries give no complete list of first-party names.

So the mount's availability follows the private registry's. A private outage stops every build through the mount, including builds that need only public packages. A public outage with a healthy private registry still serves the private versions. Écluse logs each failed private metadata read with its cause.

## First-party names stay private {#names}

A first-party declaration selects the private source exclusively. A missing first-party package stays missing. Écluse does not look for the same name publicly.

The declaration limits package names, not callers. Registry authentication still decides who may read or publish.

## Digest conflicts raise an alarm {#conflicts}

When eligible public and private metadata disagree on digests, Écluse logs the conflict and increments its divergence counter.

An unmanaged private copy can remain listed and readable because the private repository is trusted. A disagreement proves neither copy correct.

Managed mirror checks still apply. Divergence alone never authorises cleanup.
