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

## First-party names stay private {#names}

A first-party declaration selects the private source exclusively. A missing first-party package stays missing. Écluse does not look for the same name publicly.

The declaration limits package names, not callers. Registry authentication still decides who may read or publish.

## Digest conflicts raise an alarm {#conflicts}

When eligible public and private metadata disagree on digests, Écluse logs the conflict and increments its divergence counter.

An unmanaged private copy can remain listed and readable because the private repository is trusted. A disagreement proves neither copy correct.

Managed mirror checks still apply. Divergence alone never authorises cleanup.
