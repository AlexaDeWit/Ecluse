# Architecture and requirements

Index to Écluse's systems design: what it is, how a request flows, and what is out of
scope. Each concern's detailed design lives under [`architecture/`](architecture/).
Development practices, layout, testing, and CI are in
[`../CONTRIBUTING.md`](../CONTRIBUTING.md); the _why_ is in
[`../MOTIVATION.md`](../MOTIVATION.md). This document and its links are the _how_.

Écluse is a supply-chain policy proxy for package registries. It sits between consumers
(developers, CI) and the upstream registry and applies a deny-by-default policy before any
package reaches a build, without hosting packages itself. The name is French for a canal
lock: the controlled passage every dependency clears before a build. The goal is
resilience, mitigating the blast radius of a bad publish, not malware detection.

Écluse is not a registry. It delegates storage to the operator's backend (AWS
CodeArtifact today) and enforces policy on what may be fetched from, and mirrored from, the
public registry.

## Codebase decomposition

Écluse builds as three libraries behind one [`ecluse.cabal`](../ecluse.cabal):

- **`ecluse-core`** (`core/src`, `Ecluse.Core.*`): the pure, ecosystem-agnostic core (domain
  model, registry protocols, rule tiers, CVE lookup, the agnostic server layer, and the mirror
  worker). It carries local, injectable effects only, depends on the OpenTelemetry API but never
  its SDK, and never on `warp` or `amazonka`.
- **`ecluse-runtime`** (`runtime/src`, `Ecluse.Runtime.*`): the effectful edge that binds the
  process-global substrate: the OTel SDK and OTLP export, the `warp` binding, the `katip`
  scribes, and the cloud adapters. It depends on `ecluse-core`.
- **`ecluse`** (`src`, `Ecluse.*`): the composition shell (config loader and resolver, the
  `Boot` bracket, and the role runners). It depends on both tiers.

The dependency arrow points inward only (`ecluse` → `ecluse-runtime` → `ecluse-core`), so a core
module reaching outward fails to compile; [`ecluse.cabal`](../ecluse.cabal) and the
[`README`](../README.md) architecture section are the authoritative module map. The `ecluse`
executable (`app/Main.hs`) is a thin multicall router for the `proxy`, `pilot`, and `dredger`
roles, plus `check-config`, which resolves the configuration and prints the posture without
booting anything.

## Request lifecycle

The three request shapes use the upstreams differently: a tarball _falls back_, a
packument _merges_, and a publish _writes through_.

```mermaid
flowchart TD
    C(["Client request"]) --> K{"packument, tarball, or publish?"}

    K -->|"tarball"| T1["Fetch from private upstream"]
    T1 -->|"2xx hit"| TSV(["Stream unfiltered. Done."])
    T1 -->|"miss"| T2["Fetch version metadata from public<br/>+ evaluate rules (deny by default)"]
    T2 -->|"Denied / Unavailable"| TD(["403 / 503 / 500. Done."])
    T2 -->|"Admitted"| T3["Stream from public + enqueue mirror job<br/>(non-blocking)"]
    T3 --> TSV2(["Serve immediately. Done."])

    K -->|"packument"| P1["Fetch private + public in parallel"]
    P1 --> P2["Trust private versions;<br/>gate public versions (rules, deny by default)"]
    P2 --> P3["Merge (private wins; flag divergence),<br/>filter, repoint latest"]
    P3 -->|"survivors"| PSV(["Serve merged packument. Done."])
    P3 -->|"none survive"| PD(["403 / 503. Done."])

    K -->|"publish (PUT)"| W1{"ECLUSE_MOUNTS__NPM__PUBLICATION_TARGET set?"}
    W1 -->|"no"| W405(["405 Method Not Allowed. Done."])
    W1 -->|"yes"| W2["Enforce publish-scope allow-list<br/>(anti-shadowing)"]
    W2 -->|"out of scope"| WR(["4xx, no upstream write. Done."])
    W2 -->|"in scope"| W3["Write to publication target<br/>(client token forwarded)"]
    W3 --> WSV(["npm success. Done."])
```

- **Tarball/artifact**, gated for one version. A private hit streams unfiltered; a private
  miss gates the version on its public metadata and, if admitted, streams from public and
  enqueues a mirror job (else the [error model](architecture/web-layer.md#error-model)).
  Mirroring is demand-driven, so only versions actually pulled are mirrored.
- **Packument**, a merge. Public versions are rule-filtered, private versions trusted, and the
  two combine (private wins a collision, divergence flagged, `latest` repointed). Merging keeps
  not-yet-mirrored public versions visible so demand-driven mirroring can fire. See
  [Packument merge](architecture/registry-model.md#packument-merge-across-upstreams).
- **Publish** (`PUT /{pkg}`, `npm publish`), the one client-driven write. Checked against the
  publish-scope allow-list (anti-shadowing, before any upstream write) and relayed to the
  publication target with the publisher's forwarded credential. Opt-in: a `PUT` is `405` when
  `ECLUSE_MOUNTS__NPM__PUBLICATION_TARGET` is unset. See
  [Publishing first-party packages](architecture/registry-model.md#publishing-first-party-packages-the-publication-target).

## Document map

| Document | Covers |
| --- | --- |
| [Diagrams](architecture/diagrams.md) | Mermaid companion: system overview, packument / tarball / worker sequences, rule and credential lifecycles. |
| [Registry model](architecture/registry-model.md) | The four registry roles (two reads, two writes) and the registry abstraction. |
| [Internal domain model](architecture/domain-model.md) | `PackageDetails` and the ecosystem-agnostic signals the rules engine consumes. |
| [Web layer](architecture/web-layer.md) | Raw-WAI front door: routing, mounts, the control/data-plane split, streaming, and graceful shutdown. |
| [API surface and capability manifest](architecture/api-surface.md) | The OpenAPI capability manifest and the synthesised-packument schema. |
| [Rules engine and responses](architecture/rules-engine.md) | Deny-by-default evaluation, the rule tiers, the CVE subsystem, and denial responses. |
| [Cloud backends and mirroring](architecture/cloud-backends.md) | The mirror queue and the two cloud handles (`MirrorQueue`, `CredentialProvider`); AWS today, GCP planned. |
| [Configuration and authentication](architecture/configuration.md) | Environment config, outbound registry credentials, and inbound client auth. |
| [Access and credential model](architecture/access-model.md) | How reads are credentialled (the caller's credential is forwarded, public reads anonymous), why the private origin is never cached, and the planned edge-auth model. |
| [Security invariants](architecture/security.md) | Outbound-request and input-validation defences: canonicalisation, the host allowlist, internal-range blocking, and response bounds. |
| [Fault model](architecture/fault-model.md) | Failures as typed values, the confined-exception pattern, the two outer edges, and the disposition vocabulary. |
| [Threat model](https://ecluse-proxy.com/threat-model.html) | The STRIDE register, generated from the Threat Dragon model (`threat-modelling/ecluse.json`); the single source of truth for the system's threats. |
| [Observability](architecture/observability.md) | Opt-in OpenTelemetry/OTLP tracing and metrics; Datadog optional. |
| [Technology stack](architecture/technology-stack.md) | Library choices and the key cross-cutting decisions. |
| [Release and supply-chain operations](architecture/release-supply-chain.md) | The reproducible OCI image, the publish/attest chain (provenance + SBOM), and CVE and freshness scanning. |

## Out of scope

- Package hosting or storage (delegated to the registries).
- Mirroring to raw object storage (S3 / GCS): the mirror target is a registry and writes go
  through `publishArtifact`; revisit only for a non-registry mirror target.
- Web UI or admin API.
- Re-specifying upstream registry protocols in the
  [capability manifest](architecture/api-surface.md): Écluse documents its coverage, not npm's
  full contract, which clients hardcode.
- Non-npm adapters: the adapter registry, the mount model, and the protocol codec over the
  shared publish transport accommodate them (see
  [Multi-ecosystem mounts](architecture/web-layer.md#multi-ecosystem-mounts)), but only npm
  ships at launch. PyPI and RubyGems are planned.
- Cloud IAM validation at the proxy edge (a gateway concern).
- Local on-disk caching of artifacts (the mirror retry window is acceptable).
- GCP backends: the cloud handles are designed for GCP, but a GCP backend is gated on the
  client-viability spike; AWS ships first (see
  [Cloud backends](architecture/cloud-backends.md#cloud-backends)).
