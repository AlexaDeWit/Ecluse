# Architecture and requirements

Index to Écluse's systems design: what it is, the roles it runs, and what is out of scope. Each concern's detailed design lives under [`architecture/`](architecture/).
Development practices, layout, testing, and CI are in
[`../CONTRIBUTING.md`](../CONTRIBUTING.md). The _why_ is in
[`../MOTIVATION.md`](../MOTIVATION.md). This document and its links are the _how_.

Écluse is a supply-chain policy proxy for package registries. It sits between the client
(a developer or CI) and the upstream registry, and applies a deny-by-default policy before
any package reaches a build. It hosts no packages itself. The name is French for a canal
lock: the controlled passage every dependency clears before a build. The goal is
resilience, limiting the blast radius of a bad publish, not malware detection. The central
control is the quarantine: each new public version waits out a window, because registries
usually find and yank a malicious publish within it.

Écluse delegates storage to the store each mount declares. npm supports reads, mirroring, and
first-party publication, and PyPI supports reads. The code is Haskell (GHC 9.10), pinned
through Nix flakes, with `ecluse.cabal` and `flake.lock` as the dependency authority.

## Roles

One image runs every role, and the container command picks one. The operator view of each role is
in [Deploying Écluse](https://ecluse-proxy.com/docs/deployment/#the-image-and-its-roles).

| Role | What it does | Design |
|---|---|---|
| `ecluse proxy` | Serves clients, gates public versions, relays first-party publishes, and enqueues mirror jobs. It runs the mirror worker too unless started with `--no-worker`. | [Web layer](architecture/web-layer.md), [Registry model](architecture/registry-model.md), [Rules engine](architecture/rules-engine.md) |
| `ecluse mirror` | Consumes mirror jobs, re-checks each version against policy, and publishes it to the mirror store. | [Mirror queue](architecture/cloud-backends.md#mirror-queue) |
| `ecluse pilot` | Builds each ecosystem's advisory database from the OSV exports and the EPSS feed, and publishes it to the advisory store. | [Local polling, decoupled ingestion](architecture/rules-engine.md#local-polling-decoupled-ingestion) |
| `ecluse dredger` | Re-checks the mirror store against current policy and deletes what it now denies. | [Walking a store](architecture/cloud-backends.md#walking-a-store) |
| `ecluse check-config` | Validates the configuration for every role and prints the resolved posture. | [Configuration](architecture/configuration.md) |

A request's path through the proxy is in
[A request, step by step](https://ecluse-proxy.com/docs/how-it-works/#a-request-step-by-step).
What Écluse supports, and what it does not, is in
[Protocol support](https://ecluse-proxy.com/docs/protocol-support/).

## Document map

| Document | Covers |
| --- | --- |
| [Registry model](architecture/registry-model.md) | The four registry roles (two reads, two writes), the domain vocabulary, and the registry abstraction. |
| [Web layer](architecture/web-layer.md) | Raw-WAI front door: routing, mounts, the OpenAPI spec, the control/data-plane split, streaming, and graceful shutdown. |
| [Rules engine and responses](architecture/rules-engine.md) | Deny-by-default evaluation, the rule tiers, the CVE subsystem, and denial responses. |
| [Cloud backends and mirroring](architecture/cloud-backends.md) | The mirror queue, Dredger's store walk, the platform and store axes, the two store kinds, and the three handles (`MirrorQueue`, `CredentialProvider`, `StoreMaintenance`). |
| [Configuration and authentication](architecture/configuration.md) | Environment config, outbound registry credentials, and inbound client auth. |
| [Security posture](architecture/security.md) | The trust assumptions the threat model rests on, the credential posture, and the two floors that fail closed. |
| [Threat model](https://ecluse-proxy.com/docs/threat-model/) | The STRIDE register, generated from the Saerskriven model (`threat-modelling/ecluse.yaml`). The single source of truth for the system's threats. |
| [Observability](architecture/observability.md) | Opt-in OpenTelemetry/OTLP tracing and metrics, with Datadog optional. |
| [Release and supply-chain operations](architecture/release-supply-chain.md) | The reproducible OCI image, the publish/attest chain (provenance + SBOM), and CVE and freshness scanning. |

## Out of scope

- Package hosting or storage. The registries each mount declares hold every package.
- Filesystem and object-store package backends. Mirror writes use registry protocols. The S3
  advisory store is a separate capability.
- A web UI or an admin API.
- Cloud IAM validation at the proxy edge, which is a gateway concern.
- Local on-disk caching of artifacts.
- Re-specifying upstream registry protocols in the
  [OpenAPI spec](architecture/web-layer.md#openapi-spec). Écluse documents its coverage, not npm's
  full contract.
