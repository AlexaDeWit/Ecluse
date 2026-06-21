# Architecture & Requirements

This is the **index** to Écluse's systems design. It captures the vision, how a
request flows end to end, and what is out of scope; the detailed design of each
concern lives in the linked documents under [`architecture/`](architecture/).
Development practices — codebase layout, testing strategy, and CI / repo
requirements — live in [`../CONTRIBUTING.md`](../CONTRIBUTING.md).

## Vision

Supply chain attacks through malicious or hijacked package publications are an
increasing threat in high-volume ecosystems like npm. **Écluse** (package
`ecluse`) is a lightweight proxy that sits between consumers (developers, CI)
and the npm registry, applying a configurable resilience policy before any
package reaches a build — without taking on the cost or complexity of hosting
packages itself.

The name is French for a canal lock — a chamber whose gates never open at once.
That is the posture: not a wall that blocks, but a controlled passage every
dependency is held in and cleared through before it is admitted to a build. The
goal is resilience — mitigating the blast radius of a bad publish — rather than
malware detection.

The proxy is not a registry. It delegates storage to whatever backend the
operator chooses (e.g. AWS CodeArtifact or GCP Artifact Registry), and enforces a
configurable policy on what may be fetched and mirrored from the public registry.

## Request Lifecycle

```
Client request
    │
    ▼
[1] Fetch from private upstream
    │
    ├─ 2xx ──────────────────────────────────────────► Serve to client. Done.
    │
    └─ non-2xx (miss)
        │
        ▼
    [2] Fetch from public upstream
        │
        ├─ non-2xx ──────────────────────────────────► Forward error to client.
        │
        └─ 2xx
            │
            ▼
        [3] Parse into PackageInfo / PackageDetails
            │
            ▼
        [4] Evaluate RuleSet (deny by default)
            │
            ├─ Pure rules first (no IO, fast)
            ├─ Effectful rules if undecided (CVE lookups, etc.)
            │
            ├─ Denied ──────────────────────────────► 403 + denial message. Done.
            │
            └─ Allowed
                │
                ▼
            [5] Enqueue mirror job — non-blocking
                │
                ▼
            [6] Serve response to client immediately
```

Tarball/artifact requests follow the same lifecycle via `fetchArtifact`.

On the public-upstream path, the served packument is **filtered to admitted
versions** (denied versions removed, `latest` repointed to the newest survivor,
403 if none survive) before step [6] — see
[Rules Engine → Applying verdicts to a packument](architecture/rules-engine.md#applying-verdicts-to-a-packument).

## Document Map

| Document | Covers |
|---|---|
| [Registry Model](architecture/registry-model.md) | The three-registry model and the `RegistryClient` protocol seam. |
| [Internal Domain Model](architecture/domain-model.md) | `PackageDetails` and the ecosystem-agnostic signal vocabulary the rules engine consumes. |
| [Multi-Ecosystem Hosting](architecture/hosting.md) | Mounting ecosystems under path prefixes, URL rewriting, and dispatch. |
| [Web Layer](architecture/web-layer.md) | The raw-WAI front door: routing, the control/data-plane split, streaming, middleware. |
| [Rules Engine & Responses](architecture/rules-engine.md) | Deny-by-default evaluation, the rule tiers, the CVE subsystem, and denial responses. |
| [Cloud Backends & Mirroring](architecture/cloud-backends.md) | The mirror queue and the two cloud seams (`MirrorQueue`, `CredentialProvider`); AWS & GCP. |
| [Configuration & Authentication](architecture/configuration.md) | Environment configuration, outbound registry credentials, and inbound client authentication. |
| [Technology Stack](architecture/technology-stack.md) | Library choices and the key cross-cutting decisions. |

## Out of Scope (for now)

- Package hosting / storage (delegated to the configured registries).
- Mirroring to raw object storage (S3 / GCS). The mirror target is a registry and
  writes go through `publishArtifact`, so no blob-store seam is introduced;
  revisit only if a non-registry mirror target is ever wanted.
- Web UI or admin API.
- PyPI and other non-npm **adapters** — the hosting model and `RegistryClient`
  seam are designed to accommodate them (see
  [Multi-Ecosystem Hosting](architecture/hosting.md#multi-ecosystem-hosting)), but
  only the npm adapter ships at launch.
- Cloud IAM validation at the proxy edge (gateway concern).
- Local on-disk caching of artifacts (the mirror retry window is acceptable).
- **GCP backends at launch** — the cloud seams (mirror queue, managed-registry
  token) are designed for GCP from day one, but shipping a GCP backend is gated on
  the client-viability spike; AWS ships first (see
  [Cloud Backends](architecture/cloud-backends.md#cloud-backends)).
