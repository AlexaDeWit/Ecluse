+++
title = "PyPI deployment reference"
description = "Configure PyPI reads, first-party uploads, whole-version mirrors, durable state and optional CodeArtifact cleanup."
weight = 5
+++

A complete PyPI deployment combines a policy gate, a private read repository, native upload endpoints, a mirror worker and durable completion state.
The package repositories hold distribution archives. The S3-compatible state store holds completion evidence, retained index fields and declared metadata companions.
It is not an S3 package store.

## Configure the repositories

This example uses three distinct CodeArtifact repositories.
Other valid read and write topologies remain supported, subject to the [endpoint checks](@/docs/configuration.md#endpoint-collisions).
Configure the private read repository to aggregate the mirror and first-party repositories through its upstream chain.
That relationship belongs to CodeArtifact configuration. Écluse does not create it.

```yaml
server:
  publicUrl: https://ecluse.example.internal
queue:
  url: https://sqs.us-east-1.amazonaws.com/123456789012/ecluse-mirror
advisories:
  url: s3://acme-ecluse-advisories
mirrorState:
  url: s3://acme-ecluse-mirror-state/pypi
mounts:
  pypi:
    firstParty: acme,acme-*
    publicUpstream:
      registry:
        url: https://pypi.org
    privateUpstream:
      codeArtifact:
        url: https://acme-123456789012.d.codeartifact.us-east-1.amazonaws.com/pypi/internal/
    mirrorTarget:
      codeArtifact:
        url: https://acme-123456789012.d.codeartifact.us-east-1.amazonaws.com/pypi/mirror/
        uploadUrl: https://acme-123456789012.d.codeartifact.us-east-1.amazonaws.com/pypi/mirror/
    publicationTarget:
      codeArtifact:
        url: https://acme-123456789012.d.codeartifact.us-east-1.amazonaws.com/pypi/first-party/
        uploadUrl: https://acme-123456789012.d.codeartifact.us-east-1.amazonaws.com/pypi/first-party/
```

The [configuration reference](@/docs/configuration.md#the-configuration-reference) owns every key, default and environment spelling.
Every `url` is a registry root. Écluse appends `simple/` for index reads.
Each `uploadUrl` is the native upload endpoint of the same CodeArtifact repository as its `url`.
For another PyPI registry, use the `registry` tag and its native endpoints.
A generic mirror also requires a Basic `username` and a secret `token` supplied through the environment.
See [upload configuration](@/docs/configuration.md#pypi-uploads-and-mirror-state) for state consistency and retention requirements.

Point pip, uv or Poetry at `https://ecluse.example.internal/pypi/simple/`.
Point a native multipart publisher at `https://ecluse.example.internal/pypi/legacy/`.
Use [client authentication](@/docs/deployment.md#edge-authentication-and-client-credentials) and the [protocol reference](@/docs/protocol-support.md) for credential handling and supported routes.
Do not add a public fallback index that lets clients bypass the gate.
Apply the [network fences](@/docs/deployment.md) to metadata, distribution and companion traffic.

## Assign credentials and roles

Private reads use the caller's Basic credential. Public reads are anonymous.
Publication also keeps the caller's complete Basic pair unless you configure a static replacement token.
A static publication token requires edge authentication. Generic static publication credentials also require a configured username.
The CodeArtifact mirror worker mints its own token from its container role and uses the `aws` username.
Its write credential does not grant callers private read access.

Run `ecluse proxy` for an embedded worker.
For separate fleets, run `ecluse proxy --no-worker` and `ecluse mirror` with the same mounts, rules and SQS queue.
Both fleets must use the same `mirrorState.url` bucket and prefix and the same mirror-target identity.
The in-memory queue cannot connect separate processes. Split roles refuse that configuration.
The proxy needs mirror-state reads and prefix listing. The worker also needs conditional writes.
Scope state access to its bucket and prefix independently from advisory access.
For S3, readers need `s3:GetObject` and prefix-scoped `s3:ListBucket`. Workers also need `s3:PutObject`.
See [role permissions](@/docs/deployment.md) and [CodeArtifact permissions](@/docs/dredger.md#permissions).

Run Pilot for the PyPI advisory database.
Publish the artifact with `ecluse pilot compile --ecosystem pypi --out DIR --upload`
before starting admission roles with `advisories.url` configured.
[Running Pilot](@/docs/deployment.md#running-pilot) describes qualified generations and storage.
Use `ecluse check-config` to detect configuration refusals. It makes no cloud call and cannot verify repository aggregation or permissions.
Check [per-mount readiness](@/docs/operations.md#health-probes), worker progress and [telemetry](@/docs/operations.md) after startup.
The supplied example activates advisory rules and waits for its first accepted advisory generation.
Readiness does not prove that every repository or state operation succeeds.

## Choose policy and first-party names

The shipped policy admits old releases and qualified exact advisory fixes. Configuring
`advisories.url` activates the known-malicious deny unless you disable it. Scored CVSS and EPSS
denies are opt-in.
PyPI refuses `AllowScope`, `DenyInstallTimeExecution`, `DenyExternalDependencies` and `DenyIfDeprecated` in an active policy.
Do not copy npm-only rules into shared rules.
The [rule policy](@/docs/configuration.md#rule-policy) defines precedence, unavailable-data behaviour and admission versus revocation.
The [first-party declarations](@/docs/configuration.md#first-party-namespaces) define exact names and separator-bounded prefixes.

The example reserves `acme` and `acme-*` for private reads and publication.
A missing first-party project never falls back to public PyPI. The worker does not mirror first-party projects, including older queued jobs.
Declare the same names in every role.
Review existing public-derived copies before adding a declaration, because Dredger then protects those names.
[MAL advisory behaviour](@/docs/configuration.md#known-malicious-packages-and-upgrades) follows the configured rule and its precedence.

## Completion, outages and recovery

An admitted distribution GET triggers a job for the whole supported version, not only the client's wheel.
The worker copies every admitted wheel and source distribution and retains declared metadata companions.
It records expected identities before upload and verifies distribution bytes before sending them.
Completion checks current target inventory identities and digests against retained verified upload evidence, and verifies retained companions.
This check does not fetch every target distribution again.
A duplicate upload response alone does not prove completion.
[PyPI protocol support](@/docs/protocol-support.md#supported-registries) defines retained fields and companion declarations.

Physical uploads can appear one file at a time in the underlying repository.
Through Écluse, an incomplete managed version needs a validated complete public alternative or answers unavailable.
Malformed or unavailable state does not prove completion.
Retries and restarts use durable expected identities, including files that the current public index no longer lists.
Do not expire project records or referenced companions.

A completed version can serve recorded distributions and companions during a public outage after current private authorisation and identity proof.
Those reads need no new public admission. Project listings still discover public releases when the public source is available.
These guarantees cover reads through Écluse. Direct repository reads can observe partial uploads.
The [mirror design](https://github.com/AlexaDeWit/Ecluse/blob/main/docs/architecture/cloud-backends.md#durable-pypi-read-data) explains recovery and concurrent state updates.

## Upgrade the queue schema

Schema 2 jobs carry no source URL. Workers resolve current metadata and policy instead.
Deploy dual-reading workers and the new read visibility support first.
Stop old workers before deploying schema 2 producers and enabling PyPI mirroring.
Old workers must not consume schema 2 jobs.
For rollback, stop producers and drain or quarantine schema 2 messages before reverting workers.
New workers accept schema 1 jobs but discard their source URLs.
See the [queue contract](https://github.com/AlexaDeWit/Ecluse/blob/main/docs/architecture/cloud-backends.md#mirror-queue).

## Optional cleanup and evidence limits

PyPI Dredger uses CodeArtifact's inventory and deletion APIs for the mirror and private cache.
The PyPI adapter supplies no protocol listing or deletion operation for generic registries or Verdaccio.
Those maintenance configurations refuse startup. They can still serve supported read and write roles.
Keep the publication repository distinct from both maintained repositories.
Attach consent independently to each maintained repository and preview before deleting.
[Dredger](@/docs/dredger.md) owns consent, refill classification, deletion caps, recovery and permissions.

The fixed PyPI E2E suite covers the full ecosystem contract, including whole-version mirrors, companions, publication, Dredger and telemetry.
Cross-target tests prove wheel selection and installed contents. They do not execute foreign architecture code.
CodeArtifact adapter tests use deterministic control-plane responses. They do not establish live AWS permissions or service guarantees.
The [testing contract](https://github.com/AlexaDeWit/Ecluse/blob/main/docs/test-contracts.md#fixed-e2e-entrypoints) records the required independent lower-tier evidence.
