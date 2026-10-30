+++
title = "Keep copies of public packages"
description = "Mirror admitted public versions into a registry you control, so completed copies remain available if the public source disappears."

weight = 34

[extra]
nav_title = "Keep public package copies"
group = "Set up"
kind = "Guide"
reference = "/docs/pypi-setup/"
+++

## Connect the write and read paths {#repositories}

Set `mirrorTarget` to the repository receiving public copies. Set `privateUpstream` to the private endpoint clients read through Écluse.

Configure that read endpoint to include the mirror repository. Écluse does not create this relationship in your registry.

Cargo's public replacement reads completed copies directly from `mirrorTarget`.
Its `/cargo/private/` alternate view reads `privateUpstream` separately and never falls back to public.
Keep private-only packages out of crates.io source replacement.

**Example: npm with CodeArtifact**

```yaml
mounts:
  npm:
    privateUpstream:
      codeArtifact:
        url: https://DOMAIN-ACCOUNT.d.codeartifact.REGION.amazonaws.com/npm/internal/
    mirrorTarget:
      codeArtifact:
        url: https://DOMAIN-ACCOUNT.d.codeartifact.REGION.amazonaws.com/npm/mirror/
```

Replace the domain, account, and region values with your repository endpoints. Keep the mirror, private read repository, and publication repository distinct if you plan to use cleanup.

An npm or RubyGems mirror target takes `url` alone, because Écluse derives the upload address from it. PyPI and Cargo also need the `uploadUrl` their sections below describe.

## Choose where the worker runs {#worker}

`proxy` includes a mirror worker. For separate services, run `proxy --no-worker` and `mirror` against the same durable SQS queue.

**Durable queue**

```yaml
queue:
  url: https://sqs.REGION.amazonaws.com/ACCOUNT/ecluse-mirror
```

The in-memory queue suits a single-process trial. It cannot connect separate processes and loses queued work on restart.

Give the proxy permission to enqueue. Give the worker permission to consume, renew visibility, acknowledge jobs, and publish to the mirror.

A CodeArtifact worker mints its token from workload identity. Generic registries use the mirror's configured write credential.

## For PyPI, add completion state {#python}

PyPI needs an explicit native upload endpoint and durable state in addition to its registry read URL:

**Example: PyPI mirror additions**

```yaml
mirrorState:
  url: s3://your-mirror-state-bucket/pypi
mounts:
  pypi:
    privateUpstream:
      codeArtifact:
        url: https://DOMAIN-ACCOUNT.d.codeartifact.REGION.amazonaws.com/pypi/internal/
    mirrorTarget:
      codeArtifact:
        url: https://DOMAIN-ACCOUNT.d.codeartifact.REGION.amazonaws.com/pypi/mirror/
        uploadUrl: https://DOMAIN-ACCOUNT.d.codeartifact.REGION.amazonaws.com/pypi/mirror/
```

The proxy needs state reads and prefix listing. The worker also needs conditional writes. Both use the same bucket, prefix, and target identity.

Do not expire state records or referenced metadata companions. The state proves which files form a complete version. Package archives remain in the registry.

## For Cargo, retain original source records {#cargo}

Configure the mirror's sparse index root and approved upload API base using the keys in the configuration reference.
Add `mirrorState.url` and give the proxy state-read and prefix-list permissions.
Give the worker conditional-write permissions too. Preserve the same state prefix and target identity across restarts.

Do not expire or discard these state records.
A successful upload or a present archive cannot replace them, as the [Cargo protocol reference](@/docs/protocol-support.md#cargo) explains.

Separate public mirrors from first-party publication where possible.
Collapsed storage remains supported subject to existing endpoint checks and role-specific cleanup restrictions.

## For RubyGems, retain platform and source records {#rubygems}

RubyGems mirroring requires `mirrorState.url`, with the same proxy and worker state permissions as Cargo.
Preserve the same state prefix and target identity across restarts, and do not expire or discard these records.
The [configuration reference](@/docs/configuration.md#the-configuration-reference) owns the keys.
The [RubyGems protocol reference](@/docs/protocol-support.md#rubygems) describes what the state proves.

## Verify a complete copy {#verify}

An admitted artifact download triggers background mirroring. The worker checks current policy before copying. For PyPI, it copies every supported distribution in the admitted version.

Confirm completion before relying on the mirror during an outage. Then verify a private read with fresh client state.

[Read the completion and recovery rules](@/docs/mirror-behaviour.md) for partial uploads, retries, and public outages.
