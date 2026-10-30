+++
title = "Choose your deployment"
description = "Écluse runs as containers on your platform. Choose the capabilities you need before creating their supporting services."

weight = 38

[extra]
nav_title = "Choose your deployment"
group = "Set up"
kind = "Guide"
reference = "/docs/deployment/"
+++

## Start with the proxy {#minimum}

Run `ecluse proxy` behind an HTTPS endpoint that only your intended clients can reach.
Enable the npm, PyPI, Cargo, or RubyGems mounts your teams need.

A public-only deployment needs no package store, queue, or advisory database. It applies the minimum age and any other rules that do not need advisories.

## Add services for the features you use {#services}

| You want to… | Configure… | Run… |
| --- | --- | --- |
| Check advisories | An advisory store and a writable local data directory. | `pilot` to publish databases. Other roles read them. |
| Serve private packages | A private registry read endpoint and caller credentials. | The proxy. |
| Mirror public packages | A mirror registry, private reads, and a queue. PyPI, Cargo, and RubyGems also need durable completion state. | The proxy with its embedded worker, or separate `proxy --no-worker` and `mirror` processes. |
| Publish your packages | A publication target and the first-party names you own. | The proxy. |
| Remove denied stored versions | A supported maintenance backend, separate credentials, and consent for each deletion target. | `dredger`. Start with a preview. |

## Keep package stores separate {#stores}

For a deployment with private packages and mirroring, use three repository roles:

1. **First-party repository:** holds packages your team publishes.
2. **Mirror repository:** holds public versions Écluse admits.
3. **Private read repository:** combines those two sources for clients.

In CodeArtifact, the private read repository uses the other two repositories as upstreams. Do not attach a public registry to it, directly or through another upstream.

Écluse treats private reads as trusted. A public connection there would bypass admission.

Cargo clients use one registry at `/cargo/` that replaces crates.io. It reads completed managed copies through the private read repository, like the other ecosystems.
The [Cargo protocol contract](@/docs/protocol-support.md#cargo) explains its source rules.

[Configure the mirror](@/docs/mirror-setup.md) when you are ready to add these stores.

## Set the network boundary {#network}

- Limit access to the proxy at both the external endpoint and the container network.
- Route package downloads from CI through Écluse. Block direct public registry and artifact access from those clients.
- Allow each Écluse role to reach its configured registries, cloud services, identity endpoints, and advisory feeds.
- Keep private stores free of public pull-through connections.

Use workload identity for cloud access. Keep caller credentials separate: the proxy forwards them to private reads and publications.

For per-caller access, leave `server.authToken` unset and enforce access at your network or gateway. A shared edge token does not preserve distinct registry identities.

## Plan public registry traffic {#registry-traffic}

A proxy adds public metadata reads, so plan traffic across your organisation before you concentrate installs behind one egress address. Mirroring reduces repeated artifact downloads, but listings still read public metadata. Review the [request costs and registry guidance](@/docs/operations.md#registry-traffic) when you size the deployment.

## On Kubernetes {#kubernetes}

| Setting | What Écluse needs |
| --- | --- |
| Container arguments | `["proxy"]`, `["proxy", "--no-worker"]`, `["mirror"]`, `["pilot"]`, or `["dredger"]`. Keep the image entrypoint. |
| Identity | A separate service account and workload role for each service. |
| Advisory directory | A writable volume at `/var/lib/ecluse/advisories`, accessible to UID/GID `65532`. An `emptyDir` is sufficient. |
| Health checks | `/livez` for liveness and `/readyz` for readiness on port `8080`. Allow time for the first advisory download. |
| Replicas | Scale proxy and mirror services separately if needed. Run one Pilot. Run no more than one Dredger per store. |
| Resources | Set CPU and memory limits, then review the memory plan in startup logs. Validate capacity with your package mix. |
| Configuration | Use the same intended policy across the proxy, workers, advisory publisher, and cleanup service. |

## On AWS {#aws}

CodeArtifact can provide the package repositories and SQS the durable mirror queue.
S3 can hold advisories and the completion state for PyPI, Cargo, and RubyGems mirrors.

Use EKS Pod Identity, IRSA, or an ECS task role for service credentials. The mirror worker mints its CodeArtifact write token. Client reads still use each caller's token.

The [permission reference](@/docs/permissions.md) lists what each role needs. Creating the resources and attaching those permissions follows your normal platform process.

{{ next(path="@/docs/configure.md", title="Package your configuration") }}
