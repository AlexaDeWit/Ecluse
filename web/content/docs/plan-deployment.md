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

Run `ecluse proxy` behind an HTTPS endpoint that only your intended clients can reach. Enable an npm mount, a PyPI mount, or both.

A public-only deployment needs no package store, queue, or advisory database. It applies the minimum age and any other rules that do not need advisories.

## Add services for the features you use {#services}

| You want to… | Configure… | Run… |
| --- | --- | --- |
| Check advisories | An advisory store and a writable local data directory. | `pilot` to publish databases. Other roles read them. |
| Serve private packages | A private registry read endpoint and caller credentials. | The proxy. |
| Mirror public packages | A mirror registry, a private read endpoint that includes it, and a queue. PyPI also needs durable completion state. | The proxy with its embedded worker, or separate `proxy --no-worker` and `mirror` processes. |
| Publish your packages | A publication target and the first-party names you own. | The proxy. |
| Remove denied stored versions | A supported maintenance backend, separate credentials, and consent for each deletion target. | `dredger`. Start with a preview. |

## Keep package stores separate {#stores}

For a deployment with private packages and mirroring, use three repository roles:

1. **First-party repository:** holds packages your team publishes.
2. **Mirror repository:** holds public versions Écluse admits.
3. **Private read repository:** combines those two sources for clients.

In CodeArtifact, the private read repository uses the other two repositories as upstreams. Do not attach a public registry to it, directly or through another upstream.

Écluse treats private reads as trusted. A public connection there would bypass admission.

[Configure the mirror](@/docs/mirror-setup.md) when you are ready to add these stores.

## Set the network boundary {#network}

- Limit access to the proxy at both the external endpoint and the container network.
- Route package downloads from CI through Écluse. Block direct public registry and artifact access from those clients.
- Allow each Écluse role to reach its configured registries, cloud services, identity endpoints, and advisory feeds.
- Keep private stores free of public pull-through connections.

Use workload identity for cloud access. Keep caller credentials separate: the proxy forwards them to private reads and publications.

For per-caller access, leave `server.authToken` unset and enforce access at your network or gateway. A shared edge token does not preserve distinct registry identities.

## Plan public registry traffic {#registry-traffic}

Estimate public requests before you put all installs behind one egress address. Écluse adds policy metadata reads, so a proxy alone does not reduce registry traffic.

The table counts public requests for admitted packages outside `firstParty`, with the shipped local cache and a stable registry. It excludes private reads, redirects, retries, and failed requests. A document means the full npm packument or the project's PyPI Simple index, even when Écluse keeps only one version.

| Client request | Serve-only mount or pure gate | Mirrored mount, complete private copy available | Mirrored mount, copy unavailable |
| --- | --- | --- | --- |
| Listing: npm packument or PyPI Simple page | 1 document | 1 document | 1 document |
| Download: npm tarball or PyPI distribution | 1 document unless the selected-version cache hits, plus 1 artifact request | 0 public requests | Same as serve-only, plus a separate mirror job after a successful admitted GET |
| Mirror job for an incomplete version | Not applicable | No public read when the job confirms completion | Per copy pass: 1 document unless npm finds a selected-version cache hit, plus files needing copying or verification and missing companions |
| PyPI declared metadata companion | 1 document unless the selected-version cache hits, plus 1 companion request | 0 public requests | Same as serve-only. A companion request does not itself enqueue mirroring |

Checked 2026-10-18 against the shipped [listing pipeline][traffic-listing], [artifact pipeline][traffic-artifact], and [companion pipeline][traffic-companion]. First-party names never fall back to public registries. A private authorisation refusal also stops public fallback.

A mirror job makes a separate metadata read for each copy pass. npm can reuse a selected version from the serving process. PyPI workers always fetch their own Simple document because they need the publication fields. The worker then fetches files that need copying or identity verification, plus missing declared companions. PyPI mirrors the whole supported version, including its wheels and source distributions, rather than only the file the client requested.

Completion passes and queue retries can add requests. See the [worker wiring][traffic-worker] and [copy and completion logic][traffic-copy], checked 2026-10-18.

### What the cache removes

The shipped cache never retains full public documents. Identical metadata reads share a fetch only while it runs in the same process. Each pod has separate sharing state and retained values, so more replicas can increase public traffic.

The selected-version cache keeps eligible entries for `cache.ttl`, which defaults to [60 seconds][traffic-cache-config] (checked 2026-10-18). Entries can expire, be evicted, or exceed the retention budget. A listing does not populate this cache. Caching the assembled listing response avoids rendering work, but still needs the public document first. See the [cache implementation][traffic-cache] and [metadata resolution][traffic-metadata], checked 2026-10-18.

npm metadata reads request the full packument with `application/json` because age rules need its `time` map. PyPI full and selected reads use the same Simple-index request. See the [npm reader][traffic-npm] and [PyPI reader][traffic-pypi], checked 2026-10-18.

For example, downloading 1,500 distinct npm versions through a serve-only mount can require 3,000 public requests. Each version needs one metadata read and one tarball. This estimate uses the [artifact pipeline][traffic-artifact] and [metadata resolution][traffic-metadata], checked 2026-10-18. It assumes no client or selected-version cache hits, no listing requests, and no retries. With complete private copies, those artifact downloads need no public requests. Listings still read public metadata.

[Configure mirroring](@/docs/mirror-setup.md) to reduce repeated artifact downloads at scale. Keep package-manager caches in CI too, and stagger cold installs where possible. Measure traffic across all deployments and include requests that reach registries without Écluse.

### Registry limits and guidance

These published positions were checked on 2026-10-18. Recheck them before capacity changes. Neither registry gives a guaranteed request allowance for your egress address.

| Registry | Published position | Source |
| --- | --- | --- |
| npm | Acceptable Use item 17 treats five million requests in a month-long period from an individual, organisation, or affiliated group as unreasonable. Lower volumes can also impose unreasonable load. This is not a safe quota or a per-IP limit. Contact npm sales for high-volume needs. | [npm Open-Source Terms](https://docs.npmjs.com/policies/open-source-terms/#acceptable-use) |
| PyPI | The API documentation says the edge currently applies no API rate limiting. PyPI can prohibit irresponsible consumers. It asks clients to identify themselves through `User-Agent`, preferably with contact information, and avoid thousands of requests within minutes. It recommends a mirror or cache for services that download many packages. | [PyPI API policies](https://docs.pypi.org/api/#rate-limiting) |
| PyPI | The August 14, 2026 post says PyPI is considering rate limits for its heaviest consumers. It does not announce a numeric allowance. | [PyPI infrastructure update](https://blog.pypi.org/posts/2026-08-14-how-aws-powers-pypi-and-the-psf/) |

npm's organisation-wide wording includes all your deployments and direct npm traffic, even when they use different egress addresses. A shared address also concentrates bursts, so monthly totals alone do not describe your load.

[traffic-listing]: https://github.com/AlexaDeWit/Ecluse/blob/4971c1a6422d1666cdf6a0750783f2b97bc43f78/core/src/Ecluse/Core/Server/Pipeline/Packument.hs
[traffic-artifact]: https://github.com/AlexaDeWit/Ecluse/blob/4971c1a6422d1666cdf6a0750783f2b97bc43f78/core/src/Ecluse/Core/Server/Pipeline/Tarball/Public.hs
[traffic-companion]: https://github.com/AlexaDeWit/Ecluse/blob/4971c1a6422d1666cdf6a0750783f2b97bc43f78/core/src/Ecluse/Core/Server/Pipeline/Sidecar.hs
[traffic-worker]: https://github.com/AlexaDeWit/Ecluse/blob/4971c1a6422d1666cdf6a0750783f2b97bc43f78/src/Ecluse/Composition/Worker.hs
[traffic-copy]: https://github.com/AlexaDeWit/Ecluse/blob/4971c1a6422d1666cdf6a0750783f2b97bc43f78/core/src/Ecluse/Core/Worker/Job.hs
[traffic-cache-config]: https://github.com/AlexaDeWit/Ecluse/blob/4971c1a6422d1666cdf6a0750783f2b97bc43f78/config/default.yaml#L111-L129
[traffic-cache]: https://github.com/AlexaDeWit/Ecluse/blob/4971c1a6422d1666cdf6a0750783f2b97bc43f78/core/src/Ecluse/Core/Server/Cache.hs
[traffic-metadata]: https://github.com/AlexaDeWit/Ecluse/blob/4971c1a6422d1666cdf6a0750783f2b97bc43f78/core/src/Ecluse/Core/Server/Metadata.hs
[traffic-npm]: https://github.com/AlexaDeWit/Ecluse/blob/4971c1a6422d1666cdf6a0750783f2b97bc43f78/core/src/Ecluse/Core/Registry/Npm/Metadata.hs
[traffic-pypi]: https://github.com/AlexaDeWit/Ecluse/blob/4971c1a6422d1666cdf6a0750783f2b97bc43f78/core/src/Ecluse/Core/Registry/PyPI/Metadata.hs

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

CodeArtifact can provide the package repositories, SQS the durable mirror queue, and S3 the advisory and PyPI state stores.

Use EKS Pod Identity, IRSA, or an ECS task role for service credentials. The mirror worker mints its CodeArtifact write token. Client reads still use each caller's token.

The [permission reference](@/docs/permissions.md) lists what each role needs. Creating the resources and attaching those permissions follows your normal platform process.

{{ next(path="@/docs/configure.md", title="Package your configuration") }}
