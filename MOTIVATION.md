# Why Écluse?

Écluse gives a platform, DevOps, or IT team one package policy across an organisation's
developer machines and CI. It sits in front of public registries and works with the package
managers and private repositories the organisation already uses.

The [setup guide](https://ecluse-proxy.com/docs/setup/) covers deployment.
The [architecture documents](docs/architecture.md) explain the implementation choices.
[Alternatives](ALTERNATIVES.md) compares other places to apply package policy.

<a id="the-blast-radius-of-a-bad-publish"></a>
## Reduce exposure to new public releases

A compromised public package can reach many builds before maintainers or registry operators
remove it. Écluse's default minimum age holds a new public version for seven days.
That gives the ecosystem time to discover a bad release before your builds adopt it.

<a id="the-bet-resilience-not-detection"></a>
Age is not a malware verdict. A harmful version can remain available beyond the waiting period.
Advisories, explicit denies, and other configured rules provide additional decisions.
Qualifying exact security fixes can skip the wait when the advisory evidence supports that exception.

The delay prevents exposure to a version removed before it becomes eligible only when the
request follows that policy. Exceptions, trusted private copies, and routes around the proxy
must be considered separately. An incident still requires investigation of the deployment and
its actual requests.

<a id="the-bar-a-chokepoint-you-cant-step-around"></a>
## Enforce the policy at the network

Package-manager cooldowns help when you can keep client configuration consistent.
An organisation can also have many toolchains, project-level settings, containers, and CI jobs.
Those clients can use different settings even on the same machine.

Écluse moves the common policy into a service the operating team controls.
Clients use its registry address. Network rules prevent protected builds from fetching packages
directly from public registry and artifact hosts.

The proxy cannot enforce that network boundary itself.
[Client egress restrictions](https://ecluse-proxy.com/docs/deployment/#locking-down-ci-egress)
are part of the deployment, alongside access control on the proxy's own endpoints.

<a id="why-you-cant-naively-build-it-either"></a>
## Serve admitted packages before mirroring finishes

A complete mirror of every eligible public package can require substantial storage and transfer.
Predicting every package a developer will request is another operational burden.

Écluse instead copies admitted versions on demand. A public artifact request passes policy,
streams to the client, and can enqueue background mirroring. The worker rechecks current
policy before copying.

The next request is not guaranteed to find a completed mirror. PyPI copies can include multiple
wheels, a source distribution, and metadata companions. Écluse requires a complete managed
version or a validated public alternative before it serves that view.

See the rendered [download](https://ecluse-proxy.com/docs/threat-model/#artifact-downloads)
and [mirror completion](https://ecluse-proxy.com/docs/threat-model/#mirror-writes-and-completion)
diagrams for those paths.

<a id="the-design-thats-left-three-registries"></a>
## Keep private trust and public admission separate

Private packages do not need the public release-age policy. They use a trusted read path with
caller authorisation and applicable integrity checks. Declared first-party names never fall back
to a public registry.

For a deployment with mirroring and internal publication, the recommended topology separates:

- The first-party repository, which receives the organisation's publications.
- The mirror repository, which receives admitted public versions.
- The private read repository, which combines those two sources for clients.

The private read repository must not have a direct or transitive public connection.
Otherwise ungated public packages can enter through a trusted source.
The [registry topology](https://ecluse-proxy.com/docs/threat-model/#registry-topology-and-trust)
shows the relationships and trust boundaries.

A trusted copy does not repeat public admission on every read. Removing it requires a named
revocation deny and authorised cleanup of every eligible serving copy.
The cleanup service requires consent and applies a deletion cap.
It does not recall installed packages or client caches.

<a id="what-ecluse-is-not"></a>
## Keep the responsibilities explicit

Écluse applies package policy. Registries store archives and control private package access.
The operating platform controls network reachability and service identities.
Advisory sources provide evidence, with freshness and availability limits that affect admission.

Écluse does not inspect package code to identify malware. Its supported assumptions and residual
risks belong to the [rendered threat model](https://ecluse-proxy.com/docs/threat-model/).
