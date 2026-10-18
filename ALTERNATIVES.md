# Alternatives

Package policy can run in the package manager, an update bot, a registry, or a shared proxy.
Compare where each control applies and who must operate it.

## Where the policy runs

| Approach | Example | Operating responsibility |
| --- | --- | --- |
| Package-manager release age | [npm `min-release-age`](https://docs.npmjs.com/cli/v12/commands/npm-install/#min-release-age) | Configure the clients and projects that install dependencies. Check overrides and exceptions across the organisation. |
| Update-proposal delay | [Renovate `minimumReleaseAge`](https://docs.renovatebot.com/key-concepts/minimum-release-age/) | Configure the bot's update workflow. Its proposal policy does not replace the controls on every installation path. |
| Filtering inside a registry | [Verdaccio package filter](https://github.com/verdaccio/verdaccio/blob/master/packages/plugins/package-filter/README.md) | Operate the npm registry and its filter configuration. The filter supports release-age limits, version blocks, and explicit exceptions. |
| Registry-protocol firewall | [Socket Registry Firewall](https://docs.socket.dev/docs/socket-firewall-enterprise-registry-mode-configuration-reference) | Configure the registry routes and the policies exposed by that product. Verify supported protocols and deployment requirements against its current documentation. |
| A separate self-hosted policy proxy | [Écluse](https://ecluse-proxy.com/docs/understand/) | A platform, DevOps, or IT team operates the proxy, network boundary, policy, and any supporting stores. |

These controls can coexist. An organisation can delay automated update proposals and still
apply policy when a package manager requests a version.

## What to compare

- **Enforcement:** which requests cross the control, and which client or network settings can bypass it?
- **Policy:** what happens to new versions, known-malicious releases, exact security fixes, and explicit exceptions?
- **Private packages:** which system authorises callers and keeps first-party names separate from public packages?
- **Retention:** where do admitted packages live, and what remains available during a public outage?
- **Revocation:** how are denied copies removed or blocked, including downstream caches?
- **Operation:** who runs the service, updates its evidence, investigates failures, and maintains credentials?

## Other products and controls

The following links provide additional starting points for an evaluation. Use each product's
current documentation for protocol coverage, deployment modes, licensing, and policy behaviour.

| Area | Product documentation |
| --- | --- |
| Package-manager and local controls | [pnpm settings](https://pnpm.io/settings), [SafeDep PMG](https://github.com/safedep/pmg) |
| Update automation | [Dependabot](https://docs.github.com/en/code-security/dependabot) |
| Registry and supply-chain products | [JFrog Curation](https://jfrog.com/curation/), [Sonatype Repository Firewall](https://www.sonatype.com/products/sonatype-repository-firewall), [Harness](https://www.harness.io/), [StepSecurity](https://www.stepsecurity.io/), [Cloudsmith](https://cloudsmith.com/) |
| Package storage and access | [AWS CodeArtifact](https://aws.amazon.com/codeartifact/), [Google Artifact Registry](https://cloud.google.com/artifact-registry) |
| Provenance | [npm provenance](https://docs.npmjs.com/generating-provenance-statements/) |
| Package analysis | [GuardDog](https://github.com/DataDog/guarddog), [SafeDep vet](https://github.com/safedep/vet) |

## Where Écluse fits

Écluse provides shared admission policy for npm and PyPI while registries keep the package
archives. It can mirror admitted public versions into a configured registry and relay
first-party publications to a separate target.

With an advisory source, qualifying exact security fixes can pass before the minimum age.
That depends on publication and synchronisation of advisory evidence, the remaining-advisory
conditions, and higher-ranked rules. It is not an unconditional immediate path for every fix.

The [motivation](MOTIVATION.md) explains that design. The
[security model](https://ecluse-proxy.com/docs/security/) identifies what the deployment must
provide and links to the full threat model.
