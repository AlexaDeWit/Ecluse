# Alternatives

_Other tools in this space, and when they might suit you better._

Écluse is one answer to a problem many people are working on, and nowhere near the only good
one. Its core idea, holding fresh packages behind a delay and applying a policy at a
chokepoint, has been reached independently by a range of projects and vendors, several more
mature than Écluse is today.

This page is a genuine, good-faith guide. Each entry says what a tool offers and when to reach
for it; if one fits you better than Écluse, use it. For _why_ Écluse exists and how it
differs, see [`MOTIVATION.md`](MOTIVATION.md).

## Self-hosted filtering proxies

A service you run and point your clients at, which filters or gates packages on the way
through. The closest cousins to Écluse.

- **[Verdaccio](https://verdaccio.org/)**: a lightweight, widely-adopted self-hosted npm
  registry that proxies and caches upstreams. It supports multiple uplinks with ordered
  fallback, and since v6.4 ships a bundled package filter: a minimum-age gate (`minAgeDays`),
  version blocklists, per-package allow exceptions, and replacement of a blocked version with
  an older one. That filter is first-party now, not only a third-party plugin, making
  Verdaccio a genuine open-source npm repository-firewall configuration for an npm-only team.
  _Reach for it_ when you want to run your own npm registry and a metadata age gate is enough.
  Écluse differs in that it merges versions across upstreams semantically rather than falling
  back in order; drives denials from OSV advisories with an immediate remediation fast lane,
  so a fix ships the moment it lands; swaps the advisory index atomically; and mirrors on
  demand into a registry you already run instead of caching into its own storage. Verdaccio's
  filter is also metadata-only and npm-only.

## Package-manager and bot cooldowns

Per-project, per-consumer controls that need no infrastructure: right when you can guarantee
consistent configuration across everyone who installs.

- **npm's minimum-release-age cooldown, pnpm's [`minimumReleaseAge`](https://pnpm.io/settings), and equivalents in bun and
  uv**: an install-time delay set in the project or per consumer; newer package-manager
  releases are moving toward stronger install-time defaults. _Reach for it_ for immediate,
  zero-infrastructure protection of a project you control.
- **[Renovate](https://docs.renovatebot.com/key-concepts/minimum-release-age/)** and **[Dependabot](https://docs.github.com/en/code-security/dependabot)** cooldowns: delay _update_ proposals until a version has aged.
  _Reach for it_ when updates already flow through a bot and you want a cooldown with no new
  moving parts.
- **[SafeDep PMG](https://github.com/safedep/pmg)** (Package Manager Guard): an open-source
  local guard that wraps your package-manager commands (a shell shim or CI step), enforcing
  cooldowns alongside threat-intelligence checks and OS-level sandboxing across the npm and
  Python families, and explicitly protecting AI coding agents. _Reach for it_ for
  per-developer and per-agent defence-in-depth with no central service to run.

## Commercial platforms and hosted services

Turnkey, vendor-supported central enforcement without building or maintaining it yourself.

- **JFrog Artifactory** with **[Curation](https://jfrog.com/curation/)**: a full artifact
  platform whose curation layer enforces policy at the proxy, including age-based ("immature
  package") gating and malicious-package blocking, which JFrog describes informally as a
  firewall for open-source packages. _Reach for it_ for one vendor-supported platform across
  many ecosystems, with enterprise support.
- **Sonatype** Nexus Repository with **[Repository Firewall](https://www.sonatype.com/products/sonatype-repository-firewall)**: quarantines suspicious components
  at the proxy using behavioural and metadata signals, releasing safe ones automatically.
  _Reach for it_ for mature behavioural detection backed by a large intelligence database and
  a widely-deployed repository manager.
- **[Harness](https://www.harness.io/) Dependency Firewall**: a commercial, centrally-managed
  control that blocks risky open-source dependencies at the proxy. _Reach for it_ for central
  enforcement inside the Harness platform.
- **[StepSecurity Secure Registry](https://www.stepsecurity.io/)**: an authenticated proxy you
  point CI, developers, and artifact managers at, enforcing cooldowns and malicious-package
  blocking. _Reach for it_ for a managed central chokepoint you don't operate yourself.
- **[Cloudsmith](https://cloudsmith.com/)**: a hosted, multi-format registry with a policy
  engine that can quarantine or deny by version, metadata, and other criteria. _Reach for it_
  for a managed registry with policy controls when you'd rather not self-host.
- **[Socket](https://socket.dev/)**: behavioural analysis of packages (install scripts, network
  access, and more), surfaced in CI and pull requests. Socket also offers Registry Firewall, a
  registry-protocol proxy that applies metadata filtering across roughly eight ecosystems.
  _Reach for it_ for deep per-package behavioural signals plus a proxy-level filter.
- **pkgwarden**: a hosted, commercial curated registry that quarantines new versions for 14
  days and filters on CVE data, across npm and PyPI. _Reach for it_ for a managed curated
  registry with a fixed quarantine and CVE filtering.
- **Managed cloud registries**, **[AWS CodeArtifact](https://aws.amazon.com/codeartifact/)** and **[Google Artifact Registry](https://cloud.google.com/artifact-registry)**, give
  you a chokepoint, storage, authentication, and dependency-confusion controls, but no
  freshness policy of their own, so pair them with a cooldown above. _Reach for it_ as the
  backing store you likely already run; Écluse delegates storage to exactly these rather than
  replacing them.

## Complementary tools (not substitutes)

These address a different part of the problem and pair with any of the above.

- **Provenance and attestations**: [npm provenance](https://docs.npmjs.com/generating-provenance-statements/) (Sigstore / SLSA) and `npm audit signatures`
  attest _where and how_ a package was built. _Use alongside_ a cooldown for cryptographic
  build-origin verification.
- **Malicious-package scanners**: e.g. **[GuardDog](https://github.com/DataDog/guarddog)** and
  **[SafeDep vet](https://github.com/safedep/vet)**, heuristics that flag malicious packages.
  _Use alongside_ a delay to add detection a delay alone won't provide.

## Where Écluse fits

Écluse aims at one corner of these trade-offs: an enforced, central chokepoint that's open and
self-hostable, composes in front of the managed registry you already run rather than replacing
it or hosting packages itself, and applies a deny-by-default freshness policy consistently, so
a malicious-package disclosure is answered by comparing timelines rather than auditing logs.
npm is the first supported ecosystem; the core is registry-agnostic, with PyPI on the
roadmap. It's also early and unproven (see [`MOTIVATION.md`](MOTIVATION.md) → _What Écluse
is not_).

If a different point on these trade-offs serves you better, use one of the tools above.
