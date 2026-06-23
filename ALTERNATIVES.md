# Alternatives

*Other tools in this space, and when they might suit you better.*

Écluse is one answer to a problem a great many people are working on, and it is far from the
only good one. The idea at its core — holding fresh packages behind a delay and applying a
policy at a chokepoint — has been reached independently by a number of projects and vendors,
several of them more mature than Écluse is today.

This page is a genuine, good-faith guide: each entry describes what a tool *offers* and when
you might reach for it, in a positive light. If one of these fits you better than Écluse, that
is a good outcome — use it. (For *why* Écluse exists and how it differs, see
[`MOTIVATION.md`](MOTIVATION.md).)

## Self-hosted filtering proxies

The closest cousins to Écluse: a service you run and point your clients at, which filters or
gates packages on the way through.

- **[npm-registry-firewall](https://github.com/antongolub/npm-registry-firewall)** — a
  lightweight, non-hosting npm proxy that filters on the fly: age-based quarantine plus
  allow/deny rules by name, org, version, licence, publish date, and author. MIT-licensed and
  npm-focused. *Reach for it* when you want the simplest self-hosted filtering proxy for npm
  and value something small enough to read end to end. (Its most recent release is from 2023 —
  worth confirming it still suits your maintenance needs.)
- **[Verdaccio](https://verdaccio.org/)** with a package-filter plugin — a popular, lightweight
  self-hosted npm registry that proxies and caches upstreams, with filter plugins that can hide
  versions younger than a configured age (and community security filters). *Reach for it* when
  you also want a full private registry of your own, a mature plugin ecosystem, and broad
  adoption, with the freshness filter riding along.

## Package-manager and bot cooldowns

Per-project, per-consumer controls that need no infrastructure — the right mechanism when you
can ensure consistent configuration across everyone who installs.

- **npm's minimum-release-age cooldown, pnpm's [`minimumReleaseAge`](https://pnpm.io/settings), and equivalents in bun and
  uv** — an install-time delay configured in the project (or per consumer); newer
  package-manager releases are moving toward stronger install-time defaults. *Reach for it* for
  immediate, zero-infrastructure protection of a project you control.
- **[Renovate](https://docs.renovatebot.com/key-concepts/minimum-release-age/)** and **[Dependabot](https://docs.github.com/en/code-security/dependabot)** cooldowns — delay *update* proposals until a version has aged.
  *Reach for it* when your dependency updates already flow through a bot and you want a cooldown
  with no new moving parts.
- **[SafeDep PMG](https://github.com/safedep/pmg)** (Package Manager Guard) — an open-source
  local guard that transparently wraps your package-manager commands (a shell shim, or a CI
  step), enforcing cooldown windows alongside threat-intelligence checks and OS-level
  sandboxing, across the npm and Python families — and explicitly protecting AI coding agents.
  *Reach for it* for strong per-developer and per-agent defence-in-depth on workstations, with
  no central service to run.

## Commercial platforms and hosted services

Turnkey, vendor-supported options — central enforcement without building or maintaining it
yourself.

- **JFrog Artifactory** with **[Curation](https://jfrog.com/curation/)** — a full artifact platform whose curation layer
  enforces policy at the proxy, including age-based ("immature package") gating and
  malicious-package blocking. *Reach for it* when you want one vendor-supported platform across
  many ecosystems, with enterprise support.
- **Sonatype** Nexus Repository with **[Repository Firewall](https://www.sonatype.com/products/sonatype-repository-firewall)** — quarantines suspicious components
  at the proxy using behavioural and metadata signals, releasing safe ones automatically.
  *Reach for it* for mature behavioural detection backed by a large intelligence database,
  integrated with a widely-deployed repository manager.
- **[StepSecurity Secure Registry](https://www.stepsecurity.io/)** — an authenticated proxy you
  point CI, developers, and artifact managers at, enforcing cooldowns and malicious-package
  blocking. *Reach for it* for a managed, central install-time chokepoint you don't have to
  operate yourself.
- **[Cloudsmith](https://cloudsmith.com/)** — a hosted, multi-format registry with a policy
  engine that can quarantine or deny by version, metadata, and other criteria. *Reach for it*
  for a managed registry with policy controls when you would rather not self-host.
- **[Socket](https://socket.dev/)** — behavioural analysis of packages (install scripts, network
  access, and more), surfaced in CI and pull requests and via a registry proxy. *Reach for it*
  for deep per-package behavioural signals and developer-facing reviews.
- **Managed cloud registries** — **[AWS CodeArtifact](https://aws.amazon.com/codeartifact/)** and **[Google Artifact Registry](https://cloud.google.com/artifact-registry)** — give
  you a chokepoint, storage, authentication, and dependency-confusion controls. They have no
  freshness policy of their own, so pair them with one of the cooldown approaches above. *Reach
  for it* as the backing store you most likely already run — and note that Écluse is designed to
  delegate storage to exactly these rather than replace them.

## Complementary tools (not substitutes)

These address a different part of the problem and pair well with any of the above.

- **Provenance and attestations** — [npm provenance](https://docs.npmjs.com/generating-provenance-statements/) (Sigstore / SLSA) and `npm audit signatures`
  attest *where and how* a package was built. *Use alongside* a cooldown for cryptographic
  build-origin verification.
- **Malicious-package scanners** — e.g. **[GuardDog](https://github.com/DataDog/guarddog)** and
  **[SafeDep vet](https://github.com/safedep/vet)** — run heuristics to flag malicious packages.
  *Use alongside* a delay to add detection that a delay alone won't provide.

## Where Écluse fits

Écluse aims at one particular corner of these trade-offs: an **enforced, central** chokepoint
that is **open and self-hostable**, **composes in front of the managed registry you already
run** (rather than replacing it or hosting packages itself), and applies a **deny-by-default
freshness policy** consistently — with the operational goal that a malicious-package disclosure
can be answered by comparing timelines rather than auditing logs. It is also, today, **early and
unproven** (see [`MOTIVATION.md`](MOTIVATION.md) → *What Écluse is not*).

If a different point on these trade-offs serves you better, one of the tools above is the right
choice — and that is exactly the outcome this page hopes to help with.
