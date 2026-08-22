# Governance

How decisions on Écluse get made today, and how that can change. This is the current state,
not an aspirational structure the project doesn't yet have.

Écluse is, right now, entirely the project of a single maintainer, me,
[Alexandra de Wit (@AlexaDeWit)](https://github.com/AlexaDeWit). So governance is
deliberately simple: a **benevolent-dictator (BDFL) model**.

## Roles

- **Maintainer**: currently one person, me. Holds final say on design, scope, review, merges,
  releases, and security response. Owns the repository, the published image/package identity,
  and the signing keys.
- **Contributor**: anyone who submits a change. Contributions are welcome under the
  [Developer Certificate of Origin](CONTRIBUTING.md#developer-certificate-of-origin-dco) and
  the [Code of Conduct](CODE_OF_CONDUCT.md). Contributors carry no standing obligations and no
  implied authority over the project's direction.

## How decisions are made

I decide. The architecture documents ([`docs/architecture.md`](docs/architecture.md) and the
`docs/architecture/` set) are the design authority. Contributors raise proposals as issues or
pull requests. A change merges when I approve it and the CI `gate` passes. I resolve
disagreements, and I'll explain the reasoning. There's no voting body and no second approver
today, which I've recorded as a known risk under *Continuity*.

## Becoming a maintainer

The single-maintainer state is a stage, not a ceiling. I may invite a contributor to become a
co-maintainer once they sustain high-quality, well-reviewed work and show sound judgement on
scope and security. A co-maintainer gains review and merge authority. This is the intended
path to a healthier bus factor.

## Continuity

Écluse is MIT-licensed: if I become unavailable, anyone may fork and continue the project
without permission. Reducing the project's reliance on one person is an explicit goal as the
project matures. That means adding a co-maintainer and distributing access to the repository,
the release pipeline, and the signing keys.
