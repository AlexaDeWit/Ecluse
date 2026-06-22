# Governance

Écluse is, at this time, **entirely the project of a single maintainer**,
[Alexandra de Wit (@AlexaDeWit)](https://github.com/AlexaDeWit). Governance is
therefore deliberately simple: a **benevolent-dictator (BDFL) model**. This
document records how decisions are made today and how that can change — not an
aspirational structure the project does not yet have.

## Roles

- **Maintainer** — currently one person, @AlexaDeWit. Holds final say on design,
  scope, review, merges, releases, and security response, and owns the
  repository, the published image/package identity, and the signing keys.
- **Contributor** — anyone who submits a change. Contributions are welcome under
  the [Developer Certificate of Origin](CONTRIBUTING.md#developer-certificate-of-origin-dco)
  and the [Code of Conduct](CODE_OF_CONDUCT.md). Contributors carry no standing
  obligations and no implied authority over the project's direction.

## How decisions are made

The maintainer decides. In practice: the architecture documents
([`docs/architecture.md`](docs/architecture.md) and the `docs/architecture/` set)
are the design authority; proposals are raised as issues or pull requests; a
change merges when the maintainer approves it and the CI `gate` is green.
Disagreements are resolved by the maintainer, who will explain the reasoning.
There is no voting body and no second approver today — recorded as a known risk
under *Continuity*.

## Becoming a maintainer

The single-maintainer state is a stage, not a ceiling. A contributor who sustains
high-quality, well-reviewed work and shows sound judgement on scope and security
may be invited by the maintainer to become a co-maintainer, gaining review and
merge authority. This is the intended path to a healthier bus factor.

## Continuity

Écluse is **MIT-licensed**: should the maintainer become unavailable, anyone may
fork and continue the project without permission. Reducing reliance on one
person — adding a co-maintainer and distributing access to the repository,
release pipeline, and signing keys — is an explicit goal as the project matures.

## Code of Conduct

Conduct reports are handled by the maintainer; see
[`CODE_OF_CONDUCT.md`](CODE_OF_CONDUCT.md).

## Changing this document

Governance changes are made by the maintainer, by pull request, like any other
change to the repository.
