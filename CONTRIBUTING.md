# Contributing

How we work on Écluse (package `ecluse`): the contribution *process* and the
repository's requirements. This file is policy; the practical guides live alongside it:

- **Set up and build** ([Getting Started](docs/getting-started.md)): Nix, the `task` loop,
  reproducible builds, dependency locking.
- **Testing** ([Testing Strategy](docs/testing.md)): the tiers, what gates, and coverage.
- **Code style** ([`STYLE.md`](STYLE.md)); documentation ([`HADDOCK.md`](HADDOCK.md)).
- **Design** ([`docs/architecture.md`](docs/architecture.md)).
- **CI, caches, and Semgrep internals**, and agent instructions ([`AGENTS.md`](AGENTS.md)).

## Working language

Issues and discussion are in **English**, so the next person with the same problem can search and
find them. Rough English, or your own language run through a translator, is genuinely welcome. If
English is a real barrier, I also read **French** and **Swedish**. Source code, identifiers,
comments, and commit messages stay in English.

## Automation scripting

Build and CI automation is **Bash**: one language to read and review. Scripts live in
[`scripts/`](scripts/) (`#!/usr/bin/env bash`, `set -euo pipefail`) and are invoked from
[`Taskfile.yml`](Taskfile.yml) or the workflows. The Taskfile orchestrates; complexity belongs in the
shell scripts, not a multiline YAML `cmds:` block, so it stays reviewable, runnable outside CI, and
`shellcheck`-clean. `task lint-scripts` runs `shellcheck` over `scripts/*.sh` in the gate at
`--severity=warning`. Reach for `awk`/`sort` before a heavier runtime.

Use another language only when one is forced, and say why in review: **Lua** for the pandoc filters in
[`web/`](web/), because pandoc's filter API is Lua. A new build-time dependency on Python, Node, or
similar needs a strong, stated reason; "it reads a little cleaner" isn't one.

## Coverage

Coverage is reported to **Codecov**, the merged authority over the per-tier flag uploads. The two
contributor commands are **`task coverage`** (combined, matches the dashboard, needs Docker) and
**`task coverage-unit`** (fast, unit-only, Docker-free, a partial view); the full strategy and
Codecov knobs are in [Testing Strategy](docs/testing.md) → "Coverage".

## Releases, attestations, and vulnerability scanning

Écluse ships as a reproducible OCI image built by Nix and published on GitHub Releases and Docker
Hub. Image CVEs are scanned report-only, and Renovate keeps dependency freshness by refreshing
`flake.lock` and bumping the Actions and Haskell dependencies. The full operational detail (image
contents, the publish/attest chain, token handling, scanning) is in
[Release and Supply-Chain Operations](docs/architecture/release-supply-chain.md); consumers verify an
image with `gh attestation verify`, per the [README](README.md#verifying-the-image).

## AI-assisted contributions

AI-assisted work is welcome, but the bar doesn't change: **you are the author, you have to
understand and be able to explain every line, and the contribution has to be worth more than
the time it takes to review.** Low-effort, unreviewed AI output ("slop") will be closed.

- **Disclose non-trivial AI use.** Editor autocomplete needs no disclosure; AI-generated or
  substantially AI-shaped code, prose, or commits do. Add an `Assisted-by:` git trailer naming the
  tool, e.g. `Assisted-by: <Agent Name> (<Vendor>)`, and mention it in the PR. This records a tool
  that *helped*: you remain the sole author, so it is **not** `Co-authored-by:`.
- **Verify before you file.** Never open an issue, and especially never a vulnerability report, that
  an AI produced and you haven't reproduced and confirmed yourself (see [`SECURITY.md`](SECURITY.md)).

## Developer Certificate of Origin (DCO)

Écluse is, and will remain, free and open-source software. Contributions are accepted under the
**[Developer Certificate of Origin](DCO)** (DCO, v1.1), a lightweight per-commit affirmation that you
have the right to submit your work under the project's [MIT licence](LICENSE). We use the DCO rather
than a Contributor Licence Agreement on purpose: it asks you only to certify provenance and grants
the project no power to relicense or close the code, so Écluse stays permanently FOSS.

**Sign off every commit.** `git commit -s` (or `--signoff`) appends a `Signed-off-by` trailer from
your git identity, certifying that you wrote the change (or have the right to submit it) and that it
becomes a permanent public record:

```
Signed-off-by: Your Name <you@example.com>
```

- **Every commit in a PR** needs a `Signed-off-by` matching its author.
- It is **separate from the GPG signature**: `-S` proves *who* committed; `-s` certifies your *right
  to contribute*. Use both, `git commit -S -s`.
- **We squash-merge, so sign off every commit.** The squash message is assembled from the branch
  commits and the DCO check verifies each one, so a `Signed-off-by` reaches `main` only when the
  commits carry it. Editing the PR description does not sign your commits.
- **Forgot one?** `git commit --amend -s --no-edit` fixes the last commit;
  `git rebase --signoff main` signs off a whole branch.

## Repository requirements

- **Use [Conventional Commits](https://www.conventionalcommits.org/).** Subjects are
  `type(scope): summary`, `type` one of `feat`, `fix`, `docs`, `chore`, `ci`, `refactor`, `test`,
  `build`, `perf` (scope optional). Keep the summary short and imperative.
- **Commits are GPG-signed** and **DCO signed off** (see above); **non-trivial AI
  assistance is disclosed** with an `Assisted-by:` trailer.
- **Every Haskell source file carries an SPDX licence header.** `task spdx-fix` stamps it,
  `task lint-spdx` gates it in `static-checks` ([STYLE.md](STYLE.md#14-licence-headers)).
- **Pin every GitHub Action to a full commit SHA** (never a tag), with the version in a
  trailing comment; Renovate bumps them. The shared toolchain setup (install Nix, restore
  the caches) lives once in the `setup-toolchain` composite action, and CI jobs enter the
  lean `nix develop .#ci` shell.
- **Caches are restore-only on PRs and written solely by `main`'s runs**, keyed on the dependency
  plan (`flake.lock`/`cabal.project`/`cabal.project.freeze`). The `stan` and `weeder` jobs
  deliberately build their `.hie` variant into an uncached `dist-analysis`, so a restored builddir
  can't hand them orphaned `.hie` for deleted modules; see those jobs' comments in
  [`ci.yml`](.github/workflows/ci.yml).
- **Workflows stay injection-free.** Never interpolate untrusted `${{ github.event.* }}` /
  `${{ github.head_ref }}` into `run:` blocks; pass them via `env:` or intermediate files.
- **Semgrep ignores require the repo owner's approval.** Don't add `.semgrepignore` entries
  or `nosemgrep` comments unilaterally.
- **Diagrams are Mermaid, not ASCII art**: a fenced ` ```mermaid ` block, never box-drawing
  characters.
