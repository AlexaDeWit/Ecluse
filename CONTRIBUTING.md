# Contributing

How we work on **Écluse** (package `ecluse`): the contribution *process* and the
repository's requirements. This file is policy; the practical guides live alongside it:

- **Set up & build** ([Getting Started](docs/getting-started.md)): Nix, the `make` loop,
  reproducible builds, and dependency locking.
- **Testing** ([Testing Strategy](docs/testing.md)): the tiers, what gates, and coverage.
- **Code style** ([`STYLE.md`](STYLE.md)); documentation/Haddock ([`HADDOCK.md`](HADDOCK.md)).
- **Design** ([`docs/architecture.md`](docs/architecture.md)).
- **CI, caches & Semgrep internals**, and agent-specific instructions ([`AGENTS.md`](AGENTS.md)).

## Working language

Issues and discussion are in **English**, so the next person with the same problem (and any
future maintainer) can search, find, and help with them. You don't need perfect English: rough English, or your
own language run through a translator like Google Translate, is genuinely welcome, and it
keeps your report findable for the next person with the same problem. If English is a real
barrier, I also read **French** and **Swedish**, so write in one of those and I'll manage;
including a translated version alongside helps everyone else follow along.

Source code, identifiers, comments, and commit messages stay in English.

---

## Automation scripting

Build and CI automation is **Bash**: one language, so there's one thing to read and review.
Scripts live in [`scripts/`](scripts/) (`#!/usr/bin/env bash`, `set -euo pipefail`) and are
invoked from the [`Makefile`](Makefile) or the workflows. The `Makefile` orchestrates; any
non-trivial logic belongs in a `scripts/*.sh` file rather than inline in a workflow `run:`
block, so it stays reviewable, runnable outside CI, and `shellcheck`-clean. `make
lint-scripts` runs `shellcheck` over `scripts/*.sh` in the gate (at `--severity=warning`).
`awk`/`sort` handle structured-data munging, so reach for them before a heavier runtime.

Use another language only when one is genuinely *forced*, and say why in review:

- **Lua** for the pandoc filters in [`web/`](web/), because pandoc's filter API is Lua.
- **Make** and **Nix** are the task orchestrator and the build/derivation language; they're
  not homes for procedural logic.

Introducing a new build-time dependency on Python, Node, or similar needs a strong, stated
reason. "It reads a little cleaner" isn't one.

---

## Coverage

Coverage is reported to **Codecov**, which is the **merged authority**: it sums the per-tier
flag uploads (unit ∪ integration) into one project total. The full strategy, gates, and
Codecov knobs live in [Testing Strategy](docs/testing.md) → "Coverage"; the contributor-facing
commands are:

- **`make coverage`: the canonical command.** Builds *both* gating tiers instrumented
  (HPC, in an isolated `dist-coverage/`), `hpc combine --union`s their `.tix`, and writes the
  combined Codecov JSON to `coverage/combined.json`. This reproduces the **merged total Codecov
  shows**, so a local read agrees with the dashboard. It runs the integration tier, so it
  **needs a running Docker daemon** (the ministack containers; the Nix shell ships the
  toolchain, not the daemon). With no daemon it fails with a clear message pointing at the fast
  path.
- **`make coverage-unit` (≡ `make coverage SUITE=ecluse-unit`): the fast, Docker-free loop.**
  Measures the **unit tier only**. It is a **partial view** Codecov merges with the integration
  tier (the run prints this loudly), so it under-counts every module the integration tier
  exercises (the SQS `MirrorQueue` backend, the worker's real fetch/publish path). Reach for it
  for a quick local number; reach for bare `make coverage` for the honest, Codecov-matching one.
- **`make coverage SUITE=<tier>`: one tier's report.** The per-flag form CI uses: each tier
  (`ecluse-unit`, `ecluse-integration`) writes its own `coverage/<tier>.json`, uploaded under
  its own Codecov flag. Keep this shape: the per-flag uploads depend on it.

**Only the two gating tiers surface coverage.** Codecov's total is `ecluse-unit ∪
ecluse-integration` and **nothing else**: the **E2E and Smoke suites are deliberately
excluded** (they are not built with HPC and upload no flag). That is by design: E2E is a
slow end-to-end smoke of the assembled binary and Smoke hits live third-party registries
off the gate, so neither is a coverage instrument. The practical consequence: **a line
exercised only by E2E or Smoke still reads as uncovered**, both locally and on the
dashboard. So do not reason "the e2e test covers it"; if a path needs coverage, it needs
a **unit or integration** test. (This is the inverse trap of the per-tier partial above:
there a real path looks uncovered because you ran one tier; here it looks uncovered
because the tier that exercises it never counts.)

**Reporting divergence is not a coverage gap.** This combined command exists to kill a
*reporting* confusion (a local single-tier read disagreeing with the merged dashboard). It does
not move real coverage: if the **merged** report still shows a module's error arms red, that is
a genuine uncovered path the tests owe: fix it with a test, not with tooling.

The generators are [`scripts/coverage.sh`](scripts/coverage.sh) (one tier) and
[`scripts/coverage-combined.sh`](scripts/coverage-combined.sh) (the merged view).

---

## Releases, attestations & vulnerability scanning

Écluse ships as a lean, reproducible OCI image built by Nix (`make docker-build`), published
by a tag-triggered workflow that attaches keyless SLSA provenance + SBOM attestations and a
GitHub Release pinning the digest. Image CVEs are scanned report-only (`make scan`, grype over
the SBOM); findings surface both in the **Security tab** (code scanning, alongside Semgrep and
Scorecard) and in a single auto-updating tracking issue. Dependency freshness is kept by Renovate
refreshing `flake.lock` (and bumping the GitHub Actions and Haskell dependencies).

The full operational detail (image contents, the publish/attest chain, Docker Hub token
handling, and the scanning/freshness arms) is in
[Release & Supply-Chain Operations](docs/architecture/release-supply-chain.md). Consumers
verify an image with `gh attestation verify`; the recipe is in the
[README](README.md#verifying-the-image).

---

## AI-assisted contributions

AI-assisted work is welcome, but the bar doesn't change: **you are the author, you have to
understand and be able to explain every line, and the contribution has to be worth more than
the time it takes to review.** Low-effort, unreviewed AI output ("slop") will be closed.

- **Disclose non-trivial AI use.** Editor autocomplete needs no disclosure; AI-generated or
  substantially AI-shaped code, prose, or commits do. Add an `Assisted-by:` git trailer naming
  the tool, e.g. `Assisted-by: <Agent Name> (<Vendor>)`, and mention it in the PR description. This
  records a tool that *helped*; you remain the sole author, so it's **not** `Co-authored-by:`.
- **Verify before you file.** Never open an issue, and especially never a vulnerability
  report, that an AI produced and you haven't reproduced and confirmed yourself (see
  [`SECURITY.md`](SECURITY.md)).

---

## Developer Certificate of Origin (DCO)

Écluse is, and will remain, free and open-source software. Contributions are accepted under
the **[Developer Certificate of Origin](DCO)** (DCO, v1.1), a lightweight, per-commit
affirmation that you have the right to submit your work under the project's
[MIT license](LICENSE). We use the DCO **rather than a Contributor License Agreement on
purpose**: it asks you only to *certify provenance* and grants the project no power to
relicense or close the code, so Écluse stays permanently FOSS, while MIT still lets anyone
adopt it privately.

**Sign off every commit.** `git commit -s` (or `--signoff`) appends a `Signed-off-by` trailer
from your git identity:

```
Signed-off-by: Your Name <you@example.com>
```

By signing off you certify the [DCO](DCO): in short, that you wrote the change (or otherwise
have the right to submit it under the project's license), and that your contribution and
sign-off become a permanent public record.

- **Every commit in a PR** needs a valid `Signed-off-by` matching its author.
- It's **separate from the GPG signature**: `-S` proves *who* committed (authenticity); `-s`
  certifies your *right to contribute* (provenance). Use both, `git commit -S -s`, and it
  coexists with the `Assisted-by:` trailer.
- **Forgot one?** `git commit --amend -s --no-edit` fixes the last commit;
  `git rebase --signoff main` signs off a whole branch.
- **We squash-merge, so sign off _every_ commit.** The squash commit's message is assembled
  from your branch commits' messages, so a `Signed-off-by` reaches `main` only when the commits
  themselves carry it, and the DCO check verifies it per commit regardless. When the PR is
  squashed, keep the `Signed-off-by` line(s) in the final message; don't trim them. The
  [pull-request template](.github/PULL_REQUEST_TEMPLATE.md) repeats this as a reminder, but the
  per-commit trailer is what counts; editing the PR description does not sign your commits.

---

## Repository requirements

- **Workflows stay injection-free.** Never interpolate untrusted `${{ github.event.* }}` /
  `${{ github.head_ref }}` values directly into `run:` shell blocks; pass them via `env:` or
  intermediate files instead.
- **Semgrep ignores require the repo owner's approval.** Don't add `.semgrepignore` entries or
  `nosemgrep` comments unilaterally.
- **Use [Conventional Commits](https://www.conventionalcommits.org/).** Write commit subjects
  as `type(scope): summary`, where `type` is one of `feat`, `fix`, `docs`, `chore`, `ci`,
  `refactor`, `test`, `build`, or `perf` (scope optional). Keep the summary short and
  imperative; put detail in the body.
- **Commits are GPG-signed.** Keep history verifiable.
- **Every commit is signed off (DCO).** Certify the [Developer Certificate of
  Origin](#developer-certificate-of-origin-dco) with a `Signed-off-by` trailer, `git commit -s`.
- **Disclose AI assistance.** Mark non-trivial AI-assisted commits with an `Assisted-by:`
  trailer; see [AI-assisted contributions](#ai-assisted-contributions).
- **Diagrams are Mermaid, not ASCII art** in committed Markdown docs: a fenced ` ```mermaid `
  block, never box-drawing characters. See [`AGENTS.md`](AGENTS.md) → Code Conventions.
