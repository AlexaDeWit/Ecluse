# Contributing

How we work on **Écluse** (package `ecluse`): the contribution *process* and the
repository's requirements. This file is policy; the practical guides live
alongside it:

- **Set up & build** — [Getting Started](docs/getting-started.md): Nix, the `make`
  loop, reproducible builds, and dependency locking.
- **Testing** — [Testing Strategy](docs/testing.md): the tiers, what gates, and coverage.
- **Code style** — [`STYLE.md`](STYLE.md); documentation/Haddock — [`HADDOCK.md`](HADDOCK.md).
- **Design** — [`docs/architecture.md`](docs/architecture.md).
- **CI, caches & Semgrep internals**, and agent-specific instructions — [`AGENTS.md`](AGENTS.md).

## Working language

Issues and discussion are in **English**, so the whole community — and any future
maintainer — can search, find, and help with them. You don't need perfect English:
rough English, or your own language run through a translator like Google Translate,
is genuinely welcome, and it keeps your report findable for the next person with the
same problem. If English is a real barrier, the maintainer also reads **French** and
**Swedish** — write in one of those and they'll manage; including a translated version
alongside helps everyone else follow along.

Source code, identifiers, comments, and commit messages stay in English.

---

## Releases, attestations & vulnerability scanning

Écluse ships as a lean, reproducible OCI image built by Nix (`make docker-build`),
published by a tag-triggered workflow that attaches keyless SLSA provenance + SBOM
attestations and a GitHub Release pinning the digest. Image CVEs are scanned
report-only (`make scan` — grype over the SBOM) and dependency freshness is kept
by Renovate refreshing `flake.lock` (and bumping the GitHub Actions and Haskell
dependencies).

The full operational detail — image contents, the publish/attest chain, Docker
Hub token handling, and the scanning/freshness arms — is in
[Release & Supply-Chain Operations](docs/architecture/release-supply-chain.md).
Consumers verify an image with `gh attestation verify`; the recipe is in the
[README](README.md#verifying-the-image).

---

## AI-assisted contributions

AI-assisted work is welcome, but the bar does not change: **you are the author,
you must understand and be able to explain every line, and the contribution must
be worth more than the time it takes to review.** Low-effort, unreviewed AI
output ("slop") will be closed.

- **Disclose non-trivial AI use.** Editor autocomplete needs no disclosure;
  AI-generated or substantially AI-shaped code, prose, or commits do. Add an
  `Assisted-by:` git trailer naming the tool — e.g.
  `Assisted-by: Claude (Anthropic)` — and mention it in the PR description. This
  records a tool that *helped*; you remain the sole author, so it is **not**
  `Co-authored-by:`.
- **Verify before you file.** Never open an issue — and especially never a
  vulnerability report — that an AI produced and you have not reproduced and
  confirmed yourself (see [`SECURITY.md`](SECURITY.md)).

---

## Developer Certificate of Origin (DCO)

Écluse is, and will remain, free and open-source software. Contributions are
accepted under the **[Developer Certificate of Origin](DCO)** (DCO, v1.1) — a
lightweight, per-commit affirmation that you have the right to submit your work
under the project's [MIT license](LICENSE). We use the DCO **rather than a
Contributor License Agreement on purpose**: it asks you only to *certify
provenance* and grants the project no power to relicense or close the code, so
Écluse stays permanently FOSS — while MIT still lets anyone adopt it privately.

**Sign off every commit.** `git commit -s` (or `--signoff`) appends a
`Signed-off-by` trailer from your git identity:

```
Signed-off-by: Your Name <you@example.com>
```

By signing off you certify the [DCO](DCO): in short, that you wrote the change —
or otherwise have the right to submit it under the project's license — and that
your contribution and sign-off become a permanent public record.

- **Every commit in a PR** needs a valid `Signed-off-by` matching its author.
- It is **separate from the GPG signature**: `-S` proves *who* committed
  (authenticity); `-s` certifies your *right to contribute* (provenance). Use
  both — `git commit -S -s` — and it coexists with the `Assisted-by:` trailer.
- **Forgot one?** `git commit --amend -s --no-edit` fixes the last commit;
  `git rebase --signoff main` signs off a whole branch.
- **We squash-merge, so sign off _every_ commit.** The squash commit's message
  is assembled from your branch commits' messages, so a `Signed-off-by` reaches
  `main` only when the commits themselves carry it — and the DCO check verifies
  it per commit regardless. When the PR is squashed, keep the `Signed-off-by`
  line(s) in the final message; don't trim them. The
  [pull-request template](.github/PULL_REQUEST_TEMPLATE.md) repeats this as a
  reminder, but the per-commit trailer is what counts — editing the PR
  description does not sign your commits.

---

## Repository requirements

- **Workflows stay injection-free.** Never interpolate untrusted
  `${{ github.event.* }}` / `${{ github.head_ref }}` values directly into `run:`
  shell blocks; pass them via `env:` or intermediate files instead.
- **Semgrep ignores require the repo owner's approval.** Do not add
  `.semgrepignore` entries or `nosemgrep` comments unilaterally.
- **Use [Conventional Commits](https://www.conventionalcommits.org/).** Write
  commit subjects as `type(scope): summary`, where `type` is one of `feat`,
  `fix`, `docs`, `chore`, `ci`, `refactor`, `test`, `build`, or `perf` (scope
  optional). Keep the summary short and imperative; put detail in the body.
- **Commits are GPG-signed.** Keep history verifiable.
- **Every commit is signed off (DCO).** Certify the [Developer Certificate of
  Origin](#developer-certificate-of-origin-dco) with a `Signed-off-by` trailer —
  `git commit -s`.
- **Disclose AI assistance.** Mark non-trivial AI-assisted commits with an
  `Assisted-by:` trailer — see [AI-assisted contributions](#ai-assisted-contributions).
