# Contributing

How we work on Écluse (package `ecluse`): the contribution process and the repository's
requirements. This file is policy. The practical guides live alongside it:

- **Set up and build**: [Getting Started](docs/getting-started.md).
- **Testing and CI**: [Testing Strategy](docs/testing.md) covers the tiers, what gates, and
  coverage.
- **Code style**: [`docs/style.md`](docs/style.md). Documentation:
  [`docs/haddock.md`](docs/haddock.md).
- **Design**: [`docs/architecture.md`](docs/architecture.md).
- **Agent instructions**: [`AGENTS.md`](AGENTS.md).

## Working language

Write issues and discussion in **English**, so the next person with the same problem can find
them. Rough English is welcome, and so is your own language run through a translator. If English
is a real barrier, I also read **French** and **Swedish**. Source code, identifiers, comments, and
commit messages stay in English.

## Automation scripting

Build and CI automation is **Bash**: one language to read and review. Scripts live in
[`scripts/`](scripts/) with `#!/usr/bin/env bash` and `set -euo pipefail`. The
[`Taskfile.yml`](Taskfile.yml) and the workflows only invoke them. Keep logic in the scripts, not
in a multiline YAML block, so it stays reviewable and runs outside CI. Scripts must pass
`shellcheck` (`task lint-scripts`). Reach for `awk` or `sort` before a heavier runtime.

Use another language only when the tool forces it, and say why in review. The pandoc filters in
[`web/`](web/) are Lua, because pandoc's filter API is Lua. A new build-time dependency on
Python, Node, or similar needs a strong, stated reason. "It reads a little cleaner" is not one.

## Releases

Releases are maintainer territory. The procedure lives in
[Release and supply-chain operations](docs/architecture/release-supply-chain.md). Consumers
verify an image per the [README](README.md#verifying-the-image).

## AI-assisted contributions

AI-assisted work is welcome, but the bar does not change. **You are the author. You must
understand and be able to explain every line. The contribution must be worth more than the time
it takes to review.** We close low-effort, unreviewed AI output ("slop").

- **Disclose non-trivial AI use**. Editor autocomplete needs no disclosure. AI-generated or
  substantially AI-shaped code, prose, or commits do. Add an `Assisted-by:` git trailer that names
  the tool, for example `Assisted-by: <Agent Name> (<Vendor>)`, and mention it in the PR. The
  trailer records a tool that helped. You remain the sole author, so it is **not**
  `Co-authored-by:`.
- **Verify before you file**. Never open an issue that an AI produced and you have not reproduced
  yourself. This matters most for a vulnerability report (see [`SECURITY.md`](SECURITY.md)).

## Developer Certificate of Origin (DCO)

Écluse is, and will remain, free and open-source software. We accept contributions under the
**[Developer Certificate of Origin](DCO)** (DCO, v1.1). It is a lightweight per-commit affirmation
that you have the right to submit your work under the project's [MIT licence](LICENSE). We chose
the DCO over a Contributor Licence Agreement on purpose. It asks you only to certify provenance.
It grants the project no power to relicense or close the code, so Écluse stays permanently FOSS.

**Sign off every commit**. `git commit -s` (or `--signoff`) appends a `Signed-off-by` trailer from
your git identity. It certifies that you wrote the change, or have the right to submit it, and it
becomes a permanent public record:

```
Signed-off-by: Your Name <you@example.com>
```

- **Every commit in a PR** needs a `Signed-off-by` that matches its author.
- It is **separate from the GPG signature**. `-S` proves who committed. `-s` certifies your right
  to contribute. Use both: `git commit -S -s`.
- **We squash-merge, so sign off every commit**. The DCO check verifies each branch commit, and
  GitHub assembles the squash message from those commits. Editing the PR description does not sign
  your commits.
- **Forgot one?** `git commit --amend -s --no-edit` fixes the last commit.
  `git rebase --signoff main` signs off a whole branch.

## Pull requests

Open as a draft while work or review is moving, and mark it ready when it is not. The template is
[`.github/PULL_REQUEST_TEMPLATE.md`](.github/PULL_REQUEST_TEMPLATE.md): Summary, Checklist,
Sign-off, AI assistance. Fill every section, and tick a checklist item only when it is true.
Otherwise say "not applicable" and why. An untrue tick costs a reviewer more than an honest gap.

The Summary is the part worth effort. A reviewer reads it before the diff, so write it so someone
who has not opened the diff understands the change on its own. Two to five sentences: what changed
and why, in words a sharp colleague on another team follows. For a security or behaviour change,
say who could do what before and what holds now. Name a deliberate trade-off in one sentence if
there was one. End with `Closes #NNN` where a slice completes.

Rules for the Summary:

- Lead with the point: what a reviewer or operator gains or is protected from. The mechanism comes
  second, and only as far as the diff does not already show it. No play-by-play of files.
- Prefer the plain word. Define an unavoidable term in a clause.
- Short paragraphs. A bullet list only when enumerating cases. No headings or bold lead-ins inside
  the Summary.
- Include evidence only where review depends on it (a verification table, a sweep result), and keep
  it compact.
- Canadian spelling. No em-dashes or en-dashes. No filler adjectives.

### The same change, written twice

The rules above are easier to apply against an example than against a list. Both bodies below
describe one change: the serve path stopped forwarding upstream credential headers to clients.

Verbose. It narrates the diff, so a reviewer learns the file layout before they learn the risk:

> This pull request introduces a comprehensive fix to the serving layer. First, in
> `Ecluse.Server.Serve` we add a new `scrubUpstreamHeaders` helper, which iterates over the
> response header list and carefully removes any header whose name appears in the
> `credentialHeaders` set. We then thread this helper through both `respondPackument` and
> `respondTarball`, each of which previously passed the upstream response headers directly through
> to the client without any filtering. We also add a robust new test module,
> `Ecluse.Server.ServeSpec`, with four test cases that thoroughly verify each of the affected code
> paths behaves correctly under a variety of conditions.

Concise. It leads with the exposure, states what holds now, and trusts the diff for the rest:

> An upstream registry that set `Authorization` or `Set-Cookie` on a response had those headers
> forwarded verbatim to the client, so a client could receive the mirror's own upstream credential.
>
> The serve path now drops credential headers before it responds. The packument and tarball routes
> share one list, so a route added later cannot miss it by accident.
>
> Closes #123

The second is shorter, but that is a side effect. It is better because a reviewer finishes the
first sentence knowing what was wrong, and finishes the second knowing what to check in the diff.
The first tells them which functions changed, which the diff already shows.

## Repository requirements

- **Use [Conventional Commits](https://www.conventionalcommits.org/)**. Subjects are
  `type(scope): summary`. `type` is one of `feat`, `fix`, `docs`, `chore`, `ci`, `refactor`,
  `test`, `build`, `perf`. The scope is optional. Keep the summary short and imperative.
- **Commits are GPG-signed and DCO signed off** (see above). Disclose non-trivial AI assistance
  with an `Assisted-by:` trailer.
- **Every Haskell source file carries an SPDX licence header**. `task spdx-fix` stamps it
  ([docs/style.md](docs/style.md#14-licence-headers)).
- **Pin every GitHub Action to a full commit SHA**, never a tag, with the version in a trailing
  comment. Renovate bumps them.
- **Keep workflows injection-free**. Never interpolate untrusted `${{ github.event.* }}` or
  `${{ github.head_ref }}` into `run:` blocks. Pass them through `env:` or intermediate files.
- **Semgrep and Stan ignores require the repo owner's approval**. Do not add `.semgrepignore`
  entries, `nosemgrep` comments, or Stan `[[ignore]]` entries on your own.
- **Diagrams are Mermaid, not ASCII art**: a fenced ` ```mermaid ` block, never box-drawing
  characters.
