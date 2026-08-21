---
name: open-pull-request
description: >-
  Prepare a commit and pull request the way this repo gates them. The
  requirements: GPG-signed, DCO-signed-off as the author (never the AI),
  Conventional-Commit, AI-disclosed, and opened as a draft. The body is short, and
  a reader outside the subsystem understands it on its own. Invoke it before you
  commit and before you open or finalise a PR. It carries the rules agents miss
  most often. A red DCO check, a missing trailer, and a verbose or unclear body
  then do not happen.
---

# Open a pull request

The checklist that gets a PR through this repo's gates on the first try. The value is in the literal
commands and trailers. Full rationale lives in `CONTRIBUTING.md` (*Developer Certificate of Origin*,
*Conventional Commits*, *AI-assisted contributions*) and `.github/PULL_REQUEST_TEMPLATE.md`.

Two rules bite most often, so lead with them:

1. **The `Signed-off-by:` trailer names the author, `Alexandra DeWit <alexa.dewit@gmail.com>`, never
   the AI.** The DCO probot gates on a `Signed-off-by` that matches the commit author's email. A bot
   address red-fails it. Disclose AI help separately, with `Assisted-by:`. The two trailers never
   substitute.
2. **The PR body is short and understandable on its own.** The architect reads the body before the
   diff. A body that narrates the diff, or that needs a second explanation, costs a round-trip.
   Lead with what changed and why, in plain words. There is no separate plain-language section.
   The whole body is plain.

## 1. Commits

Commit with both signing flags, every time:

```
git commit -S -s -m "<conventional subject>" -m "<body…>" -m "Assisted-by: <Agent Name> (<Vendor>)"
```

- `-S` GPG-signs, and records who committed. `-s` appends the DCO `Signed-off-by` from your git
  identity. Git here carries the identity Alexandra DeWit, so `-s` produces the correct sign-off.
  **Do not hand-write a `Signed-off-by:` line.** That is how the wrong name slips in.
- **Conventional Commits** subject: `type(scope): summary`, imperative, lower-case, no trailing
  period. Examples: `fix(egress): …`, `feat(server): …`, `docs(threat-model): …`,
  `refactor(core): …`.
- **`Assisted-by: <Agent Name> (<Vendor>)`** discloses AI help. It is not `Co-Authored-By`. Do not
  use that trailer.
- **This machine's git has no `--trailer` flag.** Put trailers in literal `-m` lines, or in a `-F`
  message file. `--trailer` errors here.
- **Squash-merge assembles the final message from the branch commits, so every commit needs the
  sign-off.** Sign off as you go. Never trim a sign-off.

## 2. The PR body

Follow `.github/PULL_REQUEST_TEMPLATE.md` (Summary · Checklist · Sign-off · AI assistance). Keep it
short, and write it so a reader who has not opened the diff understands it on its own.

```markdown
## Summary

<Two to five sentences: what changed and why, in words a sharp colleague on another team follows
 without the diff. For a security or behaviour change, say who could do what before and what holds
 now. Name a deliberate trade-off in one sentence if there was one. Closes #NNN.>

## Checklist
- [ ] `task check` passes locally (build, unit tests, fourmolu, hlint, Semgrep, weeder, stan)
- [ ] Docs updated in this PR (README / `docs/` / AGENTS.md) where behaviour, interfaces, or config changed
- [ ] Conventional Commit subjects; commits are GPG-signed
- [ ] Every commit is signed off, DCO (`git commit -s`), as the author
- [ ] Tests added or updated for the change

## Sign-off (DCO)
Signed off on every commit as the author.

## AI assistance
- [x] Disclosed: assisted by AI; `Assisted-by:` trailer on the relevant commits.
      Author reviewed and is responsible for every line.
```

Rules for the Summary:

- Lead with the point: what a reviewer or operator gains or is protected from. The mechanism comes
  second, and only as far as the diff does not already show it. No play-by-play of files.
- Prefer the plain word; define an unavoidable term in a clause.
- Short paragraphs. A bullet list only when enumerating cases. No headings or bold lead-ins inside
  the Summary.
- Include evidence only where review depends on it (a verification table, a sweep result), and keep
  it compact. Tick a checklist item only when it is true; otherwise say "not applicable" and why.
- Canadian spelling. No em-dashes or en-dashes. No filler adjectives.

## 3. Draft until hand-off

- **Open as a draft:** `gh pr create --draft …`. It stays draft while work or review is moving.
- **Pipe the body through stdin. Never write it to a repo-root file.** A root scratch file
  (`pr_body.md`) collides across concurrent agents and worktrees, and gets staged by accident:

  ```
  gh pr create --draft --title "<subject>" --body-file - <<'EOF'
  <body…>
  EOF
  ```

  `gh pr edit --body-file -` updates it the same way. If the body must exist as a file, put it under
  the gitignored `scratchpad/` with a branch-scoped name (`scratchpad/pr-body-<branch>.md`). Your
  harness scratchpad outside the repo works too. Never commit it.
- **Flip to ready only when independent review passes (reviewer APPROVE plus a team-lead diff-read)
  and the gating CI is green.** Nothing else gates the flip. The instant both hold, run
  `gh pr ready`.
- **Verify the gate with `gh pr checks`, not with `gh run watch`'s exit code**, which can exit 0 on
  failure. The gating jobs are *Build & tests*, *Static checks*, *Haddock builds*, *End-to-end
  tests*, *Dead-code check (weeder)*, *Haskell static analysis (stan)*, and the terminal *CI gate*.
  **`codecov/project` is a context the ruleset requires**, so it must be green before the flip.
  **`codecov/patch` is informational.** It reads integration-tier-covered code as under-covered, so
  a red there does not block hand-off. Note it and proceed.

## 4. If the DCO check goes red

Interactive rebase is unavailable here. Re-sign **non-interactively** with `git commit-tree -S`, and
preserve each commit's tree. Walk the branch commits. Re-create each one with the corrected
`Signed-off-by:` (and `Assisted-by:`) trailer and a GPG signature, then move the branch ref to the
new tip. Confirm that `git diff` against the old commits is empty before you force-push the feature
branch. Force-pushing a feature branch is fine. Never force-push `main`.
