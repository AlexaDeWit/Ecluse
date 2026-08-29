---
name: open-pull-request
description: >-
  Prepare a commit and pull request the way this repo gates them. The
  requirements: GPG-signed, DCO-signed-off as the author (never the AI),
  Conventional-Commit, AI-disclosed, and opened as a draft. The body is short, and
  a reader outside the subsystem understands it on its own. Invoke it before you
  commit and before you open or finalise a PR. It carries the procedure only:
  CONTRIBUTING.md owns the standards and wins on any conflict. A red DCO check, a
  missing trailer, and a verbose or unclear body then do not happen.
---

# Open a pull request

The literal commands and trailers that get a PR through this repo's gates on the first try.

**[`CONTRIBUTING.md`](../../../CONTRIBUTING.md) owns the standards. This skill owns the procedure.**
It says what a good contribution is; this says how to produce one here and what to do when a check
goes red. Where the two appear to disagree, `CONTRIBUTING.md` wins, and the disagreement is a bug in
this file. Do not restate its rules here: link to them, so a human contributor and an agent read one
source.

The two failures this file exists to prevent:

1. **A `Signed-off-by:` that names the AI.** It names the author, `Alexandra DeWit
   <alexa.dewit@gmail.com>`. The DCO probot gates on a trailer matching the commit author's email,
   so a bot address red-fails it. AI help is disclosed separately with `Assisted-by:`
   ([CONTRIBUTING.md, *AI-assisted contributions*](../../../CONTRIBUTING.md#ai-assisted-contributions)).

2. **A PR body that narrates the diff.** The architect reads it before the diff, so a body that
   needs a second explanation costs a round-trip. The rules are in
   [CONTRIBUTING.md, *Pull requests*](../../../CONTRIBUTING.md#pull-requests).

## 1. Commits

Commit with both signing flags, every time. Sign off as you go, on every commit, and never trim one:

```
git commit -S -s -m "<conventional subject>" -m "<body…>" -m "Assisted-by: <Agent Name> (<Vendor>)"
```

Why both flags and why every commit:
[CONTRIBUTING.md, *DCO*](../../../CONTRIBUTING.md#developer-certificate-of-origin-dco). The subject
format and the type list: [*Repository
requirements*](../../../CONTRIBUTING.md#repository-requirements).

What this file adds, which those do not say:

- **Do not hand-write a `Signed-off-by:` line.** `-s` derives it from the git identity, which here
  is already Alexandra DeWit. Hand-writing it is how the wrong name slips in.
- **This machine's git has no `--trailer` flag.** Put trailers in literal `-m` lines, or in a `-F`
  message file. `--trailer` errors here.
- **`Assisted-by:`, never `Co-Authored-By:`.** Git tooling and editor snippets reach for the latter
  by default, and it asserts joint authorship this project does not accept.

## 2. The PR body

Follow `.github/PULL_REQUEST_TEMPLATE.md` (Summary · Checklist · Sign-off · AI assistance).

**Read [CONTRIBUTING.md, *Pull requests*](../../../CONTRIBUTING.md#pull-requests) before you write
the Summary.** It owns the rules and carries the same change written twice, verbose and concise,
which is faster to work from than the rules alone.

A compressed reminder, not a second copy: lead with what a reviewer or operator gains or is
protected from, two to five sentences, no play-by-play of files, Canadian spelling, no em-dashes.
It exists because a bare link is skippable, and an instruction an agent skips is decoration. When it
drifts from `CONTRIBUTING.md`, `CONTRIBUTING.md` is right.

That guidance lives in `CONTRIBUTING.md` because this skill sits under `.agents/`, where a drive-by
contributor never looks.

## 3. Draft until hand-off

- **Open as a draft:** `gh pr create --draft …`. It stays draft while work or review is moving.
- **Pipe the body through stdin. Never write it to a repo-root file.** A root scratch file
  (`pr_body.md`) collides across concurrent agents and worktrees, and an agent stages it by
  accident:

  ```
  gh pr create --draft --title "<subject>" --body-file - <<'EOF'
  <body…>
  EOF
  ```

  `gh pr edit --body-file -` updates it the same way. If the body must exist as a file, put it under
  the gitignored `scratchpad/` with a branch-scoped name (`scratchpad/pr-body-<branch>.md`). Your
  harness scratchpad outside the repo works too. Never commit it.
- **Flip to ready only when independent review passes (reviewer APPROVE plus a team-lead diff-read)
  and the gating CI is green**. Nothing else gates the flip. The instant both hold, run
  `gh pr ready`.
- **Verify the gate with `gh pr checks`, not with `gh run watch`'s exit code**, which can exit 0 on
  failure. The gating jobs are *Build & tests*, *Static checks*, *Haddock builds*, *End-to-end
  tests*, *Dead-code check (weeder)*, *Haskell static analysis (stan)*, and the terminal *CI gate*.
  `codecov/project` is a context the ruleset requires, so it must be green before the flip.
  `codecov/patch` is informational. It reads integration-tier-covered code as under-covered, so a
  red there does not block hand-off. Note it and proceed.

## 4. If the DCO check goes red

Interactive rebase is unavailable here. Re-sign non-interactively with `git commit-tree -S`, and
preserve each commit's tree. Walk the branch commits. Re-create each one with the corrected
`Signed-off-by:` (and `Assisted-by:`) trailer and a GPG signature, then move the branch ref to the
new tip. Confirm that `git diff` against the old commits is empty before you force-push the feature
branch. Force-pushing a feature branch is fine. Never force-push `main`.
