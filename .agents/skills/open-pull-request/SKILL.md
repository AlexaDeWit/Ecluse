---
name: open-pull-request
description: >-
  Prepare a commit and pull request the way this repo gates them: GPG-signed,
  DCO-signed-off as the author (never the AI), Conventional-Commit, AI-disclosed,
  opened as a draft, with a body that follows the template and explains the change
  in plain terms. Invoke before committing and opening or finalising a PR; it
  encodes the rules agents most often miss, so DCO-red, missing trailers, and a
  review-time "explain it simply" round-trip do not happen.
---

# Open a pull request

The checklist that gets a PR through this repo's gates on the first try. The value is in the literal
commands and trailers. Full rationale lives in `CONTRIBUTING.md` (*Developer Certificate of Origin*,
*Conventional Commits*, *AI-assisted contributions*) and `.github/PULL_REQUEST_TEMPLATE.md`.

Two rules bite most often, so lead with them:

1. **The `Signed-off-by:` trailer names the author, `Alexandra DeWit <alexa.dewit@gmail.com>`, never
   the AI.** The DCO probot gates on a `Signed-off-by` matching the commit author's email; a bot
   address red-fails it. AI help is disclosed separately, via `Assisted-by:`; the two never
   substitute.
2. **Every non-trivial PR body carries a `## In plain terms` section.** The architect reviews each
   PR and routinely asks for a plain-language "explain it simply" of the change and its threat and
   behaviour model; writing it up front pre-empts the round-trip.

## 1. Commits

Commit with both signing flags, every time:

```
git commit -S -s -m "<conventional subject>" -m "<body…>" -m "Assisted-by: <Agent Name> (<Vendor>)"
```

- `-S` GPG-signs (who committed); `-s` appends the DCO `Signed-off-by` from your git identity. Git
  here is configured as Alexandra DeWit, so `-s` produces the correct sign-off. **Do not hand-write
  a `Signed-off-by:` line**; that is how the wrong name slips in.
- **Conventional Commits** subject: `type(scope): summary`, imperative, lower-case, no trailing
  period. Examples: `fix(egress): …`, `feat(server): …`, `docs(threat-model): …`,
  `refactor(core): …`.
- **`Assisted-by: <Agent Name> (<Vendor>)`** discloses AI help. It is not `Co-Authored-By`; do not
  use that trailer.
- **This machine's git has no `--trailer` flag.** Put trailers as literal `-m` lines (or in a `-F`
  message file); `--trailer` errors here.
- **Squash-merge assembles the final message from the branch commits, so every commit needs the
  sign-off.** Sign off as you go; never trim sign-offs.

## 2. The PR body

Follow `.github/PULL_REQUEST_TEMPLATE.md` (Summary · Checklist · Sign-off · AI assistance) and, for
any non-trivial change, add a `## In plain terms` section:

```markdown
## Summary

<What changed and why, at engineering depth. Closes #NNN.>

## In plain terms

<One or two plain sentences first: what this means for someone outside the subsystem, the human
 point before the mechanism. Then a few short signposted beats, each 1-3 sentences; use the beats
 that fit.>

**The situation / the risk.** <What was wrong, missing, or risky. For a security or behaviour
change: who could do what, under what conditions.>

**What changed.** <The key idea in everyday terms; define any unavoidable term in a clause.>

**The trade-off.** <What was deliberately accepted or chosen against, and the honest reason.>

## Checklist
- [ ] `task check` passes locally (build, unit tests, fourmolu, hlint, Semgrep)
- [ ] Docs updated in this PR where behaviour, interfaces, or config changed
- [ ] Conventional Commit subjects; commits are GPG-signed
- [ ] Every commit is signed off, DCO (`git commit -s`), as the author
- [ ] Tests added or updated for the change

## AI assistance
- [x] Disclosed: assisted by AI; `Assisted-by:` trailer on the relevant commits.
      Author reviewed and is responsible for every line.
```

Write it as you would explain the change to a sharp colleague on another team: give it a throughline
(what was going on, what changed, what you chose not to do and why), lead with the human point, and
make it scan (short paragraphs, a bold lead-in per beat, a tight bullet list when enumerating cases).
It answers "what does this mean and why should I trust it" for a reader unfamiliar with the file.
Canadian spelling throughout.

**Omit the section only when the Summary is self-evident on its own**: a process-doc or typo fix, a
dependency bump, a mechanical no-behaviour rename. Anything with a security, behaviour, interface, or
design-rationale dimension keeps it.

## 3. Draft until hand-off

- **Open as a draft:** `gh pr create --draft …`. It stays draft while work or review is moving.
- **Pipe the body via stdin; never write it to a repo-root file.** Root scratch files (`pr_body.md`)
  collide across concurrent agents and worktrees and get staged by accident:

  ```
  gh pr create --draft --title "<subject>" --body-file - <<'EOF'
  <body…>
  EOF
  ```

  `gh pr edit --body-file -` updates it the same way. If the body must exist as a file, put it under
  the gitignored `scratchpad/` with a branch-scoped name (`scratchpad/pr-body-<branch>.md`), or in
  your harness scratchpad outside the repo. Never commit it.
- **Flip to ready only when independent review has passed (reviewer APPROVE + team-lead diff-read)
  and the gating CI is green.** Nothing else gates the flip. The instant both hold, `gh pr ready`.
- **Verify the gate with `gh pr checks`, not `gh run watch`'s exit code** (it can exit 0 on failure).
  The gating jobs are *Build & tests*, *Static checks*, *Haddock builds*, *End-to-end tests*,
  *Dead-code check (weeder)*, *Haskell static analysis (stan)*, and the terminal *CI gate*.
  **`codecov/patch` and `codecov/project` are non-gating** backstops that read integration-tier-
  covered code as under-covered; a red there does not block hand-off. Note it and proceed.

## 4. If the DCO check goes red

Interactive rebase is unavailable here. Re-sign **non-interactively** with `git commit-tree -S`,
preserving each commit's tree: walk the branch commits, re-create each with the corrected
`Signed-off-by:` (and `Assisted-by:`) trailer and a GPG signature, then move the branch ref to the
new tip. Confirm `git diff` against the old commits is empty before force-pushing the feature branch
(force-pushing a feature branch is fine; never force-push `main`).
