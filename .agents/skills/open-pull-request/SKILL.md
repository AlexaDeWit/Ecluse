---
name: open-pull-request
description: >-
  Prepare a commit and pull request the way this repo gates them: GPG-signed,
  DCO-signed-off as the author (never the AI), Conventional-Commit, AI-disclosed,
  opened as a draft, with a short body a reader outside the subsystem understands
  on its own. Invoke before committing and opening or finalising a PR; it encodes
  the rules agents most often miss, so DCO-red, missing trailers, and a verbose or
  unclear body do not happen.
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
2. **The PR body is short and understandable on its own.** The architect reads the body before
   the diff; a body that narrates the diff, or needs a second explanation, costs a round-trip.
   Lead with what changed and why, in plain words. There is no separate plain-language section:
   the whole body is plain.

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
  **`codecov/project` is a context the ruleset requires**, so it must be green before the flip.
  **`codecov/patch` is informational**: it reads integration-tier-covered code as under-covered,
  so a red there does not block hand-off. Note it and proceed.

## 4. If the DCO check goes red

Interactive rebase is unavailable here. Re-sign **non-interactively** with `git commit-tree -S`,
preserving each commit's tree: walk the branch commits, re-create each with the corrected
`Signed-off-by:` (and `Assisted-by:`) trailer and a GPG signature, then move the branch ref to the
new tip. Confirm `git diff` against the old commits is empty before force-pushing the feature branch
(force-pushing a feature branch is fine; never force-push `main`).
