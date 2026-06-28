---
name: open-pull-request
description: >-
  Prepare a commit + pull request for Écluse the way this repo gates them: GPG-signed,
  DCO-signed-off (as the author, NOT Claude), Conventional-Commit, AI-disclosed, opened
  as a draft, and with a PR body that follows the template AND explains the change in
  plain terms. Invoke before you commit and open (or finalize) a PR, it encodes the
  exact rules agents most often miss, so the churn (DCO red, missing trailers, a
  review-time "explain it simply" round-trip) does not happen.
---

# Open a pull request

This is the **checklist that gets a PR through this repo's gates on the first try.** It
is deliberately prescriptive, the value is in the literal commands and trailers, not a
paraphrase. The full rationale lives in `CONTRIBUTING.md` (→ *Developer Certificate of
Origin*, *Conventional Commits*, *AI-assisted contributions*) and the on-disk
`.github/PULL_REQUEST_TEMPLATE.md`; this skill is the operational distillation of the
mistakes that have actually cost us reword-and-force-push round-trips.

Two non-negotiables up front, because they are the two that bite:

1. **The `Signed-off-by:` trailer must name the _author_, `Alexandra DeWit
   <alexa.dewit@gmail.com>`, never Claude.** The DCO check (a probot) gates on a
   `Signed-off-by` matching the commit author's email. `Signed-off-by: Claude
   <noreply@anthropic.com>` red-fails it. AI assistance is disclosed *separately*, via
   the `Assisted-by:` trailer (below), the two never substitute for each other.
2. **Every non-trivial PR body must include a plain-language section** (`## In plain
   terms`). The architect reviews every PR and routinely asks for an "explain it like
   I'm 5" of the change and its threat/behaviour model, so writing that *into the PR
   body up front* pre-empts the round-trip. A self-evident change may omit it; see §2
   for what counts as trivial; when in doubt, include it.

## 1. Commits, the exact recipe

Commit with **both** signing flags, every time:

```
git commit -S -s -m "<conventional subject>" -m "<body…>" -m "Assisted-by: Claude (Anthropic)"
```

- `-S` = GPG-sign (authenticity, *who* committed). `-s` = append the DCO
  `Signed-off-by` trailer from your configured git identity. Git here is configured as
  Alexandra DeWit, so `-s` produces the correct sign-off automatically, **do not
  hand-write a `Signed-off-by:` line** (that is exactly how the wrong name slips in).
- **Conventional Commits** subject: `type(scope): summary`,  `fix(egress): …`, `feat(server): …`, `docs(threat-model): …`, `test(bench): …`,
  `refactor(core): …`, `ci(pages): …`. Imperative mood, lower-case, no trailing period.
- **`Assisted-by: Claude (Anthropic)`** discloses the AI assistance. It is **not**
  `Co-Authored-By`, do not use that trailer. It coexists with the sign-off.
- **This machine's git has no `--trailer` flag.** Put trailers as literal lines in the
  message body (a trailing `-m` block, as above, or in a message file via `-F`). Don't
  reach for `--trailer`; it errors here.
- **We squash-merge, so _every_ commit on the branch needs the sign-off**, the squash
  message is assembled from the branch commits, so a `Signed-off-by` only reaches `main`
  if each commit carries one. Sign off as you go; never trim sign-offs from the final
  message.

## 2. The PR body, template + plain terms

Follow `.github/PULL_REQUEST_TEMPLATE.md` (Summary · Checklist · Sign-off · AI
assistance) and, for any non-trivial change, **add a `## In plain terms` section.**
Skeleton:

```markdown
## Summary

<What changed and why, at engineering depth. Closes #NNN.>

## In plain terms

<Lead with one or two plain sentences, what this change means for someone outside this
 subsystem, the human point before the mechanism. Then a few short, signposted beats:
 bold a lead-in, or use `###` sub-headings for a larger change. Use the beats that fit;
 not every PR has all three. Keep each to 1–3 sentences.>

**The situation / the risk.** <What was wrong, missing, or risky, concretely, for a
security or behaviour change, who could do what, under what conditions.>

**What changed.** <The key idea in everyday terms; define any unavoidable term in a
clause. An analogy is welcome where it genuinely illuminates. No internal slice/PR shorthand.>

**The trade-off.** <What was deliberately accepted, or chosen against, and the honest reason.>

## Checklist
- [ ] `make check` passes locally (build, unit tests, fourmolu, hlint, Semgrep)
- [ ] Docs updated in this PR where behaviour, interfaces, or config changed
- [ ] Conventional Commit subjects; commits are GPG-signed
- [ ] Every commit is signed off, DCO (`git commit -s`), as the author
- [ ] Tests added or updated for the change

## AI assistance
- [x] Disclosed: assisted by Claude (Anthropic); `Assisted-by:` trailer on the
      relevant commits. Author reviewed and is responsible for every line.
```

Write the section the way you would **explain the change aloud to a sharp colleague on
another team**, the "explain it like I'm 5" the architect asks for at review time. Give
it a throughline (what was going on → what changed → what we chose not to do, and why),
not a flat summary and not a diff walk-through. **Make it scan:** short paragraphs (2–4
sentences), a bold lead-in or `###` sub-heading per beat, a tight bullet list when you
are enumerating cases, never one dense block. Lead with the human point; reach for an
analogy where it genuinely illuminates. It answers "what does this mean and why should I
trust it" for a reader with no familiarity with the file being changed. Canadian spelling
throughout (as in all repo prose).

**Omit the section only when the change is self-evident from the Summary**, a
process-doc or typo fix, a dependency bump, a mechanical no-behaviour rename. Anything
with a security, behaviour, interface, or design-rationale dimension, anything you can
picture the architect asking you to "explain simply", keeps it. This PR (a process-doc
change) is itself a trivial one, and omits the section.

## 3. Open as a draft; flip to ready only at hand-off

- **Open the PR as a draft:** `gh pr create --draft …`. It stays draft while work or
  review is still moving.
- **Ready-for-review means exactly: independent review passed (reviewer APPROVE +
  team-lead diff-read) AND the gating CI is green.** Nothing else gates the flip, not
  optional polish, not a nice-to-have test someone floated. The instant both hold,
  `gh pr ready`.
- **Verify the gate authoritatively with `gh pr checks`, not `gh run watch`'s exit
  code** (it is unreliable; it can exit 0 on failure). The gating jobs are *Build &
  tests, CI gate, End-to-end tests, Haddock builds, Static checks*. **`codecov/patch`
  and `codecov/project` are non-gating** backstops (they read integration-tier-covered
  code as under-covered); a red there does not block the hand-off, note it and proceed.

## 4. If the DCO check goes red anyway

Interactive rebase is unavailable in this environment. Re-sign **non-interactively**,
preserving each commit's tree, with `git commit-tree -S`: walk the branch commits,
re-create each with the corrected `Signed-off-by:` (and `Assisted-by:`) trailer and a
GPG signature, then move the branch ref to the new tip. Confirm `git diff` against the
old commits is empty before force-pushing the feature branch (force-pushing a feature
branch is fine; never force-push `main`).
