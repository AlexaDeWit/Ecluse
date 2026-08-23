---
name: orchestrate-implementation
description: >-
  Run the Écluse team-lead per-PR loop. Decompose the frozen architecture into
  slices, dispatch implementer and fresh-context reviewer subagents, and drive the
  mandatory two-pass evaluation and the CI gate. Route fixes, and flip only
  review-passed, gate-green PRs ready for the architect. For post-compaction
  resume, use resume-orchestration instead.
---

# Orchestrate implementation

The team lead's procedure for turning a frozen architecture into merged PRs.
[`.agents/orchestration-strategy.md`](../../orchestration-strategy.md) is the reference and holds
every rationale. This skill is the checklist. Read the linked section when a step needs depth.

Dispatch implementation only after explicit architect kickoff. Never merge and never push to
`main`: all code lands through PRs the architect reviews.

## Per-PR loop

Run this for each DAG node once its dependencies merge
([The per-PR loop](../../orchestration-strategy.md#the-per-pr-loop)).

1. **Pick** a slice once every dependency has merged.
2. **Build**. Brief an implementer subagent in its own worktree. Carry the architect's acceptance
   criteria into the brief verbatim, plus the comment budget as a numbered criterion (a function
   comment is one or two lines, a new header at most eight). The brief also restates the owner's
   boy-scout rule for every file the slice edits: decide whether the file earns a rewrite, trim each
   comment block over the cap and each comment that restates the implementation, scoped to those
   files and behaviour-preserving, and name what was trimmed in the PR body. A minimal diff is not a
   virtue in a file that is a comment wall. Pin the `model` for design-bearing or
   security-sensitive work.
   Pick the verification mode for the host
   ([Verification](../../orchestration-strategy.md#verification-fast-local-ci-gates-build-and-test)):
   an idle host runs `task check` before pushing, a shared host runs `task format` only and lets
   the PR's CI verify. Disjoint hunks across every open PR, and the later PR owns the rebase. The
   implementer opens the draft PR at its first push and reports the head SHA at once.
3. **Evaluate (mandatory)**. At that first push, dispatch a fresh-context reviewer pinned to the
   head SHA, with no exposure to the implementer's reasoning. It runs beside CI and covers Stage A
   (requirements, gating-test evidence, scope) and Stage B (quality, security, and the comment
   count) ([Evaluation](../../orchestration-strategy.md#evaluation-two-independent-passes)).
   Critical findings block. Route the fix as a distinct commit: resume the implementer, apply a
   small reviewer-specified fix directly, or brief a fresh agent. Then re-evaluate.
4. **Gate**. Watch the CI `gate` to green beside the evaluation. Review findings and CI reds land
   as one follow-up commit, and both re-verify on the new head. Before every push, run
   `gh run list --branch <branch>`: a push cancels an in-flight run.
5. **Hand off**. Flip the PR ready only when the evaluation passed and the gate is green on the
   same head commit, and every required context passes (`gh pr checks`). Then `gh pr ready`, then
   report. A draft is never reported as done. `codecov/patch` is informational
   ([Definition of done](../../orchestration-strategy.md#definition-of-done)).

## Always on

[Guardrails](../../orchestration-strategy.md#guardrails-always-on) and
[Escalation](../../orchestration-strategy.md#escalation) in full. The ones that bite:

- Escalate, don't guess: a bounded attempt, then a decision-ready question with options and a
  recommendation.
- Surface decisions one at a time, paced by a task list. Record each ruling before the next
  question.
- Reference work by PR or issue number, never an internal task ID.
- After every merge, rebase the dependent worktrees and re-run their gate. Between waves, run the
  [inter-wave pass](../../orchestration-strategy.md#inter-wave-quality-and-alignment-pass).
- Commit and PR mechanics: the [`open-pull-request`](../open-pull-request/SKILL.md) skill.
