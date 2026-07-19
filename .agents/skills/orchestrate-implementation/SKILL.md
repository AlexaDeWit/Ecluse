---
name: orchestrate-implementation
description: >-
  Run the Écluse team-lead per-PR loop: decompose the frozen architecture into slices,
  dispatch implementer and fresh-context reviewer subagents, drive the mandatory two-pass
  evaluation and the CI gate, route fixes, and flip only review-passed, gate-green PRs
  ready for the architect. For post-compaction resume, use resume-orchestration instead.
---

# Orchestrate implementation

The team lead's procedure for turning a frozen architecture into merged PRs.
[`.agents/orchestration-strategy.md`](../../orchestration-strategy.md) is the reference (the why and
the full detail); this skill is the procedure to run. Read the doc's linked section when a step needs
depth.

Dispatch implementation only after explicit architect kickoff. Never merge and, during
implementation, never push to `main`: all code lands through PRs the architect reviews.

## The hand-off gate: two gates, not one

A PR flips **ready for review** only when both hold:

1. **Independent evaluation passed.** The mandatory Stage A + Stage B evaluation, run by a
   fresh-context reviewer with no exposure to the implementer's reasoning, cleared with no open
   critical findings.
2. **CI `gate` green.** Every job the terminal `gate` needs is green on the PR.

A green gate is necessary but not sufficient. It verifies build and test; it does not judge
requirements, quality, or security, which the evaluation does. Neither substitutes for the other. Do
not flip a PR ready on a green gate alone. This is the failure the procedure exists to prevent.

## Per-PR loop

Run this for each DAG node whose dependencies are merged. Detail: [The per-PR
loop](../../orchestration-strategy.md#the-per-pr-loop).

1. **Pick** a slice whose dependencies are all merged.
2. **Build.** Brief an implementer subagent in its own worktree. Carry the architect's full
   acceptance criteria into the brief verbatim, not a paraphrase; a too-terse brief invites a guess.
   Pin the `model` for design-bearing or security-sensitive work. The implementer runs a fast local
   check, not the full gate.
3. **Evaluate (mandatory).** Dispatch a fresh-context reviewer with no exposure to the implementer's
   reasoning, read-and-verify only. It runs both passes below. Read the diff yourself as well.
4. **Gate.** Open the draft PR and watch the CI `gate` to green.
5. **Hand off.** Flip the PR ready only when both hand-off gates hold. Until then it stays a draft.

## Evaluation: the two mandatory passes

Independent evaluation is mandatory for every PR; the implementer's own "it works" is not evidence.
Detail: [Evaluation](../../orchestration-strategy.md#evaluation-two-independent-passes).

- **Stage A, requirements.** Every acceptance criterion met and backed by a deterministic, gating
  (unit or integration) test; a non-gating smoke test never stands in for a criterion. Nothing in the
  slice's architecture scope silently dropped; changes stay within the slice's file scope;
  documentation updated in the same PR.
- **Stage B, quality and security.** Idiomatic, total, `-Werror`-clean Haskell; no unsafe or partial
  functions; a security review appropriate to a supply-chain tool (input parsing, deny-by-default
  invariants, injection-free workflows); non-tautological tests with foreseeable branches covered;
  Haddock documents the contract and the why only.

Critical findings block. Route the fix, then re-evaluate.

## Fix routing

A reviewer's "changes required" routes one of three ways, landing as a distinct, separately-reviewable
commit:

- **Resume** a background implementer (`SendMessage` to its agent ID) with its build context intact:
  the first choice for a fix continuing what it just built.
- **Apply directly** a small, reviewer-specified fix, then re-run the gate.
- **Brief a fresh** build agent for a larger rework.

## Verification mode

Match the mode to how many agents share the host. Detail: [Verification](../../orchestration-strategy.md#verification-fast-local-ci-gates-build-and-test).

- **Single-slice on an idle host:** the fast floor is `task check` before pushing (Semgrep clean and
  a clean weeder/stan floor are hard stops), then push and watch the real run to green.
- **CI-verified batch (default for parallel work):** the floor shrinks to `env -u IN_NIX_SHELL nix
  develop --command task format` as the last edit before each commit; no local `task check`, builds,
  test tiers, Docker, or HLS. The PR's own CI run is the verification loop. The invariant that makes
  the width safe is **disjoint file sets, one owner per file across every open PR**; a colliding
  issue waits for the merge and restarts from the new base.

Reproduce a tier locally only to debug a red: map the failing CI job to its `task` target and run
that one, never the whole gate. Before every push, `gh run list --branch <branch>` first: a push
cancels an in-flight run.

## Definition of done

A PR reaches the architect only when every box in [Definition of
done](../../orchestration-strategy.md#definition-of-done) holds. The load-bearing gates:

- Every acceptance criterion met with gating-test evidence.
- Independent Stage A + Stage B evaluation passed, no open critical findings (a green gate does not
  substitute).
- CI `gate` and every job it needs green on the PR.
- Docs updated in the same PR; changes limited to the slice's file scope.
- The slice-completing PR names its issue (`Closes #N`) and folds in the as-built delta.
- Commits GPG-signed, DCO-signed off as the author, Conventional Commit, AI help disclosed with
  `Assisted-by:`. Use the [`open-pull-request`](../open-pull-request/SKILL.md) skill.
- PR flipped out of draft to ready, done only once every box holds.

## Always-on guardrails

Detail: [Guardrails](../../orchestration-strategy.md#guardrails-always-on) and [Escalation](../../orchestration-strategy.md#escalation).

- **Escalate, don't guess.** On an ambiguous, missing, or contradictory spec, stop and surface it
  decision-ready (question, what was tried, 2-3 options with a recommendation, blast radius). Make a
  bounded attempt first; never fabricate a value, weaken a test, or leave a stub.
- **PRs only.** The team lead never merges and never pushes to `main`.
- **Cross-cutting invariants live in one shared helper**, called by each slice, never duplicated.
- **Surface decisions one at a time**, lead-with-a-recommendation; park a backlog in a short-lived
  `.agents/design-queue.md`.
- **Reference work by PR or issue number**, never an internal task-tracker ID the architect cannot
  see.
- After every merge, rebase the dependent worktrees onto the new base and re-run their gate. Between
  waves, run the [inter-wave quality and alignment
  pass](../../orchestration-strategy.md#inter-wave-quality-and-alignment-pass) before dispatching the
  next wave.

This skill drives dispatch and hand-off; it does not merge or push to `main`.
