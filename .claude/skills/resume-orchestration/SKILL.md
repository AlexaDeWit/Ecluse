---
name: resume-orchestration
description: >-
  Re-establish full team-lead orchestration context after a /compact (or at the start
  of a fresh session) for the Écluse build: re-read the strategy + design canon, sync
  ground truth from git/gh, reconcile memory, and report state before acting. Also
  holds the canonical /compact prompt template to use before compacting. Invoke as the
  first action in a freshly-compacted context.
---

# Resume orchestration

The durable state of this project lives in **files**, the repo docs, the planning
DAG, and agent memory (`MEMORY.md` plus the memory files, which survive compaction).
The conversation is the most disposable layer. This routine rebuilds working context
from those authoritative sources rather than trusting a summary.

## When invoked, the re-ingestion routine

Do this before acting on any task or summary:

1. **Re-read the process + design canon** (the team-lead operating manual and the
   design of record):
   - `planning/orchestration-strategy.md`, the per-PR loop, **Fix routing** (a
     reviewer's "changes required" can resume the original background agent via
     `SendMessage`, or the team lead applies a small fix directly, or briefs a fresh
     agent), the **draft-until-ready**
     procedure (PRs open as draft; ready-for-review only at hand-off), the Definition
     of Done, the Guardrails.
   - `planning/delivery-plan.md`, the slice DAG and wave state.
   - `AGENTS.md`, `CONTRIBUTING.md`, `docs/testing.md`, `STYLE.md`, `HADDOCK.md`.
   - `docs/architecture.md` and `docs/architecture/{registry-model,web-layer,
     rules-engine,domain-model}.md`, the design of record.
   - the next slice file under `planning/slices/` (whatever the DAG marks next).
2. **Sync ground truth** (trust this over any summary); run anything that builds
   through the current flake (`env -u IN_NIX_SHELL nix develop --command …`):
   - `git -C /home/alexa/Workspace/npm-secure-proxy checkout main && git pull --ff-only`
   - `git log --oneline -15` ; `git worktree list`
   - `gh pr list --state open --json number,title,headRefName,isDraft`
   - `gh issue list --state open`
3. **Reconcile memory**, read `MEMORY.md` and the memory files; update or delete any
   the merged state has made stale (a memory reflects what was true when written), and
   ensure none conflict with repository-level guidance (e.g. retired terminology).
4. **Report and wait**, summarise what is merged, what is in review vs still draft,
   and the next dispatchable slice. Then **wait for the architect's kickoff** before
   dispatching any build (standing rule: dispatch implementers only on kickoff).

This routine is read-only: it restores context and hands back to the architect; it
never dispatches work or pushes. It is equally useful at the start of any fresh
session, not only after `/compact`.

## The /compact prompt to use *before* compacting

A skill cannot drive `/compact`, compaction summarises and resets the very context
the skill runs in, so the continuation is a new context that never saw the skill run.
Before compacting, therefore, run `/compact` with the template below: fill the
bracketed CURRENT-STATE / NEXT / STALE-MEMORY slots from live state; the rest is
durable and copied verbatim. Its final line bootstraps the routine above.

> Preserve the active orchestration state verbatim; summarize the rest. Multi-agent
> build of Écluse (Haskell npm-registry resilience proxy; package `ecluse`, modules
> `Ecluse.*`).
>
> ROLE / RULES (in force): team lead orchestrating implementer + reviewer subagents;
> the user is principal architect (reviews and merges every PR). Team lead NEVER
> merges and NEVER pushes to main (feature-branch pushes for PRs are fine). Dispatch
> implementers only on explicit architect kickoff. One git worktree per agent; 2–3
> slices in flight. Per slice: BUILD (implementer, own worktree, TDD) → EVALUATE
> (fresh reviewer + team-lead diff-read) → GATE (hermetic nix flake check) → hand off.
> Escalate-don't-guess. Commits GPG-signed, Conventional Commits, trailer
> `Assisted-by: Claude (Anthropic)` (NOT Co-Authored-By).
>
> CRITICAL CONVENTIONS: (1) run ALL build/format/gate via
> `env -u IN_NIX_SHELL nix develop --command make <target>` (ambient shell stale →
> wrong fourmolu). (2) Canadian spelling in all prose/Haddock. (3) HADDOCK §11: no
> slice/PR/issue/roadmap narration in source Haddock. (4) a reviewer's "changes
> required" can resume the original background agent (`SendMessage`), or the team
> lead applies the small fix directly, or briefs a fresh agent. (5) PRs open as
> DRAFT, marked ready-for-review only at
> hand-off (after EVALUATE + gate + confident).
>
> CURRENT STATE (verify against git/gh): [fill: what is merged, in review, in flight].
> NEXT (architect kickoff only): [fill: next slice + any rule it must respect].
> MEMORY TO RECONCILE: [fill: which memory files the merged state has made stale].
>
> End the summary with: "First action on resume: invoke /resume-orchestration."
