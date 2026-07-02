---
name: resume-orchestration
description: >-
  Resume the Écluse team-lead seat after compaction or restart by restoring volatile
  orchestration state, verifying it against git and GitHub, and retrieving only the
  process or design sections needed for the next decision.
---

# Resume orchestration

Resume from a compact checkpoint and live state. Do not rebuild general project knowledge by
rereading the full process and design canon: `AGENTS.md` is already loaded, and stable detail
remains retrievable from repository files.

## Delta-based resume

1. **Extract the checkpoint:** identify the objective, active slices, agents/worktrees, PRs,
   decisions, blockers, verification state, and exact next action from the compacted thread. Treat
   all git, GitHub, CI, and slice status as provisional.
2. **Verify volatile state:** run bounded queries for:
   - current branch and working tree;
   - recent commits relevant to active work;
   - worktrees or agents named in the checkpoint;
   - open PRs and their draft/check state;
   - the `status:` and acceptance criteria of active or next-dispatchable slice files.
3. **Retrieve by decision:** read only the sections needed for the next orchestration action:
   - per-PR loop, fix routing, evaluation, gate, or guardrails from
     `planning/orchestration-strategy.md`;
   - the relevant dependency row or in-flight section from `planning/delivery-plan.md`;
   - active slice files;
   - architecture sections explicitly implicated by a blocker or review finding.
4. **Reconcile selective memory:** only if the checkpoint names stale memory, or a current decision
   depends on it, read `MEMORY.md` and the relevant linked file. Verify its volatile claims before
   updating or deleting it.
5. **Report and wait:** summarise what is merged, in review, or still draft; blockers or conflicts;
   and the next dispatchable action. Wait for explicit architect kickoff before dispatching an
   implementation build.

Do not routinely reread `README.md`, `AGENTS.md`, `CONTRIBUTING.md`, `STYLE.md`, `HADDOCK.md`, all
of `docs/testing.md`, all of `planning/delivery-plan.md`, or the architecture set. Retrieve one
when the next decision actually depends on it.

This routine may query live state, but it does not edit project files, dispatch work, merge, or
push.

## Compaction

The canonical history-compaction instruction is `.agents/compact-prompt.md`. Compact at a phase
boundary while enough context remains to record decisions accurately. The checkpoint must preserve
volatile state and rationale, not duplicate stable repository guidance.

When invoking your agent's compaction command manually (`/compact` or equivalent), request that
it follow `.agents/compact-prompt.md`. The result must end by directing the resumed thread to
this skill so it verifies live state before acting.
