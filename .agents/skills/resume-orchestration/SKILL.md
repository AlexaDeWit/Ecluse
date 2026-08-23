---
name: resume-orchestration
description: >-
  Resume the Écluse team-lead seat after compaction or restart. Restore the volatile
  orchestration state, verify it against git and GitHub, and retrieve only the
  process or design sections the next decision needs.
---

# Resume orchestration

Resume from a compact checkpoint and live state. Do not rebuild general project knowledge by
rereading the full process and design canon. `AGENTS.md` is already loaded, and stable detail stays
retrievable from repository files.

## Delta-based resume

1. **Extract the checkpoint:** identify the objective, active slices, agents/worktrees, PRs,
   decisions, blockers, verification state, and exact next action from the compacted thread. Treat
   all git, GitHub, CI, and slice status as provisional.
2. **Verify volatile state:** run bounded queries for:
   - the current branch and working tree
   - recent commits relevant to active work
   - the worktrees or agents the checkpoint names
   - open PRs and their draft/check state
   - the issues of PRs merged since the checkpoint. GitHub auto-close is not configured on this
     repository, so a merged PR's issue stays open until the team lead closes it by hand.
   - the `status:` and acceptance criteria of active or next-dispatchable slice files
3. **Retrieve by decision:** read only the sections the next orchestration action needs:
   - per-PR loop, fix routing, evaluation, gate, or guardrails from
     `.agents/orchestration-strategy.md`
   - active slice files
   - architecture sections a blocker or review finding implicates
4. **Reconcile selective memory:** only if the checkpoint names stale memory, or a current decision
   depends on it, read `MEMORY.md` and the relevant linked file. Verify its volatile claims before
   updating or deleting it.
5. **Report and wait:** summarise what has merged, what is in review, and what is still draft, plus
   blockers or conflicts and the next dispatchable action. List the merged PRs whose issues still
   need closing by hand. Close only an issue whose acceptance criteria the merged PR met, never one
   it merely cross-references. The inter-wave pass in
   [`.agents/orchestration-strategy.md`](../../orchestration-strategy.md) owns that housekeeping.
   Wait for explicit architect kickoff before dispatching an implementation build, unless the
   checkpoint records the architect's kickoff for the active wave and names the next dispatchable
   issue. Then continue that wave without asking again.

Do not routinely reread `README.md`, `AGENTS.md`, `CONTRIBUTING.md`, `docs/style.md`,
`docs/haddock.md`, all of `docs/testing.md`, or the architecture set. Retrieve one when the next
decision actually depends on it.

This routine may query live state, but it does not edit project files, dispatch work, merge, or
push.

## Compaction

The canonical history-compaction instruction is `.agents/compact-prompt.md`. Compact at a phase
boundary while enough context remains to record decisions accurately. The checkpoint must keep
volatile state and rationale, and must not duplicate stable repository guidance.

When invoking your agent's compaction command manually (`/compact` or equivalent), request that
it follow `.agents/compact-prompt.md`. The result must end by directing the resumed thread to
this skill so it verifies live state before acting.
