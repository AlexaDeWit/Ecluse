---
name: orientation
description: >-
  Bootstrap a cold Écluse task session with minimal context: read the project entry
  point and task contract, route to only relevant sources, verify necessary live state,
  and report the working picture. Do not use for team-lead orchestration resume.
---

# Orientation

Orient by retrieval, not ingestion. Repository files are durable memory; the conversation should
contain only the current task contract, applicable invariants, and volatile evidence.

## Procedure

1. **Read the entry point:** read `README.md`. Agent harnesses load `AGENTS.md` (directly or
   through their native context file) at session startup; do not reread it unless an exact
   passage is disputed.
2. **Classify the task:** implementation, review, architecture, operations/configuration,
   documentation, release/CI, or orchestration. Identify the authoritative task contract:
   the user request, issue, PR, or the relevant GitHub issue.
3. **Route context:** follow the table in `AGENTS.md` and
   `.agents/context-management.md`. Read only files and sections needed for the next decision.
   Architecture describes target intent; code, git, and slice status establish shipped state.
4. **Verify necessary live state:** inspect the branch, working tree, relevant recent commits, and
   relevant PR or issue state. Do not enumerate every issue, slice, architecture document, or
   worktree unless the task requires it. Do not checkout, pull, edit, or dispatch as part of
   orientation.
5. **Use memory selectively:** if `MEMORY.md` exists, read its index only when the task depends on
   prior user feedback or decisions. Follow only relevant links and verify volatile claims live.
6. **Report briefly:** state the objective, current shipped/in-flight state that matters,
   applicable invariants, sources selected, unresolved questions, and next action.

Before writing Haskell or prose, retrieve the applicable sections of `STYLE.md` and `HADDOCK.md`.
Before committing or opening a PR, invoke `open-pull-request`; do not preload its instructions at
startup.

For the team-lead seat after restart or compaction, use `resume-orchestration` instead. Never run
both skills for the same startup.

This skill only builds a bounded working picture. It changes no files and dispatches no work.
