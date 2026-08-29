Keep the active task state and the decisions. Discard reproducible repository content and
chronological narration. When in doubt, discard.

Produce a compact checkpoint with these headings:

- Objective and acceptance criteria
- Active phase, branch/worktree, and PR
- Decisions and rationale
- Files changed
- Verification evidence and relevant failures
- Blockers and open questions
- Exact next action
- Sources to retrieve next (paths and headings only)

Do not copy AGENTS.md, architecture prose, style rules, successful command logs, discarded
hypotheses, or old status. Repository files stay authoritative and you can retrieve them on demand.
Mark every volatile git, GitHub, CI, or slice status as needing live verification on resume.

For a team-lead orchestration thread, also keep the active slices, the assigned worktrees or
agents, the review state, and the next dispatchable work. End that checkpoint with: "Resume per
the startup procedure in .agents/context-management.md: verify volatile state against git and
GitHub and restart the CI watches before acting."
