# Planning

Working artifacts for **how Écluse gets built** — the process. This is distinct
from [`../docs/`](../docs/), which describes **what** Écluse is (the system
design). Design lives in `docs/`; process lives here.

- **[orchestration-strategy.md](orchestration-strategy.md)** — how multi-agent
  implementation is coordinated: the principal-architect / team-lead roles, the
  *escalate-don't-guess* contract, the per-PR build → evaluate → gate → handoff
  loop, worktree isolation, and how the CI gate is reproduced before anything is
  handed back for review.
- **[delivery-plan.md](delivery-plan.md)** — the concrete, dependency-ordered
  DAG of PR-sized slices (the milestones M0–M8, the parallelization waves, and
  the operating cadence). This is the **index**; it is the single-writer
  (team-lead) entry point and does not carry per-slice status.
- **[slices/](slices/)** — one markdown file per slice (`SNN-*.md`), each the
  authoritative detail for its slice: goal, acceptance criteria traced to the
  architecture, file scope, test tier(s), dependencies, and a `status:`
  field in its frontmatter. **Status lives here, one file per slice**, so
  parallel agents and their status updates touch disjoint files and never collide
  on a shared table; the git history of these files is the milestone log.
