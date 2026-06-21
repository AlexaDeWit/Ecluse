# Planning

Working artifacts for **how Écluse gets built** — the process. This is distinct
from [`../docs/`](../docs/), which describes **what** Écluse is (the system
design). Design lives in `docs/`; process lives here.

- **[orchestration-strategy.md](orchestration-strategy.md)** — how multi-agent
  implementation is coordinated: the principal-architect / team-lead roles, the
  *escalate-don't-guess* contract, the per-PR build → evaluate → gate → handoff
  loop, worktree isolation, and how the CI gate is reproduced before anything is
  handed back for review.
- **delivery-plan.md** *(added once the architecture is finalized)* — the
  concrete, dependency-ordered DAG of PR-sized slices, each one traced to the
  architecture requirements it satisfies.
