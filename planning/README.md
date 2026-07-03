# Planning

Working artifacts for **how Écluse gets built**, the process. This is distinct
from [`../docs/`](../docs/), which describes **what** Écluse is (the system
design). Design lives in `docs/`; process lives here.

- **[orchestration-strategy.md](orchestration-strategy.md)**, how multi-agent
  implementation is coordinated: the principal-architect / team-lead roles, the
  *escalate-don't-guess* contract, the per-PR build → evaluate → gate → handoff
  loop, worktree isolation, and how the CI gate is reproduced before anything is
  handed back for review.

- **`design-queue.md`** (transient, absent when nothing is queued), a holding area
  for architectural decisions raised but **not yet resolved**, worked **one at a
  time** with the architect rather than front-loaded. Decisions drain into the
  relevant [`../docs/`](../docs/) design document (and a slice, if they need
  building); the queue is a staging area, not design-of-record, and is removed once
  empty.
