# Implementation Orchestration Strategy

How **Écluse** (package `ecluse`) is built as a coordinated multi-agent effort.
This document is about _process_; the system design lives in
[`../docs/architecture.md`](../docs/architecture.md), the development workflow and
CI in [`../CONTRIBUTING.md`](../CONTRIBUTING.md), Haskell style in
[`../STYLE.md`](../STYLE.md), and agent-facing essentials in
[`../AGENTS.md`](../AGENTS.md).

## Roles

- **Principal architect** (the repo owner) — owns the design and the
  requirements, and is the final decision-maker on both. Reviews and merges every
  PR.
- **Team lead** (the coordinating agent) — decomposes the _finalized_ architecture
  into PR-sized work, dispatches and supervises implementation subagents,
  evaluates their output, reproduces the CI gate, and hands review-ready PRs to
  the architect. **The team lead never merges and, during implementation, never
  pushes to `main`** — all code lands through PRs the architect reviews.

## Operating principle: escalate, don't guess

The single most important rule. Whenever an agent is **stuck, unsure, blocked, or
facing ambiguous / missing / contradictory spec, it stops and surfaces the
problem** rather than inventing a way past it. Agents make a _bounded_ attempt
against the existing specs first, then escalate — they do not thrash, and they do
not paper over uncertainty. Specifically, an implementation agent must never:

- fabricate a config key, path, value, or **API behaviour** (verify via
  `hoogle` / docs, or escalate);
- silently weaken, skip, or `xfail` a test to reach green;
- add a `.semgrepignore` entry or `nosemgrep` comment (those require the
  architect's approval, always);
- sprawling beyond the slice's file scope to route around a blocker (rather than staying in scope or justifying the exception);
- leave a `TODO` / `undefined` / stub and call the work done.

A leftover stub or a quietly-relaxed test **is a blocker, not a delivery** — the
team lead scans for exactly that in review, because it is how guessing hides.

Surfacing is also **proactive**: concerns, limitations, and risks are raised _as
warranted_, not only when something is hard-blocked.

## Phase 0 — architecture → delivery plan

Done once, when the architecture is frozen (not before). The team lead turns the
design into a **dependency-ordered DAG of PR-sized slices**, recorded in
`planning/delivery-plan.md`:

- **Walking skeleton first** — the thinnest end-to-end path, then capabilities
  layered onto it.
- **Handles before consumers** — the Handle-pattern records (`RegistryClient`,
  `MirrorQueue`, `CredentialProvider`) are defined as interfaces early so
  downstream slices can be built in parallel against them.
- **Each slice is one coherent, reviewable-in-a-sitting capability**, with
  acceptance criteria traced to specific architecture sections, the test tier(s)
  it owes, a **limited file scope**, and its dependencies.

The architect signs off on this breakdown **before any code is written**.

## Convergence slices: contract before construction

The DAG encodes _ordering_ (`depends-on`) but not the **shape** of what crosses
each edge. Where several producer slices converge on one consumer — e.g.
`S09 + S33 → S14` (the packument pipeline) or `S15/S17/S18/S19 → S20` (launch
composition) — specify the **consumer's interface, the types that flow across the
boundary, before building the producers**. Producers then build _to_ a known
contract instead of the consumer reverse-engineering whatever they happened to
emit. A convergence slice's interface is a deliverable of **this planning pass**,
not a discovery of the build pass. (Skipping it is how the packument pipeline's
typed-decision-vs-served-`Value` contract surfaced late — see
[Registry Model → decision vs served surface](../docs/architecture/registry-model.md#decision-surface-vs-served-surface).)

## The per-PR loop

```
   pick DAG node (dependencies merged)
        │
   ┌────▼──────────────────────────────────────────────┐
   │ BUILD     implementer agent, its own git worktree   │
   │           TDD: RED → GREEN → REFACTOR               │
   │           self-runs the local gate before reporting │
   └────┬───────────────────────────────────────────────┘
   ┌────▼──────────────────────────────────────────────┐
   │ EVALUATE  fresh reviewer agent(s), two stages:      │
   │   A. spec / requirements compliance + traceability  │
   │   B. code quality + security + test quality         │
   │   team lead reads the diff; fixes are routed by the  │
   │   team lead (agents can't be resumed in this harness)│
   └────┬───────────────────────────────────────────────┘
   ┌────▼──────────────────────────────────────────────┐
   │ GATE      reproduce CI locally → push branch →      │
   │           confirm the real gate green on the PR     │
   └────┬───────────────────────────────────────────────┘
        ▼
   HAND OFF — mark the PR ready for review (only if all the above is green;
              until then it stays a draft)
```

**Draft until ready.** A PR is opened as a **draft** and stays one until it has
cleared EVALUATE and the gate and the team lead is confident handing it over. Taking
it out of draft — **marking it ready for review** — is the hand-off signal: it means
_ready for the architect to review and potentially merge_, nothing less. A PR still
building, mid-review, gate-red, or that the team lead is simply not yet sure of stays
a **draft**, so "ready for review" is never ambiguous and the architect never spends
attention on — or merges — work that was not deliberately offered.

**Fix routing.** A reviewer's "changes required" does **not** resume the original
implementer — in this harness a running or rested agent cannot be redirected or
continued. Small, reviewer-specified fixes the team lead applies **directly** (then
re-runs the gate); a larger rework is dispatched as a **fresh** build agent briefed
with the review. Either way the fix lands as a distinct, separately-reviewable
commit.

## Subagents and isolation

- **Implementer** — builds one slice. General-purpose agent, full tools.
- **Reviewer** — evaluates a slice with **fresh context** (no exposure to the
  implementer's reasoning), read-and-verify only.

**One git worktree per agent**, each on its own branch, is a hard rule: it keeps
parallel slices from colliding on a shared tree and contains each agent's blast
radius. Concurrency is capped (**2–3 slices in flight**) so evaluation quality
does not degrade. After every merge, the team lead rebases the dependent
worktrees onto the new base and re-runs their gate, so integration drift surfaces
immediately rather than at PR time. Slices that genuinely cannot be split become
**stacked PRs**; otherwise they stay small and independent.

**Warm each worktree's HLS index at creation.** A fresh worktree is a fresh HLS
workspace: its `dist-newstyle` and `.hie` start empty, so the first `mcp__hls__*`
call in it would otherwise pay a cold full-project typecheck — the failure mode an
agent can stall on. Create worktrees with `make new-worktree BRANCH=<branch>`, which
adds the worktree and kicks off a background `make build` so the interface files HLS
reuses are on disk before the agent arrives. Dependencies already come warm from the
shared Nix store, so only this project's own modules cost anything; still, HLS holds
~1–3 GB per load, so with 2–3 worktrees in flight, **stagger** the creations rather
than firing the cold builds in parallel. After a post-merge rebase, re-run `make
build` in the rebased worktree to re-warm it incrementally.

**Pin the model; there is no effort dial.** The Agent tool's `model` argument, left
unset, takes the general-purpose agent's default — which may be **lighter than the
team lead's own model** — and the tool exposes **no** thinking-effort parameter, so
`model` is the only capability lever the dispatcher controls. A lighter default tends
to head straight to implementation and skip the exploration a slice needs (e.g. it
will name a tool but not bootstrap into it; see below). For **design-bearing or
security-sensitive** work — a shared type, the credential-discipline serve path, a
parse-don't-validate boundary — dispatch with `model` pinned to the strongest
available rather than defaulting; reserve the default only for genuinely mechanical
slices.

**Have agents bootstrap their tools — the LSP MCP especially.** The HLS-over-MCP
navigation tools (`mcp__hls__definition` / `references` / `hover` / `diagnostics`) are
surfaced as *deferred* tools: an agent must **load them before it can call them**, and
a less-exploratory agent skips that step and falls back to `grep`. A brief should
direct the agent to load and use them early for cross-module work — find-references for
a refactor's blast radius, go-to-definition across re-exports, type-at-point to confirm
a signature — which is higher-precision and faster than `grep` over this codebase's
qualified imports and re-exports (the compiler stays ground truth for correctness).
An instruction to use a tool the agent cannot actually reach is just decoration, so
confirm the MCP is wired into the agent's environment when you rely on it.

**Invoke the toolchain through the current flake, never the ambient shell.** A
long-lived agent session enters a `nix develop` once and holds it for the whole
session; if a flake upgrade merges _mid-session_ (a new GHC, fourmolu, or
dependency pin), that ambient shell goes stale while the code on disk moves on.
The Makefile trusts `IN_NIX_SHELL` and runs tools directly when it is set — which
is silently wrong for a stale agent shell (e.g. a 0.14 fourmolu reflowing a 0.19
codebase's Haddock). So agents and the team lead run every build/format/gate
command as `env -u IN_NIX_SHELL nix develop --command make <target>`, which
rebuilds the shell from the on-disk flake and uses its pinned tools regardless of
session age. This is an **agent-workflow rule, not a repo one**: CI enters the
shell fresh per run and humans' direnv re-evaluates on pull, so neither is ever
stale — the Makefile is deliberately left as-is rather than taxing every consumer
to compensate for an environment defect unique to long-lived agent sessions.

## Evaluation — two independent passes

The implementer's own "it works" does not count; evidence does.

- **Stage A — requirements.** Every acceptance criterion is met _and backed by a
  deterministic, gating test_ (unit or integration) — a non-gating smoke test
  detects drift but never stands in for a criterion ([Testing Strategy](../docs/testing.md) → _What gates, and what doesn't_); nothing in the slice's architecture
  scope is silently dropped; **limited scope** (changes stay within the slice's files; touching others needs strong justification); documentation is updated in the _same_ PR
  (per [`../AGENTS.md`](../AGENTS.md) → Documentation Policy).
- **Stage B — quality & security.** Idiomatic Haskell per
  [`../STYLE.md`](../STYLE.md); totality; `-Werror`-clean; no unsafe/partial
  functions; a **security review** appropriate to a supply-chain tool (input
  parsing, deny-by-default invariants, injection-free workflows); **test
  quality** — properties present where required (e.g. rules-engine
  deny-precedence), not tautological assertions, with **new/changed lines ≥ 95%
  covered** (`codecov/patch`); and **comment appropriateness** — Haddock documents
  the timeless contract and the _why_, never project / roadmap / slice narration
  ([`../HADDOCK.md`](../HADDOCK.md) §11). Completeness is not enough: a comment can
  be present, and the wrong kind.

Critical findings block; the fix is routed per __Fix routing__ above — the team lead
applies a small, reviewer-specified fix directly, or briefs a fresh build agent for a
larger rework — then re-verified. A running or rested agent cannot be resumed in this
harness, so review never "bounces back" to the original implementer.

## Inter-wave quality & alignment pass

Per-PR review judges each slice **in isolation**; it cannot see the whole that
parallel slices compose into. Slices built concurrently against the handles drift —
divergent idioms, duplicated helpers, inconsistent Haddock, and type-conversion
churn at the boundaries (bouncing a value through `String` / `Text` /
`ByteString`) — and none of that fails a single-slice review. So **between waves**,
once a wave's PRs are all merged and before the next wave is dispatched, the team
lead runs a codebase-wide **quality & alignment pass**.

A dedicated agent audits the integrated tree (fresh context, read-and-verify) for:

- **Structural improvements** — cross-slice duplication, misplaced or mis-sized
  modules, abstractions that should be shared or split, leaky handles, and
  error/idiom patterns that diverged across the slices that just landed.
- **Haddock cleanup** — gaps, drift, and HADDOCK.md §11 violations (roadmap /
  slice narration that crept in); consistent voice and cross-references across
  modules.
- **Performance problems likely to surface** — needless type conversions (the
  `String`↔`Text`↔`ByteString` bounce), avoidable re-parsing / re-allocation,
  lazy/strict mismatches, accidentally-quadratic patterns — caught structurally
  now, before later slices build on them. Once the benchmark harness
  ([S37](slices/S37-benchmark-harness.md)) exists, this audit is **measured, not
  eyeballed**: the micro-benchmarks quantify these regressions and the audit
  consults the informational trend (which itself never gates).
- **Spec & doc accuracy reconciliation** — for every slice merged in the wave,
  reconcile the as-built code against the slice file and the architecture
  document(s) it derives from: fold any learnings, discoveries, and deviations
  from the original acceptance criteria back into the [`slices/`](slices/) file,
  and update the architecture doc so the design of record matches what shipped.
  Confirm every merged slice's `status:` reads `merged`. This stops the plan and
  the architecture drifting from reality as parallel slices land — drift the
  per-PR loop cannot catch, because it never re-reads the spec after the merge.
  **Material design changes are escalated to the architect** (they may reshape
  later slices), not silently rewritten into the docs.

The audit produces a **categorized findings report**; the team lead triages it:

- **Safe, in-scope, behaviour-preserving fixes** (rename, dedupe, Haddock, a
  localized conversion, slice/architecture doc reconciliation) land together as
  one reviewed, gated `refactor` / `docs` PR — the same BUILD → EVALUATE → GATE
  loop.
- **Design-level or far-reaching findings** are **escalated to the architect** as
  new slices / issues rather than silently absorbed — they may reshape later waves.

The pass also includes **housekeeping**: once the wave's PRs have landed, prune the
spent worktrees and their merged branches so `git worktree list` stays an accurate
map of live work. Prune only the merged-and-clean ones — a worktree carrying
uncommitted or not-yet-merged work is surfaced to the architect, never force-removed.

The pass **gates the next wave**: the integrated base a wave builds on is made
coherent first. It is recorded in the
[delivery plan](delivery-plan.md#parallelization--3-slices-in-flight)'s wave sequence.

## Reproducing the CI gate

Because every CI job just calls `make`, the team lead reproduces the **entire
gate** locally before pushing. The gating jobs (the `needs` of the terminal
`gate` job in [`../.github/workflows/ci.yml`](../.github/workflows/ci.yml)) map
one-to-one to make targets:

| Gating CI job                              | Local command                                          |
| ------------------------------------------ | ------------------------------------------------------ |
| `build-and-test` (build + unit)            | `make check` _(build, unit, format-check, lint, sast)_ |
| `lint` (fourmolu + hlint)                  | ↳ included in `make check`                             |
| `semgrep` (`--config auto`, ERROR/WARNING) | ↳ included in `make check`                             |
| `integration` (ministack / Docker)         | `make test-integration`                                |
| `docs` (Haddock)                           | `make docs-site`                                       |
| `gate`                                     | green iff all of the above are green                   |
| `smoke` (live registries)                  | `make test-smoke` — **non-gating, never blocks**       |

Pre-push command:

```bash
make check && make test-integration && make docs-site && make nix-check
```

`make nix-check` is the hermetic backstop: it catches `-Werror` warnings and the
_flakes only see git-tracked files_ trap, so new modules must be `git add`-ed
(and listed in the `.cabal` file) **before** the Nix checks run.

`make coverage` reproduces the patch-coverage check: Codecov's `codecov/patch`
requires **≥ 95%** on new/changed lines (the documented server-side exception to
the single-`gate` rule). Inspect the generated `coverage/<suite>.json` for any
0-hit changed lines before pushing, and close them — coverage is a quality bar on
the work, not an afterthought.

Hard stops: **Semgrep reports zero findings** before any push (no new ignores
without the architect's approval); commits are **GPG-signed** and use
[Conventional Commits](https://www.conventionalcommits.org/); any workflow change
stays **injection-free** with **SHA-pinned** actions. After pushing, the real run
is confirmed green (`gh pr checks` / `gh run watch`) before handoff — on the
result, not the prediction. A red gate is root-caused, not patched over.

## Definition of done

A PR reaches the architect only when **all** hold:

- [ ] All acceptance criteria met, each with passing **deterministic, gating** (unit/integration) test evidence — a non-gating smoke test never stands in for a criterion
- [ ] Independent review (Stage A + B) passed; no open critical issues
- [ ] Local gate green: `make check && make test-integration && make docs-site && make nix-check`
- [ ] New/changed lines ≥ 95% covered (`codecov/patch` green; reproduce via `make coverage`)
- [ ] Comments are contract + why only — no roadmap / slice / PR references (HADDOCK.md §11)
- [ ] Semgrep clean (no new ignores)
- [ ] CI `gate` (and every job it needs) green on the PR
- [ ] Docs updated in the same PR; changes limited to the slice's file scope (other files only with strong justification)
- [ ] Slice file `status:` advanced to `merged` (true once this PR lands) and any as-built delta — design decisions, discoveries, deviations from the acceptance criteria — recorded in it; the slice's `planning/slices/SNN-*.md` is part of the slice's file scope
- [ ] Commits GPG-signed + Conventional Commits
- [ ] PR taken **out of draft and marked ready for review** — the hand-off itself, done only once every box above holds; until then the PR stays a **draft** so it is never mistaken for review-ready

## Escalation

The team lead is a filter, not a megaphone: the architect should not see noise,
but must see every real fork.

**Handled by the team lead (silently):** idiomatic implementation choices among
equivalent options; formatting / lint / build wiring and test plumbing; flaky-CI
reruns; worktree / rebase conflicts; anything answerable from the existing specs.

**Escalated to the architect:**

- ambiguous / missing / contradictory **spec or requirement**;
- a requirement that proves infeasible, or materially costlier / riskier than it
  looked;
- a **security or correctness trade-off** with no clear right answer;
- a design assumption that turns out false; scope questions ("is X in this
  slice?");
- external blockers (a missing secret / credential; an upstream API that behaves
  unlike the spec);
- an agent genuinely stuck after its bounded attempt.

Escalations arrive **decision-ready**:

> **Decision needed** (one sentence, phrased as a question) · **Context / what was
> tried** · **Options** (2–3, with a recommendation marked) · **Blast radius**
> (this PR only, or blocking dependents?) + urgency.

## Guardrails (always on)

- Implementation work lands via **PRs only**; the team lead never merges and never
  pushes to `main`.
- **PRs open as a draft; marked ready for review only at hand-off** — once EVALUATE
  and the gate are green and the team lead is confident, never before. "Ready for
  review" is the signal it is ready for the architect to review and potentially merge.
- **One worktree per agent**; agents keep changes within their slice's file scope, touching other files only with strong justification.
- **GPG-signed** commits, **Conventional Commits** (`type(scope): summary`).
- **Semgrep clean** before every push; ignores need the architect's approval.
- GitHub Actions **SHA-pinned**; workflows kept **injection-free**.
- Documentation updated in the **same** PR as the change it describes.
- Generated artifacts (e.g. version-ordering fixtures via
  `make gen-version-fixtures`) are regenerated with their tooling, never
  hand-edited.
- **Cross-cutting invariants live in one helper.** When the same invariant is
  enforced by more than one slice (`latest` resolution in the npm filter and the
  packument merge; lossless `Value` passthrough across filter/merge/serve), extract
  it into a single shared helper the slices call — duplicated invariant logic drifts
  and gets fixed N times.
- **Surface decisions one at a time.** When several design questions are open at
  once, the team lead does **not** front-load them all on the architect in one
  message. They are **parked** (a short-lived `design-queue.md` under `planning/`, spun
  up when decisions accumulate and removed once drained into `docs/` + slices) and
  brought **one at a time**, lead-with-a-recommendation; the rest wait their turn. This
  complements *escalate, don't guess* — surface proactively, but serialized, not in
  a flood.
- **"The term Seam" is retired.** The records-of-functions abstraction (`RegistryClient`,
  `MirrorQueue`, `CredentialProvider`) is **the Handle pattern**. Don't reintroduce
  "seam" — not even in the generic "integration boundary" sense, which re-muddies the
  rename. Say **"the Handle pattern"** for the abstraction and **"integration
  boundary" / "interface contract"** for where components meet, refer to it simply as "an abstraction boundary"
  or similar terminology as appropriate.

## What lives under `planning/`

This strategy; the concrete **delivery plan** (the PR DAG) once the architecture is
finalized; and — when design questions accumulate — a short-lived `design-queue.md`
holding area (worked one at a time, then drained into `docs/` and slices, and removed
once empty). See [README](README.md).
