# Implementation Orchestration Strategy

How **Écluse** (package `ecluse`) is built as a coordinated multi-agent effort.
This document is about *process*; the system design lives in
[`../docs/architecture.md`](../docs/architecture.md), the development workflow and
CI in [`../CONTRIBUTING.md`](../CONTRIBUTING.md), Haskell style in
[`../STYLE.md`](../STYLE.md), and agent-facing essentials in
[`../AGENTS.md`](../AGENTS.md).

## Roles

- **Principal architect** (the repo owner) — owns the design and the
  requirements, and is the final decision-maker on both. Reviews and merges every
  PR.
- **Team lead** (the coordinating agent) — decomposes the *finalized* architecture
  into PR-sized work, dispatches and supervises implementation subagents,
  evaluates their output, reproduces the CI gate, and hands review-ready PRs to
  the architect. **The team lead never merges and, during implementation, never
  pushes to `main`** — all code lands through PRs the architect reviews.

## Operating principle: escalate, don't guess

The single most important rule. Whenever an agent is **stuck, unsure, blocked, or
facing ambiguous / missing / contradictory spec, it stops and surfaces the
problem** rather than inventing a way past it. Agents make a *bounded* attempt
against the existing specs first, then escalate — they do not thrash, and they do
not paper over uncertainty. Specifically, an implementation agent must never:

- fabricate a config key, path, value, or **API behaviour** (verify via
  `hoogle` / docs, or escalate);
- silently weaken, skip, or `xfail` a test to reach green;
- add a `.semgrepignore` entry or `nosemgrep` comment (those require the
  architect's approval, always);
- scope-creep outside the slice's file fence to route around a blocker;
- leave a `TODO` / `undefined` / stub and call the work done.

A leftover stub or a quietly-relaxed test **is a blocker, not a delivery** — the
team lead scans for exactly that in review, because it is how guessing hides.

Surfacing is also **proactive**: concerns, limitations, and risks are raised *as
warranted*, not only when something is hard-blocked.

## Phase 0 — architecture → delivery plan

Done once, when the architecture is frozen (not before). The team lead turns the
design into a **dependency-ordered DAG of PR-sized slices**, recorded in
`planning/delivery-plan.md`:

- **Walking skeleton first** — the thinnest end-to-end path, then capabilities
  layered onto it.
- **Seams before consumers** — the Handle-pattern records (`RegistryClient`,
  `MirrorQueue`, `CredentialProvider`) are defined as interfaces early so
  downstream slices can be built in parallel against them.
- **Each slice is one coherent, reviewable-in-a-sitting capability**, with
  acceptance criteria traced to specific architecture sections, the test tier(s)
  it owes, a **file-scope fence**, and its dependencies.

The architect signs off on this breakdown **before any code is written**.

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
   │   team lead reads the diff; critical issues bounce   │
   │   back to the implementer (resumed, context intact)  │
   └────┬───────────────────────────────────────────────┘
   ┌────▼──────────────────────────────────────────────┐
   │ GATE      reproduce CI locally → push branch →      │
   │           confirm the real gate green on the PR     │
   └────┬───────────────────────────────────────────────┘
        ▼
   HAND OFF to the architect (only if everything above is green)
```

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

## Evaluation — two independent passes

The implementer's own "it works" does not count; evidence does.

- **Stage A — requirements.** Every acceptance criterion is met *and backed by a
  test*; nothing in the slice's architecture scope is silently dropped; **no scope
  creep** (only fenced files touched); documentation is updated in the *same* PR
  (per [`../AGENTS.md`](../AGENTS.md) → Documentation Policy).
- **Stage B — quality & security.** Idiomatic Haskell per
  [`../STYLE.md`](../STYLE.md); totality; `-Werror`-clean; no unsafe/partial
  functions; a **security review** appropriate to a supply-chain tool (input
  parsing, deny-by-default invariants, injection-free workflows); **test
  quality** — properties present where required (e.g. rules-engine
  deny-precedence), not tautological assertions, with **new/changed lines ≥ 95%
  covered** (`codecov/patch`); and **comment appropriateness** — Haddock documents
  the timeless contract and the *why*, never project / roadmap / slice narration
  ([`../STYLE.md`](../STYLE.md) §5.6). Completeness is not enough: a comment can be
  present, and the wrong kind.

Critical findings block and bounce to the implementer (resumed so context is
kept), then are re-verified.

## Reproducing the CI gate

Because every CI job just calls `make`, the team lead reproduces the **entire
gate** locally before pushing. The gating jobs (the `needs` of the terminal
`gate` job in [`../.github/workflows/ci.yml`](../.github/workflows/ci.yml)) map
one-to-one to make targets:

| Gating CI job | Local command |
|---|---|
| `build-and-test` (build + unit) | `make check` *(build, unit, format-check, lint, sast)* |
| `lint` (fourmolu + hlint) | ↳ included in `make check` |
| `semgrep` (`--config auto`, ERROR/WARNING) | ↳ included in `make check` |
| `integration` (ministack / Docker) | `make test-integration` |
| `docs` (Haddock) | `make docs-site` |
| `gate` | green iff all of the above are green |
| `smoke` (live registries) | `make test-smoke` — **non-gating, never blocks** |

Pre-push command:

```bash
make check && make test-integration && make docs-site && make nix-check
```

`make nix-check` is the hermetic backstop: it catches `-Werror` warnings and the
*flakes only see git-tracked files* trap, so new modules must be `git add`-ed
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

- [ ] All acceptance criteria met, each with passing test evidence
- [ ] Independent review (Stage A + B) passed; no open critical issues
- [ ] Local gate green: `make check && make test-integration && make docs-site && make nix-check`
- [ ] New/changed lines ≥ 95% covered (`codecov/patch` green; reproduce via `make coverage`)
- [ ] Comments are contract + why only — no roadmap / slice / PR references (STYLE §5.6)
- [ ] Semgrep clean (no new ignores)
- [ ] CI `gate` (and every job it needs) green on the PR
- [ ] Docs updated in the same PR; only fenced files touched
- [ ] Commits GPG-signed + Conventional Commits

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
- **One worktree per agent**; agents touch only files inside their slice's fence.
- **GPG-signed** commits, **Conventional Commits** (`type(scope): summary`).
- **Semgrep clean** before every push; ignores need the architect's approval.
- GitHub Actions **SHA-pinned**; workflows kept **injection-free**.
- Documentation updated in the **same** PR as the change it describes.
- Generated artifacts (e.g. version-ordering fixtures via
  `make gen-version-fixtures`) are regenerated with their tooling, never
  hand-edited.

## What lives under `planning/`

This strategy now; the concrete **delivery plan** (the PR DAG) once the
architecture is finalized. See [README](README.md).
