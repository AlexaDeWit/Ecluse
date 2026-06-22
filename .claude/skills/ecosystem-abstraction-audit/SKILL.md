---
name: ecosystem-abstraction-audit
description: >-
  Audit Écluse's ecosystem-agnostic core against its ecosystem-specific edges and
  surface ways to generalize core behaviour through abstractions, so a new package
  ecosystem (PyPI, RubyGems, …) slots in as an adapter only. Read-only; produces a
  categorized findings report, never code changes. Run periodically (e.g. weekly) or
  before adding an ecosystem.
---

# Ecosystem abstraction audit

## Purpose

Keep a strong, generalized **ecosystem-agnostic core** with **ecosystem-specific
detail confined to the edges** — the `Ecluse.Registry.<Eco>.*` adapter, the
`Ecosystem` / `VersionKey` cases, and the per-ecosystem parsers. As the system grows,
patterns for abstraction emerge and the test suites make refactors progressively
safer; this routine seeks those generalizations out before one ecosystem's
assumptions ossify into the core.

## How to run

A **read-only investigation that produces a report — never code changes.** If you are
a long-lived orchestrating session, dispatch a **fresh-context read-only subagent** to
do the audit (fresh eyes, no bias); otherwise perform it inline. Run any tool through
the current flake — `env -u IN_NIX_SHELL nix develop --command …` — never the ambient
shell. Do not modify files; do not build.

## The litmus test (frame every finding against it)

> Could the **next** ecosystem be added by writing ONLY edge code — a `RegistryClient`
> adapter (wire decoders + projection to the domain model), the `Ecosystem` /
> `VersionKey` case, and per-ecosystem parsers/predicates — with **zero changes** to
> the agnostic core (`Ecluse.Package`, `Ecluse.Rules`, `Ecluse.Package.Merge`,
> `Ecluse.Server.*`, and `Ecluse.Version`'s public API)?

Every place where adding an ecosystem would force a core change, or where one
ecosystem's assumptions have leaked inward, is a finding.

## Read

- **Agnostic core:** `Ecluse.Package`, `Ecluse.Version`, `Ecluse.Rules(.Types)`,
  `Ecluse.Package.Merge`, `Ecluse.Ecosystem`, `Ecluse.Server.*`, `Ecluse.Registry`
  (the handle interface), `Ecluse.Config` / `Ecluse.Env`.
- **Edge adapters:** `Ecluse.Registry.<Eco>.*` (today `Npm.*`).
- **Composition root:** `Ecluse.hs` / `Ecluse.App` — the only place that should name a
  concrete adapter.
- **Design of record:** `docs/architecture.md`, `docs/architecture/registry-model.md`,
  `docs/architecture/domain-model.md` (the multi-registry model, the Handle pattern);
  reconcile code against the intended end state.
- `STYLE.md` for the module-organization principles and the Handle pattern.

## Look for

1. **Inward leaks** — one ecosystem's field names, URL shapes, or wire quirks reaching
   the core rather than staying in its adapter.
2. **Ecosystem-branching in the core** — core logic that `case`s on `Ecosystem` where
   it should dispatch through an abstraction (a handle method, a typeclass, a
   per-ecosystem record). Parsing/comparison *inside* `Ecluse.Version` is the right
   place to dispatch; flag only branching that belongs behind an adapter.
3. **Agnostic logic stuck in an adapter** — behaviour in `Registry.<Eco>.*` that is
   actually ecosystem-neutral and should be hoisted to the core, so a second adapter
   need not re-implement it.
4. **Over-fit core types** — assumptions not every ecosystem shares (e.g. `dist-tags`,
   scoped names, a `latest` tag — PyPI and RubyGems lack these).
5. **Missing seams** — where a small abstraction would let the core stay closed while
   the edges extend; propose it concretely (name the type / handle method / typeclass
   and where it would live).

## Output — a categorized report

- **Already agnostic (with evidence)** plus a litmus verdict: how close is "add an
  ecosystem = adapter-only" today? **A clean result is a valid, valuable outcome — do
  not manufacture work.**
- **Findings** — each with `file:line`, what was seen, why it impedes generalization,
  a severity, and a disposition: **SAFE-FIX** (a behaviour-preserving generalization
  landable as a normal refactor PR — rename / hoist / introduce a seam) or
  **ESCALATE** (a design-level abstraction decision for the architect, possibly a new
  slice).
- **Recommended seams**, prioritized — concrete, naming the type/handle/typeclass and
  its home.

## After the report

The team lead triages: SAFE-FIX items land through the normal slice loop
(BUILD → EVALUATE → GATE → PR); ESCALATE items go to the architect as new
slices/issues. **This routine never refactors directly — it only finds and proposes.**
Record recurring or deferred findings (in the report, and in agent memory) so
successive runs build on prior ones rather than re-deriving them. The routine is most
valuable as the codebase grows and a second ecosystem approaches; early runs may
simply confirm the boundary is clean and name the seams to watch.
