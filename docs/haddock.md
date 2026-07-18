# Haddock and documentation guide

How we write the in-source documentation for Écluse (package `ecluse`). It is the
companion to [`docs/style.md`](style.md), which owns formatting, naming, totality, and the
compiler flags, and is written to be followed directly.

> When in doubt, match the nearest existing module.
> [`core/src/Ecluse/Core/Package.hs`](../core/src/Ecluse/Core/Package.hs) and
> [`core/src/Ecluse/Core/Rules.hs`](../core/src/Ecluse/Core/Rules.hs) are the reference
> implementations.

## What Haddock is for

Haddock is our **reference** documentation, the authoritative "what is this and how do I call it?"
for the public API, rendered to a browsable site by `task docs` and published from `main`. It is one
of the [four kinds of documentation](https://diataxis.fr/): reference, not tutorial, how-to, or
explanation. Narrative and onboarding belong in [`README.md`](../README.md) and
[`docs/architecture.md`](architecture.md), not spread across every function. Haddock answers the
focused question at each type and function: its contract, its caveats, and the reasoning a signature
can't carry, in one-line summaries rather than a wall of prose.

---

## 1. Terminology

| Term | Meaning |
|---|---|
| **Documentation comment** / **Haddock comment** | A comment Haddock reads: `-- \|`, `-- ^`, or `{- \| … -}`. A plain `--` comment is invisible to Haddock. |
| **Pre-comment** (`-- \|`) / **post-comment** (`-- ^`) | A comment placed *before* vs *after* the thing it documents. |
| **Module header** | The `{- \| … -}` comment immediately before the `module` keyword. |
| **Section heading** | A `-- *`, `-- **`, … marker *in the export list*; it groups exports and builds the page's table of contents. |
| **Example** / **doctest** | A `>>>` line plus its expected output. `task doctest` *runs* it (§9). |

---

## 2. The one rule: document the why, not the what

Your sentence says what a declaration is *for* and what a caller must know that the types cannot
express: invariants, ordering/precedence, totality, failure behaviour, and, for Écluse especially,
the security rationale (§10). Never restate a signature; Haddock already prints it. Document the
exported surface, not internals (a plain `--` comment on a helper where the *why* is unclear, never
Haddock), and one crisp summary line per export makes a module scannable where a paragraph on every
binding does the opposite. When you cross-reference, link *upward* to a sibling module or an
architecture doc rather than narrating changeable internals.

---

## 3. How much to document, and what to skip

> A summary sentence on everything exported; an example or caveat where it earns its place;
> nothing on internals. Never restate the type.

| Entity | Document? | What to say |
|---|---|---|
| **Module** | Always | Header: what it is for and how it fits the system (§5). |
| **Exported function** | Always (≥ 1 line) | Purpose; preconditions, totality, failure modes; an example if non-obvious. |
| **Exported type / `newtype`** | Always | What it represents and any invariant it protects. |
| **Sum constructors** | Usually | A `-- \|` per constructor where the name isn't self-evident; for `Rule`/`RuleOutcome`-style domain types, *always* (the domain knowledge lives here). |
| **Record fields** | Usually | `-- ^` per field: units, ranges, invariants. |
| **Type class + methods** | Always | The abstraction and any laws; default behaviour. |
| **Instances** | Rarely | Only when behaviour is surprising. |
| **Non-exported helper** | **No Haddock** | Plain `--` only where the *why* is unclear. |
| **Trivial, self-evident export** | One line, no more | Don't pad it to look thorough. |

**Don't:** restate the signature in words; Haddock a `where` helper or any unexported binding;
narrate the implementation ("first we fold, then we map…"); write a paragraph where a sentence or a
`>>>` example works; add ceremony like `-- | The constructor.`

A **high-level signpost is not narration.** One line naming a complex function's phases or stating an
observable guarantee orients a reader in a way the types cannot, so it stays. The **drift test** tells
a signpost from a restatement: if someone refactored the internals without changing the contract,
would this sentence become false? If yes and it describes internals, cut it or lift it to the
contract. And don't re-describe a sibling module's policy in a caller's comment: state your local
contract and point at the owning definition.

---

## 4. Comment syntax

```haskell
-- | Documents the declaration that FOLLOWS (the default; a "pre-comment").
mkScope :: Text -> Scope

renderScope :: Scope -> Text  -- ^ Documents the declaration that PRECEDES.

{- | Block form, for the module header and any multi-paragraph doc.
The marker goes on the first line only. -}
```

Default to `-- |` before a declaration; use `-- ^` inline for arguments, constructor arguments, and
record fields; use `{- | … -}` for module headers and long blocks.

---

## 5. Module headers

Every module opens with a `{- | … -}` header stating what it is for and how it fits the system, the
model and the load-bearing decisions, not a list of type names. Cross-reference sibling modules and
the architecture docs.

```haskell
{- | The policy rules engine.

A rule set is evaluated against a single 'PackageDetails' snapshot to produce a
'Decision'. The model is __deny by default; precedence decides__: the
highest-precedence rule that does not abstain wins, and at equal precedence a
deny beats an allow.

The rule data types live in "Ecluse.Core.Rules.Types".
-}
module Ecluse.Core.Rules ( ... ) where
```

**We do not use the `Module:/Copyright:/License:/Maintainer:` header fields.** Those are ceremony for
libraries published standalone to Hackage; Écluse is one application whose licence (`MIT`) lives in
the cabal file and `LICENSE`, so a plain prose header is the convention. Structure a long header with
`==` / `===` subsection headings; keep a short one to a single lead paragraph.

---

## 6. Documenting declarations

**Functions, with per-argument docs.** Annotate the contract the signature can't state, here a
load-bearing totality (a crash would take down the gate), not a reflexive "pure and total" tag
(docs/style.md §9.2):

```haskell
{- | Evaluate a single rule against a single package version. Total: a
malformed rule or package yields an outcome, never an exception, so hostile
metadata cannot crash the gate.
-}
evalRule
    :: EvalContext     -- ^ Ambient inputs (the current time, …)
    -> Rule            -- ^ The rule to apply
    -> PackageDetails  -- ^ The version under evaluation
    -> RuleOutcome
```

**Sum types, a `-- |` per constructor.** This is where Écluse's domain knowledge lives, so
document each one and its *why* (§10):

```haskell
data Rule
    = -- | Unconditionally allow every package under the given scope.
      AllowScope Scope
    | -- | Allow a version only if it was published at least this long ago.
      -- Guards against race-to-publish attacks: an attacker publishes a
      -- malicious version hoping it is consumed before takedown.
      AllowIfOlderThan NominalDiffTime
    deriving stock (Eq, Show)
```

**Records** get a `-- ^` per field carrying units and invariants (e.g. `pkgPublishedAt :: Maybe
UTCTime  -- ^ When this version was published, if the registry reports it.`).

---

## 7. Organising a module for navigation

**Group the export list with section headings** (`-- *`, `-- **`). They become the page's
table of contents, the single biggest aid to a newcomer. (docs/style.md → "Exports" states the
export-list-is-contract rule.)

```haskell
module Ecluse.Core.Package (
    -- * Scopes
    Scope,
    mkScope,
    unScope,
    renderScope,

    -- * Package identity
    PackageName,
    mkPackageName,
    renderPackageName,
    -- ...
) where
```

---

## 8. The markup we use

This is the whole everyday vocabulary. Reach for markup only where it changes the render,
and match how a sibling module does it.

| Want | Write | Renders as |
|---|---|---|
| Link an identifier | `'mkScope'` | a link to `mkScope` |
| Link a module | `"Ecluse.Core.Rules.Types"` | a link to the module |
| Inline code / a literal | `@"1.2.3"@` | `1.2.3` in monospace |
| Load-bearing emphasis | `__deny by default__` | **deny by default** |
| Italic (sparingly) | `/why/` | *why* |
| A bulleted list | lines starting `*` after a blank line | • items |
| A runnable example | `>>> expr` then its output | an Examples block (§9) |
| Export-list section | `-- * Scopes` | a doc section (§7) |
| Header subsection | `== Conventions` | a heading (§5) |

**Don't over-escape.** Only `@`, `<`, and `>` are active characters (plus `'` wrapped tightly around
an identifier). Write prose apostrophes and punctuation **bare** (`npm's`, `a/b`), never `npm\'s`. To
show an active character literally, escape just it (`\@`, `\<`, `\>`); if a literal needs a thicket
of backslashes, rephrase.

---

## 9. Examples that run (doctest)

Prefer a `>>>` example to a paragraph: because we run it, it cannot rot. An example is the
expression plus its expected output:

```haskell
{- | Render a duration as an approximate, human-friendly string for use in
decision messages. Always non-negative.

>>> renderDuration 604800
"7 days"

>>> renderDuration 90
"1 minute"
-}
renderDuration :: NominalDiffTime -> Text
```

- **`task doctest`** runs every `>>>` example as a test, and the CI gate runs it too, so an example
  that disagrees with the code fails the build. It works via `cabal repl --with-ghc=doctest`, which
  inherits the package's exact build configuration (crucially the `relude` prelude), so examples see
  the same names the module does.
- **Keep examples pure, total, and deterministic**: they run in a plain GHCi session with no `IO`
  setup, so the pure core (`Ecluse.Core.Rules`, `Ecluse.Core.Version`, `Ecluse.Core.Package`) is
  their natural home. Output is compared to GHCi's printed form: a `Text` shows with quotes
  (`"7 days"`).

---

## 10. Explain the why, especially the security rationale

Écluse is a supply-chain policy proxy. A comment that explains the **threat a rule defends
against** (as `AllowIfOlderThan` does in §6) is worth far more than one describing the
mechanics. This is the single most valuable thing a Haddock comment here can carry, because
it is exactly what a type signature, a test, or a later reader cannot reconstruct.

---

## 11. Document the code, not the project

Haddock is the durable contract, read long after any PR. Keep project-management narration out of it:
no status or roadmap ("for now", "currently", "a later slice will…"); no slice/PR/issue references
("(see S07)", "added in #42", "TODO(after the spike)"), which belong in git history and the issue
tracker; no test-plumbing narration (document a test double where it is defined, not
in the production module it stands in for). The test: if a sentence would read as false or pointless
a year from now, once the "later" work has landed, it is project narration, so cut it.

---

## 12. Tooling and workflow

- **`task docs`** builds the hyperlinked, searchable Haddock site and opens it.
- **`task doctest`** runs every `>>>` example as a test (also in the CI gate).
- **`task check`** is the full local gate and includes both.

**Read the rendered page, not just the source.** A markup slip is invisible in the source. After
reworking a header, run `task docs` and check that every link resolves, code spans are closed, no
stray `@` or backslash leaks into prose, and headings nest as intended.

---

## 13. Checklist (before you open a PR)

- [ ] Every new module has a prose `{- | … -}` header; every exported type and function has a Haddock
      comment (≥ 1 line), with sum constructors and record fields documented where they carry meaning.
- [ ] The *why* is captured, especially the security rationale (§10).
- [ ] No restated signatures, no Haddock on non-exported helpers, no project/PR/status narration (§11).
- [ ] Markup is minimal and unescaped in prose (§8).
- [ ] Non-obvious behaviour has a runnable `>>>` example; `task doctest` passes.
- [ ] `task docs` builds clean and the rendered page was eyeballed (§12).
