# Haddock and documentation guide

How to write the in-source documentation for Écluse (package `ecluse`). Follow it directly.
[`docs/style.md`](style.md) owns formatting, naming, totality, and the compiler flags.

> When in doubt, match the nearest existing module.
> [`core/src/Ecluse/Core/Package.hs`](../core/src/Ecluse/Core/Package.hs) and
> [`core/src/Ecluse/Core/Rules.hs`](../core/src/Ecluse/Core/Rules.hs) are the reference
> implementations.

## What Haddock is for

Haddock is Écluse's **reference** documentation for the public API: what a thing is, and how to call
it. `task docs` renders it to a browsable site, published from `main`. It is one of the
[four kinds of documentation](https://diataxis.fr/), not tutorial, how-to, or explanation. Narrative
and onboarding belong in [`README.md`](../README.md) and [`docs/architecture.md`](architecture.md).
At each type and function, give the contract, the caveats, and the reasoning a signature cannot
carry, in one-line summaries, never a wall of prose.

---

## 1. Terminology

| Term | Meaning |
|---|---|
| **Documentation comment** / **Haddock comment** | A comment Haddock reads: `-- \|`, `-- ^`, or `{- \| … -}`. A plain `--` comment is invisible to Haddock. |
| **Pre-comment** (`-- \|`) / **post-comment** (`-- ^`) | A comment placed *before* vs *after* the thing it documents. |
| **Module header** | The `{- \| … -}` comment immediately before the `module` keyword. |
| **Section heading** | A `-- *`, `-- **`, … marker *in the export list*. It groups exports and builds the page's table of contents. |
| **Example** / **doctest** | A `>>>` line plus its expected output. `task doctest` *runs* it (§9). |

---

## 2. The one rule: document the why, not the what

Say what a declaration is *for*, and the one thing a caller must know that the types cannot
express: an invariant, a precedence, a failure behaviour, or the security rationale (§10). Never
restate a signature. Haddock already prints it.

Document the exported surface, not internals. On a helper, use a plain `--` comment where the *why*
is unclear, never Haddock. One crisp summary line per export keeps a module scannable.
Cross-reference *upward*, to a sibling module or an architecture document, rather than narrating
changeable internals.

---

## 3. How much to document, and what to skip

A summary sentence on everything exported, one or two lines at most. That is a cap, not a target.
An example or caveat where it earns its place. Nothing on internals. Never restate the type or
narrate the body. Only the module header may run longer, within its own cap (§5).

| Entity | Document? | What to say |
|---|---|---|
| **Module** | Always | Header: what it is for and how it fits the system (§5). |
| **Exported function** | Always, one or two lines | The purpose, plus the one precondition, failure mode, or invariant the signature hides. A `>>>` example only where the shape is not obvious. |
| **Exported type / `newtype`** | Always | What it represents and any invariant it protects. |
| **Sum constructors** | Usually | A `-- \|` per constructor where the name isn't self-evident. For `Rule`/`RuleOutcome`-style domain types, *always*: the domain knowledge lives here. |
| **Record fields** | Usually | `-- ^` per field: units, ranges, invariants. |
| **Type class + methods** | Always | The abstraction, any laws, and the default behaviour. |
| **Instances** | Rarely | Only when behaviour is surprising. |
| **Non-exported helper** | **No Haddock** | Plain `--` only where the *why* is unclear. |
| **Trivial, self-evident export** | One line, no more | Don't pad it to look thorough. |

**Don't:**

- Restate the signature in words.
- Haddock a `where` helper or any unexported binding.
- Narrate the implementation ("first we fold, then we map…").
- Write a paragraph where a sentence or a `>>>` example works.
- Add ceremony like `-- | The constructor.`
- Match the length of a long block beside yours. The cap applies to the line you write, whatever
  the file around it does.

A **signpost** is not narration. One line naming a complex function's phases, or stating an
observable guarantee, orients a reader in a way the types cannot, so it stays. The **drift test**
separates a signpost from a restatement: if someone refactored the internals without changing the
contract, would this sentence become false? If yes, and it describes internals, cut it or lift it to
the contract. Don't re-describe a sibling module's policy in a caller's comment. State your local
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

Default to `-- |` before a declaration. Use `-- ^` inline for arguments, constructor arguments, and
record fields. Use `{- | … -}` for module headers and long blocks.

---

## 5. Module headers

Every module opens with a `{- | … -}` header. State what the module is for and how it fits the
system: the model and the load-bearing decisions, never a list of type names. Cross-reference
sibling modules and the architecture documents.

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

**Do not use the `Module:/Copyright:/License:/Maintainer:` header fields**. They are ceremony for
libraries published standalone to Hackage. As one application, Écluse keeps its licence (`MIT`) in
the cabal file and `LICENSE`, so a plain prose header is the convention. A new or rewritten header
is one lead paragraph of at most eight lines. Add `==` / `===` subsections only for a contract that
several modules depend on, and keep each one within the same cap.

---

## 6. Documenting declarations

**Functions, with per-argument docs.** Annotate the contract the signature can't state. Here that is
a load-bearing totality: a crash would take down the gate. Never add a reflexive "pure and total"
tag (docs/style.md §9.2):

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

**Sum types, a `-- |` per constructor.** Écluse's domain knowledge lives here, so document each
constructor and its *why* (§10):

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

**Records** get a `-- ^` per field carrying units and invariants (e.g.
`pkgPublishedAt :: Maybe UTCTime  -- ^ When this version was published, if the registry reports it.`).

---

## 7. Organising a module for navigation

**Group the export list with section headings** (`-- *`, `-- **`). They become the page's table of
contents, the single biggest aid to a newcomer. `docs/style.md` → "Exports" states the
export-list-is-contract rule.

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

This is the whole everyday vocabulary. Reach for markup only where it changes the render, and match
how a sibling module does it.

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

**Don't over-escape.** Only `@`, `<`, and `>` are active characters, plus `'` wrapped tightly around
an identifier. Write prose apostrophes and punctuation **bare** (`npm's`, `a/b`), never `npm\'s`. To
show an active character literally, escape just it (`\@`, `\<`, `\>`). If a literal needs a thicket
of backslashes, rephrase.

---

## 9. Examples that run (doctest)

Prefer a `>>>` example to a paragraph. We run it, so it cannot rot. An example is the expression
plus its expected output:

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

- **`task doctest`** runs every `>>>` example as a test, and so does the CI gate. An example that
  disagrees with the code fails the build. It runs `cabal repl --with-ghc=doctest`, which inherits
  the package's exact build configuration, including the `relude` prelude, so examples see the
  same names the module does.
- **Keep examples pure, total, and deterministic**. They run in a plain GHCi session with no `IO`
  setup, so the pure core (`Ecluse.Core.Rules`, `Ecluse.Core.Version`, `Ecluse.Core.Package`) is
  their natural home. `doctest` compares output to GHCi's printed form: a `Text` shows with quotes
  (`"7 days"`).

---

## 10. Explain the why, especially the security rationale

Écluse is a supply-chain policy proxy. A comment that explains the **threat a rule defends
against**, as `AllowIfOlderThan` does in §6, is worth far more than one describing the mechanics. No
type signature, no test, and no later reader can reconstruct that threat. It is the most valuable
thing a Haddock comment here carries.

---

## 11. Document the code, not the project

Haddock is the durable contract, read long after any PR. Keep project-management narration out of
it:

- No status or roadmap: "for now", "currently", "a later slice will…".
- No slice, PR, or issue reference: "(see S07)", "added in #42", "TODO(after the spike)". Those
  belong in git history and the issue tracker.
- No test-plumbing narration. Document a test double in the module that defines it, never in the
  production module it stands in for.

The test: if a sentence would read as false or pointless a year from now, once the "later" work
lands, it is project narration. Cut it.

---

## 12. Checklist (before you open a PR)

- [ ] Every new module has a prose `{- | … -}` header.
- [ ] Every exported type and function has a Haddock comment (≥ 1 line), with sum constructors and
      record fields documented where they carry meaning.
- [ ] The comment carries the *why*, especially the security rationale (§10).
- [ ] No restated signatures, no Haddock on non-exported helpers, no project/PR/status narration
      (§11).
- [ ] Markup is minimal and unescaped in prose (§8).
- [ ] Non-obvious behaviour has a runnable `>>>` example, and `task doctest` passes.
- [ ] `task docs` builds clean, and you read the rendered page.
