# Haddock & Documentation Guide

How we write the in-source documentation for **Écluse** (package `ecluse`). It is
the companion to [`STYLE.md`](STYLE.md) (which owns formatting, naming, totality,
and the compiler flags) and is written to be followed directly by both newcomers
to Haskell and coding agents.

> **The golden rule still applies: when in doubt, match the nearest existing
> module.** [`src/Ecluse/Package.hs`](src/Ecluse/Package.hs) and
> [`src/Ecluse/Rules.hs`](src/Ecluse/Rules.hs) are the reference implementations.

## What Haddock is for

Haddock is our **Reference** documentation — the authoritative "what is this and
how do I call it?" for the public API, rendered to a browsable site by
`make docs` and published from `main`. It is one of the
[four kinds of documentation](https://diataxis.fr/): reference, not a tutorial,
a how-to, or an explanation. So:

- **Narrative and onboarding** ("how the proxy fits together", "how to add a
  registry") belong in [`README.md`](README.md) and
  [`docs/architecture.md`](docs/architecture.md) — not spread across every
  function.
- **Haddock answers the focused question** at each type and function: its
  contract, its caveats, and the reasoning a type signature can't carry.

The aim is a manual a Haskeller who is *new to this codebase* can navigate
quickly — which comes from **structure and one-line summaries**, not from a wall
of prose on every binding.

---

## 1. Terminology

Use these words precisely; the rest of the guide relies on them.

| Term | Meaning |
|---|---|
| **Haddock** | The tool that turns annotated source into the HTML docs. "the Haddocks" = the rendered site. |
| **Documentation comment** / **Haddock comment** | A comment Haddock reads: `-- \|`, `-- ^`, or `{- \| … -}`. A plain `--` comment is invisible to Haddock. |
| **Markup** | The lightweight formatting *inside* a documentation comment (links, emphasis, code, …). |
| **Declaration** | A top-level definition: a function, type, `newtype`, class, or instance. |
| **Pre-comment** (`-- \|`) / **post-comment** (`-- ^`) | A comment placed *before* vs *after* the thing it documents. |
| **Module header** | The `{- \| … -}` comment immediately before the `module` keyword. |
| **Section heading** | A `-- *`, `-- **`, … marker *in the export list*; it groups exports and builds the page's table of contents. |
| **Example** / **doctest** | A `>>>` line plus its expected output. Haddock renders it as an "Examples" block; `make doctest` *runs* it (§9). |

---

## 2. Guiding principles

**Document the *why* and the *surprising*, not the *what*.** The signature
already says `Scope -> Text`. Your sentence says what it is *for* and what a
caller must know that the types cannot express: invariants, ordering/precedence,
totality, failure behaviour, and — for Écluse especially — the security rationale
(§10).

**Let the types carry their weight.** Haddock already prints every signature and
data declaration. Never restate one in prose ("takes a `Scope` and returns
`Text`") — that is pure noise.

**Document the exported surface; leave internals alone.** A module's export list
*is* its public contract (`-Wmissing-export-lists`). Document everything in it;
do **not** put Haddock on non-exported helpers — a plain `--` comment, only where
the *why* is unclear, is right there (§3).

**Navigability over volume.** One crisp summary line on each export, grouped under
section headings, makes a module scannable. A paragraph on every binding does the
opposite.

**Point from the volatile to the stable.** When you cross-reference, link
*upward* — to a sibling module or an architecture doc — rather than narrating the
changeable internals of the current one.

---

## 3. How much to document — and what to skip

The balance, in one line:

> **A summary sentence on everything exported; an example or caveat where it earns
> its place; nothing on internals. Never restate the type.**

| Entity | Document? | What to say |
|---|---|---|
| **Module** | Always | Header: what it is for and how it fits the system (§5). |
| **Exported function** | Always (≥ 1 line) | Purpose; preconditions, totality, failure modes; an example if non-obvious. |
| **Exported type / `newtype`** | Always | What it represents and any invariant it protects. |
| **Sum constructors** | Usually | A `-- \|` per constructor where the name isn't self-evident — and for `Rule`/`RuleOutcome`-style domain types, *always* (this is where the domain knowledge lives). |
| **Record fields** | Usually | `-- ^` per field: units, ranges, invariants. |
| **Type class + methods** | Always | The abstraction and any laws; default behaviour. |
| **Instances** | Rarely | Only when behaviour is surprising. |
| **Non-exported helper** | **No Haddock** | Plain `--` only where the *why* is unclear. |
| **Trivial, self-evident export** | One line, no more | Don't pad it to look thorough. |

**Anti-bloat checklist — don't:**

- ❌ Restate the signature in words.
- ❌ Haddock-document a `where` helper or any unexported binding.
- ❌ Narrate the implementation ("first we fold, then we map…").
- ❌ Write a paragraph where a sentence (or a `>>>` example) works.
- ❌ Add ceremony like `-- | The constructor.` that says nothing.

---

## 4. Comment syntax

```haskell
-- | Documents the declaration that FOLLOWS (the default; a "pre-comment").
mkScope :: Text -> Scope

renderScope :: Scope -> Text  -- ^ Documents the declaration that PRECEDES.
```

```haskell
{- | Block form, for the module header and any genuinely multi-paragraph doc.
The marker goes on the first line only. -}
```

Conventions:

- **Default to `-- |`** before a declaration — it reads top-down.
- **Use `-- ^`** inline, for function arguments, constructor arguments, and
  record fields, where the comment naturally sits after the thing.
- **Use `{- | … -}`** for module headers and long multi-paragraph blocks.

---

## 5. Module headers

Every module opens with a `{- | … -}` header stating what it is for and how it
fits the system — the model and the load-bearing decisions, not a list of the
type names. Cross-reference sibling modules (`"Ecluse.Rules.Types"`) and the
architecture docs.

```haskell
{- | The policy rules engine.

A rule set is evaluated against a single 'PackageDetails' snapshot to produce a
'Decision'. The model is __deny by default; precedence decides__: the
highest-precedence rule that does not abstain wins, and at equal precedence a
deny beats an allow.

The rule data types live in "Ecluse.Rules.Types".
-}
module Ecluse.Rules ( ... ) where
```

**We do not use the `Module:/Copyright:/License:/Maintainer:/…` header fields.**
Those are ceremony for libraries published standalone to Hackage; Écluse is one
application whose licence (`MIT`) lives in the cabal file and `LICENSE`, so
repeating it atop every module is bloat. A plain prose header is the convention
here. (If we ever split a module set out as its own Hackage package, add the
fields there — in their fixed order — and nowhere else.)

**Structure a long header with subsection headings.** When a header covers
several distinct points, `==` / `===` headings render as real, quick-jump
sections and read far better than one block of prose. Keep a short header a
single lead paragraph (optionally a `*` bullet list). Pick one shape per header.

---

## 6. Documenting declarations

**Functions, with per-argument docs.** Annotate the contract the signature can't
state — here, totality that is *load-bearing* (a crash would take down the gate),
not a reflexive "pure and total" tag (STYLE.md §9.2):

```haskell
{- | Evaluate a single rule against a single package version. Total — a
malformed rule or package yields an outcome, never an exception, so hostile
metadata cannot crash the gate.
-}
evalRule
    :: EvalContext     -- ^ Ambient inputs (the current time, …)
    -> Rule            -- ^ The rule to apply
    -> PackageDetails  -- ^ The version under evaluation
    -> RuleOutcome
```

**Sum types — a `-- |` per constructor.** This is where Écluse's domain knowledge
lives, so document each one (and the *why*, §10):

```haskell
data Rule
    = -- | Unconditionally allow every package under the given scope.
      AllowScope Scope
    | -- | Allow a version only if it was published at least this long ago.
      -- Guards against race-to-publish attacks: an attacker publishes a
      -- malicious version hoping it is consumed before takedown.
      AllowIfPublishedBefore NominalDiffTime
    deriving stock (Eq, Show)
```

**Records — a `-- ^` per field**, carrying units and invariants:

```haskell
data PackageDetails = PackageDetails
    { pkgName :: PackageName
    , pkgPublishedAt :: Maybe UTCTime
    -- ^ When this version was published, if the registry reports it.
    , pkgInstallCode :: CodeExecSignal
    -- ^ Whether the version runs code at install time.
    }
    deriving stock (Eq, Show)
```

---

## 7. Organising a module for navigation

**Group the export list with section headings** (`-- *`, `-- **`). They become
the page's table of contents — the single biggest aid to a newcomer. (See also
[`STYLE.md`](STYLE.md) → "Exports".)

```haskell
module Ecluse.Package (
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

This is the whole everyday vocabulary. Reach for markup only where it changes the
render, and match how a sibling module does it.

| Want | Write | Renders as |
|---|---|---|
| Link an identifier | `'mkScope'` | a link to `mkScope` |
| Link a module | `"Ecluse.Rules.Types"` | a link to the module |
| Inline code / a literal | `@"1.2.3"@` | `1.2.3` in monospace |
| Load-bearing emphasis | `__deny by default__` | **deny by default** |
| Italic (sparingly) | `/why/` | *why* |
| A bulleted list | lines starting `*` after a blank line | • items |
| A runnable example | `>>> expr` then its output | an Examples block (§9) |
| Export-list section | `-- * Scopes` | a doc section (§7) |
| Header subsection | `== Conventions` | a heading (§5) |

**Don't over-escape.** Only `@`, `<`, and `>` are active characters (plus `'`
when wrapped tightly around an identifier). Write prose apostrophes and
punctuation **bare** — `npm's`, `the package's name`, `a/b` — never `npm\'s`.
Escaping ordinary prose is the noise that makes a page look untrustworthy. To
show an active character literally, escape just it: `\@`, `\<`, `\>`. If a
literal needs a thicket of backslashes to render, rephrase instead.

---

## 9. Examples that run (doctest)

Prefer a `>>>` example to a paragraph — it teaches faster and, because we run it,
it cannot rot. An example is the expression plus its expected output:

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

- **`make doctest`** runs every `>>>` example as a test, and the CI gate runs it
  too — so an example that disagrees with the code fails the build. It works via
  `cabal repl --with-ghc=doctest`, which inherits the package's exact build
  configuration (crucially the `relude` prelude), so examples see the same names
  the module does.
- **Keep examples pure, total, and deterministic** — they run in a plain GHCi
  session with no `IO` setup. The pure core (`Ecluse.Rules`, `Ecluse.Version`,
  `Ecluse.Package`) is the natural home for them.
- Output is compared to GHCi's printed form: a `Text` shows **with quotes**
  (`"7 days"`).
- Haddock also supports `prop>` properties, but Écluse pins invariants with
  `hedgehog` in the test suites (see [`CONTRIBUTING.md`](CONTRIBUTING.md) →
  "Testing Strategy")—use `>>>` here for illustration, not for property testing.

---

## 10. Explain the *why* — especially the security rationale

Écluse is a supply-chain resilience tool. A comment that explains the **threat a
rule defends against** (as `AllowIfPublishedBefore` does in §6) is worth far more
than one describing the mechanics. This is the single most valuable thing a
Haddock comment here can carry, because it is exactly what a type signature, a
test, or a later reader cannot reconstruct on their own.

---

## 11. Document the code, not the project

Haddock is the durable contract, read long after any PR. Keep project-management
narration out of it:

- **No status or roadmap** — drop "for now", "currently", "at launch", "a later
  slice will…". Describe what *is*, in the present tense.
- **No slice / PR / issue references** — "(see S07)", "added in #42",
  "TODO(after the spike)" belong in git history and
  [`planning/`](planning/), not the source.
- **No test-plumbing narration** — document a test double where it is defined, by
  what it does, not in the production module it stands in for.

The test: if a sentence would read as false or pointless a year from now — once
the "later" work has landed — it is project narration. Cut it. The model, the
decisions, and the security *why* stay.

---

## 12. Tooling & workflow

```sh
make docs       # build the hyperlinked, searchable Haddock site and open it
make doctest    # run every >>> example as a test (also in the CI gate)
make check      # the full local gate — includes both of the above
```

**Read the rendered page, not just the source.** A markup slip is invisible in
the source. After writing or reworking a header, run `make docs` and look: every
`'identifier'` / `"Module"` link resolves, code spans are closed, no stray `@` or
backslash leaks into prose, and headings nest as intended.

---

## 13. A worked example

Everything above, in one small module:

```haskell
{- | Circle geometry for the layout engine.

Lengths are unit-less; this module is pure and performs no 'IO'. For the wider
geometry vocabulary see "Acme.Geometry".
-}
module Acme.Geometry.Circle (
    -- * The Circle type
    Circle (..),
    -- * Construction
    circleFromRadius,
    -- * Measurements
    area,
) where

-- | A circle, defined by its radius. The centre is irrelevant to the
-- measurements here, so it is not stored.
newtype Circle = Circle
    { radius :: Double  -- ^ Radius; always non-negative (see 'circleFromRadius').
    }
    deriving stock (Eq, Show)

-- | Build a 'Circle', rejecting a negative radius.
--
-- >>> circleFromRadius 2
-- Just (Circle {radius = 2.0})
--
-- >>> circleFromRadius (-1)
-- Nothing
circleFromRadius
    :: Double        -- ^ Desired radius
    -> Maybe Circle  -- ^ 'Nothing' when the radius is negative
circleFromRadius r
    | r < 0 = Nothing
    | otherwise = Just (Circle r)

-- | The area enclosed by the circle.
--
-- >>> area (Circle 1)
-- 3.141592653589793
area :: Circle -> Double
area (Circle r) = pi * r * r
```

---

## 14. Checklist (before you open a PR)

- [ ] Every new module has a prose `{- | … -}` header saying what it is for.
- [ ] Every **exported** type and function has a Haddock comment (≥ 1 line);
      sum constructors and record fields are documented where they carry meaning.
- [ ] The *why* is captured — especially the security rationale (§10).
- [ ] No restating of signatures; no Haddock on non-exported helpers; no
      project / PR / status narration (§11).
- [ ] Markup is minimal and unescaped in prose (§8).
- [ ] Non-obvious behaviour has a runnable `>>>` example; `make doctest` passes.
- [ ] `make docs` builds clean and the rendered page was eyeballed (§12).
