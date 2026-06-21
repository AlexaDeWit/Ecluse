# Haskell Style Guide

This is the coding-style reference for **Écluse** (package `ecluse`). It is
deliberately verbose and example-driven so that both newcomers to Haskell and
coding agents can follow it without guesswork.

It covers *how code is written* — formatting, documentation, naming, function
design. For *where code lives* (module layout, the `Ecluse.<Area>` namespacing,
the `.Types` split), see [`CONTRIBUTING.md`](CONTRIBUTING.md) → "Codebase
Layout". For *why the stack is what it is* (relude, raw WAI, the effect style),
see [`docs/architecture.md`](docs/architecture.md).

> **The golden rule: when in doubt, match the nearest existing module.**
> `src/Ecluse/Package.hs` and `src/Ecluse/Rules.hs` are the reference
> implementations of this guide. Read one before adding code beside it.

---

## Guiding principle: Simple Haskell

Écluse follows the [**Simple Haskell**](https://www.simplehaskell.org/)
philosophy: prefer the boring, readable subset of the language over clever or
advanced features. The goal is code that an engineer who is *new to Haskell* can
read, reason about, and change safely — and that an automated agent can extend
without inventing exotic machinery. Simplicity here is a feature, not a
limitation.

In practice:

- **Favour concrete types and plain functions** over type-level programming.
  Prefer a sum type or a record to a fancy generic encoding.
- **Be conservative with language extensions** — see §2. The simpler the
  language you write in, the smaller the surface a reader must learn.
- **Avoid the advanced toolbox unless it clearly pays for itself:** heavy
  type-level features (`GADTs`, `TypeFamilies`, `DataKinds`), deep typeclass
  hierarchies and `MultiParamTypeClasses`/`FunctionalDependencies`, lawless or
  overly abstract typeclasses, and `TemplateHaskell` (which has two sanctioned
  exceptions — see §2). Each adds power but also cognitive load and compile-time
  cost; reach for one only when the simple encoding is genuinely worse, and
  justify it (see §2).
- **Optimise for the reader, not the writer.** A few more lines of obvious code
  beats a terse abstraction that takes a page of types to understand.

This principle outranks personal taste. If a clever solution and a boring one
both work, the boring one wins.

---

## 1. Formatting is mechanical — never do it by hand

Layout (indentation, commas, line breaks, import alignment) is owned entirely by
**fourmolu**, configured by [`fourmolu.yaml`](fourmolu.yaml). Do not argue with
it and do not format by hand.

```sh
make format        # reformat in place — run this before committing
make format-check  # the gating check (fails if anything is unformatted)
```

`fourmolu.yaml` pins fourmolu's defaults explicitly (4-space indent, leading
commas, diff-friendly import/export lists) so the rules are visible and cannot
drift between tool versions. The dev shell pins the fourmolu binary itself.

Likewise, a large class of correctness and simplification issues is owned by
**hlint** ([`.hlint.yaml`](.hlint.yaml)): `make lint`. Apply its suggestions
rather than overriding them.

Because these two tools decide formatting and many idioms, the rest of this
guide is about the things a tool *cannot* decide for you: documentation,
naming, decomposition, and totality.

---

## 2. Language baseline

- **GHC2021** is the language edition (set in `ecluse.cabal`). It already
  enables common, uncontroversial extensions — notably `ImportQualifiedPost`
  (postpositive `qualified`, see §8).
- **Default extensions** are declared once in the cabal `common` stanza, not
  per-file: `DerivingStrategies`, `LambdaCase`, `OverloadedStrings`. Reach for a
  per-module `{-# LANGUAGE #-}` pragma only for something genuinely local; if an
  extension becomes common, promote it to the cabal stanza instead.
- **Keep the extension set small (Simple Haskell).** GHC2021 plus the three
  above is the baseline. Mild, widely-used conveniences (e.g.
  `RecordWildCards`, `TupleSections`, `MultiWayIf`, `DerivingVia`) are fine when
  they make code *simpler*. But the **advanced** extensions — `GADTs`,
  `TypeFamilies`, `DataKinds`, `MultiParamTypeClasses`,
  `FunctionalDependencies`, `TemplateHaskell`, `UndecidableInstances`, and
  similar — are opt-in per case: enable one only when the simpler encoding is
  genuinely worse, and **justify it in a comment and the PR description**. When
  two designs work, pick the one needing fewer extensions.
- **`TemplateHaskell` — two sanctioned uses.** TH is accepted *without*
  per-case justification for exactly two things:
  1. **Deriving `aeson` instances** (e.g. `deriveJSON` / TH-generated
     `FromJSON`/`ToJSON`), where it removes large amounts of decoder boilerplate.
     (Plain `DeriveGeneric`-based deriving is still the simplest default; reach
     for the TH form when you need its field-options control.)
  2. **Generating optics/lenses** (`makeLenses` and friends). Lenses are a bit
     opaque to *build* via TH, but they make *working with* nested records far
     simpler than manual nested pattern matches and record updates — a good
     Simple-Haskell trade. Lenses are encouraged for that purpose; keep their
     use idiomatic and shallow rather than building elaborate optic towers.

  Any *other* TH use still needs the justification above.
- **`relude` is the prelude**, wired in transparently via cabal mixins (see
  architecture doc). Practical consequences you must internalise:
  - **`Text`, not `String`.** String literals are `Text` via `OverloadedStrings`;
    concatenate with `<>`. Use `putTextLn` (not `putStrLn`).
  - **Partial functions are hidden by default** — relude removes the partial
    `head`, `tail`, `fromJust`, etc. Keep it that way (see §6, Totality).
  - `containers`, `text`, `bytestring`, and `stm` are re-exported, so common
    types like `Map` are in scope without an import.

---

## 3. Compiler flags

The warning set lives in the `common` stanza of `ecluse.cabal`; **warnings are
errors** for our package, set in [`cabal.project`](cabal.project) (`-Werror`).
A clean build is therefore a hard requirement, not a suggestion.

Enabled on top of `-Wall`, each reinforcing a rule in this guide:

| Flag | Guards |
|------|--------|
| `-Wcompat` | Upcoming breaking changes — fix them early. |
| `-Widentities` | Redundant numeric/`id` conversions. |
| `-Wincomplete-record-updates` | Record updates that could fail. |
| `-Wincomplete-uni-patterns` | Partial patterns in lambdas/`let` (totality). |
| `-Wmissing-deriving-strategies` | Forces the `deriving stock` style of §7. |
| `-Wmissing-export-lists` | Forces explicit export lists (§5). |
| `-Wpartial-fields` | Partial record selectors on sum types. |
| `-Wredundant-constraints` | Constraints a signature does not use. |

Deliberately **not** enabled: `-Wunused-packages` (the relude mixin defeats its
attribution), `-Wmissing-import-lists` (we use open imports for internal
modules, §8), and `-Wmissing-local-signatures` (`where`-helpers may rely on
inference). Test suites add `-Wno-missing-export-lists` because `hspec-discover`
generates a `Main` without one.

If a refactor makes warnings unbearable for a moment, relax locally with an
untracked `cabal.project.local` (see the comment in `cabal.project`) — never by
weakening the committed flags.

---

## 4. Modules and exports

Module *layout* is covered in `CONTRIBUTING.md`. Two style points belong here:

**Every module has an explicit export list** (enforced by
`-Wmissing-export-lists`). The list is the module's public contract; everything
absent is private and free to change.

**Group large export lists with Haddock section headers** (`-- *`). They become
the structure of the generated documentation and a table of contents for the
reader:

```haskell
module Ecluse.Package (
    -- * Scopes
    Scope,
    mkScope,
    unScope,
    renderScope,

    -- * Package identity
    PackageName (..),
    renderPackageName,
    -- ...
) where
```

Export an abstract type as `Scope` (constructor hidden — callers must use the
smart constructor) or as `PackageName (..)` (constructors and fields exposed)
deliberately; the choice encodes whether the type has invariants to protect
(see §6).

---

## 5. Documentation (Haddock)

Documentation is not optional here, and it is the rule agents most often skip.
We write Haddock so the *why* survives, and so the generated docs
(`cabal haddock`) are a usable manual.

**Rule 5.1 — Every module opens with a Haddock header** (`{- | … -}`) that says
what the module is for and how it fits the system. State the model and the
important decisions, not a restatement of the type names. Cross-reference
sibling modules and the architecture doc.

```haskell
{- | The policy rules engine.

A rule set is evaluated against a single 'PackageDetails' snapshot to produce a
'Decision'. The model is __deny by default__: a package is allowed only if some
rule explicitly allows it __and no rule denies it__.

The rule data types live in "Ecluse.Rules.Types".
-}
module Ecluse.Rules (
    -- ...
```

**Rule 5.2 — Every exported type and function has a Haddock comment.** Describe
the contract and any total/partial, ordering, or precedence guarantees — the
things a type signature cannot express.

```haskell
-- | Evaluate a single rule against a single package version. Pure and total.
evalRule :: EvalContext -> Rule -> PackageDetails -> RuleOutcome
```

**Rule 5.3 — Document data declarations field-by-field and
constructor-by-constructor.** Records use `-- ^` on each field; sum types put a
`-- |` on each constructor. This is where domain knowledge lives.

```haskell
data Rule
    = -- | Unconditionally allow every package under the given scope.
      AllowScope Scope
    | -- | Allow a version only if it was published at least this long ago.
      -- Guards against race-to-publish supply-chain attacks where an attacker
      -- publishes a malicious version and hopes it is consumed before takedown.
      AllowIfPublishedBefore NominalDiffTime
    deriving stock (Eq, Show)

data PackageDetails = PackageDetails
    { pkgName :: PackageName
    , pkgPublishedAt :: UTCTime
    -- ^ When this version was published to the source registry.
    , pkgHasInstallScripts :: Bool
    -- ^ Whether the version declares install / pre- / post-install scripts.
    }
    deriving stock (Eq, Show)
```

**Rule 5.4 — Explain the *why*, especially the security rationale.** Écluse is a
supply-chain resilience tool; a comment that explains the threat a rule defends
against (as `AllowIfPublishedBefore` does above) is worth more than one
describing the mechanics.

**Rule 5.5 — Use Haddock markup consistently:**
- `'identifier'` links to a function or type — `produces a 'Decision'`.
- `"Module.Name"` links to a module — `see "Ecluse.Rules.Types"`.
- `@code@` for inline code/literals — `@"1.2.3"@`; escape special characters
  (`@\@scope\/name@`).
- `__bold__` for load-bearing emphasis (`__deny by default__`); `/italic/`
  sparingly.
- `-- * Heading` for export-list sections (§4).

---

## 6. Naming and domain types

**Rule 6.1 — Wrap domain values in newtypes; keep them opaque when they have an
invariant.** A `Scope` is not just `Text`; making it its own type stops it being
mixed up with a package name and gives one place to enforce normalisation.

**Rule 6.2 — Give an opaque type the `mk*` / `un*` / `render*` trio:**

- `mkX :: Raw -> X` — the smart constructor; the *only* way to build an `X`, so
  it can enforce the invariant.
- `unX :: X -> Raw` — the plain accessor back to the underlying value.
- `renderX :: X -> Text` — the canonical wire / display form (which may differ
  from the stored form).

```haskell
newtype Scope = Scope Text
    deriving stock (Eq, Ord, Show)

-- | Build a 'Scope', tolerating an optional leading @\'@\'@.
mkScope :: Text -> Scope
mkScope raw = Scope (fromMaybe raw (T.stripPrefix "@" raw))

unScope :: Scope -> Text          -- bare value
renderScope :: Scope -> Text      -- wire form, here with the leading '@'
```

Keep types as small as the domain requires and no smaller-than-honest: `Version`
is intentionally an opaque `Text` wrapper that does **not** parse semver, with a
comment saying so, until a rule actually needs ordering.

**Rule 6.3 — Prefix record fields with a short type tag** so selectors are
unambiguous at the use site and across modules: `pkgName`, `pkgVersion`,
`pkgHasInstallScripts` for `PackageDetails`; `distTarball` for `Dist`; `ctxNow`
for `EvalContext`.

**Rule 6.4 — Names read as domain language.** Constructors are verbs/phrases of
intent (`AllowScope`, `DenyHasInstallScripts`, `DeniedByDefault`); booleans and
predicates read as assertions (`pkgHasInstallScripts`, `isAllow`).

---

## 7. Data types and deriving

**Always name the deriving strategy** (`-Wmissing-deriving-strategies` enforces
it). In practice that means `deriving stock (Eq, Show, Ord)` for ordinary data
types, and `deriving newtype` when you genuinely want the wrapped type's
instance.

```haskell
newtype EvalContext = EvalContext
    { ctxNow :: UTCTime
    }
    deriving stock (Eq, Show)
```

Model decisions and outcomes as **sum types**, not booleans or stringly-typed
flags — `RuleOutcome = Allow Text | Deny Text | Abstain Text` makes the three
cases (and the audit reason carried with each) explicit and total to pattern
match on.

---

## 8. Imports

- **Qualified imports are postpositive** (`ImportQualifiedPost`, from GHC2021):

  ```haskell
  import Data.Text qualified as T
  import Data.Map.Strict qualified as Map
  ```

- **Use the conventional aliases:** `T` for `Data.Text`, `Map` for
  `Data.Map.Strict`. Qualify anything whose unqualified names would collide or
  mislead (`T.intercalate`, `Map.empty`).
- **Open (unqualified, unrestricted) imports are fine for our own internal
  modules** — `import Ecluse.Package` — because we control those names. This is
  why `-Wmissing-import-lists` is off. For third-party modules, prefer a
  qualified import or an explicit import list.
- Let fourmolu order and align imports; do not hand-sort.

---

## 9. Function design

**Rule 9.1 — Functions are small and do one thing.** Build complex behaviour by
*composing* small, named, individually-understandable pieces rather than writing
one large function. If a function needs a paragraph to explain its middle, split
the middle out.

**Rule 9.2 — Prefer pure and total.** Keep the core logic (the rules engine,
parsers, rendering) pure; push `IO` to the edges (`app/Main.hs`, the server and
worker layers). Annotate guarantees you rely on (`-- … Pure and total.`). The
effect style for the parts that *are* effectful is `ReaderT Env IO`
(architecture doc) — handlers take `Env` and run in plain `IO`.

**Rule 9.3 — Use local `where` helpers** to name sub-steps and keep the main
equation readable. Top-level bindings always have a signature; `where`-helpers
have one when it aids clarity (they are exempt from the missing-signature
warning, but a non-trivial helper such as a typed accumulator should still get
one).

```haskell
renderDuration :: NominalDiffTime -> Text
renderDuration d =
    let secs = max 0 (round (realToFrac d :: Double)) :: Integer
     in pick units secs
  where
    units :: [(Text, Integer)]
    units = [("day", 86400), ("hour", 3600), ("minute", 60)]

    pick [] secs = plural secs "second"
    pick ((unit, size) : rest) secs
        | secs >= size = plural (secs `div` size) unit
        | otherwise = pick rest secs

    plural n unit = show n <> " " <> unit <> (if n == 1 then "" else "s")
```

**Rule 9.4 — Dispatch on a sum type with `LambdaCase`** when the argument is
only there to be matched:

```haskell
ruleName :: Rule -> Text
ruleName = \case
    AllowScope{} -> "AllowScope"
    AllowIfPublishedBefore{} -> "AllowIfPublishedBefore"
    DenyHasInstallScripts -> "DenyHasInstallScripts"
```

Match every constructor explicitly (no wildcard) when you want the compiler to
flag you the day a new constructor is added — useful for exhaustive logic like
`ruleName` and `evalRule`.

---

## 10. Totality — no partial functions

A resilience proxy must not crash on hostile input, so **partial functions are
banned** (enforced by `.hlint.yaml`; most are already hidden by relude). Do not
reach for `head`, `tail`, `fromJust`, `read`, `(!!)`, `error`, `undefined`, or
`unsafePerformIO`.

Instead:
- pattern-match and handle the empty/missing case, or
- use the total alternative: `fromMaybe`, `listToMaybe`, `readMaybe`,
  `viaNonEmpty head`, `Map.lookup`, `(!!?)`.

Represent "this might not exist" in the type (`Maybe`, `Either`, a sum type),
and let the caller decide — see `Decision`/`RuleOutcome`, which carry a human
reason for every branch precisely so failures are explainable rather than
thrown.

---

## 11. Tests

Tests are documentation too; keep them as readable as the code. (Layout and the
three-tier strategy are in `CONTRIBUTING.md`; this is style.)

- **Structure with `hspec`**: `describe` per function/area, `it` with a
  full-sentence expectation.

  ```haskell
  describe "evalRule" $ do
      it "AllowScope allows a matching scope" $
          evalRule ctx (AllowScope (mkScope "myorg")) (pkg (Just "myorg") 0)
              `shouldSatisfy` isAllow
  ```

- **Name fixtures and helpers, and give them signatures** — `now :: UTCTime`,
  `pkg :: Maybe Text -> Integer -> PackageDetails`. A small builder like `pkg`
  that fills defaults and exposes only the axis under test keeps each case to
  one line.
- **Add small predicate/extractor helpers** instead of inlining pattern matches
  in assertions: `isAllow`, `approvedBy`, `deniedBy`.
- **Express invariants as `hedgehog` properties**, grouped under
  `describe "properties"`, using `forAll` generators and `(===)`. Properties are
  where the rules engine's guarantees (deny-by-default, deny-precedence,
  first-allow-wins) are pinned down.
- Comment a non-obvious case with the reasoning it encodes (as the
  first-allowing-rule test does).

---

## 12. Checklist (before you open a PR)

- [ ] `make format` run; `make check` is green (build with `-Werror`, unit
      tests, fourmolu, hlint, Semgrep).
- [ ] Every new module has a Haddock header; every exported type and function
      has a Haddock comment; record fields and sum constructors are documented.
- [ ] New domain values are newtypes; opaque ones expose `mk*`/`un*`/`render*`
      as appropriate.
- [ ] No partial functions; no `error`/`undefined`/`unsafePerformIO`.
- [ ] Functions are small and composed; logic is pure where it can be.
- [ ] Docs that the change affects (`README.md`, `docs/`, this file) are updated
      in the same commit.
