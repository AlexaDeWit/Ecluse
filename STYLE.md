# Haskell Style Guide

This is the coding-style reference for **Écluse** (package `ecluse`). It is
deliberately verbose and example-driven so that both newcomers to Haskell and
coding agents can follow it without guesswork.

It covers *how code is written*, formatting, documentation, naming, function
design. For *where code lives* (module layout, the `Ecluse.<Area>` namespacing,
the `.Types` split), see [`docs/getting-started.md`](docs/getting-started.md) → "Codebase Layout". For *why the stack is what it is* (relude, raw WAI, the effect style),
see [`docs/architecture.md`](docs/architecture.md).

> **The golden rule: when in doubt, match the nearest existing module.**
> `core/src/Ecluse/Core/Package.hs` and `core/src/Ecluse/Core/Rules.hs` are the reference
> implementations of this guide. Read one before adding code beside it.

---

## Guiding principle: Simple Haskell

Écluse follows the [**Simple Haskell**](https://www.simplehaskell.org/)
philosophy: prefer the boring, readable subset of the language over clever or
advanced features. The goal is code that an engineer who is *new to Haskell* can
read, reason about, and change safely, and that an automated agent can extend
without inventing exotic machinery. Simplicity here is a feature, not a
limitation.

In practice:

- **Favour concrete types and plain functions** over type-level programming.
  Prefer a sum type or a record to a fancy generic encoding.
- **Be conservative with language extensions**; see §2. The simpler the
  language you write in, the smaller the surface a reader must learn.
- **Avoid the advanced toolbox unless it clearly pays for itself:** heavy
  type-level features (`GADTs`, `TypeFamilies`, `DataKinds`), deep typeclass
  hierarchies and `MultiParamTypeClasses`/`FunctionalDependencies`, lawless or
  overly abstract typeclasses, and `TemplateHaskell` (which has two sanctioned
  exceptions; see §2). Each adds power but also cognitive load and compile-time
  cost; reach for one only when the simple encoding is genuinely worse, and
  justify it (see §2).
- **Optimise for the reader, not the writer.** A few more lines of obvious code
  beats a terse abstraction that takes a page of types to understand.

This principle outranks personal taste. If a clever solution and a boring one
both work, the boring one wins.

---

## Guiding principle: Parse, don't validate

Écluse ingests untrusted input from many registries, so it leans hard on Alexis
King's [**"Parse, don't validate"**](https://lexi-lambda.github.io/blog/2019/11/05/parse-don-t-validate/).
The distinction: a *parser* turns less-structured input into more-structured
output and *may fail*, capturing what it learned **in the type**; a *validator*
only checks and returns `()`/`Bool`, throwing that knowledge away so the rest of
the code must re-check the invariant or assume it. We parse.

Concretely:

- **Make illegal states unrepresentable.** Reach for a type that cannot hold the
  bad case, `NonEmpty a` instead of "a list I checked wasn't empty", a sum type
  instead of a `Bool` plus a convention. The type becomes the proof, so the check
  can't be forgotten.
- **Parse once, at the boundary; carry the refined type inward.** "Push the
  burden of proof upward as far as possible, but no further", get input into its
  most precise representation as early as possible (the registry adapter, the
  config loader, the request handler) so nothing downstream re-validates or trips
  over a case that was already ruled out. This is the registry handle in action:
  adapters project wire formats into `Ecluse.Core.Package` types and nothing above
  them sees raw wire data (see §4.4 and `docs/architecture.md`).
- **No shotgun parsing.** Don't scatter input checks through processing logic. That LangSec anti-pattern lets malformed input get partially processed before
  it's rejected, leaving state hard to reason about. Keep a clean parse phase,
  then an execution phase that can trust its inputs.
- **Smart constructors are parsers.** When a type can't *structurally* exclude
  the bad case, make it opaque and expose a smart constructor that returns
  `Maybe`/`Either` of the refined type (§6). A constructor that can reject input
  returns `Either Err T`, never `Bool`; `mkScope`/`mkVersion` are the
  total/normalising form of the same pattern.
- **Distrust `m ()` and `Bool`-returning "checks".** If a function's job is to
  assert something, have it *return the evidence* (the refined value) so call
  sites cannot skip it. "Let your datatypes inform your code, not the other way
  around."

This is the same instinct as §10 (totality): the partial-function bans exist
because a parsed, precise type removes the case that would otherwise crash.

---

## 1. Formatting is mechanical, never do it by hand

Layout (indentation, commas, line breaks, import alignment) is owned entirely by
**fourmolu**, configured by [`fourmolu.yaml`](fourmolu.yaml). Do not argue with
it and do not format by hand.

```sh
make format        # reformat in place, run this before committing
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
  enables common, uncontroversial extensions, notably `ImportQualifiedPost`
  (postpositive `qualified`, see §8).
- **Default extensions** are declared once in the cabal `common` stanza, not
  per-file: `DerivingStrategies`, `LambdaCase`, `OverloadedStrings`. Reach for a
  per-module `{-# LANGUAGE #-}` pragma only for something genuinely local; if an
  extension becomes common, promote it to the cabal stanza instead.
- **Keep the extension set small (Simple Haskell).** GHC2021 plus the three
  above is the baseline. Mild, widely-used conveniences (e.g.
  `RecordWildCards`, `TupleSections`, `MultiWayIf`, `DerivingVia`) are fine when
  they make code *simpler*. But the **advanced** extensions, `GADTs`,
  `TypeFamilies`, `DataKinds`, `MultiParamTypeClasses`,
  `FunctionalDependencies`, `TemplateHaskell`, `UndecidableInstances`, and
  similar, are opt-in per case: enable one only when the simpler encoding is
  genuinely worse, and **justify it in a comment and the PR description**. When
  two designs work, pick the one needing fewer extensions.
- **`TemplateHaskell`, two sanctioned uses.** TH is accepted *without*
  per-case justification for exactly two things:
  1. **Deriving `aeson` instances** (e.g. `deriveJSON` / TH-generated
     `FromJSON`/`ToJSON`), where it removes large amounts of decoder boilerplate.
     (Plain `DeriveGeneric`-based deriving is still the simplest default; reach
     for the TH form when you need its field-options control.)
  2. **Generating optics/lenses** (`makeLenses` and friends). Lenses are a bit
     opaque to *build* via TH, but they make *working with* nested records far
     simpler than manual nested pattern matches and record updates, a good
     Simple-Haskell trade. Lenses are encouraged for that purpose; keep their
     use idiomatic and shallow rather than building elaborate optic towers.

  Any *other* TH use still needs the justification above.
- **`relude` is the prelude**, wired in transparently via cabal mixins (see
  architecture doc). Practical consequences you must internalise:
  - **`Text`, not `String`.** String literals are `Text` via `OverloadedStrings`;
    concatenate with `<>`. Use `putTextLn` (not `putStrLn`).
  - **Partial functions are hidden by default**: relude removes the partial
    `head`, `tail`, `fromJust`, etc. Keep it that way (see §6, Totality).
  - `containers`, `text`, `bytestring`, and `stm` are re-exported, so common
    types like `Map` are in scope without an import.
- **String types, pick by role; strict over lazy by default.**
  - Strict **`Data.Text`** is the working and API default (UTF-8, `text ≥ 2.0`).
    It is what functions take and return unless there is a reason otherwise.
  - Strict **`ByteString`** holds raw bytes (digests, request/response bodies you
    keep). Reserve **lazy `ByteString`** for streaming passthrough, the tarball
    body the proxy relays, never for held state.
  - **`ShortText`** (`text-short`) is for **bulk-stored, equality-only
    identifiers**, values held in quantity that are only compared, keyed, or
    rendered, and never sliced, parsed, or rewritten. Convert only at the `mk` /
    `render` boundary, never in a hot loop (see §6, Rule 6.5).
  - Fall back to **`String`** only at an unavoidable library edge (an API that
    speaks `String`); convert to `Text` at once and keep it out of the core.

---

## 3. Compiler flags

The warning set lives in the `common` stanza of `ecluse.cabal`; **warnings are
errors** for our package, set in [`cabal.project`](cabal.project) (`-Werror`).
A clean build is therefore a hard requirement, not a suggestion.

Enabled on top of `-Wall`, each reinforcing a rule in this guide:

| Flag | Guards |
|------|--------|
| `-Wcompat` | Upcoming breaking changes, fix them early. |
| `-Widentities` | Redundant numeric/`id` conversions. |
| `-Wincomplete-record-selectors` | A partial record selector at the *use* site, catches dependency-defined ones `-Wpartial-fields` cannot see. |
| `-Wincomplete-record-updates` | Record updates that could fail. |
| `-Wincomplete-uni-patterns` | Partial patterns in lambdas/`let` (totality). |
| `-Wmissing-deriving-strategies` | Forces the `deriving stock` style of §7. |
| `-Wmissing-export-lists` | Forces explicit export lists (§5). |
| `-Wname-shadowing` | A local binding that shadows a name already in scope. |
| `-Wpartial-fields` | Partial record selectors on sum types, at the *definition* site. |
| `-Wredundant-bang-patterns` | A `!` pattern that forces nothing (already strict/irrefutable). |
| `-Wredundant-constraints` | Constraints a signature does not use. |

**`-Werror` makes _every_ warning fatal, not only the ones tabled above.** That
includes the full `-Wall` set (unused imports and bindings, incomplete and
overlapping patterns, missing top-level type signatures, type defaulting, …)
*and* relude's own `WARNING` pragmas. In particular **`undefined` and the
`trace*` debugging functions fail to compile** in committed code, via relude's
warnings under `-Werror`, at build time, before the `.hlint.yaml` ban (§10)
even runs. So a `trace` you drop in to debug will break `make build`; remove it
(use `katip` for real logging). `error` is the one relude deliberately leaves
warning-free; see §10 for its policy.

Deliberately **not** enabled: `-Wunused-packages` (the relude mixin defeats its
attribution), `-Wmissing-import-lists` (we use open imports for internal
modules, §8), and `-Wmissing-local-signatures` (`where`-helpers may rely on
inference). Test suites add `-Wno-missing-export-lists` because `hspec-discover`
generates a `Main` without one.

If a refactor makes warnings unbearable for a moment, relax locally with an
untracked `cabal.project.local` (see the comment in `cabal.project`), never by
weakening the committed flags.

---

## 4. Module organisation, namespacing, and exports

This section is the durable how-to for *structuring* modules. The *current*
concrete module list is the module index of the published Haddock (and the root
`Ecluse` synopsis); [`docs/getting-started.md`](docs/getting-started.md) → "Codebase Layout" records the
project-specific layout patterns. The principles below are what decide where new
code goes.

### 4.1 Organise by concept, favouring smaller modules

Group each area's types **and** the functions that operate on them into well-scoped,
single-purpose modules. Historically, this guide recommended "vertical" grouping
at a broad feature level, which led to 900+ line unmanageable monoliths.

Now, we strongly prefer extracting discrete concepts into their own files. If a
file starts doing three different things (e.g., domain types, JSON parsers, and
resolution logic), split it. 

So `Ecluse.Core.Package` can contain just the core identity types, with logic
split into `Ecluse.Core.Package.Logic` or similar, depending on size. Small,
hyper-focused modules are easier to digest, test, and safely change.

As a working threshold drawn from the codebase's own shape: when an edit leaves a
module creeping past about 400 lines (roughly twice the repo median; healthy
capability modules here cluster in the 150-400 band), treat that as the prompt to
seriously consider a split. The deciding test is capability clusters, not the line
count alone: split when the module holds two or more clusters whose definitions do
not reference each other, with the module's own Haddock export groups, the mirrored
spec's describe blocks, and disjoint importer sets as the evidence. A module that is
one cohesive decision stays intact whatever its size; `Ecluse.Core.Security.Host`
(one SSRF decision table under a shared property floor) is the canonical example.

### 4.2 One namespace per area; module name = file path

- Each area of the system gets its own namespace, `Ecluse.Core.<Area>` in the
  capability core (`Rules`, `Registry`, `Queue`, `Security`, …) and `Ecluse.<Area>`
  in the application shell (`Config`, `Env`, `Log`, …). Organise by *feature* (what
  the code is about), not by *layer* (type vs class vs handler).
- GHC requires the module name to match the file path, PascalCase and
  hierarchical: `Ecluse.Core.Rules.Types` ⇄ `core/src/Ecluse/Core/Rules/Types.hs`. The
  compiler enforces this; tests mirror it (`core/test/unit/Ecluse/RulesSpec.hs`, see §12).
- **Prefer small, single-purpose modules.** A module should do one thing well.
  If a module spans multiple concerns (e.g., data types + parsing + domain logic),
  split it. Large modules become dumping grounds that obscure intent.
  (`Ecluse.Config` is a good example of how split domains keep code manageable:
  `Ecluse.Config.Types`, `Ecluse.Config.Rule`, `Ecluse.Config.Parser`.)

### 4.3 Extract `.Types` and `.Helpers` modules freely

Because small, single-purpose modules are the goal, splitting out an
`Ecluse.<Area>.Types` module is completely acceptable and encouraged, especially
when it aids navigation or separates simple data definitions from complex logic
or heavy dependencies (like Aeson parsers).

Typical reasons to split:
1. **Breaking a cyclic import.** Extract the shared data types.
2. **A shared vocabulary**: the types are a stable contract imported by several
   modules.
3. **Size or Clarity**: separating data declarations from implementation makes
   the code more approachable.

When logic grows, do not hesitate to extract `.Types`, `.Parser`, or `.Logic`
modules.

### 4.4 Functional core, effects at the edges

Let the module layout mirror the system's "functional core, imperative shell"
shape (cf. Matt Parsons' *Three Layer Haskell Cake*):

- **Domain/leaf modules are pure**: the rules engine, parsers, renderers
  (`Ecluse.Core.Rules`, `Ecluse.Core.Version`, `Ecluse.Core.Package`). No `IO`; trivially
  testable.
- **Effects live at the boundary**, `app/Main.hs`, the server, and the worker
  layer, which run in `ReaderT Env IO` (see `docs/architecture.md`). Swappable
  effectful backends (registry, queue, credentials) are records of functions
  chosen at a single composition root, the Handle pattern (`CONTRIBUTING.md` /
  architecture).
- **Keep the dependency arrow pointing inward:** pure modules must never import
  the effectful shell.

### 4.5 Put instances with the type or the class, no orphans

Define a typeclass instance in the module that defines the data type, or the one
that defines the class. *Orphan instances* (in a third module) are a maintenance
hazard: they can be silently overlapped and make import order matter. If you need
an instance for a type and a class you don't own, wrap the type in a `newtype`.

### 4.6 Internal modules expose innards without widening the public API

Our domain types are deliberately opaque (e.g. `Scope`'s constructor is hidden;
§6). When a test or an advanced caller genuinely needs the guts, do **not** widen
the public export list. Instead add an `Ecluse.<Area>.Internal` module that
exports everything, and have the public module re-export only the curated, stable
surface. Importing `.Internal` is, by convention, opting out of our stability
promises, the same pattern the `text` and `bytestring` libraries use.

### 4.7 Exports

**Every module has an explicit export list** (enforced by
`-Wmissing-export-lists`). The list is the module's public contract; everything
absent is private and free to change.

**Group large export lists with Haddock section headers** (`-- *`). They become
the structure of the generated documentation and a table of contents for the
reader:

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
    pkgEcosystem,
    renderPackageName,
    -- ...
) where
```

Export an abstract type with its constructor hidden, `Scope`, or `PackageName`
(callers build it with `mkPackageName` and read it through the exposed
accessors), or with its constructors and fields exposed, as `PackageDetails
(..)`, deliberately; the choice encodes whether the type has invariants to
protect (see §6), and pairs with the `.Internal` escape hatch in 4.6.

### 4.8 Delete superseded code when a replacement lands

When a new implementation replaces an old one, **delete the old one in the same
change**; do not leave a parallel, unreferenced version in place. Dead code is not
free. A fix applied to the dead copy is a silent no-op, it drifts from the live
path, and it misleads a reader into trusting a code path that never runs. This is
worse the more authoritative the code looks (a "the sanctioned way to build X"
module) or the more security-relevant it is: dead code that appears load-bearing is
worse than an obvious gap.

Dead code is what has **no caller and no intended future caller**: a handle field
nothing invokes, a module only its own tests reach, or a helper a better one has
replaced. "We might need it later" is not an intended caller. Git history is the
archive, not an unreferenced definition. If the migration is only partial (the old
path still has live callers), finish it, or state the remaining callers explicitly
at the definition; never leave two live-looking paths silently.

The compiler will not always catch this: an **exported** unused definition, a dead
record field, or a field bound only to refusing test stubs all compile clean under
`-Werror`. Judge reachability from the live composition root, not from whether the
code builds.

---

## 5. Documentation (Haddock)

Documentation is not optional here, and it is the rule agents most often skip.
Its conventions have their own focused, example-driven reference, **[`HADDOCK.md`](HADDOCK.md)**, which you should read before writing doc
comments. The essentials:

- **Every module** opens with a prose `{- | … -}` header saying what it is for
  and how it fits the system. **Every exported type and function** gets a Haddock
  comment; sum constructors and record fields are documented where they carry
  domain meaning. Non-exported helpers get a plain `--` comment at most, never
  Haddock.
- **Document the *why*, not the *what*** the signature already states. This is especially true for the security rationale of a rule, the most valuable thing a comment
  here can carry.
- **Keep Haddock free of project narration**. No status/roadmap, no slice / PR /
  issue references. It is the durable contract, read long after any PR.
- **Examples run.** Prefer a `>>>` example to prose; `make doctest` (part of the
  CI gate) executes them, so they cannot drift from the code.
- **Markup is minimal.** Only `@`, `<`, `>` (and a tight `'identifier'`) are
  active; never escape prose apostrophes or punctuation. After a doc change, read
  the rendered page (`make docs`), not just the source.

The full guide, terminology, the markup table, per-declaration examples, the
anti-bloat rules, doctest, and a worked module, is in
[`HADDOCK.md`](HADDOCK.md).

---

## 6. Naming and domain types

**Rule 6.1, Wrap domain values in newtypes; keep them opaque when they have an
invariant.** A `Scope` is not just `Text`; making it its own type stops it being
mixed up with a package name and gives one place to enforce normalisation. This
is "parse, don't validate" in practice (see the guiding principle): the opaque
type plus its smart constructor *is* the parser, and downstream code receives a
value that already carries its invariant.

**Rule 6.2, Give an opaque type the `mk*` / `un*` / `render*` trio:**

- `mkX :: Raw -> X`, the smart constructor; the *only* way to build an `X`, so
  it can enforce the invariant.
- `unX :: X -> Raw`, the plain accessor back to the underlying value.
- `renderX :: X -> Text`, the canonical wire / display form (which may differ
  from the stored form).

```haskell
-- 'Scope' is an equality-only identifier, so it is stored as ShortText and the
-- Text <-> ShortText conversion happens once, here at the boundary (see 6.5).
newtype Scope = Scope ShortText
    deriving stock (Eq, Ord, Show)

-- | Build a 'Scope', tolerating an optional leading @\@@ sigil.
mkScope :: Text -> Scope
mkScope raw = Scope (TS.fromText (fromMaybe raw (T.stripPrefix "@" raw)))

unScope :: Scope -> Text          -- bare value
renderScope :: Scope -> Text      -- wire form, here with the leading '@'
```

Keep types as small as the domain requires and no smaller-than-honest: `Version`
is intentionally an opaque `Text` wrapper that does **not** parse semver, with a
comment saying so, until a rule actually needs ordering.

**Rule 6.3, Prefix record fields with a short type tag** so selectors are
unambiguous at the use site and across modules: `pkgName`, `pkgVersion`,
`pkgHasInstallScripts` for `PackageDetails`; `distTarball` for `Dist`; `ctxNow`
for `EvalContext`.

**Rule 6.4, Names read as domain language.** Constructors are verbs/phrases of
intent (`AllowScope`, `DenyInstallTimeExecution`, `DeniedByDefault`); booleans and
predicates read as assertions (`pkgHasInstallScripts`, `isAllow`).

**Rule 6.5, Store bulk equality-only identifiers as `ShortText`, converting only
at the boundary.** When an identifier is held in quantity, repeated across every
version of a packument, or every entry of a dependency list, and is only ever
compared, used as a `Map`/`Hashable` key, or rendered (never sliced, parsed, or
rewritten), store it as `ShortText` rather than `Text`. It is more compact and has
no slice-sharing surprises. Do the conversion *once* at the type's boundary: `mkX`
does the single `Text -> ShortText` (`Data.Text.Short.fromText`), and
`unX` / `renderX` the single `ShortText -> Text` (`toText`). Derive `Eq` / `Ord` /
`Hashable` so interior compares, dedup, and `Map` keys run `ShortText`-native with
no conversion. The discipline that earns the win: **never convert in a hot loop**
(per-version, per-dependency, per-rule), a `renderX`/`unX` on a bulk identifier
inside an inner loop defeats the purpose, so reach for `Eq`/`Ord` on the value
instead. If a value is *ever* sliced, parsed, pattern-matched, or rewritten after
construction (a URL rewritten at serve, an SRI digest parsed, a version range),
keep it `Text`, the conversion churn is not worth it and the value is not
equality-only. `Scope` and `PackageName`'s `pkgCanonical`/`pkgDisplay` are `ShortText`;
`Hash.hashValue` and `Artifact.artUrl` stay `Text`.

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
flags, `RuleOutcome = Allow Text | Deny Text | Abstain Text` makes the three
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
  modules**, `import Ecluse.Core.Package`, because we control those names. This is
  why `-Wmissing-import-lists` is off. For third-party modules, prefer a
  qualified import or an explicit import list.
- Let fourmolu order and align imports; do not hand-sort.

---

## 9. Function design

**Rule 9.1, Functions are small and do one thing.** Build complex behaviour by
*composing* small, named, individually-understandable pieces rather than writing
one large function. If a function needs a paragraph to explain its middle, split
the middle out.

Three tripwires demand a second look, never an automatic split: a body past
roughly 25 lines, nesting past three levels, or a `where` block that outweighs
the equation it serves. At a tripwire, ask *why* the function is long.
Essential length is fine: an exhaustive per-constructor dispatch (§9.4) or a
flat, linear sequence of steps runs long because the domain has that many cases
or steps, and splitting it only scatters what belongs together. Incidental
length is the target: a body that interleaves concerns (validating, deciding,
rendering, performing effects) is long because structure is missing, so name
the concerns and split along them. The reader's test for the result: the parent
reads as a short sequence of named steps, and each step is comprehensible on
its own.

**Rule 9.2, Prefer pure and total.** Keep the core logic (the rules engine,
parsers, rendering) pure; push `IO` to the edges (`app/Main.hs`, the server and
worker layers). Annotate a purity/totality guarantee **only where it is
surprising or load-bearing**, such as a boundary parser a reader would expect to throw, or a totality that carries domain meaning (`mkVersion` never dropping a version; `evalRule` never crashing the gate on hostile metadata). Do **not** tag `-- … Pure and total.` reflexively: in a module whose header already says it is pure, or on a signature with no `IO` and a total return type, the tag only restates the header and the type ([`HADDOCK.md`](HADDOCK.md) §3). The effect style for the parts that *are* effectful is `ReaderT Env IO` (architecture doc); handlers take `Env` and run in plain `IO`.

**Rule 9.3, Use local `where` helpers** to name sub-steps and keep the main
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

A `where` helper earns its place by borrowing the parent's context and staying
small: a few lines each, and the block as a whole smaller than the equation it
supports. A helper that grows past roughly ten lines, nests its own control
flow, or reads as a step with standalone meaning has outgrown the block; lift
it (§9.5).

**Rule 9.4, Dispatch on a sum type with `LambdaCase`** when the argument is
only there to be matched:

```haskell
ruleName :: Rule -> Text
ruleName = \case
    AllowScope{} -> "AllowScope"
    AllowIfOlderThan{} -> "AllowIfOlderThan"
    DenyInstallTimeExecution -> "DenyInstallTimeExecution"
```

Match every constructor explicitly (no wildcard) when you want the compiler to
flag you the day a new constructor is added, useful for exhaustive logic like
`ruleName` and `evalRule`.

**Rule 9.5, Lift a non-capturing helper to the top level.** A local `where` or
`let` binding earns its place by *closing over* something in the enclosing
equation: a parameter, the scrutinee, an intermediate result. When a helper refers
only to its own arguments and module-level names, so it captures nothing local,
make it a top-level function instead. A top-level function is independently
testable, shows up by name in a profile or a stack, and reads without the reader
scanning the parent to see what it captured. The §9.3 helpers (`pick`, `plural`,
`units`) earn their `where` because they close over the parent's values; reserve
local bindings for that. A pure formatter such as a log-line builder, which touches
only its arguments, belongs at the top level, where a test can pin its output
directly rather than reaching it through the effectful caller that happened to
enclose it.

Lift a *capturing* helper too when it has outgrown the block (§9.3): pass the
values it captured as explicit parameters and keep it unexported. Two limits
guard the trade. First, the parameter list must travel light, roughly four or
fewer; a helper that needs more context than that reads worse lifted than
nested, so restructure the parent instead (often by bundling the step's inputs
in a small purpose-built record). Second, the lift must not change what the
program computes: pass the *computed value*, never repeat the expression that
computed it (a `where` binding used in two places is computed once; duplicating
its expression at two call sites recomputes it, which can change space or even
termination), and leave the parent's effect order exactly as it was. An
instance method body is a function like any other: past a few lines, delegate
to a named top-level function.

**Rule 9.6, Extraction pays rent.** An extraction is justified when the new
name and signature let the reader *skip the body*: the parent becomes a
sequence of steps a reader can trust without chasing them. If the only honest
name for the piece needs an "and", the boundary is wrong; find a different
split. If the piece is single-use glue that only makes sense beside its caller,
leave it inline: a scatter of tiny functions the reader must chase to
reassemble the picture is *less* readable than the nesting it replaced, and
§9.1's tripwires find candidates, not quotas.

---

## 10. Totality, no partial functions

A policy proxy must not crash on hostile input, so **partial functions are
banned** (enforced by `.hlint.yaml`; most are already hidden by relude). Do not
reach for `head`, `tail`, `fromJust`, `read`, `(!!)`, `error`, `undefined`, or
`unsafePerformIO`.

Instead:
- pattern-match and handle the empty/missing case, or
- use the total alternative: `fromMaybe`, `listToMaybe`, `readMaybe`,
  `viaNonEmpty head`, `Map.lookup`, `(!!?)`.

Represent "this might not exist" in the type (`Maybe`, `Either`, a sum type),
and let the caller decide; see `Decision`/`RuleOutcome`, which carry a human
reason for every branch precisely so failures are explainable rather than
thrown.

**Partial record selectors and updates fall under the same ban**, caught at
compile time rather than by `.hlint.yaml`. A field that is not present in every
constructor of a sum type yields a selector, and a record update, that throws
on the other constructors, even though its type looks total. `-Wpartial-fields`
rejects *defining* one, `-Wincomplete-record-selectors` rejects a *use* site
that could fail (including selectors defined in a dependency), and
`-Wincomplete-record-updates` rejects the update form, all fatal under
`-Werror` (§3). Keep selectors total: give each constructor its own nested
record, or hoist the shared fields out of the sum.

### `error` and the unreachable-branch escape hatch

`error` is banned along with the rest. "This branch is impossible" is a claim
about *today's* code; a later refactor can quietly make it reachable, turning a
dead branch into a live crash in a service that is supposed to stay up. (Note
`undefined` and the `trace*` functions are stopped even earlier, at compile
time, by relude's warnings under `-Werror`; see §3.)

So **first try to make the impossible case un-representable** instead of
asserting it away:
- use `NonEmpty` instead of `[]` so there is no empty case to handle;
- return the value from the function that established the invariant, rather than
  re-deriving it and handling a "can't happen" `Nothing`;
- restructure the guards or patterns so the leftover branch disappears.

For the rare branch that is *genuinely* unreachable and cannot be designed away,
use a **per-declaration HLint ignore paired with a comment explaining why**:

```haskell
-- Exhaustive over Int, but GHC's checker can't prove it, so the final branch
-- is required yet unreachable. (Illustrative: this one is better fixed by
-- dropping the redundant `n > 0` guard, shown only for the annotation's form.)
{- HLINT ignore classify "Avoid restricted function" -}
classify :: Int -> Text
classify n
    | n < 0 = "negative"
    | n == 0 = "zero"
    | n > 0 = "positive"
    | otherwise = error "unreachable: an Int is < 0, == 0, or > 0"
```

Rules for the escape hatch:
- The annotation **names the single declaration** it applies to (`classify`
  above) and sits directly above it. `"Avoid restricted function"` is HLint's
  fixed name for this hint, use it verbatim.
- Keep that declaration **small**: the ignore unblocks *every* restricted
  function inside it, not just `error`, so a tiny scope limits the blast radius.
- The justifying comment is **mandatory**, "why this cannot happen" is the
  whole point of the exception.
- Reach for it sparingly, like a Semgrep ignore: a reviewer should be able to
  agree the branch is genuinely dead.

---

## 11. Errors: values in the core, typed exceptions at the edge

A policy proxy spends most of its effort deciding how to *respond* when
something upstream goes wrong, so how an error is represented is a design
decision, not an afterthought. The rule has two halves: what shape an error
takes, and what monad it lives in.

**Rule 11.1, A domain outcome is a value; a fault is a typed exception.** Before
you reach for `throwIO`, ask which kind of failure you are holding:

- A **domain outcome** is a result the caller must *decide on*: a fetch that
  404s, a publish the registry rejected, a packument that did not parse, a rule
  that denied a version. Return it in the type (an `Either`, a `Maybe`, or a
  purpose-built sum) so the caller cannot forget it. This is "parse, don't
  validate" (the guiding principle) carried to the effectful edge: the outcome is
  evidence the type forces the next step to read.
- A **fault** is a condition no local caller can sensibly act on: a base URL
  misconfigured at the composition root, an invariant the program itself broke, a
  resource that vanished mid-stream. Raise it as a *typed* exception that unwinds
  to a boundary built to log it and fail closed. `BootAborted` and the credential
  breaker's `CredentialError` are this category.

The same surface can sit on either side of the line depending on context. The npm
handle returns an unformable publish URL as a `PublishUrlUnformable` value,
because the mirror worker has a real decision to make (drop it and never retry, as
against a `PublishRejected` it should leave un-acked and redeliver). The *same*
unformable URL from the unconfigured null handle is a typed `RegistryUnconfigured`
exception, because there is no request to decide about: the handle was wired
wrong, and the only correct move is to fail loudly.

```haskell
-- A value: the worker pattern-matches and chooses retry vs. drop.
publishArtifact :: PackageName -> Version -> ByteString -> IO (Either PublishFault ())

-- An exception: a misconfigured composition root has nothing to decide.
refuse = throwIO RegistryUnconfigured
```

**Rule 11.2: If you throw, throw a typed `Exception`, never a stringly one.**
`stringException`, `throwString`, and `userError` are banned (`.hlint.yaml`):
they erase the type, so nothing downstream can catch the condition by category or
read its cause, and a `try` decays into grepping a message. Give the condition a
type with an `Exception` instance (a nullary type like `RegistryUnconfigured`, or
a small sum), exactly as the codebase already does for `BootAborted`. A typed
exception is catchable, testable, and self-describing; a string is none of those.

**Rule 11.3, Surface errors as values; do not thread `ExceptT` through the base
monad.** The effectful shell runs in `ReaderT Env IO` over `unliftio`, and
`MonadUnliftIO` has no instance for `ExceptT` (nor `StateT` or `WriterT`):
unlifting a short-circuiting monad across an async-exception boundary is unsafe,
so the library refuses to. The consequence is concrete and unforgiving: a
function in `ExceptT e (ReaderT Env IO)` cannot use `bracket`, `finally`, `mask`,
`async`, or `withRunInIO`, which is precisely the resource and concurrency
machinery the edge is built on. So:

- Keep the base monad `ReaderT Env IO`. An edge function reports its error as a
  *value* it returns (`IO (Either DomainError a)`), not as a transformer layer.
- `ExceptT` earns its place only in a **small, IO-free or non-bracketing span**
  where `do`-notation short-circuiting genuinely reads better, collapsed back to
  an `Either` at the boundary. Reach for it rarely.
- The `either` package's `EitherT` (`Control.Monad.Trans.Either`) is
  **deprecated** in favour of `ExceptT` from `transformers`; do not introduce it.

**Rule 11.4, Justify every throw; a throw caught nearby wanted to be a value.**
Throwing is the exception, not the default, so each `throwIO` carries a one-line
reason *why a value would not do*, in the Haddock or a comment at the throw site.
The good reasons are narrow:

- **It integrates with an exception-based boundary you do not own.** The credential
  breaker runs its mint leaf and catches `SomeException` to count failures and trip,
  so that leaf must *throw* to be seen; returning a value would fight the contract.
- **It is a wiring or programming fault with no per-request meaning.** An
  unconfigured config leaf, or the unconfigured registry handle, has no caller
  decision to make, only "fail loudly", so the exception is the fail-fast.

The tell-tale that a value was the right answer: **a throw that the throwing
function, or its immediate caller, catches and turns back into a normal result**. For example, throwing here, `tryAny` one frame up, and degrading to `Nothing`. That round-trip is a value
wearing an exception's clothes. Prefer returning the value; reach for the throw only
when threading it back is genuinely worse (e.g. it would ripple a `Maybe` through a
signature several layers off), and when you make that trade, say so at the site.

This is the same instinct as §10 (totality). A partial function crashes on inputs
*it did not name*; a stringly throw discards the *cause* it did know. Both trade a
value the type could have carried for a surprise at run time.

**Rule 11.5, Catch on the unliftio combinators, never base `Control.Exception`.**
Use `UnliftIO.Exception`; when catching broadly, reach for `tryAny` / `catchAny`,
which catch *synchronous* exceptions but re-raise asynchronous ones. A base `catch` /
`try` at `SomeException` also swallows the asynchronous exceptions the runtime delivers
(cancellation from `race` / `concurrently`, a timeout, a `ThreadKilled`), silently
defeating the structured-concurrency shutdown the effectful shell depends on (§11.3).
The rule holds uniformly: the serve gate, the mirror worker, the advisory sync, and the
credential breaker all catch on the sync-only combinators, so a blanket catch never
turns a cancellation into a swallowed no-op or a mis-counted failure. When you must
instead act on *every* exit including an async one (releasing a resource), reach for
`finally` / `withException` / `bracket`, which are async-aware by construction, not a
broad `catch`.

**Rule 11.6, Place a new failure mode in the fault-model vocabulary before choosing
its shape.** The system-wide map of channels, confinements, and the two outer edges is
[`docs/architecture/fault-model.md`](docs/architecture/fault-model.md). Before adding a
throw, a catch, or an error type, name the failure's disposition there (Transient /
Permanent / Cancelled for a loop; Deny / Propagate for a request; BootAbort / FailUp /
Graceful for the process) and pick the matching shape: an `Either` on the field when a
caller decides per call, a confined typed exception only when one named boundary absorbs
it, and a classification at the adapter edge when a client library's exception must not
travel. A new broad catch belongs in that document's stays-inner inventory or it does
not belong in the code.

---

## 12. Tests

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

- **Name fixtures and helpers, and give them signatures**, `now :: UTCTime`,
  `pkg :: Maybe Text -> Integer -> PackageDetails`. A small builder like `pkg`
  that fills defaults and exposes only the axis under test keeps each case to
  one line.
- **Add small predicate/extractor helpers** instead of inlining pattern matches
  in assertions: `isAllow`, `approvedBy`, `deniedBy`.
- **Express invariants as `hedgehog` properties**, grouped under
  `describe "properties"`, using `forAll` generators and `(===)`. An invariant
  that must hold for *every* input, not just the handful an example covers, belongs here (e.g. an order-independence or round-trip law).
- Comment a non-obvious case with the reasoning it encodes.
- **Share cross-suite helpers through `ecluse-test-support`**: a helper or fixture
  that more than one suite needs lives in the internal `ecluse-test-support`
  library (`test/support/`), not copied into each suite. Its modules mirror the
  main-library namespace, so a helper supporting `Ecluse.X` lives in
  `Ecluse.Test.X`: the digest fixtures and `unsafeHash` for `Ecluse.Core.Package` live
  in `Ecluse.Test.Package`, following the same `module name = file path` rule §4.2
  sets for the library. Genuinely cross-cutting helpers that belong to no single
  module live in the general `Ecluse.Test.Support`. A suite imports a shared helper
  from the library rather than re-defining it; a helper only one suite uses stays
  local to that suite. See `docs/testing.md`.

## 13. Character Sets

Only the ASCII standard character set is permitted in the codebase or documentation except where specific language-specific terms require it, such as our name Écluse, or if for some reason Vi hadde någon (väldigt stort) behöv och skriva på svenska. But things like em-dashes, emoji, and so on? No. Exactly one emoji is allowed: `⚜️`