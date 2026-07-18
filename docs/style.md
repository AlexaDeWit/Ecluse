# Haskell style guide

The coding-style reference for Écluse (package `ecluse`): how code is written
(formatting, documentation, naming, function design). For where code lives (module
layout, the `Ecluse.<Area>` namespacing, the `.Types` split), see
[`docs/getting-started.md`](getting-started.md) → "Codebase layout". For why the
stack is what it is (relude, raw WAI, the effect style), see
[`docs/architecture.md`](architecture.md).

> When in doubt, match the nearest existing module.
> `core/src/Ecluse/Core/Package.hs` and `core/src/Ecluse/Core/Rules.hs` are the reference
> implementations. Read one before adding code beside it.

## Two principles that outrank taste

**Simple Haskell.** Prefer the boring, readable subset of the language. Favour concrete
types and plain functions over type-level programming: a sum type or a record beats a fancy
generic encoding. Keep the extension set small (§2), and reach for the advanced toolbox
(`GADTs`, `TypeFamilies`, `DataKinds`, deep typeclass hierarchies, `TemplateHaskell`) only
when the simple encoding is genuinely worse, justifying it. When a clever solution and a
boring one both work, the boring one wins.

**Parse, don't validate.** Écluse ingests untrusted input, so it parses rather than validates: a
parser turns loose input into a precise type that captures what it learned, where a validator
returns `()`/`Bool` and discards it. So: make illegal states unrepresentable (a `NonEmpty a`, not
"a list I checked"; a sum type, not a `Bool` plus a convention); parse once at the boundary and
carry the refined type inward (adapters project wire formats into `Ecluse.Core.Package` types, so
nothing above sees raw wire data, §4.4); and make smart constructors the parsers (§6), returning
`Either Err T`, never `Bool`. Same instinct as §10: a precise type removes the case that would
otherwise crash.

---

## 1. Formatting

**fourmolu** owns layout (indentation, commas, line breaks, import alignment), configured by
[`fourmolu.yaml`](../fourmolu.yaml): run `task format` before committing, `task format-check`
gates it. **hlint** ([`.hlint.yaml`](../.hlint.yaml)) owns idioms and a large class of
correctness issues: run `task lint` and apply its suggestions. Don't format or argue by hand;
the dev shell pins both binaries. The rest of this guide covers what a tool can't decide.

---

## 2. Language baseline

- **GHC2021** is the language edition (set in `ecluse.cabal`), enabling `ImportQualifiedPost`
  (postpositive `qualified`, §8) among others.
- **Default extensions**, declared once in the cabal `common` stanza: `DerivingStrategies`,
  `LambdaCase`, `OverloadedStrings`, `StrictData`. Use a per-module `{-# LANGUAGE #-}` only
  for something genuinely local; promote a common one to the stanza.
- Mild conveniences (`RecordWildCards`, `TupleSections`, `MultiWayIf`, `DerivingVia`) are fine
  when they simplify. The advanced extensions are opt-in per case, justified in a comment and
  the PR. When two designs work, pick the one needing fewer extensions.
- **`TemplateHaskell` has two sanctioned uses**, no justification needed: deriving `aeson`
  instances (plain `DeriveGeneric` is still the default) and generating optics/lenses
  (`makeLenses`, kept shallow). Any other TH use needs justification.
- **`relude` is the prelude** (via cabal mixins): `Text`, not `String` (literals are `Text`;
  concatenate with `<>`, print with `putTextLn`); partial functions are hidden by default, so
  keep them that way (§10); `containers`, `text`, `bytestring`, and `stm` are re-exported.
- **String types by role, strict over lazy.** Strict `Data.Text` is the working and API default.
  Strict `ByteString` holds raw bytes; reserve lazy `ByteString` for streaming passthrough (the
  relayed tarball), never held state. `ShortText` is for bulk equality-only identifiers (§6.5). Fall
  back to `String` only at a library edge that speaks it, converting to `Text` at once.

---

## 3. Compiler flags

The warning set lives in the `common` stanza of `ecluse.cabal`; **warnings are errors**
(`-Werror` in [`cabal.project`](../cabal.project)), so a clean build is a hard requirement. On
top of `-Wall`, each flag reinforces a rule here:

| Flag | Guards |
|------|--------|
| `-Wcompat` | Upcoming breaking changes; fix them early. |
| `-Widentities` | Redundant numeric/`id` conversions. |
| `-Wincomplete-record-selectors` | A partial record selector at the *use* site; catches dependency-defined ones `-Wpartial-fields` cannot see. |
| `-Wincomplete-record-updates` | Record updates that could fail. |
| `-Wincomplete-uni-patterns` | Partial patterns in lambdas/`let` (totality). |
| `-Wmissing-deriving-strategies` | Forces the `deriving stock` style of §7. |
| `-Wmissing-export-lists` | Forces explicit export lists (§4.7). |
| `-Wname-shadowing` | A local binding that shadows a name already in scope. |
| `-Wpartial-fields` | Partial record selectors on sum types, at the *definition* site. |
| `-Wredundant-bang-patterns` | A `!` pattern that forces nothing. |
| `-Wredundant-constraints` | Constraints a signature does not use. |

`-Werror` makes *every* warning fatal, not only these: the full `-Wall` set plus relude's own
`WARNING` pragmas. So `undefined` and the `trace*` functions fail to compile in committed code, at
build time, before the `.hlint.yaml` ban (§10) even runs; a stray `trace` breaks `task build`, so
remove it (use `katip` for logging). `error` is the one relude leaves warning-free (§10).
Deliberately off: `-Wunused-packages` (the relude mixin defeats its attribution),
`-Wmissing-import-lists` (open internal imports, §8), and `-Wmissing-local-signatures`
(`where`-helpers may infer). Test suites add `-Wno-missing-export-lists` because `hspec-discover`
generates a `Main` without one. Relax warnings mid-refactor with an untracked `cabal.project.local`,
never the committed flags.

---

## 4. Module organisation, namespacing, and exports

The durable how-to for structuring modules; the current module list is the published Haddock
index and the root `Ecluse` synopsis, and
[`docs/getting-started.md`](getting-started.md) → "Codebase layout" records the
project-specific patterns.

**4.1 Organise by concept, favouring small modules.** Group each area's types and the functions
over them into single-purpose modules; if a module does three things (domain types, JSON parsers,
resolution logic), split it. Past about 400 lines (healthy modules cluster in 150-400), consider a
split, but the deciding test is capability clusters, not line count: split when a module holds two
or more clusters whose definitions don't reference each other. A module that is one cohesive
decision stays intact whatever its size.

**4.2 One namespace per area; module name = file path.** Each area gets a namespace:
`Ecluse.Core.<Area>` in the capability core (`Rules`, `Registry`, `Queue`, `Security`) and
`Ecluse.<Area>` in the application shell (`Config`, `Env`, `Log`). Organise by feature, not by
layer. GHC requires the module name to match the file path (`Ecluse.Core.Rules.Types` ⇄
`core/src/Ecluse/Core/Rules/Types.hs`); tests mirror it (§12).

**4.3 Extract a `.Types` or `.Helpers` module when the split is earned.** Splitting out
`Ecluse.<Area>.Types` is fine once it earns its place: breaking a cyclic import, giving several
modules a stable shared vocabulary, or separating simple data from heavy logic (like Aeson
parsers). Don't spin up a generic bucket before the split pays for itself.

**4.4 Functional core, effects at the edges.** Domain/leaf modules are pure (`Ecluse.Core.Rules`,
`Ecluse.Core.Version`, `Ecluse.Core.Package`), with no `IO`. Effects live at the boundary
(`app/Main.hs`, the server and worker layers) in `ReaderT Env IO`; swappable effectful backends
(registry, queue, credentials) are records of functions chosen at one composition root, the Handle
pattern (see [`docs/getting-started.md`](getting-started.md) and
[`docs/architecture/cloud-backends.md`](architecture/cloud-backends.md)). Keep the dependency
arrow pointing inward: pure modules never import the effectful shell.

**4.5 Put instances with the type or the class, no orphans.** Define an instance in the module
that defines the type or the one that defines the class. Orphan instances can be silently
overlapped and make import order matter; if you need an instance for a type and class you don't
own, wrap the type in a `newtype`.

**4.6 Internal modules expose innards without widening the public API.** Domain types are
deliberately opaque (`Scope`'s constructor is hidden, §6). When a test or an advanced caller needs
the guts, don't widen the export list: add an `Ecluse.<Area>.Internal` module that exports
everything, and re-export only the curated surface from the public module. Importing `.Internal`
opts out of the stability promise, as `text` and `bytestring` do.

**4.7 Exports.** Every module has an explicit export list (`-Wmissing-export-lists`); the list is
the public contract, and everything absent is private and free to change. Exporting a type abstract
(constructor hidden, built with `mkX`) or with its constructors and fields (`PackageDetails (..)`)
encodes whether it has invariants to protect (§6), pairing with the `.Internal` hatch (§4.6). Group
a large export list with Haddock section headers; [`docs/haddock.md`](haddock.md) → "Organising a module
for navigation" owns that convention and the example.

**4.8 Delete superseded code when a replacement lands.** Delete the old implementation in the same
change. A fix applied to a dead copy is a silent no-op that drifts from the live path and misleads a
reader into trusting a code path that never runs, worse the more authoritative or security-relevant
it looks. Dead code has no caller and no intended future caller; "we might need it later" is not one.
The compiler won't always catch it: an exported unused definition, a dead record field, or a field
bound only to refusing test stubs all compile clean under `-Werror`, so judge reachability from the
live composition root.

---

## 5. Documentation (Haddock)

Documentation is not optional, and it's the rule agents most often skip. Its conventions have
their own reference, **[`docs/haddock.md`](haddock.md)**, which you read before writing doc
comments: every module opens with a prose header, every exported type and function gets a
Haddock comment, non-exported helpers get a plain `--` at most, and you document the *why*
(especially a rule's security rationale), never the signature. `task doctest` runs the `>>>`
examples in the CI gate, so they can't drift.

---

## 6. Naming and domain types

**6.1 Wrap domain values in newtypes; keep them opaque when they carry an invariant.** A `Scope` is
not just `Text`: its own type stops it being mixed up with a package name and gives one place to
normalise. The opaque type plus its smart constructor is the parser.

**6.2 Give an opaque type the `mk*` / `un*` / `render*` trio:** `mkX :: Raw -> X` (the only
builder, so it enforces the invariant), `unX :: X -> Raw` (the plain accessor), and
`renderX :: X -> Text` (the canonical wire/display form, which may differ from the stored
form).

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

Keep types as small as the domain requires and no smaller-than-honest: `Version` is
deliberately an opaque `Text` wrapper that does **not** parse semver, with a comment saying
so, until a rule actually needs ordering.

**6.3 Prefix record fields with a short type tag** so selectors are unambiguous across
modules: `pkgName`, `pkgVersion`, `distTarball`, `ctxNow`.

**6.4 Names read as domain language.** Constructors are verbs/phrases of intent
(`AllowScope`, `DenyInstallTimeExecution`); predicates read as assertions
(`pkgHasInstallScripts`, `isAllow`).

**6.5 Store bulk equality-only identifiers as `ShortText`, converting only at the boundary.** When
an identifier is held in quantity (every version of a packument, every dependency entry) and is
only compared, keyed, or rendered (never sliced, parsed, or rewritten), store it as `ShortText`:
more compact, no slice-sharing surprises. Convert once at the boundary (`mkX` does
`Text -> ShortText`, `unX`/`renderX` the reverse) and derive `Eq`/`Ord`/`Hashable` so interior
compares and `Map` keys run native. The discipline: **never convert in a hot loop**. If a value is
*ever* sliced, parsed, or rewritten (a URL rewritten at serve, an SRI digest parsed), keep it
`Text`. `Scope` and `PackageName`'s canonical/display keys are `ShortText`; `Hash.hashValue` and
`Artifact.artUrl` stay `Text`.

---

## 7. Data types and deriving

**Always name the deriving strategy** (`-Wmissing-deriving-strategies`): `deriving stock
(Eq, Show, Ord)` for ordinary types, and `deriving newtype` when you want the wrapped type's instance.
Model decisions and outcomes as **sum types**, not booleans or stringly flags: `RuleOutcome =
Allow Text | Deny Text | Abstain Text` makes the three cases (and the audit reason with each) explicit
and total to match.

**Test a derivation only when its *specific* shape is load-bearing.** A derived instance is
lawful, but the behaviour it picks rides on declaration structure, invisible to the compiler, so a
later "cosmetic" refactor can silently move a contract. The discriminator: does an external party or
a domain rule depend on the *specific* shape, or only that *some* instance exists?

- **Order (`Ord`/`Enum`/`Bounded`)** is lexicographic by field/constructor declaration order; when
  the domain depends on it (severity, priority, version ranges), pin it with a test or route
  ordering through a function. `Version` and `PrecededRule` deliberately don't derive `Ord`.
- **Equality (`Eq`/`Ord`)** folds in every field; when one must be excluded, hand-write it.
  `PackageName` hand-writes `Eq`/`Ord` over a canonical key so the display form never affects
  identity.
- **Wire contract (`ToJSON`/`FromJSON`)** couples the wire shape to the record structure, so a
  rename is a silent breaking change for clients you can't recompile; for an owned response, derive
  the schema from the *same* codec, or design the coupling away (the served packument relays the raw
  upstream `Value`, edited in place).

Prefer designing the coupling away over testing a fragile derivation. Which derived lines the
coverage gate treats as accepted partials is in [`docs/testing.md`](testing.md) → "Coverage".

---

## 8. Imports

- **Qualified imports are postpositive** (`ImportQualifiedPost`): `import Data.Text qualified
  as T`.
- **Use the conventional aliases:** `T` for `Data.Text`, `Map` for `Data.Map.Strict`. Qualify
  anything whose unqualified names would collide or mislead.
- **Open imports are fine for our own internal modules** (`import Ecluse.Core.Package`),
  because we control those names; this is why `-Wmissing-import-lists` is off. For third-party
  modules, prefer a qualified import or an explicit import list.
- Let fourmolu order and align imports; do not hand-sort.

---

## 9. Function design

**9.1 Functions are small and do one thing.** Compose small, named pieces. Three tripwires demand a
second look, never an automatic split: a body past roughly 25 lines, nesting past three levels, or a
`where` block that outweighs its equation. Essential length is fine (an exhaustive per-constructor
dispatch, §9.4, or a flat sequence of steps). Incidental length is the target: a body that
interleaves validating, deciding, rendering, and effects is long because structure is missing, so
name the concerns and split along them. An extraction earns its place only when the new name lets the
reader skip the body; single-use glue that only makes sense beside its caller stays inline.

**9.2 Prefer pure and total.** Keep the core logic pure; push `IO` to the edges. Annotate a
purity/totality guarantee **only where it is surprising or load-bearing** (a boundary parser a reader
would expect to throw; `evalRule` never crashing the gate on hostile metadata), never reflexively: in
a module whose header already says it is pure, or on a signature with no `IO` and a total return type,
`-- Pure and total.` only restates the header and the type ([`docs/haddock.md`](haddock.md)). The effectful
parts run in `ReaderT Env IO`; handlers take `Env` and run in plain `IO`.

**9.3 Use `where` helpers, and lift them when they stop earning the nesting.** Name sub-steps with
local `where` helpers; top-level bindings always have a signature, `where`-helpers get one when it
aids clarity. A helper earns its place by *closing over* the parent's context and staying small (a
few lines, the block smaller than its equation). Lift it to the top level when it captures nothing
local (it refers only to its own arguments and module-level names), or when a capturing one outgrows
the block: a top-level function is independently testable, visible by name in a profile, and readable
without scanning the parent. When lifting a capturing helper, pass what it captured as explicit
parameters (roughly four or fewer; bundle inputs in a small record if it needs more), keep it
unexported, and don't change what the program computes (pass the computed value, never re-inline the
expression, leave effect order unchanged).

**9.4 Dispatch on a sum type with `LambdaCase`** when the argument is only there to be matched:

```haskell
ruleName :: Rule -> Text
ruleName = \case
    AllowScope{} -> "AllowScope"
    AllowIfOlderThan{} -> "AllowIfOlderThan"
    DenyInstallTimeExecution -> "DenyInstallTimeExecution"
```

Match every constructor explicitly (no wildcard) when you want the compiler to flag you the day a new
constructor is added.

---

## 10. Totality: no partial functions

A policy proxy must not crash on hostile input, so **partial functions are banned** (enforced by
`.hlint.yaml`; most are already hidden by relude). Don't reach for `head`, `tail`, `fromJust`,
`read`, `(!!)`, `error`, `undefined`, or `unsafePerformIO`; pattern-match the empty/missing case, or
use the total alternative (`fromMaybe`, `listToMaybe`, `readMaybe`, `viaNonEmpty head`, `Map.lookup`,
`(!!?)`). Represent "might not exist" in the type; `Decision`/`RuleOutcome` carry a human reason for
every branch so failures are explainable rather than thrown.

**Partial record selectors and updates fall under the same ban**, caught at compile time. A field
absent from some constructor of a sum type yields a selector, and an update, that throw on the
others. `-Wpartial-fields` rejects *defining* one, `-Wincomplete-record-selectors` a *use* site that
could fail (including dependency-defined selectors), and `-Wincomplete-record-updates` the update,
all fatal under `-Werror` (§3). Keep selectors total: give each constructor its own nested record,
or hoist the shared fields out of the sum.

### `error` and the unreachable-branch escape hatch

`error` is banned along with the rest. "This branch is impossible" is a claim about *today's* code,
and a later refactor can make it reachable, turning a dead branch into a live crash. So first make
the case un-representable: use `NonEmpty` so there is no empty case; return the value from the
function that established the invariant; restructure the guards so the branch disappears. For the
rare branch that is *genuinely* unreachable and can't be designed away, use a per-declaration HLint
ignore with a comment explaining why:

```haskell
-- Exhaustive over Int, but GHC's checker can't prove it, so the final branch
-- is required yet unreachable.
{- HLINT ignore classify "Avoid restricted function" -}
classify :: Int -> Text
classify n
    | n < 0 = "negative"
    | n == 0 = "zero"
    | n > 0 = "positive"
    | otherwise = error "unreachable: an Int is < 0, == 0, or > 0"
```

The annotation **names the single declaration** it sits above; `"Avoid restricted function"` is
HLint's fixed hint name, use it verbatim. Keep the declaration **small** (the ignore unblocks
*every* restricted function inside it), make the justifying comment **mandatory**, and reach for
it sparingly, like a Semgrep ignore: a reviewer should agree the branch is genuinely dead.

---

## 11. Errors: values in the core, typed exceptions at the edge

An error's representation is a design decision: what shape it takes, and what monad it lives in.

**11.1 A domain outcome is a value; a fault is a typed exception.** A domain outcome is a result the
caller must *decide on* (a 404, a rejected publish, a packument that didn't parse, a denied version):
return it in the type (`Either`, `Maybe`, or a purpose-built sum) so the caller can't forget it. A
fault is a condition no local caller can act on (a misconfigured base URL, a broken invariant, a
vanished resource): raise it as a typed exception that unwinds to a boundary built to log it and fail
closed (`BootAborted`, the credential breaker's `CredentialError`). The same surface can sit on
either side by context: the mirror write returns an unformable publish URL as a value (the worker has
a real retry-vs-drop decision), while a credential wrapper built without its mint leaf throws the
typed `Unconfigured` (nothing to decide).

**11.2 If you throw, throw a typed `Exception`, never a stringly one.** `stringException`,
`throwString`, and `userError` are banned (`.hlint.yaml`): they erase the type, so nothing downstream
can catch by category and a `try` decays into grepping a message. Give the condition a type with an
`Exception` instance (a nullary marker, or a small sum like `CredentialError`), as the codebase does
for `BootAborted`. `throwString` is permitted only in the listed test modules.

**11.3 Surface errors as values; don't thread `ExceptT` through the base monad.** The effectful shell
runs in `ReaderT Env IO` over `unliftio`, and `MonadUnliftIO` has no instance for `ExceptT` (nor
`StateT`/`WriterT`): unlifting a short-circuiting monad across an async-exception boundary is unsafe.
So a function in `ExceptT e (ReaderT Env IO)` cannot use `bracket`, `finally`, `mask`, `async`, or
`withRunInIO`, precisely the machinery the edge is built on. Keep the base monad `ReaderT Env IO` and
report the error as a returned value (`IO (Either DomainError a)`). `ExceptT` earns its place only in
a small, IO-free span where `do`-notation short-circuiting reads better, collapsed to an `Either` at
the boundary. Don't introduce the `either` package's deprecated `EitherT`.

**11.4 Justify every throw; a throw caught nearby wanted to be a value.** Each `throwIO` carries a
one-line reason why a value wouldn't do. The good reasons are narrow: it integrates with an
exception-based boundary you don't own (the credential breaker catches `SomeException` from its mint
leaf to count failures), or it's a wiring fault with no per-request meaning. The tell that a value was
right: a throw the throwing function, or its immediate caller, catches and turns back into a normal
result.

**11.5 Catch on the unliftio combinators, never base `Control.Exception`.** Use `UnliftIO.Exception`;
catching broadly, use `tryAny`/`catchAny`, which catch *synchronous* exceptions but re-raise
asynchronous ones. A base `catch`/`try` at `SomeException` also swallows the async exceptions the
runtime delivers (cancellation from `race`/`concurrently`, a timeout, a `ThreadKilled`), defeating
the structured-concurrency shutdown the shell depends on (§11.3). To act on *every* exit including an
async one, use `finally`/`withException`/`bracket`, async-aware by construction.

**11.6 Place a new failure mode in the fault-model vocabulary before choosing its shape.** The
system-wide map is [`docs/architecture/fault-model.md`](architecture/fault-model.md). Before
adding a throw, catch, or error type, name the failure's disposition there (Transient / Permanent /
Cancelled for a loop; Deny / Propagate for a request; BootAbort / FailUp / Graceful for the process)
and pick the matching shape: an `Either` when a caller decides per call, a confined typed exception
when one named boundary absorbs it, a classification at the adapter edge when a client library's
exception must not travel.

---

## 12. Tests

Tests are documentation too; keep them as readable as the code. The layout and tier strategy are
in [`docs/testing.md`](testing.md); this is style.

- **Structure with `hspec`**: `describe` per function/area, `it` with a full-sentence
  expectation.

  ```haskell
  describe "evalRule" $ do
      it "AllowScope allows a matching scope" $
          evalRule ctx (AllowScope (mkScope "myorg")) (pkg (Just "myorg") 0)
              `shouldSatisfy` isAllow
  ```

- **Name fixtures and helpers, and give them signatures** (`now :: UTCTime`,
  `pkg :: Maybe Text -> Integer -> PackageDetails`). A small builder that fills defaults and
  exposes only the axis under test keeps each case to one line.
- **Add small predicate/extractor helpers** (`isAllow`, `approvedBy`) instead of inlining
  pattern matches in assertions.
- **Express invariants as `hedgehog` properties** under `describe "properties"` with `forAll`
  and `(===)`: an invariant that must hold for *every* input (order-independence, a round-trip
  law) belongs here.
- **Share cross-suite helpers through `ecluse-test-support`** (`test/support/`): a helper more
  than one suite needs lives there, not copied per suite. Its modules mirror the main-library
  namespace, so a helper for `Ecluse.X` lives in `Ecluse.Test.X` (the digest fixtures and
  `unsafeHash` for `Ecluse.Core.Package` live in `Ecluse.Test.Package`); cross-cutting helpers
  live in `Ecluse.Test.Support`. A helper only one suite uses stays local.

---

## 13. Character set

Only the ASCII character set is permitted in code and documentation, except where a term requires
it (our name Écluse, or the occasional passage a maintainer needs to write in Swedish). No
em-dashes, en-dashes, or emoji. Exactly one emoji is allowed: `⚜️`.

---

## 14. Licence headers

Every tracked `.hs` file opens with a machine-readable licence header, as line comments above any
pragmas and the module Haddock block (so module documentation and the `-Werror` build are
undisturbed):

```haskell
-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

```

The header attaches the licence to the unit that travels: a source file keeps its licensing when
vendored or forked from the repository-root `LICENSE`, and SBOM tooling parses the tags
deterministically. The referenced text lives in `LICENSES/MIT.txt`. Don't type the header by hand:
`task spdx-fix` stamps every tracked `.hs` file that lacks it (idempotent, discovering files via
`git ls-files`), and `task lint-spdx` gates it in `task static-checks`. The format is REUSE-native,
but the gate is scoped to Haskell sources; there is no repo-wide REUSE regime.
