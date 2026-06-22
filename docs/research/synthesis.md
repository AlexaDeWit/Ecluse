# Synthesis: Cross-Ecosystem Patterns & the Internal Model

This document synthesizes the three protocol studies —
[`reverse-engineering/npm.md`](reverse-engineering/npm.md),
[`pypi.md`](reverse-engineering/pypi.md),
[`rubygems.md`](reverse-engineering/rubygems.md) — into the part that actually
drives Écluse's business logic: **the ecosystem-agnostic types the rules engine
evaluates.**

It does three things:

1. names the **invariants** the design can rely on across all ecosystems, and the
   **variabilities** the model must abstract over;
2. distils the one **central pattern** those imply — *rules consume normalized
   signals, and signal availability must be explicit*;
3. **assesses the current types** (`Ecluse.Package`, `Ecluse.Rules`) against that
   pattern and proposes a concrete revision.

The proposed Haskell here is a **design proposal for discussion**, not applied
code. The durable content is the patterns; the type sketch is how we'd cash them
out.

---

## 1. Invariants — what holds everywhere

These are true for npm, PyPI, and RubyGems, so the architecture can lean on them:

| Invariant | Consequence for Écluse |
|-----------|------------------------|
| **Resolution is client-side.** No registry resolves a version *range/requirement*; it serves discrete versions + a file list. | The proxy never resolves ranges. It serves a (possibly filtered) index; **rules act per concrete version**, which is exactly what `PackageDetails` already models. |
| **Two metadata tiers** — a cheap install-facing view and a rich view. | Adapters prefer the cheap view; fetch the rich view only when a needed signal is missing from the cheap one. |
| **Deny-by-default = a filtered projection of the upstream index.** | Serving logic everywhere is "reproduce the upstream index minus denied versions/files, preserving its invariants." |
| **Integrity is a content hash, verified client-side; published artifacts are immutable.** | Any mirror/rewrite must preserve bytes exactly; Écluse can cache artifacts forever and must carry the hash(es) through faithfully. |
| **Reads are anonymous (public registries); auth gates writes.** | The read proxy needs no upstream credentials for public registries; credentials are a private-upstream / mirror concern. |
| **Every version has a publish timestamp** *somewhere*. | The age signal (`AllowIfPublishedBefore`) generalizes — but see §2 on *where* it lives and that it can be absent from the cheap view. |
| **Advisories are available via OSV** for all three. | The CVE/effectful tier is largely shared work, keyed by `(ecosystem, name, version)`. |

---

## 2. Variabilities — the axes the model must abstract over

Each row is a place the **current npm-shaped types** meet friction. These are the
real design forcing-functions.

| Axis | npm | PyPI | RubyGems | Modelling demand |
|------|-----|------|----------|------------------|
| **Artifacts per version** | 1 tarball | sdist + N wheels | N platform gems | A version owns a **set** of artifacts, not one `Dist`. |
| **Name identity** | scopes (`@s/n`), case-sensitive | PEP 503 **normalized** | **verbatim** | A **canonical key** for matching + the **raw** name for round-trip + an optional **namespace** (scope). |
| **Version grammar & order** | semver | PEP 440 | `Gem::Version` | Version stays opaque text, but **ordering is ecosystem-provided** — never derived lexicographically. |
| **Install-time code-exec signal** | `hasInstallScript` (free in cheap view) | `packagetype == sdist` (free) | native `extensions` — **gemspec only, requires a fetch** | A **tri-state** signal (known-true / known-false / **unknown**), not a `Bool`. |
| **"Don't use" semantics** | `deprecated` (advisory, still resolvable) | `yanked` (file kept, hidden from ranges) | `yanked` (**removed**) | An **availability enum**, not a single deprecation string. |
| **Integrity algorithms** | SRI + SHA-1 | SHA-256 (+ md5, blake2b) | SHA-256 | **Algorithm-tagged** hashes, per artifact. |
| **Dependency spec** | semver range | PEP 508 (markers, extras) | `Gem::Requirement` (`~>`, `&`-joined; runtime/dev split) | A **raw-preserving** dependency list (a flat `Map name→range` loses markers/extras and can't hold duplicates). |
| **License cardinality** | one SPDX string | expression + classifiers | **multiple** | A **list** of licenses. |
| **Provenance / trust signals** | `_npmUser` (publisher), `dist.signatures` | PEP 740 attestations, classifiers | `rubygems_mfa_required` | A **publisher** field + an **ecosystem-specific extension** for the rest. |
| **Transport** | JSON re-download | JSON | **plain-text, append-only, Range-incremental** | Adapter-internal, but the caching/ETag/Range contract differs — already noted in architecture.md. |

---

## 3. The central pattern: rules consume *signals*, and availability is explicit

Everything above collapses into one idea.

> **The rules engine should evaluate normalized *signals*, not raw ecosystem
> fields — and each signal must encode whether it is *known*.**

Two reasons this matters, both grounded in the research:

1. **A signal can be absent from the view we fetched.** The age signal is in
   npm's *full* packument but not the abbreviated one; install-code is free in npm
   /PyPI but gemspec-only in RubyGems. A field typed `UTCTime` or `Bool` *forces*
   a value the adapter may not have, so the adapter has to fabricate one — which
   silently corrupts decisions.

2. **A pure rule over an unknown signal must abstain, not guess.** Encoding
   "unknown" in the type lets the engine do the right thing: a pure rule abstains
   when its input is unknown, and the **effectful tier** (architecture.md → Rules
   Engine) can go *resolve* the signal (fetch the gemspec, do the CVE lookup) and
   re-run. This is the type-level form of the "a rule's tier depends on where its
   signal lives" decision already recorded in the architecture.

So the adapter's job sharpens: **project wire format → normalized signals**,
marking each `Known`/`Unknown`. The rules engine stays ecosystem-agnostic because
it never sees `extensions` vs `scripts` vs `packagetype` — only
`RunsCodeOnInstall` / `NoCodeOnInstall` / `Unknown`.

---

## 4. Assessment of the current types

`Ecluse.PackageDetails` as it stands ([`src/Ecluse/Package.hs`](../../src/Ecluse/Package.hs)):

| Field | Today | Verdict |
|-------|-------|---------|
| `pkgName :: PackageName` (`Maybe Scope` + base) | npm scopes baked in | **Revise** — generalize to canonical + raw + optional namespace; PyPI needs normalization, RubyGems is verbatim. |
| `pkgVersion :: Version` (opaque `Text`, **derives `Ord`**) | fine as opaque… | **Revise** — the derived `Ord` is lexicographic and *wrong* for all three version grammars. Latent footgun (no rule orders versions yet). Drop derived `Ord`. |
| `pkgPublishedAt :: UTCTime` | single, mandatory | **Revise** → `Maybe UTCTime` (absent from cheap views; per-artifact in PyPI/RubyGems — keep a version-level representative). |
| `pkgHasInstallScripts :: Bool` | npm-only, mandatory | **Revise** → tri-state `CodeExecSignal` (the §3 pattern; RubyGems can't honestly fill a `Bool`). |
| `pkgDeprecated :: Maybe Text` | conflates deprecate/yank | **Revise** → `Availability` enum (advisory vs hidden vs removed differ in serving + rules). |
| `pkgDist :: Dist` (single) | one artifact | **Revise** → `NonEmpty Artifact`; PyPI/RubyGems have many per version. |
| `Dist` integrity (`distIntegrity` SRI + `distShasum` SHA-1) | npm-shaped | **Revise** → algorithm-tagged hash set; add size, filename, kind/platform, interpreter constraint, provenance. |
| `pkgLicense :: Maybe Text` | single | **Revise** → `[Text]` (RubyGems has many; PyPI adds classifiers). |
| `pkgMaintainers :: [Maintainer]` | ok | **Keep + extend** — add a `pkgPublisher` (who pushed *this* version) for provenance. |
| `pkgDependencies :: Map Text Text` | name→range | **Revise** → `[Dependency]` preserving raw spec + kind + marker/extras. |
| *(ecosystem-specific signals)* | none | **Add** — an extension for `rubygems_mfa_required`, PyPI classifiers/attestations, npm signatures. |

**Verdict:** the types are a sound *npm* model and a good starting point — the
shape (a per-version snapshot the rules fold over) is right and survives. But as
an *ecosystem-agnostic* model they need targeted revision on ~8 of 10 fields,
clustered around three themes: **artifact multiplicity**, **explicit signal
availability**, and **identity/version generality**.

---

## 5. Proposed revision (for discussion)

A sketch, not applied code. Names follow current conventions (`relude` prelude).

```haskell
-- Identity ------------------------------------------------------------------
data Ecosystem = Npm | PyPI | RubyGems
  deriving stock (Eq, Ord, Show)

data PackageName = PackageName
  { pkgEcosystem :: Ecosystem
  , pkgNamespace :: Maybe Text  -- npm scope (sans '@'); Nothing for PyPI/RubyGems
  , pkgCanonical :: Text        -- normalized key for matching/equality
  , pkgDisplay   :: Text        -- raw name, for rendering & round-trip
  }
  deriving stock (Show)
-- Eq/Ord defined on (pkgEcosystem, pkgNamespace, pkgCanonical) ONLY — the
-- adapter normalizes (PEP 503 / scope-join / verbatim) when constructing.

newtype Version = Version Text deriving stock (Eq, Show)  -- NOTE: no derived Ord;
-- ordering is ecosystem-provided (semver / PEP 440 / Gem::Version) when needed.

-- Signals with explicit availability ---------------------------------------
-- Does installing this version execute code? The unifying signal behind npm
-- install scripts, PyPI sdist builds, and RubyGems native extensions.
data CodeExecSignal
  = NoCodeOnInstall          -- determined safe
  | RunsCodeOnInstall Text   -- determined; Text says how (audit trail)
  | CodeExecUnknown          -- not yet fetched (e.g. RubyGems gemspec) ⇒ pure rules abstain
  deriving stock (Eq, Show)

data Availability
  = Available
  | Deprecated Text       -- advisory; still resolvable (npm)
  | Yanked (Maybe Text)   -- excluded from resolution (PyPI keeps file; RubyGems removes)
  deriving stock (Eq, Show)

-- Artifacts (was the single Dist) ------------------------------------------
data Artifact = Artifact
  { artFilename     :: Text
  , artUrl          :: Text
  , artKind         :: ArtifactKind
  , artHashes       :: [Hash]          -- algorithm-tagged; >=1 expected
  , artSize         :: Maybe Int
  , artInterpreter  :: Maybe Text      -- requires-python / required_ruby_version
  , artProvenance   :: Maybe Text      -- attestation/signature URL (PEP 740 / npm sig)
  }
  deriving stock (Eq, Show)

data ArtifactKind
  = Tarball                 -- npm
  | Sdist                   -- PyPI source
  | Wheel  { platformTag :: Text }   -- PyPI binary
  | Gem    { gemPlatform :: Text }   -- RubyGems ('ruby' = pure)
  deriving stock (Eq, Show)

data Hash = Hash { hashAlg :: HashAlg, hashHex :: Text } deriving stock (Eq, Show)
data HashAlg = SHA1 | SHA256 | SHA512 | MD5 | Blake2b | SRI
  deriving stock (Eq, Ord, Show)

-- Dependencies (was Map Text Text) -----------------------------------------
data Dependency = Dependency
  { depName       :: Text     -- canonical
  , depConstraint :: Text     -- raw: semver range / PEP 508 / Gem::Requirement
  , depKind       :: DepKind
  , depMarker     :: Maybe Text  -- PEP 508 marker / extras, if any
  }
  deriving stock (Eq, Show)
data DepKind = Runtime | Dev | Optional | Peer deriving stock (Eq, Show)

data Person = Person { personName :: Text, personEmail :: Maybe Text, personUrl :: Maybe Text }
  deriving stock (Eq, Ord, Show)

-- Ecosystem-specific signals that don't generalize -------------------------
data EcosystemMeta
  = NpmMeta    { npmSignedDist :: Bool }
  | PyPIMeta   { pypiClassifiers :: [Text], pypiHasAttestations :: Bool }
  | RubyGemsMeta { rgMfaRequired :: Maybe Bool }
  | NoMeta
  deriving stock (Eq, Show)

-- The snapshot the rules engine folds over ---------------------------------
data PackageDetails = PackageDetails
  { pkgName        :: PackageName
  , pkgVersion     :: Version
  , pkgPublishedAt :: Maybe UTCTime
  , pkgInstallCode :: CodeExecSignal
  , pkgAvailability:: Availability
  , pkgArtifacts   :: NonEmpty Artifact
  , pkgLicenses    :: [Text]
  , pkgPublisher   :: Maybe Person     -- who pushed THIS version (provenance)
  , pkgMaintainers :: [Person]
  , pkgDependencies:: [Dependency]
  , pkgEcosystemMeta :: EcosystemMeta
  }
  deriving stock (Show)
```

### Trade-offs / why these shapes

- **`NonEmpty Artifact`** — a version with zero artifacts is meaningless; encode
  that. Rules that want "the" artifact pick by policy (e.g. prefer `Wheel`/native
  `Gem` over `Sdist`).
- **`CodeExecSignal` tri-state over `Maybe Bool`** — names the three cases the
  rules care about and reads at the call site (`RunsCodeOnInstall why`).
- **`EcosystemMeta` as a sum, not an open `Map Text Value` bag** — keeps ecosystem
  signals type-checked. Cost: adding an ecosystem touches this type. Acceptable
  while the set is small; revisit if it sprawls. (Alternative for the record:
  a typeclass per signal — heavier than warranted today.)
- **`Version` loses derived `Ord`** — forces ecosystem-aware comparison to be a
  deliberate, adapter-supplied operation rather than a silently-wrong default.
- **Dependencies as a raw-preserving list** — we don't parse PEP 508 / `~>` yet
  (YAGNI for current rules); we keep enough to *not lose information* for when we
  do (graph rules, transitive policy).

---

## 6. Impact on the rules engine

[`Ecluse.Rules`](../../src/Ecluse/Rules.hs) is in good shape structurally —
deny-precedence fold, allow-abstain-deny outcomes, deny-by-default with collected
reasons. The revision touches inputs, not the fold:

- **`DenyHasInstallScripts`** changes from reading a `Bool` to matching
  `CodeExecSignal`: `RunsCodeOnInstall why → Deny why`; `NoCodeOnInstall →
  Abstain`; **`CodeExecUnknown → Abstain`** (pure tier defers; the effectful tier
  fetches the gemspec and re-evaluates). This is the single most important
  behavioural change and the reason the tri-state exists.
- **`AllowScope`** is effectively npm-only (only npm has namespaces). Either keep
  it npm-scoped or rename to `AllowNamespace`; on PyPI/RubyGems it simply never
  matches (abstains), which is correct.
- **Unlocked by the richer model** (candidate future rules, no engine changes):
  - `DenyYanked` / `DenyDeprecated` — over `Availability`.
  - `RequirePublishedUnderMfa` — over `EcosystemMeta` (RubyGems `rgMfaRequired`),
    a real resilience signal.
  - `PreferBinaryOverSource` / `DenySdistOnly` — over `ArtifactKind`, the
    cross-ecosystem framing of "avoid build-time code execution."
  - `AllowName <allowlist>` — over `pkgCanonical` (must match on the canonical
    key, hence the identity change).
- **`EvalContext`** stays `{ ctxNow }` for the pure tier; the effectful tier will
  extend it with the fetchers/lookups needed to *resolve* `Unknown` signals.

---

## 7. Decisions (resolved)

These were worked through one at a time and are now **implemented** in
`Ecluse.Package` / `Ecluse.Rules` (and captured in
[`../architecture.md`](../architecture.md) → "Internal Domain Model"):

1. **Yank/availability granularity** → version-level `Availability`
   (`Available | Deprecated Text | Yanked (Maybe Text)`) **plus** a per-artifact
   `artYanked :: Bool`. Faithful to npm deprecation, RubyGems version-yank, and
   PyPI per-file yank.
2. **Version ordering** → built now, per-ecosystem, and made *parse, don't
   validate*: `parseVersionKey :: Ecosystem -> Text -> Either VersionError
   VersionKey` yields an opaque, canonical key and `compareVersions :: Version
   -> Version -> Maybe Ordering` works only on keys, so non-canonical text
   cannot reach the comparator. `Version` keeps the raw text (round-trip) plus a
   `Maybe VersionKey`; an unparseable version is still served but ordering rules
   abstain. `Version`'s derived `Ord` dropped.
3. **Ecosystem-specific signals** → folded into ecosystem-blind normalised
   signals rather than an `EcosystemMeta` sum. Trust is
   `Trusted (NonEmpty TrustEvidence) | Untrusted | TrustUnknown` with
   `TrustEvidence = Signed | Attested | MfaPublished | OtherEvidence Text` (the
   escape hatch). Raw residue needed only for faithful *serving* stays in the
   adapter, below the rules layer; nothing ecosystem-tagged reaches a rule.
4. **Dependency structure** → a lossless `[Dependency]` (raw constraint + kind +
   optional marker); no constraint parsing yet.
5. **Rollout** → landed as one coherent revision (the parallel rules work having
   settled); the `evalRules` fold is unchanged — only rule *inputs* and the
   adapter projection moved.

---

## 8. Recommendation

Adopt the three high-value revisions first, since they remove the genuine
*correctness* gaps (not just ergonomics):

1. **`pkgDist :: Dist` → `pkgArtifacts :: NonEmpty Artifact`** with
   algorithm-tagged hashes — without it, PyPI/RubyGems cannot be represented at
   all.
2. **`pkgHasInstallScripts :: Bool` → `CodeExecSignal`** — without it, RubyGems
   forces a dishonest value and the effectful tier has nothing to key off.
3. **Identity generalization** (canonical + raw + namespace) and **dropping
   `Version`'s derived `Ord`** — without it, name matching and any version
   comparison are subtly wrong off-npm.

`Availability`, `[Text]` licenses, `[Dependency]`, `pkgPublisher`, and
`EcosystemMeta` are lower-risk additions that can follow. The rules-engine fold
is untouched throughout; only rule *inputs* and the adapter projection change —
which is exactly the handle the architecture was built around.
```