# Internal Domain Model

> Part of the [Écluse architecture overview](../architecture.md).

`PackageDetails` ([`core/src/Ecluse/Core/Package.hs`](../../core/src/Ecluse/Core/Package.hs)) is the
ecosystem-agnostic per-version snapshot every adapter produces and the rules
engine consumes. Its shape is the synthesis of the npm/PyPI/RubyGems protocol
studies ([`research/reverse-engineering/`](../research/reverse-engineering/README.md));
two principles govern it:

- **The rules engine is ecosystem-blind.** It never branches on npm vs PyPI vs
  RubyGems. Adapters project each ecosystem's wire format into *normalised
  signals*; a rule sees `CodeExecSignal`, `Trust`, `Availability`, never
  `hasInstallScript`, `packagetype`, or `extensions`.
- **Signal availability is explicit.** A signal the adapter has not (or cannot
  cheaply) determined is represented as such (`CodeExecUnknown`, `TrustUnknown`,
  `Nothing`), so a pure rule yields no decision rather than guessing and an
  effectful rule can resolve it later (see [Rules Engine](rules-engine.md#rules-engine)).

## The shared vocabulary

| Concern | Representation | Why |
|---|---|---|
| **Identity** | `PackageName`: ecosystem tag + optional namespace (npm scope) + a normalised `canonical` key + a `display` form; equality is on the canonical key only. | npm is case-sensitive with scopes, PyPI normalises (PEP 503), RubyGems is verbatim, matching must use one canonical key while rendering stays faithful. |
| **Version** | (in [`Ecluse.Core.Version`](../../core/src/Ecluse/Core/Version.hs)) opaque; holds the raw text (round-trip) **plus** a `Maybe VersionKey` parsed at construction. `parseVersionKey :: Ecosystem -> Text -> Either VersionError VersionKey` is the only way to obtain a `VersionKey`, and `compareVersions` is defined *only* on keys, so non-canonical text cannot reach the comparator (parse, don't validate). Unparseable ⇒ no key ⇒ ordering rules abstain, but the version is still served. | Lexicographic ordering is wrong for every grammar (`"10.0.0" < "9.0.0"`); and a proxy must keep serving a version even when our hand-rolled parser can't order it. |
| **Install-time code execution** | `CodeExecSignal = NoCodeOnInstall \| RunsCodeOnInstall reason \| CodeExecUnknown`. | Unifies npm install scripts, PyPI sdist builds, and RubyGems native extensions; `Unknown` carries the gemspec-fetch case. |
| **Trust / provenance** | `Trust = Trusted (NonEmpty TrustEvidence) \| Untrusted \| TrustUnknown`; `TrustEvidence = Signed \| Attested \| MfaPublished \| OtherEvidence text`. | Signing/attestation/MFA differ per ecosystem but reduce to one signal; the evidence captures the *how* without leaking the ecosystem. |
| **Availability** | `Availability = Available \| Deprecated msg \| Yanked (Maybe reason)`, plus a per-artifact `artYanked`. | npm deprecates (advisory) and RubyGems yanks whole versions; PyPI yanks individual *files*, the per-file flag preserves PyPI's "listed-but-yanked" so exact pins still resolve. |
| **Artifacts** | a version owns `NonEmpty Artifact`; each carries algorithm-tagged `Hash`es, kind/platform, size, interpreter constraint, and a provenance URL. | npm has one tarball; PyPI has an sdist + many wheels; RubyGems has one gem per platform. |
| **Dependencies** | `[Dependency]` with the constraint kept as **raw text** + kind + optional marker. | Lossless and agnostic across semver / PEP 508 / `Gem::Requirement`; parsed only when a rule needs to compare. |

## Decisions captured

The model resolves the open questions surfaced by those protocol studies (worked
through one at a time):

1. **Yank/availability granularity**, version-level `Availability` **and** a
   per-artifact `artYanked` flag (faithful to all three ecosystems).
2. **Version ordering**, built now, per-ecosystem, and made *parse, don't
   validate*: an ecosystem-aware parser (`parseVersionKey`) yields an opaque,
   canonical `VersionKey`, and comparison is defined only on keys.  `Version`
   keeps the raw text for round-trip plus a `Maybe VersionKey`, so an
   unparseable version is still served but causes ordering rules to abstain
   (the same Unknown→abstain pattern as the other signals). The misleading
   derived `Ord` is gone.
3. **Ecosystem-specific signals**, folded into ecosystem-blind normalised
   signals (notably `Trust`, with a `TrustEvidence` vocabulary and an
   `OtherEvidence` escape hatch) rather than an ecosystem-tagged sum. Raw residue
   needed only for faithful *serving* stays in the adapter, below the rules
   layer.
4. **Dependencies**, a lossless structured list with raw constraints (no
   constraint parsing yet).
5. **Module layout**, the version model and its three per-ecosystem parsers
   live in their own module, [`Ecluse.Core.Version`](../../core/src/Ecluse/Core/Version.hs), and
   the shared `Ecosystem` tag in
   [`Ecluse.Core.Ecosystem`](../../core/src/Ecluse/Core/Ecosystem.hs);
   [`Ecluse.Core.Package`](../../core/src/Ecluse/Core/Package.hs) holds the rest of the
   vocabulary (identity, signals, artifacts, dependencies, `PackageDetails`) and
   embeds a `Version`. The split keeps each module to a single responsibility and
   breaks the `Package`↔`Version` import cycle (the shared tag is the handle). The
   `evalRules` fold and the rule inputs are unchanged.
6. **Cross-upstream merge**, a served packument is the merge of several upstreams'
   `PackageInfo` (trusted private ∪ gated public; see
   [Registry Model → Packument merge](registry-model.md#packument-merge-across-upstreams)).
   This is a **pure operation over the domain model**, living above the
   `RegistryClient` handle in its own module (`Ecluse.Core.Package.Merge`), never in an
   adapter, so a new ecosystem inherits merging for free. **Provenance** (trusted
   vs gated) is a **merge-time parameter**, not a persisted `PackageDetails` field,
   so identity/equality stay unchanged; if threaded through for observability it is
   kept out of the equality key.
