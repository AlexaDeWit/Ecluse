# Internal domain model

> Part of the [Écluse architecture overview](../architecture.md).

`PackageDetails` ([`core/src/Ecluse/Core/Package.hs`](../../core/src/Ecluse/Core/Package.hs))
is the ecosystem-agnostic per-version snapshot every adapter produces and the rules engine
consumes; its shape follows the npm, PyPI, and RubyGems protocol studies in
[`research/reverse-engineering/`](../research/reverse-engineering/README.md). Two principles
govern it:

- **The rules engine is ecosystem-blind.** It never branches on npm vs PyPI vs RubyGems.
  Adapters project each wire format into normalised signals: a rule sees `CodeExecSignal`,
  `Trust`, `Availability`, never `hasInstallScript`, `packagetype`, or `extensions`.
- **Signal availability is explicit.** A signal the adapter has not (or cannot cheaply)
  determined is represented as such (`CodeExecUnknown`, `TrustUnknown`, `Nothing`), so a pure
  rule yields no decision rather than guessing and an effectful rule can resolve it later.

## The shared vocabulary

| Concern | Representation | Why |
|---|---|---|
| **Identity** | `PackageName`: an ecosystem tag, an optional namespace (npm scope), a normalised `canonical` key, and a `display` form. Equality and ordering are on `(ecosystem, namespace, canonical)`; the display and base forms are excluded. | npm is case-sensitive with scopes, PyPI normalises (PEP 503), RubyGems is verbatim. `Flask` and `flask` are one PyPI package but two npm ones, so the ecosystem tag is part of identity; matching uses the canonical key while rendering stays faithful. |
| **Version** | In [`Ecluse.Core.Version`](../../core/src/Ecluse/Core/Version.hs): opaque, holding the raw text plus a `Maybe VersionKey` parsed at construction. `parseVersionKey :: Ecosystem -> Text -> Either VersionError VersionKey` is the only way to a key, and `compareVersions` works only on keys, so non-canonical text never reaches the comparator. Unparseable means no key, so ordering rules abstain, but the version is still served. `Version` carries no derived `Ord`. | Lexicographic ordering is wrong for every grammar (`"10.0.0" < "9.0.0"`), and the proxy must keep serving a version even when the parser can't order it. |
| **Install-time code execution** | `CodeExecSignal = NoCodeOnInstall \| RunsCodeOnInstall reason \| CodeExecUnknown`. | Unifies npm install scripts, PyPI sdist builds, and RubyGems native extensions; `Unknown` carries the gemspec-fetch case. |
| **Trust / provenance** | `Trust = Trusted (NonEmpty TrustEvidence) \| Untrusted \| TrustUnknown`; `TrustEvidence = Signed \| Attested \| MfaPublished \| OtherEvidence text`. | Signing, attestation, and MFA differ per ecosystem but reduce to one signal; the evidence captures the how without the ecosystem. |
| **Availability** | `Availability = Available \| Deprecated msg \| Yanked (Maybe reason)`, plus a per-artifact `artYanked`. | npm deprecates and RubyGems yanks whole versions; PyPI yanks individual files, so the per-file flag keeps "listed-but-yanked" and lets exact pins resolve. |
| **Artifacts** | A version owns `NonEmpty Artifact`; each carries algorithm-tagged `Hash`es, kind/platform, size, interpreter constraint, and a provenance URL. | npm has one tarball; PyPI an sdist plus many wheels; RubyGems one gem per platform. |
| **Dependencies** | Deliberately not modelled, nor parsed off the wire. | A dependency matters only when itself fetched, and that fetch returns through this gate for its own verdict, so gating a parent's dependency list would duplicate the gate on every child. The raw document still relays the lists untouched. Restore the `Dependency` / `DepKind` vocabulary from history if a dependency-reading rule is designed. |

The types live in [`Ecluse.Core.Package`](../../core/src/Ecluse/Core/Package.hs),
[`Ecluse.Core.Version`](../../core/src/Ecluse/Core/Version.hs), and
[`Ecluse.Core.Ecosystem`](../../core/src/Ecluse/Core/Ecosystem.hs).

A served packument is the merge of several upstreams' `PackageInfo`; see
[Registry model → Packument merge](registry-model.md#packument-merge-across-upstreams) for
how trusted and gated provenances combine.
