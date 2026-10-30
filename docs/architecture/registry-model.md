# Registry model

> Part of the [Écluse architecture overview](../architecture.md).

How Écluse uses the registries around it: the four roles a mount can point at, and how each
request shape draws on them. It also covers the domain model above the wire and the adapter
boundary a new ecosystem plugs into.

## Registry roles

A mount carries up to four registry roles, two reads and two writes, each set independently.
Several may map to one physical registry. The recommended topology keeps the first-party store
and the public-derived store separate, and unions them at the registry level (see
[Registry-level composition](#registry-level-composition-the-recommended-topology)). A single
shared registry is a supported reduced topology, subject to each role's endpoint checks.

| Role | Purpose |
|------|---------|
| **Private upstream** | The authoritative, already-vetted source, read as trusted. A tarball is a conventional stable read at `{base}/{pkg}/-/{file}`. Écluse trusts a packument's versions and merges them with the gated public set. Its availability bounds the mount's: a failed private read refuses the request. Optional on a serve-only mount. |
| **Public upstream** | The source of versions not yet in the private upstream. The rules gate everything here. It is the tarball fallback on a private `404`, and Écluse fetches it alongside the private upstream for a packument. The only required role: a pure public gate serves from it alone. |
| **Mirror target** | Where the worker writes approved public packages once they pass the rules. Declaring one is what makes a mount mirror. Best a distinct store unioned into the private read path. |
| **Publication target** | Where Écluse writes client-published first-party packages (`npm publish`). Distinct from the mirror target: client-driven first-party content, not proxy-driven approved-public content. See [Publishing first-party packages](#publishing-first-party-packages-the-publication-target). |

Whether a mount mirrors is **derived from its endpoints, never declared as a mode**. A declared
`mirrorTarget` makes the mount mirror, and the mount then requires a private upstream so a client
can read the mirror back. Without one, boot fails with `MountMissingPrivateUpstream`. An absent
`mirrorTarget` makes the mount serve-only: no mirror writes, and the private upstream is optional.
First-party publication remains independently available when `publicationTarget` is configured.
A mount with neither read nor mirror store is a pure public gate. It applies the same public
admission rules, but retains no mirror copy for later trusted reads.

### Credential flow and authority

Reads use **passthrough**: Écluse forwards the caller's own credential to the private upstream and
reads the public upstream anonymously.

- **Private upstream (read)**: Écluse forwards the client's credential and the upstream authorises
  each request. Per request, never cached across clients. A credential is a **pair**: a secret and
  the username half a scheme such as HTTP Basic carries beside it. The pair travels verbatim,
  because a private registry has username conventions of its own and rewriting one would
  authenticate as somebody else. The edge gate compares the secret half alone, so one configured
  edge token serves every ecosystem's presentation.
- **Public upstream (read/fallback)**: queried anonymously. Any auth a public mirror needs is
  Écluse's own, not the client's.
- **Mirror target (write)**: always Écluse's own
  [`CredentialProvider`](cloud-backends.md#the-credential-mint) token, from the tag the target
  declares. The `codeArtifact` tag mints per its domain, and the other tags carry a static write
  token (see [Configuration](configuration.md#outbound-registry-credentials)). Declare it
  under its own key even when it equals the private upstream: the client reads it, Écluse writes
  it.
- **Publication target (write)**: follows the configured
  [publish credential mode](https://ecluse-proxy.com/docs/deployment/#edge-authentication-and-client-credentials). Écluse mints no token.

The non-negotiable invariant, under every strategy: **the client's credential is never sent to
the public upstream.** The boot enforces it on the one client-driven write: a `publicationTarget`
on any mount's `publicUpstream` host, or equal to another mount's endpoint, refuses to start.

The private upstream authorises each caller. Raw private metadata bypasses shared retention
and request coalescing. Every request performs a fresh private fetch.
The assembled-response cache can contain private data, but reuse requires the same inputs,
including the digest returned by that request's authorised private fetch. A cache hit cannot
skip authorisation. See the [metadata cache](web-layer.md#metadata-cache) and
[threat model](https://ecluse-proxy.com/docs/threat-model/).
The [security invariants](security.md) bound outbound requests further: the host allowlist,
internal-range blocking, canonicalisation, and response bounds.

Separate Écluse instances per tenant are a blast-radius or policy-isolation choice. They are not
a substitute for the credential model, and they scale to team granularity, never per-developer.

## Publishing first-party packages (the publication target)

The publication target adds the one client-driven write path. Écluse accepts a `PUT /{pkg}`
(`npm publish`) at the mount and relays it to the publication target. That write is distinct in
trigger, content, and credential from the mirror write.

- **First-party publication guard.** Écluse refuses a publish whose name falls
  outside the mount's `firstParty` namespaces (for npm, scopes such as `@acme`). That stops a client
  publishing a name that shadows an existing public package, a dependency-confusion vector. The
  guard holds a **guard-name ≡ URL-path name ≡ every declared body name** invariant. The scope
  check keys on the URL-path name. An npm publish document declares its own identity (`_id`,
  top-level `name`, every `versions[].name`), so the guard validates those names too. Any present
  declared name that disagrees is a `403` before any relay, under the same `PackageName` equality
  the route uses. An absent name is no claim. The guard never decodes the base64 `_attachments`.
PyPI publication uses the same guard with native multipart name declarations. Pathless uploads
require one consistent canonical project identity. A store can key the write off the uploaded file
name, so the `content` part's file name must name that project at the declared `version`, or the
upload is a `400`. Framing and name checks run after edge
authentication within the mount's request bound. The relay preserves the incoming body and exact
Content-Type, including its boundary. It does not rebuild the client's distribution.
- **New-release shape only.** The relays forward only a body with the shape of a new-release
  upload. An npm document must carry exactly one `versions` entry and a non-empty `_attachments`
  object, and every `dist-tags` value must name that version. A PyPI upload must carry exactly
  one `:action` part, with the value `file_upload`. Any other readable body, such as the one
  `npm deprecate` or `npm star` sends, a null tag or a tag on another version, or a PyPI
  `remove_pkg`, is a `501` before any relay. `npm deprecate` sends back Écluse's served document,
  which omits the stored versions Écluse hides and carries its own `latest`. Verdaccio applies
  that document over its own, and pypiserver deletes a release on `remove_pkg`. A repeated key is
  a `400`, because a store that keeps the last copy would read a different body from the one the
  guard checked. A version document whose `version` field is missing or differs from its key is a
  `400` for the same reason. An unreadable body, or a missing or repeated `:action`, is also a
  `400`. The check reads the parsed document, never the attachment data.
- **No dependency from outside Écluse.** An npm version whose installable dependencies name a git
  source, a remote tarball URL, or any scheme Écluse does not know to resolve through the registry
  or locally is a `400` before any relay, because a consumer's client would fetch that dependency
  directly, past the quarantine, the advisories and the mirror. An unknown scheme fails closed, so
  one a client adds later is refused too. The publish refusal and the public-version rule share one
  classifier, so a spec a first-party release cannot carry is also one a public version cannot
  carry. Cargo refuses a dependency on another registry for the same reason.

- **Credential.** See [Credential flow and authority](#credential-flow-and-authority).
- **No read-back role.** Write-only from the proxy's view. Published packages read back through
  the private upstream, so the operator aggregates the publication target into that read path. A
  publication target on the private upstream's own registry also serves reads, but Dredger refuses
  that setup, because its deletions would reach first-party packages that exist only there.
- **Opt-in.** A mount needs `publicationTarget` before it relays a client upload. Otherwise
  npm PUT and PyPI multipart POST answer `405 Method Not Allowed`.

[First-party publication through authentication and namespace checks](https://ecluse-proxy.com/docs/threat-model/#first-party-publication)

## Where an artifact lives

npm serves a package's bytes from the registry host that served its listing. PyPI does not: a
Simple index names each file's own location, and public PyPI puts those on
`files.pythonhosted.org` rather than on `pypi.org`. A client that resolved metadata through
Écluse and then pulled the bytes from that host directly would be past the gate, so a mount
rewrites every served location back under its own prefix.

Which locations Écluse will fetch from is not the operator's to widen. An ecosystem declares the
hosts it serves artifact bytes from by design, and a mount honours a file only on the authority
that served its listing or on one of those declared hosts. A file anywhere else is **dropped
from the served listing**, not listed and refused at download, so a client never resolves
against a file it could not have installed. A release disappears when no file of it survives.
The same check runs at the download gate from the same definition, so the listing and the gate
cannot disagree.

For npm, a packument that names a tarball on a foreign host drops that version from the served
document.

## Serving a tarball

A tarball is one concrete version from one source, so the proxy streams a private-upstream hit
straight through. The two legs locate the bytes differently, by the trust of their origin.

The **private leg is a conventional stable read**. It fetches the tarball at
`{private-base}/{pkg}/-/{file}` by the client's requested filename, the URL an `npm ci` issues,
and never fetches the private packument first. A lockfile fan-out then pays one artifact
round-trip instead of a per-tarball packument fetch it would discard. The proxy forwards the
client's credential and disables redirect-following, so the leg never follows a `3xx`
([Security posture](security.md#trust-assumptions--credential-posture)).
A `2xx` streams and a conditional `304` passes through. An explicit private `401` or `403`
refuses the read, because public bytes can differ from a private patch under the same identity.
A private `404` is a private miss. Any other failure refuses the read for every name, with the
status [its cause sets](#private-availability). The leg applies **no
serve-time integrity floor**: it fast-tracks a lockfile-pinned version from a trusted registry.
The client and the mirror worker still verify those bytes, so the leg gives up the proactive
refuse-weak-integrity stance, not tamper-evidence. The packument route's listing-side trusted
floor ([Integrity floors](security.md#integrity-floors)) stays in force. A private upstream that serves
tarballs off-convention (a separate files host, or a presigned CDN URL the `/-/` path cannot
rebuild) is a private miss only when its `/-/` path answers `404`. A redirect from that path to
such a host refuses the read, as above.

The **public leg** honours the `dist.tarball` the gated version declares and fetches exactly that
URL, not a reconstructed `/-/` path. The proxy can then front a registry that serves bytes from a
separate host (the PyPI-files-host shape) or from a signed CDN URL. The ecosystem's adapter
declares such hosts and the same-host gate admits them, with no operator knob to widen the
surface. The proxy never trusts that location. The allowlist and the same-host gate bound where
it may fetch the bytes, and https-only egress with certificate validation authenticates the host.
The proxy upgrades a legacy `http` tarball on the same host, or drops it (see
[Securing network egress](https://ecluse-proxy.com/docs/deployment/#network-egress)).

On a private miss the proxy gates that one version, streams from public, and enqueues a
demand-driven mirror job. The enqueue does not block, so the proxy serves the client immediately.
See [Streaming](web-layer.md#streaming-and-resource-lifetime) and
[Mirror queue](cloud-backends.md#mirror-queue).

[Private artifact preference and admitted public fallback](https://ecluse-proxy.com/docs/threat-model/#artifact-downloads)

## Distribution metadata companions

PyPI companions retain the exact source entry's canonical declaration and use that parent's admitted location.
The companion URL appends `.metadata` to the path before any query or fragment.
Both private and public selection apply their own parent integrity floor and origin-sensitive authority check.
The metadata path does not evaluate rules, read advisories or enqueue a mirror.
Private index reads authorise each caller again, and only the private authority receives that caller's credential.
A private refusal or a failed private read stops fallback, and first-party names never fall back to public metadata.
A surviving private version owns its companion requests, including a missing declaration, missing parent or failed companion fetch.
Public fallback remains available when no private version survives the listing's location and integrity checks.
Selection releases metadata admission before the bounded relay streams the companion.
The upstream asserts that the companion matches the archive's Core Metadata. The resolver verifies advertised digests.

## Packument merge across upstreams

A packument is the set of available versions, spread across upstreams. The private upstream holds
what has already been vetted, plus what the worker has mirrored. The public upstream holds the
full history, including the versions not yet mirrored. Serving only the private packument would
hide the new versions, so a client never requests them and demand-driven mirroring never fires
(see [Mirror queue](cloud-backends.md#mirror-queue)). The proxy therefore merges the packument
above the [protocol boundary](#registry-abstraction), as a pure, ecosystem-agnostic fold over
`PackageInfo` that a new ecosystem does not rewrite. The merge is **order-independent**: private
wins a collision and the merge flags divergence whatever the fetch order. Only positional labels
track which input a survivor came from, so assembly can select its retained source representation.

- **Fetch in parallel.** Private (passthrough) and public (anonymous) concurrently. A name inside
  the mount's `firstParty` namespaces is the exception: it has one authority, so Écluse fetches the
  private origin alone and a miss there is a `404`. A public package registered under a name the
  deployment owns is a dependency-confusion attack, and refusing the public leg is what keeps one
  verdict on a name wherever the privilege is read. The mirror worker reads that same predicate, so
  a job enqueued before the declaration drops before any public request rather than mirroring public
  content under an owned name.
- **Trust split by provenance.** Private versions enter unfiltered. The rules engine gates public
  versions first (see [Applying verdicts](rules-engine.md#applying-verdicts-to-a-packument)). The
  result is `trusted(private) ∪ filtered(public)`.
- **Collision: private wins, divergence is a signal.** On a shared version key the private copy
  wins. Where versions that differ only in build metadata name one release (Cargo), the merged index
  first drops a public version that a private version claims under another spelling, so the private
  row wins that release too. The public copy may contradict it on a shared artifact's shared integrity algorithm: same
  file, same algorithm, disagreeing digests. The merge logs a WARNING with both digests
  and increments the bounded-cardinality `ecluse.registry.merge.divergence` counter.
  Validated public entries remain evidence for this comparison even when a public rule denies
  their version. Evidence must pass the origin's identity, artifact-location, and integrity checks.
  Denied entries contribute no served versions, files, or tags.
  The private version and its tags remain available. Artifact GET, HEAD, and conditional
  requests keep private preference. A disagreement proves neither copy correct, so a damaged
  private copy can still reach clients. The private repository holds previously admitted or
  first-party content. Current public metadata cannot revoke that trust.
  Detection adds no persistent conflict state, per-read public revalidation, or deletion authority.
  Dredger still requires a named denial and its existing deletion guards.
  One upstream carrying a digest the other omits is not a divergence.
- **Below-floor versions are inadmissible.** A version whose strongest digest is too weak or absent
  is a divergence blind spot, refused before the merge. The listing drops it, and the public
  artifact path `403`s it as `MissingIntegrity` or `BelowIntegrityFloor`. The private tarball leg
  is the exception: the client and the worker verify its bytes. The floors are
  `ECLUSE_INTEGRITY__MIN_PUBLIC` (hard-floored at SHA-256) and `ECLUSE_INTEGRITY__MIN_TRUSTED`
  (loosenable, refinable per mount: see
  [`config/default.yaml`](../../config/default.yaml)). This is
  [Integrity floors](security.md#integrity-floors).
- **Reconcile over the union.** `dist-tags.latest` follows the
  [keep-unless-denied, stable-preferring rule](rules-engine.md#applying-verdicts-to-a-packument):
  kept when it survives, else repointed to the highest stable survivor. The kept tag is the public
  document's alone: a private document's own `latest`, mirror store or not, is never consulted, so
  a stale tag cannot hold `latest` back. The merge carries every other tag from the
  highest-precedence source and drops it at an absent version, and restricts `time` to the
  surviving versions while keeping `created` and `modified`.
- **Private access authority.** An explicit private `401` or `403` refuses the request even when
  public metadata succeeds. The refusal precedes merge and conditional response selection.
  See the [operator policy](../../web/content/docs/configuration.md#first-party-namespaces).
- <a id="private-availability"></a>**Private availability.** A failed private read refuses the
  request for every name, on listings, metadata and artifacts alike. A private `404` still lets the
  gated public set serve. Otherwise a private package whose name is not declared first-party would
  be replaced by its public namesake once that version passed the rules, and builds would switch
  packages silently. First-party declarations cannot close that gap, because most registries give no
  reliable mechanism to establish a complete first-party naming system, and a missed name fails
  silently. The mount's availability therefore follows the private upstream's. The cause sets the
  status, and no public outcome changes it:
  - `503` when the private registry is unavailable: a transport error, a timeout, a `5xx` or a
    `429`. The refusal forwards the private registry's `Retry-After`, so a client that honours it
    waits as that registry asked.
  - `502` when its reply is invalid or unusable: an unfollowed redirect, another unexpected status,
    a document for another name, or a document that does not decode or exceeds its bound. The
    private registry answered, so retrying the same request is unlikely to help.
- **Public availability.** If the public upstream fails while the private one succeeds, the merge
  serves the private-only union with a degraded signal (readiness stays
  [lenient about public reachability](web-layer.md#meta-routes-ping-health-and-search)). Only when
  nothing resolves does the request error.

A packument request filters but never mirrors. See
[Applying verdicts to a packument](rules-engine.md#applying-verdicts-to-a-packument).

[Caller-authorised private metadata merged with admitted public metadata](https://ecluse-proxy.com/docs/threat-model/#metadata-listing-and-admission)

### The route name is the served name's validation authority

The proxy knows the requested name from the route, so an upstream's self-reported top-level `name`
is a cross-check, never the served authority. The served `name` is always a value an upstream
genuinely reported, and validation guarantees it equals the route name. An origin whose
`name` agrees merges normally. The proxy treats an origin whose `name` disagrees as untrusted for
this request and logs it. A public origin drops out, and the private set still serves `200`. A
private origin refuses the request with `502 Bad Gateway`, as any invalid private reply does
([Private availability](#private-availability)), so a public namesake never stands in for it. An
absent or undecodable name makes the packument undecodable, with the same split. The request is
`502` (`PackumentBadGateway`, see [Error model](web-layer.md#error-model)) when no origin yields a
valid packument *because the responding origins mismatched*. That is distinct from a genuine
absence. This forecloses cache-poisoning: a misreporting upstream cannot shadow a real package or
win the union with a divergent `name`.

### Decision surface vs served surface

The merge decides over the typed `PackageInfo` and rebuilds served metadata from the winning
source representations. Extraction retains supported installation, policy and artifact fields
before constructing decoded values. Unknown fields do not enter the retained document.
npm also retains mirror fields and replaces author lists with source-specific pointers.
Assembly selects admitted versions and files, rewrites artifact URLs, and takes npm tags from the
merge plan. It uses retained source fields instead of recreating installation data from the policy
model. The API surface [owns the served schema](web-layer.md#the-synthesised-packument-schema--the-trust-boundary).

Artifact admission identifies a raw entry by its source position, upstream-byte digest, and
adapter-assigned entry key. A filename alone cannot distinguish admitted and refused siblings.
The merge carries compact admitted-entry records, without retaining full artifact metadata.
The shared selector rejects missing keys, ambiguous keys, and mismatched source snapshots.
It preserves each source's entry order and supported fields. PyPI stores original array keys
beside compact file records, so a discarded file cannot shift later admission coordinates.

Adapters must establish these contracts before their metadata reaches assembly:

- Assign each artifact an explicit key before lenient parsing drops any entry. PyPI uses raw array positions, including malformed gaps.
- Use an equivalent key for other shapes. npm uses its version-map key. A singleton key is valid only for a source with one artifact entry.
- Bind the typed and compact views to the same source digest. Incremental hashing covers the complete decompressed source, including omitted fields.
- Pair a selected npm version with its retained publication fields from the same response. Those views stay paired through caching and mirroring. Mirror publication refuses a missing source representation instead of recreating one from the policy model.
- Ordinary PyPI selected reads retain typed files. Worker-only selected reads also retain their exact supported source entries and project envelope. Both modes consume the complete response inside its HTTP lifetime.
- Preserve keys through admission and use the shared selector before rendering. Selective reads must retain the full decoder's entry keys.

Snapshot scope is transient and content-addressed. It changes no stored schema or epoch.
Source positions distinguish identical content from different upstream contributions.
`PackageInfo` equality remains independent of snapshot scope. npm, PyPI, Cargo, and RubyGems implement this contract.
Future adapters must supply equivalent keys and contract tests before activation.

The metadata cache charges new entry keys, including text backing allocations. The merge plan
reuses the fetch digest and retains only each admitted key and filename. Array and object-key
allocation checks run in CI alongside the adapter regressions.

### Graceful degradation: per-version, not per-package

Decoding into the decision surface is lenient at version granularity, with a fail-closed boundary.
An undecodable `dist.unpackedSize` reads as absent and the version survives. `fileCount` and
`signatures` do not drive policy. npm retains them as declared artifact fields for serving, while
mirror publication removes signatures and attestations issued by the source registry. The decoder
drops a version broken in a required field (no `dist` or `tarball`,
an unusable `version`) rather than serve it unverifiable. Its healthy siblings keep serving. Only
an unusable top-level document (not an object, absent `name`, or non-object and non-null `versions`,
`time` or `dist-tags`) denies the package wholesale. Missing and null maps retain their empty meaning.
Inventory reads reject an invalid `versions` container and ignore unrelated policy maps. The decoder
tracks dropped entries as `InvalidEntry`
([`Package.hs`](../../core/src/Ecluse/Core/Package.hs)), so the drop is observable. This turns
"one poisoned version denies the whole package" into a per-version drop.

### Registry-level composition (the recommended topology)

The recommended deployment keeps the first-party store and the public-derived mirror store
physically separate. It unions them at the registry level into the private-upstream read path.
One example is an AWS CodeArtifact repository drawing from a mirror-target repo and a first-party
repo. The private upstream then returns the full trusted set in one fetch, while each store stays
independently governable (distinct scanning per provenance, clean post-disclosure scoping).
Combining the roles in one store trades away auditability and
defence-in-depth, not the perimeter.

Écluse fetches ordinary package metadata from the public and private sources. The private
read endpoint combines the mirror and first-party repositories. The worker copies admitted
public versions into the mirror, making completed copies available through the private path.

A mirrored deployment is intended to serve repeated artifact requests through that private
path. Performance changes must preserve private-read behaviour and report public-path costs
separately. A serve-only mount keeps fetching public artifacts from the public source and
retains no mirror copy through Écluse. Its availability remains coupled to that source.
Adding a `mirrorTarget` enables mirroring without changing the public admission policy.

#### The one rule of registry composition: Écluse is the only path from public

Écluse applies ingestion-time policy (the quarantine, integrity floors, the rule algebra) that
managed registries do not. That value holds only if public packages enter through Écluse and
nowhere else. So the aggregating read endpoint, the private upstream, must union trusted stores
only: your first-party publications and Écluse's sanitised mirror. It must not carry a direct
upstream connection to the public registry. Such a connection would let raw, ungated public
packages reach clients behind the gate rather than through it, and silently nullify the
protection. The proxy cannot detect this from a request, because it trusts the private upstream by
construction.

It asks the backend at boot instead. Where a store's control plane reports what a repository
aggregates, every role that reads the private upstream walks its upstream chain once and refuses
when any repository in it carries an external connection to a public registry: both proxy roles,
and both Dredger modes through the store handle they already hold. The mirror worker and Pilot read
no private upstream and never ask. The refusal names the mount, the repository and the connection.
On CodeArtifact the walk is `DescribeRepository` under the role identity, bounded to 10 hops and 25
calls. An identity that cannot ask cannot clear the store, so a refused grant and an environment
with no AWS identity both refuse the mount. A `registry` or `verdaccio` store reports no such
configuration, so the boot warns once and the topology stays an operator-architecture invariant,
catalogued in the [threat model](https://ecluse-proxy.com/docs/threat-model/).

## The internal domain model

`PackageDetails` ([`core/src/Ecluse/Core/Package.hs`](../../core/src/Ecluse/Core/Package.hs))
is the ecosystem-agnostic per-version snapshot every adapter produces and the rules engine
consumes. Adapters map their protocols into this shared model. Four principles govern it:

- **The rules engine is ecosystem-blind.** It evaluates the shared signals instead of branching on ecosystem names.
  Adapters project each wire format into normalised signals: a rule sees `CodeExecSignal`,
  `Availability`, never `hasInstallScript`, `packagetype`, or `extensions`.
- **Adapters declare rule blocks.** Every rule applies unless the adapter blocks its typed identity
  with a reason. A block covers unsuitable meaning or a required fact the ecosystem can never
  determine. Boot checks each active mount's resolved policy and accumulates every blocked entry
  beside independent refusals. The engine still evaluates only core evidence.
- **Signal availability is explicit.** A signal the adapter has not determined, or cannot
  determine cheaply, has its own representation (`CodeExecUnknown`, `DependencySourcesUnknown`,
  `Nothing`).
  A pure rule then yields no decision rather than guessing, and an effectful rule can resolve it
  later.
- **The policy model holds the facts that rules and admission need.** Installation fields
  remain in the retained source representation. The typed model carries the vocabulary below
  and the artifact facts used by merging, serving, and mirroring. Licences, publishers, deprecation
  notices, per-file yanks with their reasons, and provenance reach clients in the served document,
  within its supported fields.

### The shared vocabulary

| Concern | Representation | Why |
|---|---|---|
| **Identity** | `PackageName`: an ecosystem tag, an optional namespace (npm scope), a normalised `canonical` key, and a `display` form. Equality and ordering use `(ecosystem, namespace, canonical)` only, never the display or base forms. | npm is case-sensitive with scopes, PyPI normalises (PEP 503), RubyGems is verbatim. `Flask` and `flask` are one PyPI package but two npm ones, so the ecosystem tag is part of identity. Matching uses the canonical key while rendering stays faithful. |
| **Version** | In [`Ecluse.Core.Version`](../../core/src/Ecluse/Core/Version.hs): opaque, holding the raw text plus a `Maybe VersionKey` parsed at construction. `parseVersionKey :: Ecosystem -> Text -> Either VersionError VersionKey` is the only way to a key, and `compareVersions` works only on keys, so non-canonical text never reaches the comparator. Unparseable means no key, so ordering rules abstain and the proxy still serves the version. `Version` carries no derived `Ord`. | Lexicographic ordering is wrong for every grammar (`"10.0.0" < "9.0.0"`), and the proxy must keep serving a version even when the parser can't order it. |
| **Install-time code execution** | `CodeExecSignal = NoCodeOnInstall \| RunsCodeOnInstall reason \| CodeExecUnknown`. | Unifies npm install scripts, PyPI sdist builds, and RubyGems native extensions. Cargo and RubyGems retain `CodeExecUnknown` and block the unsupported execution-denial rule at boot. |
| **Availability** | `Availability = Available \| Deprecated \| Yanked`, a flag with no text. | npm deprecates whole versions. npm and RubyGems drop a removed or yanked version from their metadata instead of flagging it, so both block the yank deny at boot. PyPI yanks individual files, so a release reads as `Yanked` only when every file of it is. |
| **Artifacts** | A version owns `NonEmpty Artifact`. Each carries its entry key, file name, location, algorithm-tagged `Hash`es, and size. | npm has one tarball, PyPI an sdist plus many wheels, and RubyGems one gem per platform. |
| **Dependency sources** | `DependencySources = RegistryDependencies \| FetchesOutsideRegistry reason \| DependencySourcesUnknown`. | A git or URL dependency installs straight from its source and never receives a verdict of its own, so the deny judges the version that names it. npm reads `dependencies`, `optionalDependencies` and `peerDependencies` in both read forms. Cargo reads each index row's normal and build dependency registries. PyPI and RubyGems retain `DependencySourcesUnknown` and block the dependency deny at boot. |
| **Dependencies** | The relationships themselves are retained for installation, outside the typed policy model. | Each dependency receives its own verdict when the client fetches it. npm retains supported dependency relationships in its installation representation. Only where a dependency installs from enters the rules vocabulary, as its dependency sources. |

The types live in [`Ecluse.Core.Package`](../../core/src/Ecluse/Core/Package.hs),
[`Ecluse.Core.Version`](../../core/src/Ecluse/Core/Version.hs), and
[`Ecluse.Core.Ecosystem`](../../core/src/Ecluse/Core/Ecosystem.hs).

A served packument is the merge of several upstreams' `PackageInfo`. See
[Registry model → Packument merge](registry-model.md#packument-merge-across-upstreams) for
how trusted and gated provenances combine.

## Cargo integration

Cargo uses one mount and one native view, `/cargo/`, which replaces crates.io and merges caller-authorised private crates with admitted public crates, as npm, PyPI and RubyGems do.
Every consumer reads only through Écluse, so the view departs by design from the Cargo book's rule that a replacement source holds no crate absent from the original.
`Cargo.lock` therefore records private crates under the crates.io source identity.
The mirror target stays write-only under Écluse's token, and completed copies are read through `privateUpstream`.
The [operator protocol contract](https://ecluse-proxy.com/docs/protocol-support/#cargo) owns the routes and retained fields.

### Discovery and transport authority

`PrepareRead` captures checked discovery from the selected `OriginClient` before returning a pure `EcosystemRead`.
Public preparation runs inside existing cache-miss work.
Private preparation follows caller authorisation and stays uncached and uncoalesced.
Worker and maintenance preparation follows service credential acquisition and the applicable charge.
Boot constructs factories without a remote discovery request.
There is no separate private discovery cache.
The client-facing `config.json` is local: it names Écluse's archive route and publication API, and requires authentication whenever a private upstream or the edge token is configured.

Discovery describes download and optional API locations. It grants neither network nor credential authority.
A checked coordinate binds published name, raw version, logical archive filename, and SHA-256 to an approved download template.
Current source authorisation and discovery choose the address, including after durable-state restoration.
Retained source rows cannot authorise a new host or restore an old credential destination.
The publication path separately checks the exact configured API authority and base path.
Read and upload coordinates participate in the existing endpoint checks.
Those checks preserve accepted own-store collapse and the Dredger's role-specific refusals.

### Native identity and retained records

Published spelling identifies archive coordinates and native row claims.
ASCII lowercase supplies index lookup identity. A separate key folds hyphen and underscore for ownership collisions.
Keeping these keys separate prevents one alias lookup from poisoning another path's negative cache result.
Cargo does not inherit PyPI period folding or separator collapsing.
Raw SemVer spelling remains available, while native uniqueness excludes build metadata.
The index merge applies the same uniqueness across sources: rows that differ only in build metadata are one release, and the trusted row wins.

The bounded reader frames one native row at a time and keeps its physical `RecordEntry` coordinate.
Full and selected reads scan through EOF, so later duplicate identities can quarantine earlier records.
Assembly emits each admitted winning-source record once, in source order, with a final LF.
It preserves additive fields and absence versus null instead of rebuilding a reduced install document.
Public mirror relocation translates source-local dependency references to the canonical crates.io source identity.
The archive bytes stay unchanged.

### Why Cargo requires durable mirror state

Cargo publication can reset the original time or yank and omit additive index fields.
One checksum-verified archive proves physical presence but cannot prove faithful original-source metadata.
`DurableVersionState`, `CargoMirrorRead`, and `pcNativeComplete=False` therefore bind target proof to retained source facts.
The worker records the admitted original row, source identity, and expected archive before upload.
Completion retains the existing conditional-write, retry, lost-response, and restart rules.

A target row that reaches `/cargo/` through `privateUpstream` without a durable record is trusted private content, as on PyPI and RubyGems.
Without new state, Écluse cannot tell such a row from a first-party crate.
Missing, corrupt, or unrecorded history cannot become an original-source managed read during outage.
An available admitted public source can still supply a version.
Retained facts reflect first admission. There is no continuous yank synchronisation or target-yank mutation.

### Bounded archive inspection and publication

The sparse index declares no build script or procedural macro, and archive inspection for each evaluated version costs too much on the serve path.
The adapter blocks `DenyInstallTimeExecution` through its typed rule capability, so no policy decision reads an archive.

Archive inspection verifies complete bounded compressed bytes and the effective TOML manifest without executing code.
Publication can make one further local pass over those same bytes for selected text.
Both passes charge their work.
Resolver disagreement can change the package description used for resolution without changing archive bytes.
First-party validation therefore compares upload claims, including native-library uniqueness constraints, with the retained effective manifest before target discovery.
It also refuses the unstable dependency fields that mirroring refuses, so both paths take one stance on nightly-only resolver features.
After agreement, every normal and build dependency must come from crates.io or Écluse, because a dependency on another registry would send every consumer's Cargo to that registry directly.
A development dependency is exempt, as npm's `devDependencies` are, because no consumer resolves it.
Écluse knows its own Cargo index from `server.publicUrl` and the mount path, so this check needs no configuration of its own.
Shared bounded target parsing and logical source identities account for native formatting without changing source rows or upload bytes.
The relay preserves validated upload bytes. Mirror publication derives required upload fields from the verified effective manifest.

## Registry abstraction

The proxy core is registry-agnostic. A mount is where two axes meet: its ecosystem contributes the
protocol adapter, and its store contributes the backend the mount's private packages live in.
[Cloud backends](cloud-backends.md#cloud-backends) covers the two store kinds and what each one
supplies.

The two sides share only the vocabulary above: `Ecosystem`, `PackageName`, and `Version`, plus the
abstract mechanism a store declares for a service credential. Neither imports the other, and a store
that needs an ecosystem fact reads it off the `Ecosystem` value, as the CodeArtifact format token
and its credential presentation table do. Every credential Écluse owns renders through that table,
the Dredger's store reads included. A Basic row carries the store's own username. A backend matrix
therefore costs one adapter per ecosystem plus one backend per store, never a cell per pair. npm,
PyPI, Cargo, and RubyGems ship read and native publication capabilities in this build. Their stores
are CodeArtifact, any host that speaks the protocol, and Verdaccio for development.

Shared code reads what it needs from an adapter's declarations instead of branching on its
ecosystem. Durable mirror state stores, weighs and rechecks a mount's retained read data through the
state format its adapter declares, so a new ecosystem extends no switch in mirror state. The
adapter's first-party declaration reads a mount's `firstParty` entries and decides which names they
own, so the configuration format stays the same while the parser names no ecosystem.

### Incremental npm extraction

npm and PyPI reads walk the tokens of the maintained json-stream lexer (`json-stream/`) inside
the HTTP response lifetime. The reader feeds chunks of at most 32 KiB and counts decompressed bytes.
A full read passes the response body through a digesting source, which updates SHA-256 from each
chunk the reader consumes. A selected read reads the body
directly, because a selected release carries no source digest. A successful result consumes the
complete response, including data after extraction finishes. Transport failure, body limits and
cancellation still close the response through `withResponse`.

Each retained release produces its typed policy record as it arrives. Separate timestamp and tag
maps join those records at the end, so source member order does not affect release association.
A selected npm read skips each sibling release object. A selected PyPI read reads each file up to
its name, and skips the rest of a file that the name puts in another release. Neither read decodes
what it skips, so a member there that would not decode does not fail it. Presence probes and store
enumeration retain only version identifiers and the field shapes needed to exclude unusable
releases.

The [operator field contract](https://ecluse-proxy.com/docs/protocol-support/#npm-metadata-fields)
owns the retained set. Extraction skips unknown fields before constructing values. The parser does
not establish whole-document JSON validity. A scalar or empty retained container consumes one depth
level, and each enclosing retained container adds one. Specialised readers check their own level
before reading members. Skipped structures pass token by token without decoding. Body and version
ceilings bound other work. The walk accepts and refuses the same input as json-stream's parser
combinators, except in the rest of a file that a selected PyPI read skips, and it builds each
retained object once from its members.

The Haskell lexer scans immutable input slices, with no C lexer or foreign result buffer.
Each scan returns a token and a new immutable cursor. Tree reads stay pure, while packed writers
retain their state threads. Returned tokens retain their input slices through later reads.
The private library exposes the parser and one lexer interface. Cursor constructors and raw decoding
stay private. Plain string tokens carry validation that their bytes need no JSON escaping or UTF-8
decoding. Registry readers enforce body and chunk limits outside that interface.
The key accumulator stops at about 64 KiB and the number accumulator at about
200,000 digits. Retained strings become owned `Text`. A full read, and a selected npm read, keeps one
copy of each key and string its releases or files repeat. The walk finds that copy by the bytes it
read, in a table keyed by SipHash-1-3 under a key drawn for that read. When the read ends, it seals
the table's strings into the document's `DocTable` and drops the table. Full reads keep npm releases
and PyPI files packed against that table. Listings copy packed bytes through the render plan.
An npm builder observes typed policy facts beside the generic packed writer. Successful releases
use those facts without decoding a release object. Invalid facts use the existing diagnostic
decoder, including the source's original author. Explicit tree reads keep the whole-release
decoder. Script validation covers every command, while typed facts retain only the presence of the
three install hooks. The packed release preserves every retained command.
PyPI reduces scalar and digest members into typed file facts while the writer packs them.
Successful files need no whole-file tree decode. Invalid files decode only for the existing
diagnostic payload. Both ecosystems retain typed URLs and digests. PyPI files stay in source order
across each winning snapshot. Selected PyPI reads keep their tree reducer and early filename
rejection.
Parser continuations advance before the next chunk, so successful reads do not retain a complete
source buffer.
These bounds do not make required output constant in size. Process peak, native allocation and
retained-live measurements remain distinct from the cached accounting estimate.

## RubyGems integration

RubyGems merges caller-authorised private gems with admitted public gems. It discovers gems through the global Compact Index and acquires metadata per gem.
The public directory shares bounded retention. Caller-authorised private discovery remains fresh and separate from public retention.
Discovery bounds gem names with a fixed ceiling rather than the per-package count limits, so lowering those limits for npm or PyPI cannot break RubyGems discovery.
The public directory follows public availability, except that a bound breach refuses the request as an exceeded input bound does.
Per-gem `/info/<gem>` rows follow the shared merge, admission and mirror restoration paths.
Unknown transformed info-file MD5 values use explicit blank hints in the supported clients.
This keeps detailed metadata acquisition on demand without publishing a false checksum.
Selected compact specifications and archives use exact native coordinates.
The [operator protocol reference](https://ecluse-proxy.com/docs/protocol-support/#rubygems) owns the route and client contract.

Raw version identity differs from native version ordering. Platforms identify separate physical artifacts of the same raw version.
A conflicting coordinate quarantines its version. Selected reads still scan through EOF and enforce counts before selection.
Full reads count distinct raw versions, while selected reads count all nonblank artifact positions, including malformed or skipped rows.
Artifact counts include every nonblank row before projection.

RubyGems supplies no supported deprecation flag or scope. Native builds are normal installation behaviour.
Compact Index `/info` omits a yanked version instead of flagging it, so public reads and the mirror worker never see a yanked gem.
The global `/versions` removal marker is the only yank fact, and no serve, mirror or Dredger path reads it.
The adapter blocks `DenyIfYanked`, `DenyIfDeprecated`, `AllowScope`, `DenyInstallTimeExecution`, and `DenyExternalDependencies` through typed rule capabilities.
No policy-only archive fetch or execution-evidence cache follows from unknown installation metadata.
Bounded archive interpretation remains necessary for publication and mirroring.

A mirror binds all supported platform archives to their original rows, checksums, publication times, and compact specifications in durable state.
A target's regenerated metadata or publication time cannot replace those source facts.
Current target membership and verified bytes must prove the complete inventory before managed reads use retained source data.
Native raw `.gem` publication validates archive identity and relays the original bytes under separate publication authority.
