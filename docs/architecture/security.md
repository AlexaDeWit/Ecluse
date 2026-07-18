# Security: outbound-request and input-validation invariants

> Part of the [Écluse architecture overview](../architecture.md).

Écluse builds outbound HTTP requests (private upstream, public upstream, mirror target) from
client-supplied package identifiers and upstream-supplied artifact locations. The defences
against abusing that are stated, testable invariants, enforced as outbound-request and
input-validation guards at the points below. This document is the why behind the guards; the
deployment assumptions they rest on are in
[Trust assumptions & credential posture](#trust-assumptions--credential-posture).

> The full STRIDE threat register lives in the OWASP Threat Dragon model
> ([`threat-modelling/ecluse.json`](../../threat-modelling/ecluse.json)), published readably
> at [Threat model](https://ecluse-proxy.com/threat-model.html). The threat statements and
> dispositions live there, not here. The guards below implement its mitigations for two
> classes: SSRF and client-controlled fetch targets, and resource-amplification DoS from
> pathological upstream payloads.

<!--
  Do not re-grow this into a full threat enumeration. The authoritative register is
  the Threat Dragon model (threat-modelling/ecluse.json), rendered to the Pages site
  from web/threat-model.md. Keep this a short pointer to the threat classes these
  guards address; add or revise threats in the model, not in prose here.
-->

## Invariants

1. **Identifiers are parsed and canonicalised at the boundary.** An identifier that doesn't
   match the ecosystem's grammar is rejected, and an upstream URL is built from the canonical
   identifier and the configured base, never from raw client path segments. Component safety is
   enforced at the npm router (`isSafeComponent`, applied by `Ecluse.Core.Registry.Npm.Route`),
   which rejects traversal, encoded-slash, and control-character components (see
   [Web layer](web-layer.md#web-layer)). That structural gate is a denylist,
   so it's paired with encode-on-build: every accepted component is percent-encoded
   (`Ecluse.Core.Server.Path.encodeComponent`) when the URL is composed, so a reserved byte the
   denylist admits reaches the upstream re-encoded rather than raw, where a decode-and-normalise
   CDN could resolve it to traversal or a `?`/`#` could inject a query or fragment. The npm URL
   builder (`Ecluse.Core.Registry.Npm.Request`) applies the encoder around the `@` sigil and
   `%2F` scope separator it writes, so a scoped name yields exactly one `%2F`.

2. **Outbound fetches are restricted to the configured upstream hosts and ports** (an
   allowlist of `host:port` pairs). Artifact bytes are fetched only from the upstream-declared
   `dist.tarball`, after the allowlist check, never from a client-supplied URL. The allowlist
   entries are the configured upstream URLs' authorities: a URL that writes no port authorises
   port 443 alone (egress is https-only), and an upstream on a nonstandard port authorises
   exactly the written pair. The comparison always carries the port, so a `dist.tarball`
   naming an allowlisted host on a different port is refused, and a URL with an invalid port
   fails closed at config load. The allowlist is enforced when the URL is built, and Écluse
   never follows an upstream redirect (`redirectCount = 0` for every data-plane request,
   `Ecluse.Core.Registry.Npm.Request.withToken`), so an allowlisted upstream can't `302` a
   fetch off-allowlist. See [Registry model](registry-model.md#registry-abstraction) and
   [URL rewriting](web-layer.md#multi-ecosystem-mounts).

3. **Registry egress is https-only by construction, and certificate validation is the
   endpoint-authentication boundary.** Every outbound registry URL (the public and private
   base URLs, every `dist.tarball` target, any redirect target) is built through one typed
   boundary (`mkRegistryUrl`, `Ecluse.Core.Security.Egress`) that rejects any non-https
   scheme, so a plain-HTTP target can't be represented; a non-https configured endpoint fails
   closed at boot with an error naming the URL. The data-plane manager is a standard
   validating-TLS manager, so the certificate the dialled host presents is checked against the
   system trust store for the requested name. An attacker who steers a name to an internal or
   rebound address can't make it present a CA-trusted certificate for the host, so the
   credential-exfiltration and resolve-to-internal SSRF class is closed by certificate
   validation rather than a resolved-IP recheck. (An operator whose private registry uses an
   internal CA extends the image with their own cert chain; the proxy doesn't pre-bake custom
   CA trust.)

   An upstream-declared `dist.tarball` is normalised before it's dialled: an https target is
   kept, a same-host legacy `http` target is upgraded to https, and `http` on any other host
   is dropped as a graceful per-entry refusal. Behind the host allowlist, a cheap literal
   internal-range block stays as a second gate on the `dist.tarball` host; the trusted private
   origin is exempt, since a private registry may live on an internal address. The fixed range
   set widens with `ECLUSE_EGRESS__ADDITIONAL_BLOCKED_RANGES` (comma-separated CIDRs, applied
   to every mount, fails closed at boot on a malformed entry); there's no knob to narrow it.

4. **Parsed upstream responses are bounded**, on body size, version count, and JSON nesting
   depth, and fail closed past any bound: an oversized or pathological document is refused,
   never partially served. `Ecluse.Core.Registry.Npm.fetchMetadataFormBounded` reads the body
   through `Ecluse.Core.Security.boundedRead` at the `http-client` boundary, so a body past
   the cap is refused as a typed fault before it's buffered whole, and
   `Ecluse.Core.Registry.Npm.Metadata.projectNpmManifest` applies `checkNestingDepth` on the
   decoded document and `checkVersionCount` after projection. Every breach degrades the
   contribution to nothing (the same fail-closed path a parse failure takes), logged at
   `WARNING` with which ceiling and observed-vs-cap, and the merge then serves the best-effort
   union of whatever resolved within budget. The ceilings are operator-tunable with secure
   defaults: `ECLUSE_LIMITS__MAX_RESPONSE_BYTES`, `ECLUSE_LIMITS__MAX_VERSION_COUNT`
   (default 100000), and `ECLUSE_LIMITS__MAX_NESTING_DEPTH` (default 64); see
   [Response bounds](configuration.md#response-bounds). Artifacts stream in constant memory
   and aren't subject to the body-size bound; the inbound client-to-proxy request-body cap
   (default 25 MiB) is enforced at the publish read site as a value, not as middleware: an
   over-cap `Content-Length` fails closed before any byte is read, and a chunked body is
   bounded by a counted read, each answered `413`.

5. **Every served version must carry a strong integrity digest**, by default, in both trust
   contexts. Écluse trusts a digest only as far as its algorithm is collision-resistant, so
   both contexts default to a SHA-256-or-stronger digest; the floors differ only in how far
   they may move.

   - The public (untrusted) floor is a hard SHA-256 boundary (`ECLUSE_INTEGRITY__MIN_PUBLIC`,
     default `sha256`). It may be raised to `sha384`, `sha512`, or `blake2b` but never
     lowered: a sub-floor or unknown value is
     [rejected at config load](configuration.md#public-integrity-floor), never clamped. A
     public version with no digest or one below the floor (e.g. a legacy SHA-1 `dist.shasum`
     with no SRI) is refused: the artifact gate answers `403` and the packument path filters
     it from the listing, so a client never sees a version it couldn't verify. SHA-1 and MD5
     have practical collisions, so a match can't prove the bytes weren't substituted.
   - The trusted (private) floor carries the same `sha256` default
     (`ECLUSE_INTEGRITY__MIN_TRUSTED`), so a SHA-1-only or hashless private version is dropped
     exactly as a public one is. But it's operator-loosenable below SHA-256 (down to
     `sha1`/`md5`) for a legacy private mirror, where trust in the operator's own vetted
     source substitutes for cryptographic strength. That's the only way Écluse serves a
     sub-SHA-256 digest, and only on the trusted private origin. On the serve path the trusted
     floor filters the private listing; the private tarball leg is a
     [conventional stable read](registry-model.md#serving-a-tarball)
     with no serve-time floor, so a below-floor private artifact is still served, its bytes
     verified client-side and by the mirror worker.

   The asymmetry is the point: trust may substitute for cryptographic strength on the
   operator's own vetted source, never on untrusted public bytes. The types enforce it.
   `MinIntegrity` (public) can't be constructed below SHA-256, while `MinTrustedIntegrity`
   (trusted) can, so no config or constructor path lowers the public floor. The floor admits by
   algorithm strength, the digest is computable for every algorithm it admits, and the worker's
   tamper gate verifies it, so an admitted public artifact is always verifiable and reaches the
   mirror.

## Posture

Every guard is deny-by-default and fail-closed, consistent with the rules engine. The
invariants are verified by a hostile-input corpus (traversal, encoded slashes, alternate-host
and absolute URLs, CRLF, metadata and RFC1918 targets, oversized and deeply-nested payloads)
against the pure guards and through the real request path: an oversized body, a version
flood, and a deeply-nested document each drive a fail-closed refusal in
`Ecluse.Server.PipelineSpec`, and the bounded body read is unit-tested at the `http-client`
boundary in `Ecluse.Registry.NpmSpec`.

## Why `dist.tarball` is honoured, and what bounds it

Why fetch from an upstream-declared artifact location at all, rather than reconstruct every
tarball URL from the configured host and refuse anything else? Reconstruction works for public
npm (a tarball lives at `{registry}/{pkg}/-/{file}.tgz`, a pure function of name and version)
but breaks the registries Écluse fronts, where the artifact location is authoritative,
server-chosen data. Tarballs often live on a different host or path than metadata: public PyPI
serves files from a [separate host](web-layer.md#web-layer), and npm third-party registries
(CodeArtifact, Artifactory, GitHub Packages) return `dist.tarball` on a distinct CDN, often with
server-generated paths or short-lived signed query strings that can't be reconstructed.
"Reconstruct or fail" would limit Écluse to registries whose tarball layout matches their
metadata layout.

Honouring the upstream-declared location is the minimum necessary trust, and the residual risk
is bounded by two controls, not URL reconstruction. Wrong bytes are caught by client-side
integrity: the proxy streams artifacts through without rehashing, relying on the packument's
`dist.integrity` (preserved byte-for-byte), and the mirror worker verifies bytes before
publishing, so a poisoned URL can't deliver bytes that install. An unintended fetch target
(SSRF) is the only remaining axis, constrained by the host allowlist (invariant 2) with the
internal-range block (invariant 3) as defence-in-depth. Host recognition is split from the
range test: recognising whether a host is an IP literal stays a hand-rolled, intentionally
lenient parser, while membership of the blocked CIDR ranges is delegated to `iproute`. A strict
library would reject ambiguous spellings like leading-zero octets (`0127.0.0.1`) as
non-literals, letting them skip the block and reach the fetch layer as names; the lenient
recogniser parses them as the address they coerce to and blocks them.

The private leg never consults `dist.tarball`: its same-host conventional read
(`{base}/{pkg}/-/{file}`) satisfies the gate by construction, so a nonstandard private upstream
serving its tarball off-convention isn't reached by it, an accepted limitation (an opt-in
metadata-resolution mode restores it).

## Egress scope: what the outbound controls guard, and what they do not

The outbound egress controls (the host allowlist, https-only egress with TLS certificate
validation, and the literal internal-range block on the `dist.tarball` host gate) constrain
one thing: an untrusted package download whose target an attacker can influence, meaning the
public packument and every public `dist.tarball`. The host allowlist and literal block are
absent from every trusted, operator-declared destination (https-only applies to every
registry endpoint regardless). Firing them on an operator-configured destination, telemetry
export, the mirror-queue publish, or a private registry on an internal address would break
legitimate function for no security gain, since none of those is attacker-influenced.

The two data-plane managers (`envManager`, `envPrivateManager`) are the same validating-TLS
manager; the per-origin split is in credential handling and the internal-range block's
origin-awareness, not the manager. The last column is the untrusted-egress policy (host
allowlist plus literal internal-range block):

| Outbound connection | Trust | Manager / client | Allowlist + internal-range block |
|---|---|---|---|
| Public-upstream packument fetch | Untrusted | `envManager` | **Yes** |
| Public `dist.tarball` artifact stream | Untrusted | `envManager` | **Yes** (plus the tarball-host gate) |
| Mirror worker's public artifact back-fill | Untrusted | `envManager` | **Yes** |
| Private-upstream packument fetch | Trusted | `envPrivateManager` | **No** |
| Private conventional tarball read | Trusted origin | `envPrivateManager` | **No**, same-host by construction |
| Mirror-target publish (npm `PUT`) | Trusted destination | `envPrivateManager` | **No** |
| First-party publish relay | Trusted destination | `envPrivateManager` | **No**, carries the client's forwarded credential, never redirect-followed |
| OTLP telemetry export | Trusted destination | OpenTelemetry SDK client (`Ecluse.Runtime.Telemetry.Resolve`) | **No** |
| SQS mirror-queue publish / poll | Trusted destination | `amazonka` client (`Ecluse.Runtime.Queue.Sqs`) | **No** |
| IMDS instance-role credential minting | Required internal | `amazonka` client (separate from the data plane) | **No**, must reach `169.254.169.254` |

Every registry endpoint above is dialled https-only with certificate validation regardless of
trust: that authenticates the endpoint and closes the resolve-to-internal / rebinding class.
The host allowlist gates only targets built from upstream-supplied data. A configuration
destination (the private base URL, mirror target, OTLP endpoint, SQS queue) is used as given,
not re-validated against an allowlist it would itself define.

**Écluse never follows an upstream redirect.** Every outbound npm data-plane request, anonymous
and credential-bearing alike, is built with `redirectCount = 0` at the single
request-finalisation point (`Ecluse.Core.Registry.Npm.Request.withToken`). This forecloses one
danger on each plane. Credentialed: http-client's default re-sends the `Authorization` header to
a `3xx` `Location` without stripping it cross-host, so a hostile upstream could `302` a
forwarded or minted credential to an attacker-chosen host. Anonymous: the host allowlist is
enforced when the URL is built, not per hop, so following a `302` would let an allowlisted
upstream steer a fetch off-allowlist with nothing re-gating it. A read returns the `3xx` to the
serve path, which honours the packument's `dist.tarball` explicitly instead
(redirect-following for a presigned upstream is an explicit per-upstream opt-in). The invariant
covers the npm data plane; `amazonka` (CodeArtifact, SQS) and the OTLP exporter build their own
requests outside `withToken`, a noted follow-up.

## A static publish credential is fail-closed

The [first-party publish path](registry-model.md#publishing-first-party-packages-the-publication-target)
relays a client `npm publish` to the publication target. Its scope allow-list
(`ECLUSE_MOUNTS__NPM__PUBLISH_ALLOW`) constrains which package names may be published; it is
not authentication and does not verify who is publishing. So if a deployment sets
`ECLUSE_MOUNTS__NPM__PUBLICATION_TARGET_TOKEN`, substituting Écluse's own credential for a
publisher who forwards none, the composition root refuses to boot without a verifiable inbound
edge (`PublishStaticCredentialNeedsEdge`). That makes "static publish credential plus open
edge", which would let any unauthenticated client publish under the operator's credential
within the allowed scopes, unrepresentable. `ECLUSE_SERVER__AUTH_TOKEN` is the verifiable edge
Écluse checks today; an external layer (API gateway, mTLS service mesh, `NetworkPolicy`) is
defence-in-depth but can't substitute for it, since Écluse can only verify its own edge. Pure
passthrough (no static token) carries no such floor: the publisher's forwarded token is the
authority.

### The guard-name ≡ write-name ≡ body-name invariant

The anti-shadowing guard would be bypassable if it validated only the URL-path name while
relaying the publish document byte-for-byte: the npm publish document carries its own declared
identity (a top-level `_id` and `name`, and a `name` per `versions` entry), and a publication
target that resolves the written package from the body (the npm-protocol norm) would write a
name the scope guard never saw. A crafted `PUT /@acme/anything` whose body declares
`@victim/target` would publish outside the allow-list, shadowing a public package.

So the guard holds guard-name ≡ write-name ≡ body-name. After the scope check admits the
URL-path name, every present declared body name (`_id`, top-level `name`, each `versions[].name`)
is compared to it, and any disagreement is a `403` before any upstream write. The comparison is
by `PackageName` equality using the same canonicalisation the route applies (ecosystem-aware,
npm case-sensitive), so an encoding variant (`@acme%2ffoo` vs `@acme/foo`) can't disagree
silently. Only the names are parsed; the base64 `_attachments` are never decoded. An absent
declared name is not a bypass-grant; only a present, mismatching one is refused, since a
legitimate client always sends names matching its publish URL. This makes the control sound
whether the downstream target keys the write off the body or the URL.

## Network egress is a shared responsibility

Écluse's outbound guards are the primary, application-layer control; pair them with the
deployment's own egress controls, as for any service that fetches on a client's behalf. The
cloud-metadata SSRF is handled at the service-behaviour level, not by blocking metadata at the
network: Écluse follows an internal-resolving location only on the trusted private origin
(invariant 3), never on a public-upstream-derived target, so an SSRF can't steer it at
`169.254.169.254` or `fd00:ec2::254`. Meanwhile Écluse needs the metadata endpoint to mint its
instance-role credentials (`AWS.newEnv AWS.discover` builds amazonka's own HTTP client,
separate from the data-plane manager, so minting reaches IMDS regardless). So the platform
controls must protect the data targets without cutting the proxy off from metadata or its
private upstream's internal range. The concrete per-platform runbook (harden IMDSv2,
default-deny egress scoped to the upstreams and mirror target, scope the instance role) is the
operator's, in
[Securing network egress](../../USAGE.md#securing-network-egress-required).

## Trust assumptions & credential posture

The guards above constrain Écluse's own requests; this section records the deployment
assumptions the [threat model](https://ecluse-proxy.com/threat-model.html) rests on, and the
consequences of the canonical posture (per-caller passthrough credentials, the three-registry
topology, CodeArtifact over VPC endpoints).

**Edge access is an operator concern.** `ECLUSE_SERVER__AUTH_TOKEN` is off by default, so who
may reach the proxy is delegated to the deployment's access edge, which must hold east-west as
well as north-south (an ingress-only allow-list that leaves pod-to-pod traffic open is the
usual gap). Passthrough softens this: a caller with no forwarded token gets no private read or
publish, so an edge breach exposes only the public-gated view plus the untrusted-egress and
DoS surface, never private packages. A
[trusted-edge-identity mode](access-model.md#planned-service-credentials-and-trusted-edge-identity)
that accepts a signed identity from a fronting proxy is planned, and would require a
verifiable binding to the edge (mutual TLS, or a shared secret / HMAC on the assertion), not a
runtime hope. It's not yet shipped.

**Passthrough relocates credential risk to the proxy runtime.** Forwarding each caller's own
credential ([access model](access-model.md)) leaves Écluse holding no standing read or publish
credential, but transiently holding every in-transit caller's in memory. So Écluse's own runtime
and supply-chain integrity are a first-class control (the attested, reproducible image,
[release supply chain](release-supply-chain.md)), and the token-stripping boundary and the
[no-redirect-with-credential invariant](#egress-scope-what-the-outbound-controls-guard-and-what-they-do-not)
are load-bearing, because real caller credentials cross them.

**The mirror-target write token is the one standing credential a mirrored deployment holds** (a
serve-only deployment holds none), and its sharpest privilege, since it writes the trusted
store: scope it write-only, prefer container-role minting over a static secret, and minimise its
TTL. The mirror queue is part of the same trust boundary: a job is unauthenticated and directs
the worker to fetch-and-publish, so anyone who can enqueue can make the worker write the trusted
store; scope its IAM too (only the serve role enqueues, only the worker consumes). The worker
narrows what a forged or stale job can do: the artifact URL is re-formed into its https-only
`RegistryUrl` witness at wire decode, the fetch host is re-checked against the tarball-host gate
at ingest, the version is re-decided through the shared admission gate, and the fetched bytes
must match the digest of the artifact that gate re-admits before any publish.

**Registry separation is defence-in-depth and auditability, not the perimeter.** The
three-registry topology
([registry-level composition](registry-model.md#registry-level-composition-the-recommended-topology))
keeps first-party and public-derived inventory physically separable, with per-provenance
rule-sets, scanning, and clean post-disclosure scoping. Collapsing toward one registry
degrades auditability and mitigation depth but doesn't move the trust perimeter: the
public-to-trusted admission gate is identical at one registry or three. Storage-layer scanning
is out of scope for Écluse; it's ecosystem- and backend-specific, the operator's to configure.
