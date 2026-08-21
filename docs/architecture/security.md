# Security: outbound-request and input-validation invariants

> Part of the [Écluse architecture overview](../architecture.md).

Écluse builds outbound HTTP requests (private upstream, public upstream, mirror target) from
client-supplied package identifiers and upstream-supplied artifact locations. The defences
against abusing that are stated, testable invariants, enforced as outbound-request and
input-validation guards at the points below. This document is the why behind the guards. The
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

1. **Écluse parses and canonicalises an identifier at the boundary.** It rejects an identifier
   that doesn't match the ecosystem's grammar. It builds the upstream URL from the canonical
   identifier and the configured base, never from raw client path segments. The npm router
   enforces component safety (`isSafeComponent`, applied by
   `Ecluse.Core.Registry.Npm.Route`), rejecting traversal, encoded-slash, and control-character
   components (see [Web layer](web-layer.md#web-layer)). That structural gate is a denylist, so
   encode-on-build pairs with it. The builder percent-encodes every accepted component
   (`Ecluse.Core.Server.Path.encodeComponent`) as it composes the URL. A reserved byte the
   denylist admits then reaches the upstream re-encoded rather than raw. Sent raw, it could
   reach a decode-and-normalise CDN that resolves it to traversal, or a `?`/`#` could inject a
   query or fragment. The npm URL builder (`Ecluse.Core.Registry.Npm.Request`) applies the
   encoder around the `@` sigil and the `%2F` scope separator it writes. A scoped name yields
   exactly one `%2F`.

2. **Écluse restricts an outbound fetch to the configured upstream hosts and ports**, an
   allowlist of `host:port` pairs. It fetches artifact bytes only from the upstream-declared
   `dist.tarball`, after the allowlist check, never from a client-supplied URL. The allowlist
   entries are the configured upstream URLs' authorities. A URL that writes no port authorises
   port 443 alone, since egress is https-only. An upstream on a nonstandard port authorises
   exactly the written pair. The comparison always carries the port, so Écluse refuses a
   `dist.tarball` naming an allowlisted host on a different port. A URL with an invalid port
   fails closed at config load. The proxy enforces the allowlist when it builds the URL, and
   never follows an upstream redirect (`redirectCount = 0` for every data-plane request,
   `Ecluse.Core.Registry.Npm.Request.withToken`). So an allowlisted upstream can't `302` a fetch
   off-allowlist. See [Registry model](registry-model.md#registry-abstraction) and
   [URL rewriting](web-layer.md#multi-ecosystem-mounts).

3. **Registry egress is https-only by construction, and certificate validation is the
   endpoint-authentication boundary.** One typed boundary builds every outbound registry URL.
   `mkRegistryUrl` (`Ecluse.Core.Security.Egress`) covers the public and private base URLs,
   every `dist.tarball` target, and any redirect target. It rejects any non-https scheme, so a
   plain-HTTP target can't be represented. A non-https configured endpoint fails closed at
   boot with an error naming the URL. The data-plane manager is a standard validating-TLS
   manager. It checks the certificate the dialled host presents against the system trust store
   for the requested name. An attacker who steers a name to an internal or rebound address can't
   make it present a CA-trusted certificate for the host. Certificate validation, not a
   resolved-IP recheck, is what closes the credential-exfiltration and resolve-to-internal SSRF
   class. An operator whose private registry uses an internal CA extends the image with their
   own cert chain. The proxy doesn't pre-bake custom CA trust.

   Écluse normalises an upstream-declared `dist.tarball` before dialling it. It keeps an https
   target, upgrades a same-host legacy `http` target to https, and drops `http` on any other
   host as a graceful per-entry refusal. Behind the host allowlist, a cheap literal
   internal-range block stays as a second gate on the `dist.tarball` host. The trusted private
   origin is exempt, since a private registry may live on an internal address. The fixed range
   set widens with `ECLUSE_EGRESS__ADDITIONAL_BLOCKED_RANGES` (comma-separated CIDRs, applied to
   every mount, failing closed at boot on a malformed entry). There's no knob to narrow it.

4. **Écluse bounds every parsed upstream response**, on body size, version count, and JSON
   nesting depth, and fails closed past any bound. It refuses an oversized or pathological
   document rather than serving it in part.
   `Ecluse.Core.Registry.Npm.fetchMetadataFormBounded` reads the body through
   `Ecluse.Core.Security.boundedRead` at the `http-client` boundary. A body past the cap fails
   as a typed fault before anything buffers it whole.
   `Ecluse.Core.Registry.Npm.Metadata.projectNpmManifest` applies `checkNestingDepth` on the
   decoded document and `checkVersionCount` after projection. Every breach degrades the
   contribution to nothing, the same fail-closed path a parse failure takes. Écluse logs the
   breach at `WARNING` with which ceiling and observed-vs-cap. The merge then serves the
   best-effort union of whatever resolved within budget.

   The ceilings are operator-tunable with secure defaults:
   `ECLUSE_LIMITS__MAX_RESPONSE_BYTES`, `ECLUSE_LIMITS__MAX_VERSION_COUNT` (default 100000), and
   `ECLUSE_LIMITS__MAX_NESTING_DEPTH` (default 64). See
   [Response bounds](configuration.md#response-bounds). Artifacts stream in constant memory and
   aren't subject to the body-size bound. The publish read site enforces the inbound
   client-to-proxy request-body cap (default 25 MiB) as a value, not as middleware. An over-cap
   `Content-Length` fails closed before any byte is read, and a counted read bounds a chunked
   body. Écluse answers each with `413`.

5. **Every served version must carry a strong integrity digest**, by default, in both trust
   contexts. Écluse trusts a digest only as far as its algorithm is collision-resistant, so
   both contexts default to a SHA-256-or-stronger digest. The floors differ only in how far
   they may move.

   - The public (untrusted) floor is a hard SHA-256 boundary (`ECLUSE_INTEGRITY__MIN_PUBLIC`,
     default `sha256`). An operator may raise it to `sha384`, `sha512`, or `blake2b` but never
     lower it. Config load [rejects](configuration.md#public-integrity-floor) a sub-floor or
     unknown value, and never clamps it. The floor refuses a public version with no digest, or
     one below it, such as a legacy SHA-1 `dist.shasum` with no SRI. The artifact gate
     answers `403` and the packument path filters the version from the listing, so a client
     never sees a version it couldn't verify. SHA-1 and MD5 have practical collisions, so a
     match can't prove the bytes weren't substituted.
   - The trusted (private) floor carries the same `sha256` default
     (`ECLUSE_INTEGRITY__MIN_TRUSTED`), so Écluse drops a SHA-1-only or hashless private version
     exactly as a public one. But an operator can loosen it below SHA-256, down to
     `sha1`/`md5`, for a legacy private mirror. There, trust in the operator's own vetted source
     substitutes for cryptographic strength. That's the only way Écluse serves a sub-SHA-256
     digest, and only on the trusted private origin. On the serve path the trusted floor filters
     the private listing. The private tarball leg is a
     [conventional stable read](registry-model.md#serving-a-tarball) with no serve-time floor,
     so Écluse still serves a below-floor private artifact. The client and the mirror worker
     verify its bytes.

   The asymmetry is the point: trust may substitute for cryptographic strength on the operator's
   own vetted source, never on untrusted public bytes. The types enforce it. No code can
   construct `MinIntegrity` (public) below SHA-256, while `MinTrustedIntegrity` (trusted) can go
   lower, so no config or constructor path lowers the public floor. The floor admits by
   algorithm strength, and the digest is computable for every algorithm it admits. The worker's
   tamper gate verifies that digest, so an admitted public artifact is always verifiable and
   reaches the mirror.

## Posture

Every guard is deny-by-default and fail-closed, consistent with the rules engine. A
hostile-input corpus verifies the invariants against the pure guards and through the real
request path. It holds traversal, encoded slashes, alternate-host and absolute URLs,
CRLF, metadata and RFC1918 targets, and oversized and deeply-nested payloads. An oversized body,
a version flood, and a deeply-nested document each drive a fail-closed refusal in
`Ecluse.Server.PipelineSpec`. A unit test covers the bounded body read at the `http-client`
boundary in `Ecluse.Registry.NpmSpec`.

## Why `dist.tarball` is honoured, and what bounds it

Why fetch from an upstream-declared artifact location at all, rather than reconstruct every
tarball URL from the configured host and refuse anything else? Reconstruction works for public
npm, where a tarball lives at `{registry}/{pkg}/-/{file}.tgz`, a pure function of name and
version. It breaks on the registries Écluse fronts, where the artifact location is
authoritative, server-chosen data. Tarballs often live on a different host or path than
metadata. Public PyPI serves files from a [separate host](web-layer.md#web-layer). An npm
third-party registry (CodeArtifact, Artifactory, GitHub Packages) returns `dist.tarball` on a
distinct CDN. That path is often server-generated, or carries a short-lived signed query string,
and no one can reconstruct it. "Reconstruct or fail" would limit Écluse to registries whose
tarball layout matches their metadata layout.

Honouring the upstream-declared location is the minimum necessary trust. Two controls bound the
residual risk, not URL reconstruction. Client-side integrity catches wrong bytes. The proxy
streams artifacts through without rehashing, relying on the packument's `dist.integrity`,
preserved byte-for-byte. The mirror worker verifies bytes before publishing. Together they mean
a poisoned URL can't deliver bytes that install. An unintended fetch target (SSRF) is the only
remaining axis. The host allowlist (invariant 2) constrains it, with the internal-range block
(invariant 3) as defence-in-depth.

Écluse splits host recognition from the range test. Recognising whether a host is an IP literal
stays a hand-rolled, intentionally lenient parser, while `iproute` decides membership of the
blocked CIDR ranges. A strict library would reject an ambiguous spelling like a leading-zero
octet (`0127.0.0.1`) as a non-literal. That spelling would then skip the block and reach the
fetch layer as a name. The lenient recogniser parses it as the address it coerces to, and blocks
it.

The private leg never consults `dist.tarball`. Its same-host conventional read
(`{base}/{pkg}/-/{file}`) satisfies the gate by construction. So the private leg never reaches a
nonstandard private upstream that serves its tarball off-convention, an accepted limitation. An
opt-in metadata-resolution mode restores it.

## Egress scope: what the outbound controls guard, and what they do not

The outbound egress controls constrain one thing: an untrusted package download whose target an
attacker can influence, meaning the public packument and every public `dist.tarball`. Those
controls are the host allowlist, https-only egress with TLS certificate validation, and the
literal internal-range block on the `dist.tarball` host gate. The host allowlist and the
literal block are absent from every trusted, operator-declared destination. The proxy applies
https-only to every registry endpoint regardless. Firing them on an operator-configured
destination, a telemetry export, the mirror-queue publish, or a private registry on an internal
address would break legitimate function. None of those is attacker-influenced, so there is no
security gain either.

The two data-plane managers (`envManager`, `envPrivateManager`) are the same validating-TLS
manager. The per-origin split is in credential handling and the internal-range block's
origin-awareness, not the manager. The last column is the untrusted-egress policy: the host
allowlist plus the literal internal-range block.

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

Écluse dials every registry endpoint above https-only with certificate validation, regardless of
trust. That authenticates the endpoint and closes the resolve-to-internal and rebinding class.
The host allowlist gates only a target built from upstream-supplied data. The proxy uses a
configuration destination (the private base URL, mirror target, OTLP endpoint, SQS queue) as
given. It does not re-validate that destination against an allowlist it would itself define.

**Écluse never follows an upstream redirect.** The single request-finalisation point
(`Ecluse.Core.Registry.Npm.Request.withToken`) builds every outbound npm data-plane request with
`redirectCount = 0`, anonymous and credential-bearing alike. This forecloses one danger on each
plane. Credentialed: http-client's default re-sends the `Authorization` header to a `3xx`
`Location` without stripping it cross-host. A hostile upstream could `302` a forwarded or minted
credential to an attacker-chosen host. Anonymous: Écluse enforces the host allowlist when it
builds the URL, not per hop. Following a `302` would let an allowlisted upstream steer a fetch
off-allowlist with nothing re-gating it. A read returns the `3xx` to the serve path, which
honours the packument's `dist.tarball` instead. Redirect-following for a presigned upstream is
an explicit per-upstream opt-in. The invariant covers the npm data plane. `amazonka`
(CodeArtifact, SQS) and the OTLP exporter build their own requests outside `withToken`, a noted
follow-up.

## A static publish credential is fail-closed

The [first-party publish path](registry-model.md#publishing-first-party-packages-the-publication-target)
relays a client `npm publish` to the publication target. Its scope allow-list
(`ECLUSE_MOUNTS__NPM__PUBLISH_ALLOW`) constrains which package names a client may publish. It
is not authentication and does not verify who is publishing. A deployment may set
`ECLUSE_MOUNTS__NPM__PUBLICATION_TARGET_TOKEN`, substituting Écluse's own credential for a
publisher who forwards none. If it does, the composition root refuses to boot without a
verifiable inbound edge (`PublishStaticCredentialNeedsEdge`). That makes "static publish
credential plus open edge" unrepresentable. Such a pairing would let any unauthenticated client
publish under the operator's credential within the allowed scopes. `ECLUSE_SERVER__AUTH_TOKEN`
is the verifiable edge Écluse checks today. An external layer (API gateway, mTLS service mesh,
`NetworkPolicy`) is defence-in-depth but can't substitute for it, since Écluse can only verify
its own edge. Pure passthrough (no static token) carries no such floor: the publisher's
forwarded token is the authority.

### The guard-name ≡ write-name ≡ body-name invariant

The npm publish document carries its own declared identity: a top-level `_id` and `name`, and a
`name` per `versions` entry. A publication target that resolves the written package from the
body, the npm-protocol norm, would write a name the scope guard never saw. So an anti-shadowing
guard that validated only the URL-path name while relaying the document byte-for-byte would be
bypassable. A crafted `PUT /@acme/anything` whose body declares `@victim/target` would publish
outside the allow-list, shadowing a public package.

So the guard holds guard-name ≡ write-name ≡ body-name. After the scope check admits the
URL-path name, the guard compares every present declared body name to it: `_id`, top-level
`name`, and each `versions[].name`. Any disagreement is a `403` before any upstream write. The
comparison is `PackageName` equality under the same canonicalisation the route applies
(ecosystem-aware, npm case-sensitive), so an encoding variant (`@acme%2ffoo` vs `@acme/foo`)
can't disagree silently. The guard parses only the names and never decodes the base64
`_attachments`. An absent declared name is not a bypass-grant. The guard refuses only a
present, mismatching name, since a legitimate client always sends names matching its publish
URL. This makes the control sound whether the downstream target keys the write off the body or
the URL.

## Network egress is a shared responsibility

Écluse's outbound guards are the primary, application-layer control. Pair them with the
deployment's own egress controls, as for any service that fetches on a client's behalf. The
proxy handles the cloud-metadata SSRF at the service-behaviour level, not by blocking metadata
at the network. It follows an internal-resolving location only on the trusted private origin
(invariant 3), never on a public-upstream-derived target. So an SSRF can't steer it at
`169.254.169.254` or `fd00:ec2::254`.

Écluse still needs the metadata endpoint to mint its instance-role credentials.
`AWS.newEnv AWS.discover` builds amazonka's own HTTP client, separate from the data-plane
manager, so minting reaches IMDS regardless. The platform controls must therefore protect the
data targets without cutting the proxy off from metadata or its private upstream's internal
range. The concrete per-platform runbook is the operator's, in
[Securing network egress](../../USAGE.md#securing-network-egress-required). It covers hardening
IMDSv2, default-deny egress scoped to the upstreams and mirror target, and scoping the instance
role.

## Trust assumptions & credential posture

The guards above constrain Écluse's own requests. This section records the deployment
assumptions the [threat model](https://ecluse-proxy.com/threat-model.html) rests on. It also
records the consequences of the canonical posture: per-caller passthrough credentials, the
three-registry topology, and CodeArtifact over VPC endpoints.

**Edge access is an operator concern.** `ECLUSE_SERVER__AUTH_TOKEN` is off by default, so the
deployment's access edge decides who may reach the proxy. That edge must hold east-west as well
as north-south. An ingress-only allow-list that leaves pod-to-pod traffic open is the usual gap.
Passthrough softens this: a caller with no forwarded token gets no private read or publish. An
edge breach then exposes only the public-gated view plus the untrusted-egress and DoS surface,
never private packages. A
[trusted-edge-identity mode](access-model.md#planned-service-credentials-and-trusted-edge-identity)
that accepts a signed identity from a fronting proxy is planned. It would require a verifiable
binding to the edge (mutual TLS, or a shared secret / HMAC on the assertion), not a runtime
hope. It's not yet shipped.

**Passthrough relocates credential risk to the proxy runtime.** Forwarding each caller's own
credential ([access model](access-model.md)) leaves Écluse holding no standing read or publish
credential. It does hold every in-transit caller's credential in memory, transiently. So
Écluse's own runtime and supply-chain integrity are a first-class control: the attested,
reproducible image ([release supply chain](release-supply-chain.md)). The token-stripping
boundary and the
[no-redirect-with-credential invariant](#egress-scope-what-the-outbound-controls-guard-and-what-they-do-not)
are load-bearing, because real caller credentials cross them.

**The mirror-target write token is the one standing credential a mirrored deployment holds.** A
serve-only deployment holds none. The token is also the sharpest privilege, since it writes the
trusted store. Scope it write-only, prefer container-role minting over a static secret, and
minimise its TTL.

The mirror queue is part of the same trust boundary. A job is unauthenticated and directs the
worker to fetch-and-publish, so anyone who can enqueue can make the worker write the trusted
store. Scope its IAM too: only the serve role enqueues, and only the worker consumes. The worker
narrows what a forged or stale job can do. It re-forms the artifact URL into its https-only
`RegistryUrl` witness at wire decode, and re-checks the fetch host against the tarball-host gate
at ingest. It re-decides the version through the shared admission gate. The fetched bytes must
match the digest of the artifact that gate re-admits before any publish.

**Registry separation is defence-in-depth and auditability, not the perimeter.** The
three-registry topology
([registry-level composition](registry-model.md#registry-level-composition-the-recommended-topology))
keeps first-party and public-derived inventory physically separable. That gives per-provenance
rule-sets, scanning, and clean post-disclosure scoping. Collapsing toward one registry degrades
auditability and mitigation depth but doesn't move the trust perimeter. The public-to-trusted
admission gate is identical at one registry or three. Storage-layer scanning is out of scope for
Écluse. It's ecosystem- and backend-specific, the operator's to configure.
