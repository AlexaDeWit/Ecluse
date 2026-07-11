# Security: outbound-request and input-validation invariants

> Part of the [Écluse architecture overview](../architecture.md).

Écluse builds outbound HTTP requests (private upstream, public upstream, mirror target)
from client-supplied package identifiers and upstream-supplied artifact locations. The
defences against abusing that are stated, testable invariants, implemented as
outbound-request and input-validation guard primitives, enforced at the points below.

> The full STRIDE threat register lives in the OWASP Threat Dragon model
> ([`threat-modelling/ecluse.json`](../../threat-modelling/ecluse.json)), published
> readably at [Threat model](https://ecluse-proxy.com/threat-model.html). This
> document is the *why* behind the guards; the deployment assumptions it rests on are in
> [Trust assumptions & credential posture](#trust-assumptions--credential-posture).

## Threats these guards address

These guards implement the register's mitigations for SSRF / unintended fetch targets and
client-controlled artifact source (register threat #5) and resource-amplification DoS via
pathological upstream payloads (register threat #2). The threat statements and
dispositions live in the [register](https://ecluse-proxy.com/threat-model.html),
not here.

<!--
  Do not re-grow this into a full threat enumeration. The authoritative register is
  the Threat Dragon model (threat-modelling/ecluse.json), rendered to the Pages site
  from web/threat-model.md. Keep this a short pointer to the threat classes these
  guards address; add or revise threats in the model, not in prose here.
-->

## Invariants

1. **Identifiers are parsed and canonicalised at the boundary.** An identifier that does
   not match the ecosystem's grammar is rejected; an upstream URL is built from the
   canonical identifier and the configured base, never from raw client path segments. The
   router's `classify` / `isSafeComponent` rejects traversal, encoded-slash, and
   control-character components (see [Web Layer](web-layer.md#raw-wai-not-a-web-framework)).
   That structural gate is a denylist, so it is paired with encode-on-build: every
   accepted name component is percent-encoded
   (`Ecluse.Core.Server.Route.encodeComponent`) when the URL is composed, around the `@`
   sigil and `%2F` scope separator the builder writes itself, so a scoped name yields
   exactly one `%2F`. A reserved byte the denylist admits (`%`, `?`, `#`, `;`, space;
   canonically a once-decoded `%2e%2e%2f`) is re-encoded (`%2e%2e%2f` → `%252e%252e%252f`)
   rather than reaching the upstream raw, where a decode-and-normalise CDN could resolve
   it to traversal or a `?`/`#` could inject a query/fragment. The data-plane URL builder
   (`Ecluse.Core.Registry.Npm.Request`) applies this encoder around the structural sigils
   it writes; component safety itself is enforced at the router
   (`Ecluse.Core.Server.Route.isSafeComponent`) before a name is accepted.
2. **Outbound fetches are restricted to the configured upstream hosts** (an allowlist).
   Artifact bytes are fetched only from the upstream-declared `dist.tarball`, after the
   allowlist check, never from a client-supplied URL. The allowlist is enforced when the
   URL is built, and Écluse never follows an upstream redirect (`redirectCount = 0` for
   every data-plane request, `Ecluse.Core.Registry.Npm.withToken`), so an allowlisted
   upstream cannot `302` a fetch off-allowlist: the only host dialled is the one the
   allowlist admitted. See [Registry Model](registry-model.md#registry-abstraction) and
   [URL rewriting](web-layer.md#web-layer).
3. **Registry egress is https-only by construction, and certificate validation is the
   endpoint-authentication boundary.** Every outbound registry URL, the public and private
   base URLs, every `dist.tarball` target, and any redirect target, is built through a
   single typed boundary (`mkRegistryUrl`, `Ecluse.Core.Security.Egress`) that rejects any
   non-https scheme, so a plain-HTTP target cannot be represented. A non-https configured
   endpoint, public or private, fails closed at boot with an error naming the offending
   URL. The data-plane manager is a standard validating TLS manager, so the certificate
   the dialled host presents is checked against the system trust store for the requested
   name. An attacker who steers a name to an internal or rebound address cannot make it
   present a CA-trusted certificate for the host, so the credential-exfiltration and
   resolve-to-internal SSRF class is closed by certificate validation rather than a
   resolved-IP recheck. (An operator whose private registry uses an internal CA extends
   the image with their own cert chain; the proxy does not pre-bake custom CA trust.)

   An upstream-declared `dist.tarball` is normalised before it is dialled: an https target
   is kept, a same-host legacy `http` target is upgraded to https, and `http` on any other
   host is dropped as a graceful per-entry refusal. A legacy `http://` registry endpoint
   is non-supported. Behind the host allowlist (invariant 2), a cheap literal
   internal-range block stays as a second gate on the `dist.tarball` host; the trusted
   private origin is exempt from it, since a private registry may live on an internal
   address. The fixed range set is extensible with `ECLUSE_ADDITIONAL_BLOCKED_RANGES`
   (comma-separated CIDRs, applied to every mount, fails closed at boot on a malformed
   entry); it only widens the block, with no knob to narrow it.
4. **Parsed upstream responses are bounded**, maximum body size, version count, and JSON
   nesting depth, and fail closed past any bound: an oversized or pathological document is
   refused, never partially served. `Ecluse.Core.Registry.Npm.fetchMetadataFormBounded`
   reads the body through `Ecluse.Core.Security.boundedRead` at the `http-client` boundary
   (so a body past the cap is refused fail-closed, as a typed fault value, before it is
   buffered whole), and `Ecluse.Core.Registry.Npm.Metadata.projectNpmManifest` applies
   `checkNestingDepth` on the decoded document and `checkVersionCount` after projection. Every breach degrades the
   contribution to nothing, the same fail-closed path a parse failure takes, logged at
   `WARNING` (which ceiling, observed-vs-cap) so it is distinguishable from a parse
   failure; the merge then serves the best-effort union of whatever resolved within budget.
   The body-size cap precedes the decode, so the document reaching the depth check is
   already size-bounded, and the depth check bounds its traversal cost. The ceilings are
   operator-tunable with secure defaults (`ECLUSE_MAX_RESPONSE_BYTES` /
   `ECLUSE_MAX_VERSION_COUNT` / `ECLUSE_MAX_NESTING_DEPTH`; see
   [Response bounds](configuration.md#response-bounds)). Artifacts stream with constant
   memory and are not subject to the body-size bound; the inbound client→proxy
   request-body cap is the separate `sizeLimitMiddleware`.
5. **Every served version must carry a strong integrity digest, by default, in both trust
   contexts.** Écluse trusts a digest only as far as its algorithm is collision-resistant.
   By default both upstream contexts require a SHA-256-or-stronger digest; the floors
   differ only in how far they may move:

   - The public (untrusted) floor is a hard SHA-256 boundary
     (`ECLUSE_MIN_PUBLIC_INTEGRITY`, default SHA-256). It may be raised to
     `sha384`/`sha512`/`blake2b` but never lowered below SHA-256: a sub-floor or unknown
     value is [rejected at config load](configuration.md#public-integrity-floor), never
     clamped, and no configuration accepts a sub-SHA-256 digest from an untrusted public
     upstream. A public version with no digest (`MissingIntegrity`) or one below the floor
     (e.g. a legacy SHA-1 `dist.shasum` with no SRI, `BelowIntegrityFloor`) is refused: the
     artifact gate answers `403` and the packument path filters it out of the listing, so
     a client never sees a version it could not verify. SHA-1 and MD5 have practical
     collisions, so a match cannot prove the bytes were not substituted.
   - The trusted (private) floor carries the same SHA-256 default
     (`ECLUSE_MIN_TRUSTED_INTEGRITY`, default SHA-256), so a SHA-1-only or hashless private
     version is dropped exactly as a public one is. But it is operator-loosenable below
     SHA-256 (down to `sha1`/`md5`) for a legacy private mirror, where trust in the
     operator's own vetted source substitutes for cryptographic strength. That is the only
     way Écluse serves a sub-SHA-256 digest, and only on the trusted private origin. On the
     serve path the trusted floor filters the private listing (the packument route); the
     private tarball serve leg is a
     [conventional stable read](registry-model.md#serving-a-tarball-a-conventional-private-read-an-honoured-public-location)
     with no serve-time floor, so a below-floor private artifact is still served, its bytes
     verified client-side by npm and by the mirror worker (an opt-in metadata-resolution
     mode restores the floor here).

   The asymmetry is the point: trust may substitute for cryptographic strength on the
   operator's own vetted source, never on untrusted public bytes. The types enforce it:
   `MinIntegrity` (public) cannot be constructed below SHA-256, while `MinTrustedIntegrity`
   (trusted) can, so no config or constructor path lowers the public floor.

   The shared-weak-digest divergence cross-check is a consequence of loosening the trusted
   floor, not a default. For packument merging and cross-upstream divergence, see
   [Registry Model → Packument merge](registry-model.md#packument-merge-across-upstreams).

   The algorithm authority order is `HashAlg`'s `Ord` in `Ecluse.Core.Package`; the
   floor predicate and the strongest-digest selection live in
   `Ecluse.Core.Package.Integrity` (`meetsFloor`, `authoritativeDigest`); the worker's
   digest computation sits beside the validation in `Ecluse.Core.Package`
   (`computeDigest`). The floor admits by strength, the worker's
   tamper gate (`Ecluse.Core.Worker.verifyIntegrity`) verifies the one digest the shared
   selection names, and the computable set covers every algorithm the public floor admits,
   so an admitted public artifact is always verifiable and reaches the mirror. That holds
   by construction: the selection is one function both gates consult, `computeDigest` is
   total over the algorithm set, and a property pins that any floor-admitted digest set
   verifies its own bytes.

## Posture

Every guard is deny-by-default and fail-closed, consistent with the rules engine. The
invariants are verified by a hostile-input corpus (traversal, encoded slashes,
alternate-host and absolute URLs, CRLF, metadata and RFC1918 targets, oversized and
deeply-nested payloads) against the pure guards and through the real request path: an
oversized body, a version flood, and a deeply-nested document each drive a fail-closed
refusal in `Ecluse.Server.PipelineSpec`, and the bounded body read is unit-tested at the
`http-client` boundary in `Ecluse.Registry.NpmSpec`.

## Why `dist.tarball` is honoured, and what bounds it

Why fetch from an upstream-declared artifact location at all, rather than reconstruct
every tarball URL from the configured host and refuse anything else?

That works for public npm (a tarball lives at `{registry}/{pkg}/-/{file}.tgz`, a pure
function of name + version) but breaks the registries Écluse fronts. The artifact
location is authoritative, server-chosen data, not a derivable fact:

- **Tarballs often live on a different host or path than metadata.** Public PyPI serves
  files from a [separate host](web-layer.md#web-layer); npm
  third-party registries (CodeArtifact, Artifactory, GitHub Packages) return `dist.tarball`
  on a distinct CDN, often with server-generated paths or short-lived signed query strings
  that cannot be reconstructed.

This is the public leg's reasoning. The private serve leg is a
[conventional stable read](registry-model.md#serving-a-tarball-a-conventional-private-read-an-honoured-public-location)
(`{base}/{pkg}/-/{file}`, no `dist.tarball`), so a nonstandard private upstream serving
its tarball off-convention is not reached by it, an accepted limitation (an opt-in
metadata-resolution mode restores it).

For the public leg, "reconstruct or fail" would limit Écluse to registries whose tarball
layout equals their metadata layout. Honouring the upstream-declared location is the
minimum necessary trust, and the residual risk is bounded by two controls, not URL
reconstruction:

- **Wrong bytes** are caught by client-side integrity: the proxy streams artifacts through
  without rehashing, relying on the packument's `dist.integrity`, preserved byte-for-byte,
  and the mirror worker verifies bytes against `dist.integrity` before publishing. A
  poisoned URL cannot deliver bytes that install.
- **An unintended fetch target (SSRF)** is the only remaining axis, constrained by the host
  allowlist (invariant 2), with the internal-range block (invariant 3) as defence-in-depth.

The load-bearing guard is `isAllowedUpstreamHost`; the IP-range block is its backstop,
split deliberately. Recognising whether a host is an IP literal stays a hand-rolled,
intentionally lenient parser; testing a recognised address for membership of the blocked
CIDR ranges is delegated to `iproute` (`isBlockedIP`). The split matters: a strict library
rejects ambiguous spellings like leading-zero octets (`0127.0.0.1`, `010.0.0.1`) as
non-literals, letting them skip the block and reach the fetch layer as names, silently
narrowing the gate. The lenient recogniser parses them as the address they coerce to on a
typical resolver and blocks them; a malformed group overflowing 16 bits (`fe80::1ffff`) is
treated as a name the allowlist constrains. Only membership is delegated.

This is the literal internal-range block on the `dist.tarball` host gate
(`isBlockedTarget`): a `dist.tarball` whose host is an internal-address literal is refused
there. A DNS name that resolves to an internal address is not re-checked at connect time;
that window is closed by https-only egress with certificate validation (invariant 3), since
a rebound or internal address cannot present a CA-trusted certificate for the requested
host, so the handshake fails rather than reaching the internal target.

## Egress scope: what the outbound controls guard, and what they do not

The outbound egress controls, the host allowlist (`isAllowedUpstreamHost`), https-only
egress with TLS certificate validation (`Ecluse.Core.Security.Egress`), and the literal
internal-range block on the `dist.tarball` host gate (`isBlockedTarget`), constrain one
thing: an untrusted package download whose target an attacker can influence (the public
packument and every public `dist.tarball`). The host allowlist and literal block are
scoped to the untrusted egress and are absent from every trusted, operator-declared
destination (https-only applies to every registry endpoint regardless). Firing them on an
operator-configured destination, telemetry export, the mirror-queue publish, or a private
registry on an internal address, would break legitimate function for no security gain,
since none of those is attacker-influenced.

Every outbound connection Écluse makes, and its controls. The two data-plane managers
(`envManager`, `envPrivateManager`) are the same validating-TLS manager; the per-origin
split is in credential handling and the internal-range block's origin-awareness, not the
manager. The last column is the untrusted-egress policy (host allowlist plus literal
internal-range block):

| Outbound connection | Trust | Manager / client | Host allowlist + literal internal-range block |
|---|---|---|---|
| Public-upstream **packument** fetch | Untrusted | `envManager` (untrusted) | **Yes** |
| Public `dist.tarball` **artifact** stream | Untrusted | `envManager` (untrusted) | **Yes** (plus the tarball-host policy) |
| Mirror worker's public **artifact** back-fill fetch | Untrusted | `envManager` (untrusted) | **Yes** |
| Private-upstream **packument** fetch | Trusted | `envPrivateManager` (trusted) | **No** |
| Private **conventional** tarball read (`{base}/{pkg}/-/{file}`) | Trusted origin | `envPrivateManager` (trusted) | **No**, same-host by construction; the allowlist + same-host policy still apply (trivially satisfied) |
| Mirror-target **publish** (npm `PUT`) | Trusted declared destination | `envPrivateManager` (trusted) | **No** |
| **First-party publish** relay (client `npm publish` → publication target) | Trusted declared destination | `envPrivateManager` (trusted) | **No**, the destination is configuration (`ECLUSE_MOUNTS__NPM__PUBLICATION_TARGET`); it carries the client's **forwarded** credential, which is **never redirect-followed** (see below) |
| OTLP **telemetry** export | Trusted declared destination | OpenTelemetry SDK's own client | **No**, the endpoint is declared, not classified (see `Ecluse.Telemetry.Resolve`) |
| **SQS** mirror-queue publish / poll | Trusted declared destination | `amazonka`'s own client | **No** (see `Ecluse.Core.Queue.Sqs`) |
| **IMDS** instance-role credential minting | Required internal | `amazonka`'s own client (separate from the data plane) | **No**, must reach `169.254.169.254`; never routed through the data-plane manager |

Every registry endpoint above is dialled https-only with certificate validation,
regardless of trust: that authenticates the endpoint and closes the resolve-to-internal /
rebinding class.

The host allowlist gates only targets built from upstream-supplied data (the public
packument host and every `dist.tarball`). A configuration destination, the private base
URL, mirror target, OTLP endpoint, or SQS queue, is used as given, not re-validated against
an allowlist it would itself define. The literal internal-range block likewise guards the
untrusted origins alone: the trusted private origin, telemetry export, queue, and IMDS
credential minting all reach an internal address by design.

The private origin's tarball is the one subtlety: the
[conventional stable read](registry-model.md#serving-a-tarball-a-conventional-private-read-an-honoured-public-location)
is served over the trusted manager and is exempt from the literal internal-range block as a
`TrustedOrigin`, so a private registry on an internal https address serves its same-host
tarball. The URL is on the private base host, so the host allowlist and same-host policy
are satisfied by construction. It is part of the trusted private origin, not an untrusted
download.

**Écluse never follows an upstream redirect.** Every outbound npm data-plane request, the
anonymous public reads and every credential-bearing request (the private-upstream read
under `passthrough`, credential-bearing artifact reads, the first-party publish relay, and
the mirror-target publish), is built with redirect-following disabled (`redirectCount = 0`)
at the single request-finalisation point (`Ecluse.Core.Registry.Npm.withToken`). This
forecloses a danger on each plane. Credentialed: http-client's default re-sends the
`Authorization` header to a `3xx` `Location` without stripping it cross-host, so a hostile
upstream could `302` a forwarded or minted credential to an attacker-chosen host;
`redirectCount = 0` removes the hop entirely. Anonymous: the host allowlist is enforced
when the URL is built, not per hop, so following a `302` would let an allowlisted upstream
steer a fetch off-allowlist or to cloud-metadata with nothing re-gating it. A read returns
the `3xx` to the serve path rather than chasing it; the proxy honours the packument's
`dist.tarball` location explicitly instead, gated by the egress policy (redirect-following
for a presigned/redirecting upstream is an explicit per-upstream opt-in). The invariant
covers the npm data plane; `amazonka` (CodeArtifact / SQS) and the OTLP exporter build
their own requests outside `withToken`, a noted follow-up.

## A static publish credential is fail-closed

The [first-party publish path](registry-model.md#publishing-first-party-packages-the-publication-target)
relays a client `npm publish` to the publication target. Its scope allow-list
(`ECLUSE_MOUNTS__NPM__PUBLISH_SCOPES`) constrains which package names may be published; it
is not authentication and does not verify who is publishing. So if a deployment sets
`ECLUSE_MOUNTS__NPM__PUBLICATION_TARGET_TOKEN`, substituting Écluse's own credential for a
publisher who forwards none, the composition root refuses to boot without a verifiable
inbound edge (`PublishStaticCredentialNeedsEdge`). That makes "static publish credential +
open edge", which would let any unauthenticated client publish under the operator's
credential within the allowed scopes, unrepresentable. `ECLUSE_AUTH_TOKEN` is the verifiable
edge Écluse checks today; an external layer (API gateway, mTLS service mesh, `NetworkPolicy`)
is defence-in-depth but cannot substitute for it, since Écluse can only verify its own edge.
Pure passthrough (no static token) carries no such floor: the publisher's forwarded token is
the authority, and the read path is untouched. The threat is catalogued as register
[threat #3](https://ecluse-proxy.com/threat-model.html#threat-3).

### The guard-name ≡ write-name ≡ body-name invariant

The anti-shadowing guard would be bypassable if it validated only the URL-path name while
relaying the publish document byte-for-byte: the npm publish document carries its own
declared identity (a top-level `_id` and `name`, and a `name` per `versions` entry), and a
publication target that resolves the written package from the body (the npm-protocol norm)
would write a name the scope guard never saw. A crafted `PUT /@acme/anything` whose body
declares `@victim/target` would publish outside the allow-list, shadowing a public package.

So the guard holds guard-name ≡ write-name ≡ body-name. After the scope check admits the
URL-path name and the body is read, every present declared body name (`_id`, top-level
`name`, each `versions[].name`) is compared to the URL-path name, and any disagreement is a
`403` before any upstream write. The comparison is by `PackageName` equality using the same
canonicalisation the route applies (ecosystem-aware, npm case-sensitive, never a
byte-for-byte compare), so an encoding variant (`@acme%2ffoo` vs `@acme/foo`) cannot
disagree silently. Only the names are parsed; the base64 `_attachments` are never decoded,
and the body is already bounded by the request-size cap. An absent declared name is not a
bypass-grant, only a present, mismatching one is refused, since a legitimate client always
sends names matching its publish URL. This makes the control sound independent of whether
the downstream target keys the write off the body or the URL.

## Network egress is a shared responsibility

Écluse's outbound guards are the primary, application-layer control; pair them with the
deployment's own egress controls, as for any service that fetches on a client's behalf.

**The cloud-metadata SSRF is handled at the service-behaviour level, not by blocking
metadata at the network.** Écluse follows an internal-resolving location only on the
trusted private origin (invariant 3), never on a public-upstream-derived target, so an SSRF
cannot steer it at `169.254.169.254` or `fd00:ec2::254`. Meanwhile Écluse needs the metadata
endpoint to mint its instance-role credentials (`AWS.newEnv AWS.discover` builds amazonka's
own HTTP client, separate from the data-plane manager, so minting reaches IMDS regardless).
So the platform controls must protect the data targets without cutting the proxy off from
metadata or its private upstream's internal range: harden IMDSv2 rather than block it,
default-deny egress scoped to the upstreams and mirror target, and grant the instance role
only the mirror-write and (under `service`) private-read credentials it uses. The concrete
per-platform runbook is the operator's, in the operator manual:
[Securing network egress](../../USAGE.md#securing-network-egress-required).

## Trust assumptions & credential posture

The guards above constrain Écluse's own requests; this section records the deployment
assumptions the [threat model](https://ecluse-proxy.com/threat-model.html) rests
on, and the consequences of the canonical posture (per-caller passthrough credentials, the
three-registry topology, CodeArtifact over VPC endpoints). The threats and dispositions are
in the [register](https://ecluse-proxy.com/threat-model.html); this is the
assumptions framing.

**Edge access is an operator concern** (register threat #3). `ECLUSE_AUTH_TOKEN` is off by
default; who may reach the proxy is delegated to the deployment's access edge, which must
hold east-west as well as north-south (an ingress-only allow-list that leaves pod-to-pod
traffic open is the usual gap). Passthrough softens this: a caller with no forwarded token
gets no private read or publish, so an edge breach exposes only the public-gated view plus
the untrusted-egress and DoS surface, never private packages. (A static publication token
requires `ECLUSE_AUTH_TOKEN`, enforced at boot:
[a static publish credential is fail-closed](#a-static-publish-credential-is-fail-closed).)
The planned trusted-edge-identity mode must require a verifiable binding to the edge (mutual
TLS, or a shared secret / HMAC on the assertion), an [unrepresentable unsafe
combination](access-model.md#safe-defaults-and-unrepresentable-unsafe-combinations), not a
runtime hope. Not yet shipped.

**Passthrough relocates credential risk to the proxy runtime** (register threat #1).
Forwarding each caller's own credential ([access model](access-model.md)) leaves Écluse
holding no standing read/publish credential but transiently holding every in-transit
caller's in memory, so Écluse's own runtime and supply-chain integrity are a first-class
control (the attested, reproducible image, [release supply chain](release-supply-chain.md)),
and the token-stripping boundary and the
[no-redirect-with-credential invariant](#egress-scope-what-the-outbound-controls-guard-and-what-they-do-not)
(register threat #4) are load-bearing because real caller credentials cross them.

**The mirror-target write token is the one standing credential Écluse holds** and its
sharpest privilege, since it writes the trusted store: scope it write-only, prefer
container-role minting over a static secret, minimise its TTL. The mirror queue is part of
the same trust boundary: a job is unauthenticated and directs the worker to
fetch-and-publish, so anyone who can enqueue can make the worker write the trusted store;
scope its IAM too (only the serve role enqueues, only the worker consumes) (register threat
#7). The worker narrows what a forged or stale job can do: the artifact URL is re-formed
into its https-only `RegistryUrl` witness at the wire decode (an unformable URL fails the
decode), the fetch host is re-checked against the mount's tarball-host gate at ingest, the
version is re-decided through the shared admission gate, and the fetched bytes must match
the digest of the artifact that gate re-admits (the set it floor-checked against current
metadata; a job's own digests are never what the bytes are verified against) before any
publish.

**Registry separation is defence-in-depth and auditability, not the perimeter** (register
threat #10). The three-registry topology
([registry-level composition](registry-model.md#registry-level-composition-the-recommended-topology))
keeps first-party and public-derived inventory physically separable, with per-provenance
rule-sets and scanning and clean post-disclosure scoping; collapsing toward one registry
degrades auditability and mitigation depth but does not move the trust perimeter (the
public→trusted admission gate is identical at one registry or three). Storage-layer scanning
is out of scope for Écluse: it is ecosystem- and backend-specific, the operator's to
configure.

## Configurable threat tolerance (secure defaults, configurable overrides)

Écluse is secure by default, with overrides under the operator's explicit control. The
egress guards follow that principle, made concrete for the tarball path:

- **`dist.tarball` host, disallow-by-default (public leg).** The public serve leg fetches
  each tarball from its authoritative `dist.tarball`, but gates where: by default only the
  same allowlisted upstream that served the packument, refusing a `dist.tarball` on a
  different host even if otherwise allowlisted (`Ecluse.Core.Security.tarballHostAllowed`
  with `SameHostAsPackument`, applied in `Ecluse.Core.Server.Pipeline`). A cross-host
  `dist.tarball` is refused with a `403` before any fetch. (The private leg never consults
  `dist.tarball`; its same-host conventional read satisfies the gate by construction.) An
  operator whose registry serves tarballs from a separate CDN opts in with
  `ECLUSE_MOUNTS__NPM__RESPECT_UPSTREAM_TARBALL_HOST`, accepting the wider fetch surface.
  The override never escapes the allowlist or https-only: an allowlisted cross-host tarball
  is still dialled https-only with certificate validation, and a literal internal-address
  host is still refused (invariant 3). See
  [Configuration → Outbound egress safety](configuration.md#outbound-egress-safety).

The internal-range block (invariant 3) runs the opposite direction: no knob narrows it (a
literal internal address is always refused on an untrusted origin, the trusted private
origin aside), only `ECLUSE_ADDITIONAL_BLOCKED_RANGES` widens it.
