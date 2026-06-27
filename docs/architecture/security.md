# Security: Outbound-Request & Input-Validation Invariants

> Part of the [Écluse architecture overview](../architecture.md).

Écluse builds outbound HTTP requests (private upstream, public upstream, mirror
target) from **client-supplied package identifiers** and **upstream-supplied
artifact locations**. For a supply-chain security tool the defences against
abusing that are **stated, testable invariants** — not implementer discretion.
They are implemented as the guard primitives of
[`S36`](../../planning/slices/S36-security-guards.md) and enforced at the points
noted below.

> A companion **STRIDE threat model** (OWASP Threat Dragon) enumerates these and the
> broader system threats as a living register:
> [`threat-modelling/ecluse.json`](../../threat-modelling/ecluse.json). This document is
> the *why* behind the outbound/input guards; the deployment assumptions the model rests on
> are in [Trust assumptions & credential posture](#trust-assumptions--credential-posture).

## Threat model

- **SSRF / unintended fetch targets.** A crafted name or path — `../` traversal,
  percent-encoded slashes, an absolute URL, `@scope%2f..%2f`, CRLF — could steer
  the proxy to an attacker-chosen host or an internal-only address reachable from
  the proxy's network position (cloud instance-metadata endpoints, the private
  upstream's network).
- **Client-controlled artifact source.** On the tarball path (notably `npm ci`,
  which hits tarball URLs directly with no preceding packument request), the
  artifact location must come from the **upstream-declared `dist.tarball`**, not a
  client-controlled path — otherwise the client chooses what bytes Écluse fetches
  and serves.
- **Algorithmic-complexity DoS via upstream payloads.** A hostile or compromised
  upstream (or a pathological public package) could return a huge packument
  (millions of versions, deeply nested JSON); parsing and per-version rule
  evaluation over it could exhaust CPU/memory.

## Invariants

1. **Identifiers are parsed and canonicalised at the boundary.** An identifier
   that does not match the ecosystem's grammar is rejected; an upstream URL is
   built from the canonical identifier and the configured base, **never** from raw
   client path segments. The router's `classify` / `isSafeComponent` already
   rejects traversal, encoded-slash, and control-character path components — see
   [Web Layer](web-layer.md#raw-wai-not-a-web-framework). That structural gate is a
   denylist, so it is paired with **encode-on-build**: every accepted name component
   is percent-encoded (`Ecluse.Core.Server.Route.encodeComponent`) when the upstream URL
   is composed, around the structural `@` sigil and `%2F` scope separator the
   builder writes itself, so a legitimate scoped name still yields exactly one
   `%2F`. A reserved byte the denylist admits (a `%`, `?`, `#`, `;`, or space; the
   canonical case being a once-decoded segment carrying a literal `%2e%2e%2f`) is
   therefore re-encoded (`%2e%2e%2f` → `%252e%252e%252f`) rather than reaching the
   upstream raw, where a decode-and-normalise CDN could resolve it to traversal or a
   `?`/`#` could inject an upstream query/fragment. Both points that compose an
   upstream URL from a name, the data-plane request builders (`Ecluse.Core.Registry.Npm`)
   and the defence-in-depth re-check (`Ecluse.Core.Security.upstreamUrlFor`), apply the
   same encoder.
2. **Outbound fetches are restricted to the configured upstream hosts** (an
   allowlist). Artifact bytes are fetched only from the upstream-declared
   `dist.tarball`, after the allowlist check — never from a client-supplied URL.
   See [Registry Model](registry-model.md#registry-abstraction) and
   [URL rewriting](hosting.md#the-load-bearing-requirement-url-rewriting).
3. **Internal address ranges are blocked on the untrusted origins** — link-local
   (incl. the `169.254.169.254` cloud-metadata endpoint), loopback, the
   unspecified / this-host range (`0.0.0.0/8` and IPv6 `::`, since `0.0.0.0` is a
   loopback-equivalent on Linux), RFC1918, CGNAT shared space (`100.64.0.0/10`), and
   IPv6 unique-local `fc00::/7` (incl. the AWS IMDSv6 endpoint `fd00:ec2::254`). The
   block is **origin-aware**: it guards the **untrusted** egress — the public-upstream
   fetch and every **untrusted** artifact (`dist.tarball`) fetch (a public
   `dist.tarball` stream and the mirror worker's back-fill fetch) — and is **re-applied
   to every resolved IP** at connection time (so an allowlisted name that resolves to an
   internal address is refused — the DNS-rebinding backstop). The **trusted private
   origin** (the operator-configured private upstream) is deliberately *exempt* — its
   packument *and* its same-host `dist.tarball` alike: a private registry may
   legitimately live on an internal address, and only an untrusted target can be steered
   by an attacker. This is **defence-in-depth behind
   invariant 2**: the host allowlist is the load-bearing control, and the
   internal-range block is the second gate for an untrusted allowlisted name that
   resolves to an internal literal (see
   [Why `dist.tarball` is honoured](#why-disttarball-is-honoured-and-what-bounds-it)
   and [Network egress is a shared responsibility](#network-egress-is-a-shared-responsibility)).
4. **Parsed upstream responses are bounded** — maximum body size, version count,
   and JSON nesting depth — and **fail closed** past any bound: an oversized or
   pathological document is refused, never partially served. The bounds are
   **wired into the live metadata data plane**: `Ecluse.Core.Registry.Npm.fetchMetadataForm`
   reads the body through `Ecluse.Core.Security.boundedRead` at the `http-client` boundary
   (so a body past the cap aborts before it is buffered whole), `Ecluse.Core.Server.Pipeline.fetchEntry`
   applies `checkNestingDepth` on the decoded document and `checkVersionCount`
   after projection, and **every breach degrades the contribution to nothing** — the
   same fail-closed path a parse failure takes, and **logged at `WARNING`** (which
   ceiling, observed-vs-cap) so a bound breach is distinguishable from an ordinary
   parse failure — so the merge serves the best-effort union of whatever resolved
   within budget and a pathological document never reaches serve. (The body-size cap
   is what bounds an *unbounded* structure — it precedes the decode, so the document
   reaching the depth check is already bounded-by-body-size; the depth check then
   bounds the *traversal cost* of a within-size-but-deeply-nested document.) The
   ceilings are operator-tunable with secure defaults (`PROXY_MAX_RESPONSE_BYTES`
   / `PROXY_MAX_VERSION_COUNT` / `PROXY_MAX_NESTING_DEPTH`; see
   [Configuration → Response bounds](configuration.md#response-bounds)). Artifacts are
   streamed with constant memory and are not subject to the body-size bound; the
   inbound client→proxy request-body cap is the separate `sizeLimitMiddleware`.
5. **Every served version must carry a *strong* integrity digest — by default, in both
   trust contexts (uniform integrity floor, asymmetric loosenability).** Écluse trusts a
   digest only as far as its algorithm is collision-resistant. By default **both** upstream
   contexts require a **SHA-256-or-stronger** digest; the two floors differ only in **how
   far they may move**:

   - The **public (untrusted) floor** is a **hard SHA-256 boundary**
     (`PROXY_MIN_PUBLIC_INTEGRITY`, default **SHA-256**). It may be **raised** to
     `sha384`/`sha512`/`blake2b` as cryptanalysis ages an algorithm, but **never lowered
     below SHA-256** — a sub-floor or unknown value is [rejected at config
     load](configuration.md#public-integrity-floor), never clamped. **There is no
     escape-hatch:** Écluse will not accept a sub-SHA-256 digest from an untrusted public
     upstream under any configuration. A public version with **no** digest
     (`MissingIntegrity`) or only a digest **below the floor** — e.g. a legacy SHA-1
     `dist.shasum` with no SRI (`BelowIntegrityFloor`) — is refused: the artifact gate
     answers `403` (the tarball is never fetched), and the packument path **filters it out
     of the served listing** (so a client never sees a version it could not safely verify).
     SHA-1 and MD5 have practical collisions, so a match on one cannot prove the bytes were
     not substituted; admitting on such a digest would let a colliding artifact pass the
     tamper gate.
   - The **trusted (private) floor** carries the **same SHA-256 default**
     (`PROXY_MIN_TRUSTED_INTEGRITY`, default **SHA-256**), so by default a SHA-1-only or
     hashless **private** version is **dropped** exactly as a public one is — the old
     "trusted private path is exempt" model is gone as a default. But this floor is
     **operator-loosenable below SHA-256** (down to `sha1`/`md5`) for a **legacy private
     mirror**, where **trust in the operator's own vetted source substitutes for
     cryptographic strength**. Loosening the trusted floor is the **only** way Écluse will
     serve a sub-SHA-256 digest, and **only on the trusted private origin** — never on
     untrusted public bytes. On the serve path the trusted floor both filters the private
     listing and gates the private artifact serve (a below-floor private artifact is a
     private miss that falls through to the public origin).

   The asymmetry is the point: **trust may substitute for cryptographic strength on the
   operator's own vetted (private) source, but never on untrusted public bytes.** This is
   enforced in the types — `MinIntegrity` (public) cannot be constructed below SHA-256,
   while `MinTrustedIntegrity` (trusted) can — so no config or constructor path can lower
   the public floor.

   **The shared-weak-digest divergence cross-check is a consequence of loosening the
   trusted floor, not a default posture.** A
   [cross-upstream divergence](registry-model.md#packument-merge-across-upstreams) is
   detected when two copies of a version **contradict on a shared algorithm**. By default
   (uniform SHA-256) every admitted version anchors that comparison on a **strong**
   (≥ SHA-256) digest, so a weak shared digest never carries it. Only when an operator
   **explicitly loosens the trusted floor below SHA-256** can a private version be admitted
   on, say, a lone `sha1`; a private `{sha1}` vs public `{sha1, sha256}` pair then
   cross-checks on the shared `sha1` (the public copy still independently meets its own
   hard SHA-256 floor on its `sha256`). That weak cross-check is therefore the
   **opted-into** behaviour of a loosened trusted floor, never something the default model
   relies on.

   The shared notion of algorithm strength and the floor predicate live in one module,
   `Ecluse.Core.Package.Integrity`, reused by the worker's tamper gate
   (`Ecluse.Core.Worker.verifyIntegrity`) so the public floor, the trusted floor, and the
   publish-time verification rank algorithms identically.

## Posture

Every guard is **deny-by-default** and **fail-closed**, consistent with the rules
engine. The invariants are verified by a **hostile-input corpus** (`S36`) —
traversal, encoded slashes, alternate-host and absolute URLs, CRLF, metadata and
RFC1918 targets, oversized and deeply-nested payloads — asserted against the pure
guards and exercised **through the real request path** now that the fetch
([`S08`](../../planning/slices/S08-npm-data-plane.md)) and serve
([`S14`](../../planning/slices/S14-packument-path.md)/[`S15`](../../planning/slices/S15-tarball-path.md))
paths have landed: an oversized body, a version flood, and a deeply-nested document
each drive a fail-closed refusal (a degraded contribution, never a partial serve) in
`Ecluse.Server.PipelineSpec`, and the bounded body read is unit-tested at the
`http-client` boundary in `Ecluse.Registry.NpmSpec`.

## Why `dist.tarball` is honoured, and what bounds it

A natural question is why Écluse fetches from an **upstream-declared** artifact
location at all — why not reconstruct every tarball URL from the configured host
and refuse anything else, making a hostile location impossible by construction?

That works for the public-npm happy path (where a tarball lives at
`{registry}/{pkg}/-/{file}.tgz`, a pure function of name + version), but it breaks
the registries Écluse exists to front. **The artifact location is authoritative,
server-chosen data, not a derivable fact:**

- **Tarballs often live on a different host or path than metadata.** Public PyPI
  serves files from a [separate artifact host entirely](hosting.md#the-load-bearing-requirement-url-rewriting);
  npm third-party registries (CodeArtifact, Artifactory, GitHub Packages) commonly
  return `dist.tarball` on a distinct CDN, frequently with server-generated path
  segments or short-lived **signed query strings** that cannot be reconstructed.
- **The private upstream serves its tarball directly** ([Registry Model](registry-model.md#registry-abstraction)),
  exactly the path where the location is most opaque.

So "reconstruct or fail" would reduce Écluse to registries whose tarball layout
equals their metadata layout — dropping the private-registry support that is a
core goal. The minimum necessary trust is therefore "honour the upstream-declared
location," and the residual risk is bounded by two **differently-shaped** controls,
not by URL reconstruction:

- **Wrong bytes** are caught by the **client-side integrity** check — the proxy
  streams artifacts through without rehashing, relying on the packument's
  `dist.integrity`, preserved byte-for-byte (see [Web Layer](web-layer.md));
  the mirror worker additionally verifies bytes against `dist.integrity` before
  publishing. A poisoned URL cannot deliver bytes that install.
- **An unintended fetch target (SSRF)** is the only remaining axis, and the
  right-shaped control for "constrain *where* we fetch" is the **host allowlist**
  (invariant 2), with the internal-range block (invariant 3) as defence-in-depth.

The load-bearing guard is thus `isAllowedUpstreamHost`; the IP-range block is its
backstop. That block has two parts with a deliberate boundary between them. Recognising
whether a host **is** an IP literal stays a **hand-rolled, intentionally lenient**
parser; testing a recognised address for **membership** of the blocked CIDR ranges
is delegated to the `iproute` library (one shared predicate, `isBlockedIP`, for
both the literal block and the resolved-address recheck, so they gate against
identical ranges). The split is load-bearing: a strict IP library rejects ambiguous
bypass spellings — notably leading-zero octets (`0127.0.0.1`, `010.0.0.1`) — as
non-literals, which would let them **skip** the block and reach the fetch layer as
names, silently *narrowing* the gate. The lenient recogniser instead parses them as
the address they coerce to on a typical resolver and **blocks** them; conversely a
malformed group that overflows 16 bits (`fe80::1ffff`) is treated as a name the
allowlist constrains. Delegating literal *parsing* to a library would change both
behaviours, so only membership is delegated.

This literal-form coverage earns its keep because the fetch layer also re-checks
**resolved** IPs: the shared HTTP manager's connection hook resolves every outbound
host and re-applies the same internal-range block to each resolved address before
the socket is used (`Ecluse.Core.Security.Egress`), so a DNS name that resolves to an
internal address — which the pure layer cannot see — is refused at connect time.
This narrows the resolve-then-connect (DNS-rebinding) window the pure layer leaves
open.

## Egress scope: what the outbound controls guard, and what they do not

The outbound egress controls — the host allowlist (`isAllowedUpstreamHost`), the
internal-range block (`isBlockedTarget`), and the connection-time resolved-IP recheck
(`Ecluse.Core.Security.Egress`) — exist to constrain **one** thing: an **untrusted package
download** whose target an attacker can influence (the public packument and every
public `dist.tarball`). They are therefore scoped to exactly the **untrusted** egress
and are deliberately **absent** from every **trusted, operator-declared destination**.
Conflating the two is over-restriction: a control aimed at attacker-steered fetches
that fired on a destination the operator themselves configured would break legitimate
function — telemetry export, the mirror-queue publish, or a private registry that lives
on an internal address — for no security gain, since none of those targets is
attacker-influenced.

Every outbound connection Écluse makes, and the controls it carries:

| Outbound connection | Trust | Manager / client | Internal-range block + resolved-IP recheck |
|---|---|---|---|
| Public-upstream **packument** fetch | Untrusted | `envManager` (guarded) | **Yes** |
| Public `dist.tarball` **artifact** stream | Untrusted | `envManager` (guarded) | **Yes** (plus the tarball-host policy) |
| Mirror worker's public **artifact** back-fill fetch | Untrusted | `envManager` (guarded) | **Yes** |
| Private-upstream **packument** fetch | Trusted | `envPrivateManager` (unguarded) | **No** |
| Private `dist.tarball` **artifact** stream | Trusted origin | `envPrivateManager` (unguarded) | **No** — but the allowlist + same-host policy still apply |
| Mirror-target **publish** (npm `PUT`) | Trusted declared destination | `envPrivateManager` (unguarded) | **No** |
| **First-party publish** relay (client `npm publish` → publication target) | Trusted declared destination | `envPrivateManager` (unguarded) | **No** — the destination is configuration (`PUBLICATION_TARGET_URL`); it carries the client's **forwarded** credential, which is **never redirect-followed** (see below) |
| OTLP **telemetry** export | Trusted declared destination | OpenTelemetry SDK's own client | **No** — the endpoint is declared, not classified (see `Ecluse.Telemetry.Resolve`) |
| **SQS** mirror-queue publish / poll | Trusted declared destination | `amazonka`'s own client | **No** (see `Ecluse.Core.Queue.Sqs`) |
| **IMDS** instance-role credential minting | Required internal | `amazonka`'s own client (separate from the data plane) | **No** — must reach `169.254.169.254`; never routed through the data-plane manager |

The host allowlist gates only the targets built from **upstream-supplied** data (the
public packument host and every `dist.tarball`). A destination that is
**configuration** — the private base URL, the mirror target, the OTLP endpoint, the SQS
queue — is the operator's declared intent, used as given rather than re-validated
against an allowlist it would itself define. The internal-range block and its
resolved-IP recheck likewise guard the untrusted origins alone (invariant 3): the
trusted private origin, the telemetry export, the queue, and IMDS credential minting
all reach an internal address by design.

The private origin's tarball is the one subtlety: it is served over the unguarded
trusted manager and is **exempt from the internal-range block** as a `TrustedOrigin`
(so a private registry on an internal address serves its same-host `dist.tarball`),
yet it stays constrained by the host allowlist and the same-host tarball policy — see
[Why `dist.tarball` is honoured](#why-disttarball-is-honoured-and-what-bounds-it). It
is treated as part of the trusted private origin, not as an untrusted download.

**A credential-bearing request never follows a redirect.** Every outbound request that
carries a bearer — the private-upstream read under `passthrough`, the credential-bearing
artifact reads, the first-party publish relay, and the mirror-target publish — is built
with redirect-following **disabled** (`redirectCount = 0`) at the single
credential-attachment point (`Ecluse.Core.Registry.Npm.withToken`). http-client's default
re-sends the `Authorization` header to a `3xx` `Location` (and does not strip it
cross-host), so a hostile or misconfigured upstream could `302` a forwarded/minted
credential to an attacker-chosen host — and on the **unguarded** private manager that
target carries no resolved-IP recheck, so the credential could reach an internal address
with no egress guard at all. Disabling redirects forecloses that exfiltration: a
credential-bearing read returns the `3xx` to the serve path rather than chasing it (the
proxy already honours the **packument's** `dist.tarball` location explicitly, gated by the
egress policy, rather than relying on redirects). **Anonymous** public reads keep the
default redirect budget — no credential is at risk there. The invariant is enforced for
the npm data plane; `amazonka` (CodeArtifact / SQS) and the OTLP exporter build their own
requests outside `withToken`, so extending it there is a noted follow-up.

## The first-party publish surface must be protected (a shared responsibility)

The [first-party publish path](registry-model.md#publishing-first-party-packages-the-publication-target)
relays a client `npm publish` to the publication target. Its scope allow-list
(`PUBLISH_SCOPES`) constrains **which package names** may be published — it is **not** an
authentication control and says nothing about **who** may publish. So a static
`PUBLICATION_TARGET_TOKEN` paired with an **open edge** (no `PROXY_AUTH_TOKEN`) lets **any
unauthenticated client** publish under the operator's credential, within the allowed
scopes. Écluse deliberately does **not** fail closed on this combination — it cannot see
the deployment's environment-level protections (an API gateway, a service mesh with mTLS,
a `NetworkPolicy`), so blocking it would break legitimate closed-network deployments — but
**the publish surface MUST be protected**, by Écluse's own edge auth (`PROXY_AUTH_TOKEN`)
**or** an external layer. Treat this as an operator-architecture responsibility, the same
way [network egress](#network-egress-is-a-shared-responsibility) is (see also
[Access & Credential Model → Publishing](access-model.md#publishing-the-publication-target-passthrough-write)).

## Network egress is a shared responsibility

Écluse's outbound guards are the **primary, application-layer** control; a
defence-in-depth posture pairs them with the deployment's own egress controls — the
standard arrangement for any service that fetches on a client's behalf.

**The cloud-metadata SSRF is handled at the service-behaviour level, not by blocking
metadata at the network.** Écluse only follows an internal-resolving location on the
**trusted private origin** (invariant 3) — its packument *and* its same-host
`dist.tarball` — never on a **public-upstream-derived** target (the public packument or
a public `dist.tarball`), which are exactly the attacker-influenced ones — so an
SSRF cannot steer it at `169.254.169.254` or `fd00:ec2::254`. At the same time Écluse
**needs** the metadata endpoint to mint its instance-role credentials
(`AWS.newEnv AWS.discover`, which builds amazonka's **own** HTTP client, separate from
the guarded data-plane manager — so credential minting reaches IMDS regardless of the
data-plane guard). The platform controls below therefore protect the **data targets**
and add defence-in-depth; they must **not** cut the proxy off from metadata or from
its private upstream's internal range. Recommended, in rough order of leverage:

- **Harden the instance-metadata endpoint — do not block it.** Require IMDSv2 and set
  the hop limit to 1 (AWS `httpPutResponseHopLimit: 1`): this stops a neighbour or a
  forwarded request from reaching metadata through extra hops while keeping the
  proxy's own credential minting working. Denying the instance egress to
  `169.254.169.254` outright would break that minting and is **not** recommended — the
  SSRF risk is already closed at the behaviour level.
- **Restrict egress with a default-deny network policy** scoped to the **data
  targets**.
  - **AWS** — security-group egress rules / network ACLs allowing only the upstream
    registry CIDRs, the mirror target, and the metadata endpoint the instance role
    needs.
  - **GCP** — VPC firewall egress rules and, where applicable, VPC Service Controls.
  - **Kubernetes** — a default-deny `NetworkPolicy` with an explicit egress
    allowlist (and a CNI that enforces it); allow the private upstream's internal
    range.
  - **Service mesh (Istio/Linkerd)** — set the sidecar outbound policy to
    `REGISTRY_ONLY`, declare each upstream as an explicit `ServiceEntry`, and
    constrain it with a `Sidecar` egress listener and egress `AuthorizationPolicy`.
- **Run the proxy with no ambient cloud credentials it does not need.** Écluse holds
  a mirror-**write** credential, and — under `service` (and a service-populated
  `delegated-cache`) — a private-upstream **read** credential; scope the instance role
  to exactly those it is configured to use and no more (see
  [Configuration](configuration.md#outbound-registry-credentials)).

These belong in the deployment runbook ([`S32`](../../planning/slices/S32-launch-docs.md));
this section is the security rationale they implement.

## Trust assumptions & credential posture

The guards above constrain Écluse's *own* requests; this section records the **deployment
assumptions** the [threat model](../../threat-modelling/ecluse.json) rests on and the
security consequences of the **canonical posture** (per-caller passthrough credentials, the
three-registry topology, and CodeArtifact over VPC endpoints).

**Edge access is an operator concern.** Écluse builds no access boundary: app-level auth
(`PROXY_AUTH_TOKEN`) is **off by default**, and *who may reach the proxy* is delegated to
the deployment's access edge (gateway / mesh / network policy) — the same
shared-responsibility split as [network egress](#network-egress-is-a-shared-responsibility).
This rests on one assumption the deployment must hold: **Écluse is reachable only through
that edge, east-west as well as north-south.** An ingress-only allow-list that leaves
pod-to-pod traffic open is the usual gap — a compromised neighbour reaching the pod directly
steps around it. The assumption is **softened** (not carried alone) by the credential model
below: under passthrough a caller with no forwarded token gets no private read and no
publish, so an edge breach exposes only the public-gated view plus the untrusted-egress and
DoS surface — never private packages. (The publish-specific corollary — an open edge plus a
static publication token — is
[The first-party publish surface must be protected](#the-first-party-publish-surface-must-be-protected-a-shared-responsibility).)
The future **trusted-edge-identity** mode (a signed header / mTLS SAN) inverts the posture:
a bare trusted header under an open edge is forgeable into *granted* access, strictly worse
than today's "no token, no access". So Écluse **fails fast** on a `trusted-edge` mount that
lacks a *verifiable* binding to the edge (mutual TLS, or a shared secret / HMAC on the
assertion) — an [unrepresentable unsafe
combination](access-model.md#safe-defaults-and-unrepresentable-unsafe-combinations), not a
runtime hope — landing with [`S43`](../../planning/slices/S43-credential-strategy.md).

**Passthrough relocates credential risk to the proxy runtime.** Forwarding each caller's
own credential ([access model](access-model.md)) buys no privilege escalation or compression
and leaves Écluse holding no standing read/publish credential — but the proxy **transiently
holds every in-transit caller's credential in memory**. The highest-value asset in the model
is therefore *forwarded credentials in proxy memory*: a single proxy compromise (a heap
dump, a log-field leak, or a malicious dependency in Écluse's *own* supply chain) harvests
every caller in transit, not one. Two consequences follow:

- Écluse's **own runtime and supply-chain integrity are a first-class control** — hence the
  attested, reproducible image ([release supply chain](release-supply-chain.md)); a
  garbage-collected runtime cannot promise prompt heap erasure of a forwarded secret.
- The **token-stripping** boundary (the caller credential is dropped on every public fetch)
  and the **no-redirect-with-credential** invariant
  ([a credential-bearing request never follows a redirect](#egress-scope-what-the-outbound-controls-guard-and-what-they-do-not))
  become load-bearing, since real caller credentials cross them.

The one standing credential Écluse *does* hold — the mirror-target **write** token — is its
sharpest privilege, because it writes the trusted store: scope it write-only, prefer
container-role minting over a static secret, and minimise its TTL.

**Registry separation is defence-in-depth and auditability, not the perimeter.** The
three-registry topology — a first-party store, a public-derived mirror store, and a
pull-through read endpoint
([registry-level composition](registry-model.md#registry-level-composition-optional-never-required))
— is **preferred** because it keeps first-party and public-derived inventory physically
separable: distinct storage-level rule-sets and scanning per provenance, and clean
post-disclosure scoping (*which mirrored public packages did we hold?*). Collapsing toward a
single registry **degrades auditability (a Repudiation-class loss) and mitigation depth**,
but does **not** move the trust perimeter — the public→trusted admission gate is identical
at one registry or three. **Storage-layer scanning is itself out of scope** for Écluse: it
is ecosystem- and backend-specific (CodeArtifact, GCP Artifact Registry, and a self-hosted
Verdaccio differ), and is the operator's to configure.

## Configurable threat tolerance (secure defaults, configurable overrides)

Écluse's posture is **secure by default, with the override under the operator's
explicit control — the consumer decides their own threat tolerance.** The egress
guards follow that principle, and it is made concrete for the tarball path:

- **`dist.tarball` host, disallow-by-default.** The serve path fetches each tarball
  from its **authoritative upstream location** (the preserved `dist.tarball`), but
  gates *where* that location may be: by default it is fetched only from the **same
  allowlisted upstream that served the packument**, refusing a `dist.tarball` that
  points at a *different* host even if it is otherwise on the allowlist — the safest
  reading of invariant 2 (`Ecluse.Core.Security.tarballHostAllowed` with
  `SameHostAsPackument`, applied on the serve path in `Ecluse.Core.Server.Pipeline`). A
  cross-host `dist.tarball` is refused with a `403` before any artifact fetch. An
  operator whose registry legitimately serves tarballs from a separate CDN (the
  PyPI-files-host shape above) **opts in** to honouring the upstream-declared host
  (still constrained to the allowlist) by setting
  `PROXY_RESPECT_UPSTREAM_TARBALL_HOST`, accepting the documented wider fetch surface
  in exchange. The override never escapes the allowlist or the resolved-IP block: an
  allowlisted cross-host name that resolves to an internal address is still refused at
  connect time by the resolved-IP recheck (invariant 3). The configuration surface and
  its security note are in
  [Configuration → Outbound egress safety](configuration.md#outbound-egress-safety).

The internal-range opt-in (invariant 3) is the same shape: internal addresses are
blocked unless a specific private upstream is deliberately opted in.
