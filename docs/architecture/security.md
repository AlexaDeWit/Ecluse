# Security: Outbound-Request & Input-Validation Invariants

> Part of the [Écluse architecture overview](../architecture.md).

Écluse builds outbound HTTP requests (private upstream, public upstream, mirror
target) from **client-supplied package identifiers** and **upstream-supplied
artifact locations**. For a supply-chain security tool the defences against
abusing that are **stated, testable invariants** — not implementer discretion.
They are implemented as the guard primitives of
[`S36`](../../planning/slices/S36-security-guards.md) and enforced at the points
noted below.

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
   [Web Layer](web-layer.md#raw-wai-not-a-web-framework).
2. **Outbound fetches are restricted to the configured upstream hosts** (an
   allowlist). Artifact bytes are fetched only from the upstream-declared
   `dist.tarball`, after the allowlist check — never from a client-supplied URL.
   See [Registry Model](registry-model.md#registry-abstraction) and
   [URL rewriting](hosting.md#the-load-bearing-requirement-url-rewriting).
3. **Internal address ranges are blocked** for outbound requests — link-local
   (incl. the `169.254.169.254` cloud-metadata endpoint), loopback, the
   unspecified / this-host range (`0.0.0.0/8` and IPv6 `::`, since `0.0.0.0` is a
   loopback-equivalent on Linux), RFC1918, and CGNAT shared space (`100.64.0.0/10`)
   — unless the configured upstream is deliberately internal (an explicit per-host
   opt-in). This is **defence-in-depth behind invariant 2**: the host allowlist is
   the load-bearing control, and the internal-range block is the second gate for an
   allowlisted name that resolves to an internal literal (see
   [Why `dist.tarball` is honoured](#why-disttarball-is-honoured-and-what-bounds-it)).
4. **Parsed upstream responses are bounded** — maximum body size, version count,
   and JSON nesting depth — and **fail closed** past any bound: an oversized or
   pathological document is refused, never partially served.

## Posture

Every guard is **deny-by-default** and **fail-closed**, consistent with the rules
engine. The invariants are verified by a **hostile-input corpus** (`S36`) —
traversal, encoded slashes, alternate-host and absolute URLs, CRLF, metadata and
RFC1918 targets, oversized and deeply-nested payloads — asserted against the pure
guards and, as the fetch ([`S08`](../../planning/slices/S08-npm-data-plane.md)) and
serve ([`S14`](../../planning/slices/S14-packument-path.md)/[`S15`](../../planning/slices/S15-tarball-path.md))
paths land, exercised through the real request path.

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
backstop, and full literal-form completeness in that block only earns its keep once
the S08 fetch layer re-checks **resolved** IPs (a DNS name that resolves to an
internal address — which the pure layer cannot see).

## Network egress is a shared responsibility

Écluse's outbound guards are **necessary but not sufficient**. They are an
application-layer backstop; they do not replace the deployment's own egress
controls, and a defence-in-depth posture assumes both. Operators **must** constrain
where the proxy's network namespace can reach, so that a guard bug or an
unforeseen fetch path cannot become an SSRF into the cloud control plane. Recommended,
in rough order of leverage:

- **Block the instance-metadata endpoint at the platform.** Require IMDSv2 and set
  the hop limit to 1 (AWS `httpPutResponseHopLimit: 1`), or deny `169.254.169.254`
  egress outright. This single step removes the highest-value SSRF target.
- **Restrict egress with a default-deny network policy.**
  - **AWS** — security-group egress rules / network ACLs allowing only the
    upstream registry CIDRs and the mirror target; deny RFC1918 and link-local.
  - **GCP** — VPC firewall egress rules and, where applicable, VPC Service Controls.
  - **Kubernetes** — a default-deny `NetworkPolicy` with an explicit egress
    allowlist (and a CNI that enforces it).
  - **Service mesh (Istio/Linkerd)** — set the sidecar outbound policy to
    `REGISTRY_ONLY`, declare each upstream as an explicit `ServiceEntry`, and
    constrain it with a `Sidecar` egress listener and egress `AuthorizationPolicy`.
- **Run the proxy with no ambient cloud credentials it does not need.** Écluse holds
  a mirror-**write** credential, and — under the `service` / `delegated-cache`
  [credential strategies](access-model.md) — a private-upstream **read** credential;
  scope the instance role to exactly those it is configured to use and no more (see
  [Configuration](configuration.md#outbound-registry-credentials)).

These belong in the deployment runbook ([`S32`](../../planning/slices/S32-launch-docs.md));
this section is the security rationale they implement.

## Configurable threat tolerance (secure defaults, configurable overrides)

Écluse's posture is **secure by default, with the override under the operator's
explicit control — the consumer decides their own threat tolerance.** The egress
guards follow that principle, and one planned control makes it concrete for the
tarball path:

- **`dist.tarball` host, disallow-by-default (planned — design only).** By default
  the proxy will fetch a tarball only from the **same allowlisted upstream that
  served the packument**, refusing a `dist.tarball` that points at a *different*
  host even if it is otherwise on the allowlist — the safest reading of invariant 2.
  An operator whose registry legitimately serves tarballs from a separate CDN (the
  PyPI-files-host shape above) **opts in** to honouring the upstream-declared host
  (constrained to the allowlist) via configuration, accepting the documented wider
  fetch surface in exchange. Tracked in
  [`S40`](../../planning/slices/S40-egress-ssrf-hardening.md); the configuration
  surface and its security note are sketched in
  [Configuration → Outbound egress safety](configuration.md#outbound-egress-safety-planned).

The internal-range opt-in (invariant 3) is the same shape: internal addresses are
blocked unless a specific private upstream is deliberately opted in.
