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
   (incl. the `169.254.169.254` cloud-metadata endpoint), loopback, and RFC1918 —
   unless the configured upstream is deliberately internal (an explicit per-host
   opt-in).
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
