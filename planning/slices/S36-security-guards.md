---
id: S36
title: Outbound SSRF + input-validation + response-bound guards
milestone: M0, Shell, handles & foundations
status: merged
depends-on: []
test-tier: [unit, integration]
arch-refs:
  - docs/architecture/web-layer.md#raw-wai-not-a-web-framework
  - docs/architecture/registry-model.md#registry-abstraction
  - docs/architecture/hosting.md#the-load-bearing-requirement-url-rewriting
issue: 11
pr: null
---

# S36, Outbound SSRF + input-validation + response-bound guards

> Milestone **M0** · depends on:, (pure primitives buildable now; config wiring with S03) · tier: unit, integration · **security gate ([issue #11](https://github.com/AlexaDeWit/Ecluse/issues/11))**

**Goal.** Écluse builds outbound HTTP requests from **client-supplied package
identifiers** and **upstream-supplied artifact locations**, so it needs explicit,
testable SSRF / input-validation / resource-bound defences, not implementer
discretion. This slice delivers those as **pure guard primitives** (plus the
`Limits` config + bounded reader/decoder), to be **wired into** the data plane
(S08) and serve path (S14/S15) as they land. It is the gate before the request
pipeline advances ([issue #11](https://github.com/AlexaDeWit/Ecluse/issues/11)).

**Acceptance criteria.**
- [ ] **Outbound host allowlist.** A pure `isAllowedUpstreamHost`; the data plane
  (S08) fetches only from the configured upstream hosts. Artifact bytes are fetched
  only from the **upstream-declared `dist.tarball`** (after the allowlist check),  never a client-supplied URL.  _registry-model.md, issue #11_
- [ ] **Internal-range block.** A pure `isBlockedTarget` rejecting link-local
  (`169.254.0.0/16`, incl. the `169.254.169.254` cloud-metadata endpoint),
  loopback, and RFC1918, unless the configured upstream is deliberately internal
  (explicit per-host config opt-in). Applied to the resolved fetch target.  _issue #11_
- [ ] **Bounded responses, fail-closed.** Config-driven limits on every upstream
  read/parse: **max body size** (a bounded reader that aborts past N bytes), **max
  version count**, and **max JSON nesting depth**; exceeding any bound fails closed.
  Sane defaults, overridable (S03).  _issue #11 (algorithmic-complexity DoS)_
- [ ] **Identifier safety at the boundary.** Identifiers are parsed-and-canonicalised
  before any URL is built; upstream URLs are constructed from the **canonical
  identifier + upstream-declared location**, never raw client path segments. The
  router's `isSafeComponent` (S10, merged) already rejects traversal / encoded-slash
  / control-char path components, this slice restates that as a stated *security*
  requirement and owns the URL-construction side.  _web-layer.md, hosting.md, issue #11_
- [ ] **Hostile-fixture corpus.** A reusable suite of hostile inputs, `../`
  traversal, `%2f` / `@scope%2f..%2f`, absolute / alternate-host URLs, CRLF,
  `169.254.169.254`, RFC1918 hosts, and oversized / deeply-nested / million-version
  payloads, asserting each guard rejects them (plus positive cases that pass).
  Unit now; **exercised through the real request path** once S08/S14/S15 wire the
  guards (integration).  _issue #11_

**File scope.**
- `src/Ecluse/Security.hs`, the pure guards (`isAllowedUpstreamHost`,
  `isBlockedTarget`, identifier/URL-construction helpers) and the `Limits` config +
  bounded-reader / bounded-decode helpers.
- `ecluse.cabal`, register the module. Prefer a small hand-rolled CIDR/host check
  over a heavy networking dep (Simple Haskell); justify/escalate any dep added.
- `test/unit/Ecluse/SecuritySpec.hs` + `test/unit/fixtures/hostile/*`, the corpus + assertions.

**Wiring (downstream, required, not this slice's code).**
- **S08** (data plane): every outbound fetch passes `isAllowedUpstreamHost` +
  `isBlockedTarget` and reads through the bounded reader; artifact fetch uses only
  the upstream `dist.tarball`.
- **S03** (config): the allowlist (derived from the configured upstream URLs), the
  internal-upstream opt-in, and the `Limits` are configuration.
- **S14/S15** (serve): bounded decode on the parse path; the hostile corpus runs
  through the real request path (integration).

**Test tier.** Unit (the pure guards + bounds, gating) + integration (through-the-path, once wired).

**Notes / risks.** The pure primitives are buildable and independently testable
**now**; the **wiring** is the hard requirement on S08/S03/S14/S15 (reflected in
their file scopes). The bounded reader lives at the `http-client` boundary (S08); this
slice provides the limit logic + config it consumes. Escalate if address parsing
genuinely needs a dependency rather than a small CIDR check.

**Deferred (defence-in-depth, fail-safe, out of scope here).** The internal-range
block does not decode octal-form octets; they are still kept out by the host
allowlist under the composed gate (`isAllowedUpstreamHost` ∧ ¬`isBlockedTarget`),
which a unit test pins. Revisit only if the block guard is ever used standalone or
an internal IPv6 upstream is allowlisted. Post-resolution IP filtering, a DNS name
that *resolves* to an internal address, belongs to the S08 fetch layer (this pure
layer cannot resolve names).

**As-built notes (PR #31, hardened in PR #38).**
- **Opaque `LoweredHostSet` newtype.** The host allowlist / internal-opt-in set is
  no longer a bare `Set Text`, it is an opaque `newtype LoweredHostSet` built only
  by `lowerCaseHosts :: Set Text -> LoweredHostSet`. A value therefore carries the
  proof that every host in it is already lower-cased, so `isAllowedUpstreamHost` /
  `isBlockedTarget` fold only the *incoming* host and the case-insensitive match
  cannot be bypassed by an un-normalised configuration set. (Introduced in the PR #38
  hardening pass; it tightens the type so case-folding is structural, not a caller
  convention.)
- **IPv4-mapped IPv6 *is* decoded** (no longer deferred). `isBlockedTarget` parses
  the IPv4-mapped form `::ffff:a.b.c.d` (both the hex `::ffff:a9fe:a9fe` and the
  canonical dotted `::ffff:169.254.169.254` spellings), recovers the embedded IPv4,
  and re-runs the internal-range test on it, so an attacker cannot smuggle an
  internal IPv4 (e.g. the metadata address) past the per-IPv4-range checks inside an
  IPv6 literal. This closes the gap the original "Deferred" note left open (done in
  PR #38). Octal-form octets remain undecoded (still covered only by the allowlist),
  and ULA (`fc00::/7`) / NAT64 (`64:ff9b::/96`) stay out of scope.
- **Unspecified / this-host and CGNAT ranges now blocked.** `isBlockedTarget` also
  rejects the unspecified / this-host range (`0.0.0.0/8` and IPv6 `::`, `0.0.0.0`
  is a loopback-equivalent on Linux) and CGNAT shared space (`100.64.0.0/10`,
  RFC 6598), closing the gap raised in
  [issue #83](https://github.com/AlexaDeWit/Ecluse/issues/83). The remaining
  hardening, re-checking **resolved** IPs and the disallow-by-default
  `dist.tarball`-host policy, is its own follow-on slice
  ([S40](S40-egress-ssrf-hardening.md)), since both need the live fetch path.
- **`upstreamUrlFor` + `UrlError` own the URL-construction side.** The sanctioned
  builder `upstreamUrlFor :: Text -> PackageName -> Either UrlError Text` re-checks
  every structural name component with the router's `isSafeComponent` as defence in
  depth (the `mkScope`/`mkPackageName` smart constructors do no validation), and
  reports `UnsafeComponent`/`EmptyBaseUrl`. `UrlError` is the shared URL-formation
  vocabulary S08's request builders adapt into their `PublishError`.
