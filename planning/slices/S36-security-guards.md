---
id: S36
title: Outbound SSRF + input-validation + response-bound guards
milestone: M0 — Shell, seams & foundations
status: in-progress
depends-on: []
test-tier: [unit, integration]
arch-refs:
  - docs/architecture/web-layer.md#raw-wai-not-a-web-framework
  - docs/architecture/registry-model.md#registry-abstraction
  - docs/architecture/hosting.md#the-load-bearing-requirement-url-rewriting
issue: 11
pr: null
---

# S36 — Outbound SSRF + input-validation + response-bound guards

> Milestone **M0** · depends on: — (pure primitives buildable now; config wiring with S03) · tier: unit, integration · **security gate ([issue #11](https://github.com/AlexaDeWit/Ecluse/issues/11))**

**Goal.** Écluse builds outbound HTTP requests from **client-supplied package
identifiers** and **upstream-supplied artifact locations**, so it needs explicit,
testable SSRF / input-validation / resource-bound defences — not implementer
discretion. This slice delivers those as **pure guard primitives** (plus the
`Limits` config + bounded reader/decoder), to be **wired into** the data plane
(S08) and serve path (S14/S15) as they land. It is the gate before the request
pipeline advances ([issue #11](https://github.com/AlexaDeWit/Ecluse/issues/11)).

**Acceptance criteria.**
- [ ] **Outbound host allowlist.** A pure `isAllowedUpstreamHost`; the data plane
  (S08) fetches only from the configured upstream hosts. Artifact bytes are fetched
  only from the **upstream-declared `dist.tarball`** (after the allowlist check) —
  never a client-supplied URL. — _registry-model.md, issue #11_
- [ ] **Internal-range block.** A pure `isBlockedTarget` rejecting link-local
  (`169.254.0.0/16`, incl. the `169.254.169.254` cloud-metadata endpoint),
  loopback, and RFC1918 — unless the configured upstream is deliberately internal
  (explicit per-host config opt-in). Applied to the resolved fetch target. — _issue #11_
- [ ] **Bounded responses, fail-closed.** Config-driven limits on every upstream
  read/parse: **max body size** (a bounded reader that aborts past N bytes), **max
  version count**, and **max JSON nesting depth**; exceeding any bound fails closed.
  Sane defaults, overridable (S03). — _issue #11 (algorithmic-complexity DoS)_
- [ ] **Identifier safety at the boundary.** Identifiers are parsed-and-canonicalised
  before any URL is built; upstream URLs are constructed from the **canonical
  identifier + upstream-declared location**, never raw client path segments. The
  router's `isSafeComponent` (S10, merged) already rejects traversal / encoded-slash
  / control-char path components — this slice restates that as a stated *security*
  requirement and owns the URL-construction side. — _web-layer.md, hosting.md, issue #11_
- [ ] **Hostile-fixture corpus.** A reusable suite of hostile inputs — `../`
  traversal, `%2f` / `@scope%2f..%2f`, absolute / alternate-host URLs, CRLF,
  `169.254.169.254`, RFC1918 hosts, and oversized / deeply-nested / million-version
  payloads — asserting each guard rejects them (plus positive cases that pass).
  Unit now; **exercised through the real request path** once S08/S14/S15 wire the
  guards (integration). — _issue #11_

**File fence.**
- `src/Ecluse/Security.hs` — the pure guards (`isAllowedUpstreamHost`,
  `isBlockedTarget`, identifier/URL-construction helpers) and the `Limits` config +
  bounded-reader / bounded-decode helpers.
- `ecluse.cabal` — register the module. Prefer a small hand-rolled CIDR/host check
  over a heavy networking dep (Simple Haskell); justify/escalate any dep added.
- `test/unit/Ecluse/SecuritySpec.hs` + `test/unit/fixtures/hostile/*` — the corpus + assertions.

**Wiring (downstream — required, not this slice's code).**
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
their fences). The bounded reader lives at the `http-client` boundary (S08); this
slice provides the limit logic + config it consumes. Escalate if address parsing
genuinely needs a dependency rather than a small CIDR check.

**Deferred (defence-in-depth, fail-safe — out of scope here).** The internal-range
block does not decode IPv4-mapped IPv6 (`::ffff:…`) or octal-form octets; both are
still kept out by the host allowlist under the composed gate (`isAllowedUpstreamHost`
∧ ¬`isBlockedTarget`), which a unit test pins. Revisit only if the block guard is
ever used standalone or an internal IPv6 upstream is allowlisted. Post-resolution IP
filtering — a DNS name that *resolves* to an internal address — belongs to the S08
fetch layer (this pure layer cannot resolve names).
