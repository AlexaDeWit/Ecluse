---
id: S40
title: Egress / SSRF hardening, resolved-IP recheck, tarball-host policy, operator egress docs
milestone: M4, AWS cloud backends & worker
status: merged
depends-on: [S08, S15]
test-tier: [unit, integration]
arch-refs:
  - docs/architecture/security.md#network-egress-is-a-shared-responsibility
  - docs/architecture/security.md#why-disttarball-is-honoured-and-what-bounds-it
  - docs/architecture/configuration.md#outbound-egress-safety
issue: 11
pr: null
---

# S40, Egress / SSRF hardening

> Follow-on hardening for the SSRF gate ([S36](S36-security-guards.md), merged).
> The pure guard primitives exist; this slice closes the gaps that only matter once
> the guards run on a **live, resolving** fetch path, and makes the deny-by-default
> tarball-host policy real. Security architecture: [`security.md`](../../docs/architecture/security.md).

**Goal.** Take the SSRF posture from "pure primitives + host allowlist" to a fetch
path that is hardened against the cases the pure layer structurally cannot see,DNS resolving to an internal address, and a `dist.tarball` host that differs from
the packument's source, and document the operator-side egress controls the
application guards assume.

**Acceptance criteria.**

- [ ] **Post-resolution internal-range recheck.** The S08 fetch layer re-applies
      `isBlockedTarget` to the **resolved** IP of every outbound connection (not just
      the host literal), so an allowlisted DNS name that resolves to an internal
      address is refused at connect time. Closes the gap the S36 "Deferred" note names
      (the pure layer cannot resolve names)., _security.md invariant 3_
- [ ] **`dist.tarball` host policy, disallow-by-default.** A tarball is fetched only
      from the **same allowlisted upstream that served the packument** unless the
      operator opts in via `PROXY_RESPECT_UPSTREAM_TARBALL_HOST` (see
      [Configuration → Outbound egress safety](../../docs/architecture/configuration.md#outbound-egress-safety)),
      which relaxes to "any allowlisted host", never escaping the allowlist or the
      internal-range block. Secure default; configurable override., _security.md_
- [ ] **Config surface + validation.** The new setting is parsed at the config
      boundary with the same fail-fast posture as the rest (S03), defaulting to the
      secure value; its security note ships in the same change.
- [ ] **Operator egress runbook.** The platform-layer egress guidance (security
      groups / NACLs, K8s `NetworkPolicy`, Istio `ServiceEntry` + egress policy, IMDSv2
      hop-limit / metadata-endpoint denial) is carried into the deployment runbook
      ([S32](S32-launch-docs.md))., _security.md#network-egress-is-a-shared-responsibility_
- [ ] **Hostile-fixture extension.** The S36 corpus gains resolved-internal-IP and
      cross-host-`dist.tarball` cases, exercised through the real request path
      (integration).

**File scope.**

- `src/Ecluse/Registry/Npm.hs` (or the S08 fetch boundary), the resolved-IP
  recheck and the tarball-host policy decision at the connect point.
- `src/Ecluse/Config.hs`, the `PROXY_RESPECT_UPSTREAM_TARBALL_HOST` setting.
- `src/Ecluse/Security.hs`, only if the policy needs a new pure helper (the
  internal-range block itself already covers `0.0.0.0/8`, `::`, RFC1918, CGNAT,
  link-local, loopback, and the IPv4-mapped forms).
- `docs/architecture/security.md`, `docs/architecture/configuration.md`,
  `planning/slices/S32-launch-docs.md`, the operator guidance.
- `test/unit/Ecluse/SecuritySpec.hs`, `test/integration/...`, the extended corpus.

**Notes / risks.** The resolved-IP recheck must run **after** DNS resolution but
**before** the connection is used, i.e. at the `http-client` connection hook, not
in pure code. Confirm the chosen client exposes that handle abstraction; escalate if it forces a
custom `Manager`. The tarball-host policy is a behaviour change for any deployment
relying on a separate tarball CDN, it is gated behind the opt-in precisely so the
default cannot silently break those, but the launch docs must call it out.

**Out of scope.** Octal/decimal/short-form IPv4 _literal_ parsing in the pure block
(still covered by the allowlist + the resolved-IP recheck, which sees the
canonical address); NAT64 `64:ff9b::/96` (unchanged from S36). IPv6 ULA `fc00::/7`
(incl. AWS IMDSv6 `fd00:ec2::254`) is now **in scope**, added alongside the
leg-aware split per the post-review (issue #162).

## As-built notes

- **Per-origin manager split + resolved-IP recheck, shipped and live.** The data
  plane splits into a guarded `envManager` (resolved-IP recheck applied to every
  outbound connection, for the untrusted public / artifact origin) and a trusted
  `envPrivateManager` (the operator-configured private origin), with per-origin
  integration coverage (`EgressOriginSpec`). This is the enforced, working half of
  the slice, acceptance criteria 1, 3, 4 (and the operator runbook guidance).
- **Behaviour-level metadata protection (#162) + IPv6 ULA, shipped.** The egress
  guard refuses link-local / metadata ranges (incl. IPv6 ULA `fc00::/7` and IMDSv6
  `fd00:ec2::254`) at the service level; the operator docs were corrected so the
  metadata endpoint the proxy itself needs (instance-role credential minting) is not
  network-blocked. IMDSv2 + hop-limit-1 retained.
- **Tarball-host policy, plumbed and unit-tested here; wired load-bearing in
  [S51](S51-honour-artifact-url.md).** `Ecluse.Security.tarballHostAllowed`,
  `TarballHostPolicy`, the `PROXY_RESPECT_UPSTREAM_TARBALL_HOST` config, and the
  `pdTarballHostPolicy` field shipped in S40 with unit coverage; S40's serve path
  still reconstructed the tarball URL from the configured upstream, so the policy was
  inert. S51 made the serve path honour the authoritative `Artifact.artUrl` and
  consult the policy on both legs (gated, with the cross-host case exercised on the
  real request path), closing acceptance criteria 2 and 5. The configuration /
  security docs describe the now-enforced control.
