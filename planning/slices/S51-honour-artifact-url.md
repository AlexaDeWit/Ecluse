---
id: S51
title: Honour the upstream artifact location on the serve path
milestone: M3 — Request pipeline
status: merged
depends-on: [S15, S33, S40]
test-tier: [unit, integration]
arch-refs:
  - docs/architecture/registry-model.md#upstream-roles
  - docs/architecture/configuration.md#outbound-egress-safety
  - docs/architecture/security.md#outbound-egress
  - docs/architecture.md#request-lifecycle
---

## Goal

Serve each tarball from the artifact's **authoritative upstream location** — the
`Artifact.artUrl` the projection already preserves from the upstream `dist.tarball` —
gated by the existing egress controls, instead of reconstructing
`{base}/{pkg}/-/{file}` by npm convention. This conforms the serve path to the
documented model (registry-model.md upstream roles; configuration.md
`PROXY_RESPECT_UPSTREAM_TARBALL_HOST`) and makes the S40 tarball-host policy —
currently plumbed but never consulted — load-bearing.

## Background — what is already right, and the one gap

- The domain model is **lossless**: `Artifact { artFilename, artUrl, artHashes,
  artKind, artSize, … }`, and `projectArtifact` sets `artUrl = distTarball dist`.
  Name (`PackageInfo`), version (map key), filename (`artFilename`), authoritative
  URL (`artUrl`), and integrity digests all survive projection. **The parser /
  projection are correct — do not change them.**
- The dispatch is **already correct**: `serveTarballWithDeps` tries the private
  origin first and falls back to the public origin on a miss (registry-model.md).
  `gatePublicVersion` already fetches and parses the public packument and produces the
  gated version's `PackageDetails`, so the authoritative `Artifact` (with `artUrl`) is
  **in hand at the gate**.
- **The gap:** `streamPublicArtifact` / `streamPrivateArtifact` discard that and
  reconstruct `{base}/{pkg}/-/{file}` via `artifactRequestByFile`. So
  `tarballHostAllowed` / `pdTarballHostPolicy` are never consulted,
  `PROXY_RESPECT_UPSTREAM_TARBALL_HOST` is inert, and cross-host / off-convention
  upstreams (PyPI, some private npm registries) cannot be served. For same-host
  npm.js the reconstructed URL equals `artUrl`, so this is a **conditional** defect,
  not a regression on the shipped path.

## Approach

1. Thread the gated version's chosen `Artifact` from `gateVersion` /
   `gatePublicVersion` into the stream step. Select the artifact by
   `artFilename == requested file` (npm has exactly one; a PyPI-style
   many-per-version selects by filename). A requested file matching no artifact is a
   forwarded `404` (version / file absent).
2. Fetch `artUrl` **directly** — a new adapter helper (e.g. `artifactRequestByUrl`)
   living in the npm adapter, per the adapter-owns-its-URL-model principle — rather
   than reconstructing. The client-facing rewrite to the proxy stays unchanged
   (the proxy must still mediate / cache / mirror).
3. Apply the egress gate at the egress point, by trust context:
   - **Public origin:** guarded `envManager` (resolved-IP recheck, already live)
     **plus** `tarballHostAllowed` with `PROXY_RESPECT_UPSTREAM_TARBALL_HOST`
     semantics — default `SameHostAsPackument` refuses a tarball host ≠ the
     packument's host; `AnyAllowlistedHost` (knob `true`) admits an allowlisted host.
   - **Private origin:** trusted `envPrivateManager`; honour the private packument's
     `artUrl` (the private upstream is trusted; the host policy applies as configured).
4. Integrity unchanged — byte-for-byte stream; the client verifies `dist.integrity`.

## Acceptance criteria

- Same-host (npm.js) tarballs still served, no regression on the shipped path.
- **Cross-host artifact exercised through the real request path** (the coverage S40
  lacked): under the `SameHostAsPackument` default a cross-host `dist.tarball` is
  **refused**; with `AnyAllowlistedHost` and the host allow-listed it is **served**; a
  cross-host target that resolves to an internal IP is **blocked by the resolved-IP
  recheck**. Integration; npm-shaped, no PyPI adapter required.
- The artifact is selected by `artFilename`; a requested file absent from the
  version's artifacts is a forwarded `404`.
- `tarballHostAllowed` is exercised on the **real serve path**, not only its unit test.
- The private-origin honour-URL path is covered.
- Docs reconciled in the same PR: `security.md` describes the **now-enforced** control
  (drop the "planned" / hedged framing); `configuration.md`'s
  `PROXY_RESPECT_UPSTREAM_TARBALL_HOST` matches the implemented behaviour.
- Local gate green incl. `codecov/patch` ≥ 95% — mind the HPC partials lesson (no
  always-true `| otherwise` guards; verify partials on the diff, not just "every line
  hit").

## Scope

- `src/Ecluse/Server/Pipeline.hs` — thread the `Artifact` from gate → stream, both legs.
- `src/Ecluse/Registry/Npm.hs` — `artifactRequestByUrl` (fetch by authoritative URL).
- `src/Ecluse/Security.hs` — connect the existing `tarballHostAllowed` at the egress point.
- Tests (unit + integration); docs (`security.md`, `configuration.md`).

## Out of scope / do not touch

- The parser / projection — `Artifact.artUrl` is already correct.
- The private-first / public-fallback dispatch — already correct.
- The PyPI adapter — this slice delivers npm + the general mechanism; a future PyPI
  adapter rides the same boundary.

## Notes

- This completes S40's egress intent: the resolved-IP recheck (S40, live) and the
  tarball-host policy (S40, plumbed) become a matched pair gating an **honoured**
  upstream URL.
- Decision-surface-vs-served-surface: the fetch location comes from the typed
  `Artifact` (the decision model), confirming that contract rather than bypassing it.
