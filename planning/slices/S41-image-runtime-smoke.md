---
id: S41
title: Image runtime smoke, distroless `docker run` + real proxied fetch (dlopen/NSS verification)
milestone: M8, Release hardening
status: merged
depends-on: [S20]
test-tier: []
arch-refs:
  - docs/architecture/release-supply-chain.md#releases--container-image
pr: null
issue: 590
---

# S41, Image runtime smoke (distroless dynamic-link verification)

> Milestone **M8** · depends on: S20 (a serving, launch-ready proxy) · tier: n/a (release workflow, `docker run`, not a cabal suite)

**Goal.** Close the one gap in the image's runtime guarantee. The Nix image
(`dockerTools.buildLayeredImage` over `.#ecluse-bin`) gives a _static_ guarantee
by construction: every dynamically-linked C library is referenced by an absolute
`/nix/store` `RUNPATH`, Nix derives the runtime closure from those references, and
the image ships exactly that closure (`ldd` on the binary resolves every
`DT_NEEDED` into the store, none "not found"). What that does **not** prove is
runtime `dlopen`-by-soname, chiefly glibc **NSS** (`getaddrinfo`/DNS) and the TLS
path, because a bare-soname `dlopen` leaves no store-path reference for the
closure scan to capture. Today the image is built, pushed, attested and scanned
but **never run** in CI, so that class is unverified in the shipped distroless
image. Add a release-workflow smoke that loads and runs the built image and drives
one real proxied request, turning the static guarantee into belt-and-suspenders.

**Priority: low (deliberate backlog).** cabal2nix captures _declared_ C FFI deps
automatically, a package's `extra-libraries` / `pkgconfig-depends` become nixpkgs
inputs, get `RUNPATH`'d, and land in the closure, so the realistic exposure is
narrow: an _undeclared_ runtime `dlopen`, essentially just NSS/DNS, which works in
practice (the `libnss_*` modules live inside the glibc store path that is wholly
in the closure). This slice is defence-in-depth and future-proofing against a dep
that later `dlopen`s something, not a fix for a known defect.

**Acceptance criteria.**
- [ ] In the release path, `docker load` the built `.#dockerImage` archive and
  `docker run` it; assert the container **starts**, the entrypoint resolves and
  every `DT_NEEDED` library is present (catches a missing loader / startup
  `dlopen` in the distroless image).  _release-supply-chain.md#releases--container-image_
- [ ] Drive **one real proxied request** through the running container (DNS
  resolution + TLS handshake + an upstream fetch), so the glibc **NSS / resolver**
  and TLS `dlopen` paths are exercised in the shipped image, not just in the dev
  shell. Network-tolerant, like the existing smoke tier.
- [ ] Cheap static belt: assert `ldd` (or a `nix path-info`-based closure check)
  on the shipped binary reports **no "not found"**, a fast invariant guarding the
  link-time closure.
- [ ] Runs in `release.yml` (or a dedicated image-smoke job), **off the PR gate**;
  SHA-pinned actions, injection-free, `persist-credentials: false`.  _AGENTS.md (CI & Security)_
- [ ] Docs note (CONTRIBUTING → Releases / architecture) recording the distroless
  `dlopen`/NSS consideration and that the image smoke covers it.

**File scope.**
- `.github/workflows/release.yml`, load + run the built image; the request smoke + `ldd` assertion (or a dedicated step/job).
- `scripts/image-smoke.sh` _(optional)_, the `docker run` + request probe, so CI and local share one definition.
- `Makefile` _(optional)_, an `image-smoke` target mirroring the other `make` entry points.
- `CONTRIBUTING.md` / `docs/architecture/release-supply-chain.md`, the `dlopen`/NSS note.

**Test tier.** None (release workflow), a `docker run` smoke, not a cabal suite;
validated by a dispatched `rc` run like the rest of `release.yml`. The PR gate is
unaffected.

**Notes / risks.** Independent of product logic; release-only, off the PR gate.
Depends on S20 only so the request smoke can hit a _serving_ proxy, a minimal
startup-only check (entrypoint resolves, libs present) could land earlier if
desired. The dynamic-link / closure analysis motivating this slice: the binary is
"static Haskell, dynamically-linked C" (Haskell libraries statically linked by
`justStaticExecutables`; C FFI deps, `gmp`, `zlib`, `libffi`, glibc, the RTS's
`libdw`/`numa`, dynamically linked by absolute-store-path `RUNPATH`), so Nix's
closure is complete for link-time deps and only runtime `dlopen` escapes it. See
the `dockerTools` build in `flake.nix` and
[`release-supply-chain.md`](../../docs/architecture/release-supply-chain.md#releases--container-image).
