---
id: S53
title: End-to-end testing ecosystem (whole-system, public-surface, cross-component)
milestone: M8 — Release hardening
status: merged
depends-on: [S15, S19]
test-tier: [e2e]
arch-refs:
  - docs/testing.md#the-test-tiers
  - docs/architecture/cloud-backends.md#process-model
  - docs/architecture/registry-model.md
pr: null
issue: 271
---

# S53 — End-to-end testing ecosystem

> Milestone **M8** · depends on: [S15](S15-tarball-path.md), [S19](S19-mirror-worker.md) · tier: **e2e** (new, non-gating) · closes [#271](https://github.com/AlexaDeWit/Ecluse/issues/271)

**Goal.** A true **end-to-end** test tier that none of the existing three
(`ecluse-unit`, `ecluse-integration`, `ecluse-smoke`) covers: assemble the
**whole system through the real composition root**, drive its **public HTTP
surface with the real `npm` CLI**, and cross the **server↔worker boundary** end
to end. The headline flow no current tier exercises:

> a serve-time request **enqueues** a mirror job → the **worker** picks it up,
> verifies, and **publishes** to the private mirror → a subsequent request is
> served **from the private mirror**, not the public upstream.

**Design (the decisions, resolved with the architect).**
- **Whole system, the real shipped image, no production change (ship == test).**
  Run the **actual `ecluse` OCI image** (`nix build .#dockerImage`) as a container —
  so e2e validates the very artifact we publish, configured from the environment
  exactly as production is. The single process runs `runServer ‖ runWorker` over
  one `Env` with the in-memory queue (`Ecluse.hs` `newInMemoryQueue`), so the
  server→worker hand-off is exercised for real without a queue emulator. **No
  test-only code path, cabal flag, or composition hook** — the binary under test is
  byte-for-byte the release binary.
- **Reaching the stub past the egress guard — a TEST-NET docker network, not a
  code escape hatch.** S40's guard rechecks every resolved outbound IP on the
  untrusted public + artifact path and refuses internal ranges (loopback, RFC1918,
  link-local, CGNAT, ULA — `Ecluse.Security.blockedRanges`). Every loopback or
  default-docker-bridge (172.16/12) address an in-process stub could use is
  therefore blocked. So the containers run on a docker network with a
  **`203.0.113.0/24` (RFC 5737 TEST-NET-3, _documentation_) subnet** — a globally
  scoped range the guard does **not** block, because blocking documentation space
  buys zero SSRF protection. Using a documentation range for a _fake external
  upstream_ is the textbook-correct choice (it can never alias a real host), so the
  real default-build guard reaches the stub unmodified. See the **#178 relationship**
  below.
- **Orchestration: the docker CLI via `typed-process`.** The custom-subnet network
  (`docker network create --subnet 203.0.113.0/24`) is beyond `testcontainers-hs`
  0.5.3, so the harness drives docker directly: create the network, `docker load`
  the Nix image archive (parse its content-hash tag), run the three containers on
  the network, publish the proxy port to host loopback, wait on `/readyz`, tear all
  down. Verdaccio is the **private upstream + mirror target** (reached via the proxy's
  _trusted_ manager, so even its container IP needs no opt-in); the **public-upstream
  stub** is an **nginx** container serving generated npm-format packuments + tarballs
  with **correct integrity** (no bespoke Haskell stub image), its static tree
  regenerated per scenario for the allow / deny / missing / tamper cases.
- **Driver: the real `npm` CLI on the host**, against the proxy's published loopback
  port, through an isolated `userconfig` + `cache` + `prefix` (no global state),
  asserting **client-observable** outcomes. `nodejs`/`npm` are already in the dev
  shell (the version oracle), so no new build-time dependency is introduced. (npm →
  proxy is inbound, so it is unaffected by the egress guard.)
- **Gating: non-gating.** Runs **pre-merge (visibility) + nightly**, like the smoke
  tier, never a `gate` dependency (image build + containers + npm is heavy).
  Promote to gating later if it proves stable. Wired into CI as its own
  `continue-on-error`-style job, **never** a `gate` dependency. _Resource caveat:
  the container topology is the safest, most consumer-portable option but the
  heaviest; revisit if CI lacks the memory/disk/compute._
- **Sequencing: built now on `runServices`.** `runServices` already exists, so the
  tier catches composition-root + cross-component regressions today. When **S20**
  (launch-ready AWS composition) lands, rebase the harness onto the final
  composition (swap the in-memory queue for ministack SQS where it adds coverage).

**Acceptance criteria** (representative scenarios — the `e2e` suite is green on
each via `make test-e2e` and in the CI e2e job):
- [x] **install (allow):** `npm install` of an allow-listed package (published
  >7 days ago, so the default `min-age` rule admits it) succeeds end to end, with
  npm's own SRI check passing on the served bytes.
- [x] **deny:** a package carrying an **install script** (denied by
  `DenyInstallTimeExecution`) is **blocked at the public surface** — `npm install`
  fails and the package is never mirrored.
- [x] **mirror round-trip (headline):** the full core loop, end to end, as an
  upstream-outage scenario — a package missing from the private mirror but present on
  public is **served from public** (an `npm install` writes a lockfile), the worker
  mirrors the artifact to Verdaccio (server↔worker, in one process over the in-memory
  queue), and then, **with the public upstream paused**, an `npm ci` from that lockfile
  still installs it **from the private mirror** (the tarball path is private-first and
  `npm ci` never re-resolves via the packument, so public is never contacted) — proving
  the served-from-mirror hop, not merely its presence.
- [x] **integrity tamper:** the public stub serves an artifact whose bytes do
  **not** match the version's integrity; the worker's strongest-digest gate
  **rejects it and never publishes** (the mirror stays empty for that version).
- [x] **HEAD on a tarball** reports the artifact size but streams no body and
  enqueues no mirror — the [#211](https://github.com/AlexaDeWit/Ecluse/issues/211) /
  [#269](https://github.com/AlexaDeWit/Ecluse/pull/269) fix is now on this base, so the
  case is a **live assertion**, driven on a HEAD-only fixture so the empty mirror is
  attributable to the HEAD alone (no GET to back-fill it).
- [ ] **graceful drain:** a `SIGTERM` flips readiness (`/readyz`) and drains
  in-flight work — **pending**, ties to [#160](https://github.com/AlexaDeWit/Ecluse/issues/160).
- [ ] _publish round-trip via the publication target — **deferred** to land with
  [#163](https://github.com/AlexaDeWit/Ecluse/issues/163) / S52 (the publish path
  itself is not built yet)._

**Bugs the tier surfaced and fixed (the slice's payoff).** On its first real run
against a live `npm` client + Verdaccio, the e2e tier caught two composition-level
defects no unit/integration test could (none drives a real client), both fixed here:
1. **`dist.tarball` was rewritten path-relative (`/npm/…`)**, which `npm` reads as a
   local `file:` path and cannot install. Fixed by a new **`PROXY_PUBLIC_URL`** config
   that makes the composition root emit an **absolute** rewrite base
   (`Ecluse.Config`, `Ecluse.Composition`); `USAGE.md` documents it as required for
   real installs.
2. **The mirror worker's publish omitted `Content-Type: application/json`**, which a
   spec-compliant registry (Verdaccio) rejects with `415`, so no artifact ever
   mirrored. Fixed in `Ecluse.Registry.Npm.publishRequest` (the docstring already
   promised the header; the code never set it).

**File scope.**

_The e2e tier:_
- `test/e2e/Spec.hs` — `hspec-discover` entry for the new suite.
- `test/e2e/Ecluse/E2E/Harness.hs` — the docker orchestration via `typed-process`:
  pick the host port up front (for `PROXY_PUBLIC_URL`), create the TEST-NET network,
  run/stop the proxy + Verdaccio + nginx-stub containers, `/readyz` wait, the
  isolated-`npm` driver and the HTTP/mirror probes, and teardown.
- `test/e2e/Ecluse/E2E/Fixtures.hs` — generate the nginx static tree: npm-format
  packuments + `tar`-built tarballs with **correct SRI** (`crypton` sha512), in
  allow / deny / mirror / tamper variants.
- `test/e2e/Ecluse/E2E/SuiteSpec.hs` — the scenarios, each in its **own freshly booted
  environment** (`around withE2E`, per-test isolation the default so a case can halt or
  mutate its harness without leaking into another), skipping `pending` when the env is
  unavailable.
- `ecluse.cabal` — the `ecluse-e2e` test-suite stanza (non-gating).
- `scripts/e2e.sh` + `Makefile` `test-e2e` — build + load the image, run the suite;
  **not** in `check`/`gate`.
- `.github/workflows/ci.yml` — a non-gating `e2e` job (PR visibility + nightly),
  `continue-on-error`, never a `gate` dependency. Disk-prep steps free the small root
  partition (remove preinstalled toolchains the pure-Nix job never uses) and relocate
  the Nix store + Docker data-root onto the ephemeral `/mnt` volume, since the image
  build + `docker load` can exhaust root; the job is also `save-nix-store: "false"`
  (restore-only — it must not write the shared cache the `.#ci` jobs own).
- `docs/testing.md` — document the new tier (what it covers, that it never gates).
- `test/unit/Ecluse/SecuritySpec.hs` — the **tripwire** pinning the RFC 5737
  documentation ranges (incl. `203.0.113.0/24`) as deliberately **not** in
  `blockedRanges`, commented to the e2e tier + #178.

_The two production fixes the tier surfaced (see above):_
- `src/Ecluse/Config.hs` — the `PROXY_PUBLIC_URL` field + env parse.
- `src/Ecluse/Composition.hs` — `mountBaseUrl`: the absolute rewrite base under
  `PROXY_PUBLIC_URL` (relative-path fallback retained).
- `src/Ecluse/Registry/Npm.hs` — set `Content-Type: application/json` on publish.
- `USAGE.md` — document `PROXY_PUBLIC_URL`.
- `test/unit/Ecluse/ConfigSpec.hs`, `test/unit/Ecluse/CompositionSpec.hs`,
  `test/unit/Ecluse/Registry/NpmSpec.hs` — unit coverage for the three fixes.

**Test tier.** A new **`e2e`** tier — slower, real-`npm`, real-Verdaccio, the
**real OCI image** — **non-gating** (pre-merge visibility + nightly), alongside the
gating unit/integration/doctest and the non-gating smoke tiers.

**#178 relationship (egress blocklist audit).** This tier relies on the guard
treating **documentation ranges (RFC 5737 — `192.0.2/24`, `198.51.100/24`,
`203.0.113/24`) as external**. That holds today (they are not in `blockedRanges`)
and is correct: a documentation range never aliases a real service, so blocking it
adds no SSRF protection. [#178](https://github.com/AlexaDeWit/Ecluse/issues/178) will
audit and _extend_ the blocklist (cloud-metadata IPs, operator-configurable
additions); a correct audit leaves documentation space external. The tripwire test
above makes the dependency explicit so the two cannot silently collide; record it on
#178 when that work is scoped.

**Notes / risks.**
- **Determinism vs. the real `npm` CLI.** Pin npm's environment hard (isolated
  `npm_config_userconfig`, `npm_config_cache`, `npm_config_prefix`, no audit/fund
  network chatter, registry pointed only at the proxy) so a developer's global npm
  state cannot leak in.
- **`min-age` and fixtures.** The default policy admits only versions published
  >7 days ago; fixture packuments must backdate their `time` entries, or the allow
  scenario fails closed (deny-by-default) and the test reads as a false negative.
- **Container lifecycle.** Bracket the network + every container so each exit path
  tears them down (unique names per run; `docker rm -f` + `network rm` on cleanup);
  wait on `/readyz` (the readiness probe) via the published port, never a fixed sleep.
- **Verdaccio publish auth.** Configure Verdaccio to accept the mirror worker's
  publish (anonymous/`$all` publish access, or a seeded token) so the round-trip is
  not blocked by registry auth rather than by Écluse.
- **Heavier than the gate.** The tier owns the image build + multiple containers +
  npm; it must never block a merge, and may be too heavy for some CI — revisit the
  topology if so. The deterministic behaviours it asserts are **also** owed a
  `U`/`I` test elsewhere (per `docs/testing.md` → *What gates, and what doesn't*) —
  e2e is the cross-component proof, not the sole evidence for any one behaviour.
- **Runner disk.** The default `ubuntu-latest` root partition (~14 GB) is the binding
  constraint: the Nix image build's GHC closure, the `docker load` copy of the OCI
  image, and the running containers all want space. The job mitigates this in two
  cheap, coverage-preserving ways before the toolchain installs — free the root
  partition (drop preinstalled Android/.NET/Swift/ghcup the pure-Nix job never uses)
  and bind-mount the Nix store + Docker data-root onto the larger ephemeral `/mnt`
  volume. This buys headroom for more ecosystems (rubygems, pypi) landing in the
  image; if it stops sufficing, the next levers are nightly-only cadence, splitting
  the image build into its own job (push to a registry, pull to test), or a larger
  runner.

**Relationship to other slices.** Exercises the integration points of
[S14](S14-packument-path.md) (packument path), [S15](S15-tarball-path.md) (tarball
path + enqueue), [S19](S19-mirror-worker.md) (mirror worker), and
[S12](S12-wai-app-middleware.md) (readiness/drain). **S20** (launch-ready AWS
composition) is the natural adjacency — when it lands, rebase the harness onto the
final composition. The publish round-trip ties to **S52** / [#163](https://github.com/AlexaDeWit/Ecluse/issues/163).
