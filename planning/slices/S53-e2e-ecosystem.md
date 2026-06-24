---
id: S53
title: End-to-end testing ecosystem (whole-system, public-surface, cross-component)
milestone: M8 — Release hardening
status: in-progress
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
  test-only code path, cabal flag, or composition seam** — the binary under test is
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
  tier — never a `gate` dependency (image build + containers + npm is heavy).
  Promote to gating later if it proves stable. Wired into CI as its own
  `continue-on-error`-style job, **never** a `gate` dependency. _Resource caveat:
  the container topology is the safest, most consumer-portable option but the
  heaviest; revisit if CI lacks the memory/disk/compute._
- **Sequencing: built now on `runServices`.** `runServices` already exists, so the
  tier catches composition-root + cross-component regressions today. When **S20**
  (launch-ready AWS composition) lands, rebase the harness onto the final
  composition (swap the in-memory queue for ministack SQS where it adds coverage).

**Acceptance criteria** (representative scenarios — the `e2e` suite is green on
each, locally with Docker + npm and in the nightly CI job):
- [ ] **install (allow):** `npm install` of an allow-listed package (published
  >7 days ago, so the default `min-age` rule admits it) succeeds end to end, with
  the **correct bytes / integrity** (npm's own SRI check passes).
- [ ] **deny:** a package carrying an **install script** (denied by
  `DenyInstallTimeExecution`) is **blocked at the public surface** — `npm install`
  fails, the package is never served, and **no mirror job is enqueued**.
- [ ] **mirror round-trip (headline):** a first `npm install` triggers an enqueue;
  the worker mirrors the artifact to Verdaccio; a **later install is served from
  the private mirror** — proven by taking the public stub offline (or 404) and
  showing the install still succeeds.
- [ ] **integrity tamper:** the public stub serves an artifact whose bytes do
  **not** match the version's integrity; the worker's strongest-digest gate
  **rejects it and never publishes** (the mirror stays empty for that version).
- [ ] **HEAD on a tarball** does not pump the upstream body (ties to
  [#211](https://github.com/AlexaDeWit/Ecluse/issues/211) / [#270](https://github.com/AlexaDeWit/Ecluse/issues/270)).
- [ ] **graceful drain:** under a small concurrent load, a `SIGTERM` flips
  readiness (`/readyz`) and lets in-flight requests complete before exit (ties to
  [#160](https://github.com/AlexaDeWit/Ecluse/issues/160) / the S19 drain work).
- [ ] _publish round-trip via the publication target — **deferred** to land with
  [#163](https://github.com/AlexaDeWit/Ecluse/issues/163) / S52 (the publish path
  itself is not built yet); the harness leaves a marked hook for it._

**File scope.**
- `test/e2e/Spec.hs` — `hspec-discover` entry for the new suite.
- `test/e2e/Ecluse/E2E/Harness.hs` — the docker orchestration via `typed-process`:
  create/destroy the TEST-NET network, `docker load` the image archive + parse its
  tag, run/stop the proxy + Verdaccio + nginx-stub containers, publish + `/readyz`
  wait, and the isolated-`npm` driver.
- `test/e2e/Ecluse/E2E/Fixtures.hs` — generate the nginx static tree per scenario:
  npm-format packuments + tarballs with **correct SRI** (`npm pack` + `crypton`
  sha512), and the allow / deny / missing / tamper variants.
- `test/e2e/Ecluse/E2E/*Spec.hs` — one spec per scenario group above.
- `scripts/e2e-*.sh` — any non-trivial docker glue (Bash, `shellcheck`-clean), so
  logic stays out of inline blocks per `CONTRIBUTING.md` → Automation scripting.
- `ecluse.cabal` — the `ecluse-e2e` test-suite stanza (`typed-process`; Docker + npm;
  non-gating). **No library/flag change** — the production image is run as-is.
- `Makefile` — `test-e2e` target (builds `.#dockerImage`, then runs the suite;
  **not** in `check`/`gate`).
- `.github/workflows/ci.yml` — a non-gating `e2e` job (PR visibility + nightly),
  never wired as a `gate` dependency.
- `test/unit/Ecluse/Security/EgressSpec.hs` — a **tripwire** case pinning that
  `203.0.113.0/24` (and the other RFC 5737 documentation ranges) are deliberately
  **not** in `blockedRanges`, commented to the e2e tier + #178, so a future blocklist
  audit makes a conscious choice rather than silently breaking e2e. _(Add to the
  existing egress spec; this is the one touch outside `test/e2e`.)_
- `docs/testing.md` — document the new tier (what it covers, that it never gates).

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

**Relationship to other slices.** Exercises the integration points of
[S14](S14-packument-path.md) (packument path), [S15](S15-tarball-path.md) (tarball
path + enqueue), [S19](S19-mirror-worker.md) (mirror worker), and
[S12](S12-wai-app-middleware.md) (readiness/drain). **S20** (launch-ready AWS
composition) is the natural adjacency — when it lands, rebase the harness onto the
final composition. The publish round-trip ties to **S52** / [#163](https://github.com/AlexaDeWit/Ecluse/issues/163).
