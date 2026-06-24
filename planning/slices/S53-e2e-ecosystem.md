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
- **Whole system, real composition root.** Boot the actual `ecluse` **binary as a
  subprocess** (not a refactored in-process boot), configured from the environment
  exactly as production is. The single process runs `runServer ‖ runWorker` over
  one `Env` with the in-memory queue (`Ecluse.hs` `newInMemoryQueue`), so the
  server→worker hand-off is exercised for real without a queue emulator. This is
  the most faithful reading of "through the real composition root" and needs no
  test-only entry point.
- **Driver: the real `npm` CLI.** Scenarios run `npm install` / `npm publish` /
  dist-tags against the running proxy, through an isolated `userconfig` +
  `cache` + `prefix` (no global state), asserting **client-observable** outcomes.
  `nodejs`/`npm` are already in the dev shell (the version oracle), so no new
  build-time dependency is introduced.
- **Backends.** A real **Verdaccio** container (via the existing `testcontainers`
  pattern, cf. `Ecluse.Integration.Ministack`) is the **private upstream + mirror
  target** — so "served from the private mirror" is a real read from a real
  registry. A **controllable public-upstream stub** (an in-process WAI app we own)
  serves npm-format packuments + tarballs with **correct integrity**, and is
  scriptable for the allow / deny / missing-version / tamper cases.
- **Gating: non-gating.** Runs **pre-merge (visibility) + nightly**, like the
  smoke tier — never a `gate` dependency (Docker + container + npm startup is
  heavy). Promote to gating later if it proves stable. Wired into CI as its own
  `continue-on-error`-style job, **never** as a `gate` dependency.
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
- `test/e2e/Ecluse/E2E/Harness.hs` — boot/teardown of the `ecluse` subprocess
  (env-configured, `/readyz` readiness wait), Verdaccio container lifecycle, the
  controllable upstream stub, and the isolated-`npm` driver.
- `test/e2e/Ecluse/E2E/Upstream.hs` — the scriptable public-upstream WAI stub
  (packument + tarball fixtures, allow/deny/missing/tamper modes, correct SRI).
- `test/e2e/Ecluse/E2E/*Spec.hs` — one spec per scenario group above.
- `ecluse.cabal` — the `ecluse-e2e` test-suite stanza (Docker + npm; non-gating).
- `Makefile` — `test-e2e` target (mirrors `test-integration`; **not** in `check`/`gate`).
- `.github/workflows/ci.yml` — a non-gating `e2e` job (PR visibility + nightly),
  never wired as a `gate` dependency.
- `docs/testing.md` — document the new tier (what it covers, that it never gates).

**Test tier.** A new **`e2e`** tier — slower, real-`npm`, real-Verdaccio,
real-composition-root — **non-gating** (pre-merge visibility + nightly), alongside
the gating unit/integration/doctest and the non-gating smoke tiers.

**Notes / risks.**
- **Determinism vs. the real `npm` CLI.** Pin npm's environment hard (isolated
  `npm_config_userconfig`, `npm_config_cache`, `npm_config_prefix`, no audit/fund
  network chatter, registry pointed only at the proxy) so a developer's global npm
  state cannot leak in.
- **`min-age` and fixtures.** The default policy admits only versions published
  >7 days ago; fixture packuments must backdate their `time` entries, or the allow
  scenario fails closed (deny-by-default) and the test reads as a false negative.
- **Subprocess lifecycle.** Bracket the `ecluse` process so every exit path tears
  it down; use `/readyz` (readiness probe) to wait for boot rather than a sleep.
- **Heavier than the gate.** The tier owns Docker + a container pull + npm; it must
  never block a merge. The deterministic behaviours it asserts are **also** owed a
  `U`/`I` test elsewhere (per `docs/testing.md` → *What gates, and what doesn't*) —
  e2e is the cross-component proof, not the sole evidence for any one behaviour.

**Relationship to other slices.** Exercises the integration points of
[S14](S14-packument-path.md) (packument path), [S15](S15-tarball-path.md) (tarball
path + enqueue), [S19](S19-mirror-worker.md) (mirror worker), and
[S12](S12-wai-app-middleware.md) (readiness/drain). **S20** (launch-ready AWS
composition) is the natural adjacency — when it lands, rebase the harness onto the
final composition. The publish round-trip ties to **S52** / [#163](https://github.com/AlexaDeWit/Ecluse/issues/163).
