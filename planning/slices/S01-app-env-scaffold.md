---
id: S01
title: App/Env scaffold + composition root
milestone: M0 — Shell, seams & foundations
status: not-started
depends-on: [S02]
test-tier: [unit]
arch-refs:
  - docs/architecture/technology-stack.md#key-decisions
  - docs/architecture/cloud-backends.md#process-model
  - docs/architecture/web-layer.md#middleware-and-helper-libraries
pr: null
---

# S01 — App/Env scaffold + composition root

> Milestone **M0** · depends on: [S02](S02-seam-interfaces.md) · tier: unit

**Goal.** Establish the imperative shell: `App = ReaderT Env IO` (with `unliftio`),
the `Env` composition-root record that holds the seams + shared HTTP manager +
logger + caches, and a real (if minimal) composition root that `Ecluse.run`
assembles. Request handlers run in plain `IO` taking `Env`; the worker/service
layer runs in `App`.

**Acceptance criteria.**
- [ ] `App = ReaderT Env IO` defined; `unliftio` adopted so `bracket`/`async` lift
  into the reader. — _technology-stack.md#key-decisions, web-layer.md#middleware-and-helper-libraries_
- [ ] `Env` record holds the three seams (from S02), a shared `http-client`
  `Manager` (placeholder until S08 needs it), a logger handle (filled by S04), and
  slots for the caches (filled by S13). Documented as the single composition root. —
  _technology-stack.md#key-decisions_
- [ ] `runServer :: Env -> IO ()` and `runWorker :: Env -> IO ()` are declared as
  the split-ready entry functions; at this slice they are honest minimal stubs
  (server fleshed out in S12, worker in S19) — **not** silent no-ops claimed as done,
  but documented as "wired in S12/S19". — _cloud-backends.md#process-model_
- [ ] `Ecluse.run` builds an `Env` (from injected seams/doubles) and is structured
  to run server + worker concurrently once they land. The placeholder `putTextLn`
  in `src/Ecluse.hs` is replaced by genuine composition-root wiring.
- [ ] `app/Main.hs` stays thin (parse-and-delegate only).

**File fence.**
- `src/Ecluse/App.hs` — `App`, `runApp`/`withEnv` helpers.
- `src/Ecluse/Env.hs` — `Env` record + `newEnv`/`withEnv` constructor.
- `src/Ecluse.hs` — `run` becomes the composition-root skeleton (declares `runServer`/`runWorker`).
- `app/Main.hs` — unchanged in spirit (thin).
- `ecluse.cabal` — add modules; add `unliftio`, `http-client`, `http-client-tls`.
- `test/unit/Ecluse/EnvSpec.hs` — `Env` assembles from doubles; resource lifecycle is bracketed.

**Test tier.** Unit — `Env` construction/teardown with the S02 doubles; no network.

**Notes / risks.** Keep `Env` field growth additive across later slices (caches in
S13, telemetry in S24). The composition root must remain config-driven by S20; here
it accepts injected seams so tests build an `Env` from doubles. Do not import any
backend SDK in `Env`/`App` — only the seam records (decoupling invariant).
