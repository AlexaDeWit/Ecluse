# Design Queue

Architectural decisions raised but **not yet resolved** — parked here so they are
worked **one at a time** with the architect rather than front-loaded all at once
(see [orchestration-strategy → Surface decisions one at a time](orchestration-strategy.md#guardrails-always-on)).

This is a holding area, not design-of-record: when an item resolves, its outcome
lands in the relevant [`docs/`](../docs/) design document (and a slice under
[`slices/`](slices/), if it needs building), and the item is struck here with a
pointer to where it went. An item carries enough context to resume the discussion
cold; it does **not** duplicate the eventual design doc.

**Status:** `queued` → `in-discussion` → `resolved` (with a doc/slice pointer).

---

## Epic: base hardening before S15 (raised 2026-06-23)

Two foundations to get right **before** the tarball path ([S15](slices/S15-tarball-path.md))
and M4 build on them, so we don't retrofit. Decisions already taken at the epic
level:

- The config/ecosystem generalization is **full**, not minimal — design for the
  PyPI / RubyGems shapes up front, not just an ecosystem tag.
- The Reader migration should land **before S15**, so the request hot path is
  written in the target style rather than retrofitted.

Threads are ordered so the **spine decision (D1)** is settled first; the rest
depend on it. Work them top-down, one at a time.

### Item 1 — config: per-ecosystem generalization

**D1 — Ecosystem representation: value-level vs type-level.** _(spine; everything
hangs off this)_
The fork: is `Ecosystem` a **value** a mount carries, or a **type parameter**
(`Mount e`, `RegistryClient e`, `Rule e`)? Team-lead recommendation: **value-level**,
consistent with the existing grain (the `RegistryClient`/`MirrorQueue`/`CredentialProvider`
Handle pattern, the ecosystem-agnostic core, `Ecluse.Ecosystem` as a dispatch tag).
Type parameters buy compile-time "a PyPI rule can't touch an npm mount" but force
existential wrapping of a heterogeneous `MountMap` and infect the ecosystem-blind
server/dispatch/worker layers; the same safety is available from a **fail-fast
config check** (already the config philosophy). — **Status: in-discussion.**

**D2 — Registry topology generalization.** _(depends on D1)_
`RegistryTuple {private, public, mirror}` reads as npm-shaped but the three are
**architectural roles** every ecosystem has. The genuinely ecosystem-variant axis
is narrower: the **artifact/files host** (npm often co-located; PyPI splits to
`files.pythonhosted.org`; gems serves `.gem` separately — cf. the planned
`PROXY_RESPECT_UPSTREAM_TARBALL_HOST` knob). Proposal: keep the three-role tuple,
lift the **artifact-host** out as a first-class per-role coordinate, bind the
adapter per-mount. Protocol/metadata shape (packument vs PEP 503 simple-index vs
compact-index) stays the adapter's concern, not the topology's. — **Status: queued.**

**D3 — Rule vocabulary per ecosystem.** _(depends on D1)_
The agnostic domain model already did most of the work: `AllowIfPublishedBefore`
and the CVE rules are cross-ecosystem; `DenyHasInstallScripts` reads the agnostic
`CodeExecSignal` (PyPI/gems populate the same signal via their projections), so it
is arguably **mis-named, not ecosystem-bound** — candidate rename toward the
install-time-code-execution concept. `AllowScope` is **genuinely npm-specific**
(no scopes in PyPI/gems). Proposal: keep `Rule` a **single sum type**; each
constructor declares the **ecosystems it applies to**; config-load **validates**
each mount's named rules against its ecosystem and fails loudly otherwise.
Open question to settle: is the genuinely ecosystem-specific surface really just
the "namespace/scope"-shaped rules, or is deeper per-ecosystem rule *semantics*
coming? — **Status: queued.**

**D4 — Cloud coordinates: global → per-mount.** _(largely independent of D1)_
`mtCredential`/`mtQueue` (the *backends*) are already per-`MirrorTarget`, but
`AWS_REGION` / `GOOGLE_CLOUD_PROJECT` / `MIRROR_QUEUE_URL` are flat `EnvConfig`
**globals** — a multi-mount, multi-cloud deployment (one AWS mount, one GCP mount)
can't express that. Move these coordinates onto the per-mount target. — **Status: queued.**

**D5 — Adapter location: global `Env` → per-ecosystem.** _(depends on D1; tied to D2)_
Today `Env` holds one `envRegistry` ("one npm client reused across every cloud")
and the classifier is hardcoded (`npmClassifier _mount = Npm.classify` *ignores*
the mount). Under value-level ecosystem the composition root resolves
**ecosystem → `RegistryClient`** and the classifier becomes
`\mount -> classifierFor (mountEcosystem mount)`. The adapter handle can't sit on
`Config.Mount` without breaking its `Eq`/`Show`; reuse the existing precedent —
`ServerConfig.scPackumentDeps :: Mount -> Maybe PackumentDeps` keeps function-valued
deps off `Mount` — so a *resolved/runtime* mount pairs the declarative mount with
its constructed adapter. — **Status: queued.**

### Item 2 — Reader pattern over the request path

**D6 — Reader migration & request-context shape (+ the state idiom).**
_(depends on D1 for the context's mount shape)_
We already have `App = ReaderT Env IO` (`MonadReader`/`MonadUnliftIO`); the worker
runs in it. The holdout is the request hot path, which threads `Env` **and** a
per-mount `PackumentDeps` explicitly (the parked [issue #121](https://github.com/AlexaDeWit/Ecluse/issues/121)).
Proposal: extend `App` over the handlers and collapse the two-arg thread into a
request-scoped context (`RequestCtx { ctxEnv, ctxMount }`, or `ReaderT (Env,
ResolvedMount)`), with dispatch installing the matched mount via `local` after
routing. **State idiom (decided, recorded so M4 follows it):** shared mutable state
(credential-refresh cells, breaker state, in-flight sets) lives as `TVar`/`IORef`
**in `Env`** under the single `ReaderT` — **no `StateT` layer** (`StateT`-over-`IO`
loses state across `forkIO`/`async` and gives no shared state; the metadata cache
already follows the refs-in-`Env` idiom). — **Status: queued.**
