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

**Substrate that landed mid-discussion (#133, `refactor/server`):** the web layer
was generalized to a complete per-mount `MountBinding` (prefix + classifier +
packument deps + error renderer, wired as one unit at the composition root), with
**mandatory path-mounting** — a root mount is unrepresentable (`bindingPrefix ::
NonEmpty Text`) — and npm's `{"error": …}` body shape moved out of the agnostic
layer into an adapter renderer. Several threads below build on this: **D5 shrank**
to deriving the prefix from the ecosystem and flowing the ecosystem through the
binding and the config mount, and **D1 resolved** against it (see Resolved, below).

Threads are worked **one at a time**; D1 and D2 are resolved, so **D3 is next**.

### Item 1 — config: per-ecosystem generalization

> **D1 and D2 are resolved** — see [Resolved](#resolved). Outstanding threads: D3–D5.

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

**D4 — Cloud auth: concrete global provider, abstracted per-mount consumer.**
_(largely independent of D1)_
AWS/GCP credentials are usually **container-level** (instance role / IRSA / ADC /
workload identity), so the **credential store and its auth state stay global**, not
per-mount. `AWS_REGION` / `GOOGLE_CLOUD_PROJECT` scope that ambient identity and so
**stay global** too — a multi-cloud process holds one concrete provider *per cloud*
in `Env`, keyed by cloud. What is **per-mount** is only the *methodology*: the
[credential strategy](../docs/architecture/access-model.md) and which backend a
mount's mirror-write (and `service`/`delegated-cache` read) draws from —
"abstracted mount-specific consumers, concrete global providers." In practice those
providers very likely **collapse to one**: Écluse runs under a single container task
role (AWS) / workload identity (GCP), so the mirror-write and any
`service`/`delegated-cache` read resolve to the **same** identity — the service acts
as one consistent entity, and the per-mount credential *selection* usually just
points back at it. The genuinely per-mount *coordinate* is the target/queue
identifier a mount writes to
(`mtUrl` / `MIRROR_QUEUE_URL`), not the auth that reaches it. Cross-cuts **D6**: the
provider's token-refresh **state** (the minted short-lived token and its expiry) is
global mutable state — a `TVar` in `Env`, one per provider. _(Supersedes the earlier
"move region/project per-mount" framing.)_ — **Status: queued.**

**D5 — Ecosystem drives the binding & the config mount.**
_(depends on D1; tied to D2; partly delivered by #133)_
#133 already moved the web layer to a complete per-mount `MountBinding` (replacing
the old `Mount -> X` resolver fields and `defaultServerConfig`) and made every
registry path-mounted. What remains: the binding's `bindingPrefix :: NonEmpty Text`
is still set free-form (`Ecluse.hs` hard-codes `/npm`) — under D1 it must be
**derived from the ecosystem**, and the **ecosystem must flow into the binding and
the `Ecluse.Config` mount** (the env-only single mount still desugars to prefix `/`,
which the no-root rule forbids — the follow-up #133 flagged for the Config → Server
wiring, S20). The adapter (`RegistryClient`) is likewise resolved **per-ecosystem**
at the composition root rather than the single global `envRegistry`. — **Status: queued.**

### Item 2 — Reader pattern over the request path

**D6 — Reader migration & request-context shape (+ the state idiom).**
_(depends on D1 for the context's mount shape)_
We already have `App = ReaderT Env IO` (`MonadReader`/`MonadUnliftIO`); the worker
runs in it. The holdout is the request hot path, which threads `Env` **and** the
mount's per-request deps explicitly (the `PackumentDeps` now carried by the
`MountBinding`, post-#133; the parked [issue #121](https://github.com/AlexaDeWit/Ecluse/issues/121)).
Proposal: extend `App` over the handlers and collapse the two-arg thread into a
request-scoped context (`RequestCtx { ctxEnv, ctxMount }`, or `ReaderT (Env,
ResolvedMount)`), with dispatch installing the matched mount via `local` after
routing. **State idiom (decided, recorded so M4 follows it):** shared mutable state
(credential-refresh cells, breaker state, in-flight sets) lives as `TVar`/`IORef`
**in `Env`** under the single `ReaderT` — **no `StateT` layer** (`StateT`-over-`IO`
loses state across `forkIO`/`async` and gives no shared state; the metadata cache
already follows the refs-in-`Env` idiom). — **Status: queued.**

---

## Resolved

**D1 — Ecosystem representation & mount identity.** _(resolved 2026-06-23)_
**Decision:** ecosystem is **value-level** data (not a type parameter); **one mount
per ecosystem**; the path prefix is **derived from the ecosystem, not configured**
(npm → `/npm`); the mount map is keyed by ecosystem. Cross-ecosystem rule safety
(D3) will come from a fail-fast config check, not the type system. The earlier
"several mounts of the *same* ecosystem / `/npm-prod` vs `/npm-canary`" capability
was a misread requirement and is **dropped** — "multiple" means multiple
*ecosystems*.
**Rendered into:** [`hosting.md` → Mounts](../docs/architecture/hosting.md#mounts)
and [`configuration.md`](../docs/architecture/configuration.md#configuration).
**Code still owing** (a pre-S15 hardening slice; see D5): the value-level
`Ecosystem` on the declarative mount, the `ecosystem → mount` keying, and the
ecosystem-derived `bindingPrefix`. Not built yet — `Ecluse.hs` still hard-codes
`/npm` and `Ecluse.Config` still keys by `MountPrefix`.

**D2 — Registry topology.** _(resolved 2026-06-23)_
**Decision:** the topology is exactly **three architectural roles** — private
upstream, public upstream, mirror target — there for the security/flow design, not
npm-specific. Modelled as a **record of named roles, not a positional tuple** (the
code type is already a record; the fix is the name). Two roles **may coincide**: the
private upstream and the mirror target are permitted to be the same registry (a
consumer flow choice; CodeArtifact upstream-folding supports it) — and the
**credential identity** behind them very likely coincides too (see D4: a single
container task role). The **artifact/files host is *not* a topology coordinate**: it
is discovered from the upstream response and governed by the egress policy
(`PROXY_RESPECT_UPSTREAM_TARBALL_HOST`, S40).
**Rendered into:** [`hosting.md` → Mounts](../docs/architecture/hosting.md#mounts);
registry-model.md already carried the role table and the roles-may-coincide note.
**Code still owing** (hardening slice): rename `RegistryTuple` → a structural name
(e.g. `MountRegistries`); align the `S03` slice prose when the rename lands.
