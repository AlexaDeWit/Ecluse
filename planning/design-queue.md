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

Threads are worked **one at a time**. **All threads (D1–D6) are resolved** — see
[Resolved](#resolved). The remaining work is code: the consolidated
[pre-S15 hardening slice](#code-owing--the-pre-s15-hardening-slice).

### Item 1 — config: per-ecosystem generalization

> **Item 1 complete** — D1–D5 all resolved; see [Resolved](#resolved).

### Item 2 — Reader pattern over the request path

> **Item 2 complete** — D6 resolved; see [Resolved](#resolved).

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

**D4 — Cloud auth: global provider, per-mount reference.** _(resolved 2026-06-23)_
**Decision:** the credential identity is **process-global** — typically a single
container task role (AWS) / workload identity (GCP), built **once at the composition
root**, with its region/project (`AWS_REGION` / `GOOGLE_CLOUD_PROJECT`) and refresh
**state** (minted token + expiry, a `TVar` in `Env`) global too. A mount holds **no
provider of its own**; it only **references** which global provider its strategy
draws on — "abstracted mount-specific consumers, concrete global providers." In the
common deployment those references **collapse to one** identity (mirror-write and any
`service`/`delegated-cache` read are the same container role; Écluse acts as one
consistent entity); a multi-cloud process keeps one provider per cloud. A mount that
references a credential source with **no initialized provider halts the app at boot**
(aggregated fail-fast). Supersedes the earlier "move region/project per-mount" idea.
**Rendered into:** [`cloud-backends.md` → Credential Provider](../docs/architecture/cloud-backends.md#credential-provider)
and [`configuration.md` → Validation](../docs/architecture/configuration.md#validation-fail-fast-reject-the-unknown).
**Code still owing** (hardening slice): hoist `mtCredential` off `MirrorTarget` into a
global provider registry the mount references, and add the boot-time "credential
references must resolve" check to config validation / the composition root.

**D3 — Rule vocabulary.** _(resolved 2026-06-23)_
**Decision:** rules are **ecosystem-agnostic by design** — they reason only over the
agnostic `PackageDetails`; **ecosystem-specific rules are out of scope**. A rule
whose signal is absent for an ecosystem (e.g. a declared scope on a scopeless
ecosystem) **abstains** — under deny-by-default that is the sensible default, not a
per-ecosystem error. No per-ecosystem rule vocabulary, applicability tagging, or
ecosystem-gated validation (the machinery earlier proposed is **dropped**). Rule
**names** track the agnostic concept, normalized **early**.
**Rendered into:** [`rules-engine.md` → Rules Engine](../docs/architecture/rules-engine.md#rules-engine).
**Code still owing — an *early*, dedicated rename slice** (cheap pre-launch, before
more rules and usages accrete; touches `Ecluse.Rules.Types`, `Ecluse.Rules`,
`Ecluse.Config`'s wire `type` strings, the tests, and the rule-name references in
`configuration.md` / `rules-engine.md` / `observability.md` / research):
`DenyHasInstallScripts` → an agnostic install-time-code-execution name (proposed
`DenyInstallTimeExecution`); `AllowScope` → keep as the agnostic "namespace" concept
the domain model already carries (abstaining where absent), optionally
`AllowNamespace` — final identifiers the architect's call.

**D5 — Ecosystem drives the binding & the config mount.** _(resolved 2026-06-23)_
**Decision:** the config document's `mounts` object is **keyed by ecosystem name**
(`npm`, `pypi`); the path prefix is **derived** from that key, never declared (a
wrong/colliding prefix is unrepresentable). The env-only single-mount path defaults
to ecosystem = **npm** and derives `/npm` — closing the #133 gap where it desugared
to `/`, which the no-root rule forbids. The composition root resolves **ecosystem →
`RegistryClient` + classifier + derived `bindingPrefix`**, producing one
`MountBinding` per ecosystem.
**Rendered into:** [`configuration.md`](../docs/architecture/configuration.md#configuration)
(mounts keyed by ecosystem); the model is in
[`hosting.md` → Mounts](../docs/architecture/hosting.md#mounts) (D1).
**Code still owing** — this *is* the S20-flagged Config → Server wiring, folded into
the pre-S15 hardening slice: re-key `Ecluse.Config`'s `MountMap` by ecosystem, add the
value-level `Ecosystem` to the declarative mount, derive `bindingPrefix`, default the
env-only mount to npm, and resolve `ecosystem → RegistryClient` at the composition
root (replacing the single global `envRegistry`).

**D6 — Reader migration & request-context shape.** _(resolved 2026-06-23)_
**Decision:** extend the existing `App = ReaderT Env IO` over the request hot path.
Handlers run over a per-request **`RequestCtx { ctxEnv :: Env, ctxMount ::
MountBinding, … }`** (`ReaderT RequestCtx IO`): the dispatch layer runs in
`App`/`ReaderT Env`, matches the mount, builds `RequestCtx`, and runs the handler in
it, so the per-mount deps (registry set, rules, renderer, derived prefix) are read
from context rather than re-threaded through the pipeline. Shared mutable state lives
as `TVar`/`IORef` **in `Env`** under the single reader — **no `StateT`**. Accessor
style: a **concrete `RequestCtx` record with plain accessors** (matching the concrete
`App` newtype), revisable to `Has`-classes (`HasEnv r`, …) only if a real need
appears. Lands **before S15** so the tarball path is written in the target style.
Closes the parked [#121](https://github.com/AlexaDeWit/Ecluse/issues/121).
**Rendered into:** [`technology-stack.md` → Key Decisions](../docs/architecture/technology-stack.md#key-decisions)
and [`web-layer.md`](../docs/architecture/web-layer.md#web-layer).
**Code still owing** (hardening slice): introduce `RequestCtx`, run handlers in
`ReaderT RequestCtx IO` (deriving `Katip`/`KatipContext`), and retire the packument
handler's explicit `Env`+deps threading.

---

## Code owing — the pre-S15 hardening slice

All six threads are resolved in the docs; the conforming **code** is gathered here as
one hardening track, to land **before [S15](slices/S15-tarball-path.md)** so the
tarball path is built on the new base. A natural ordering:

1. **Rule-name normalization (early, standalone).** Rename `DenyHasInstallScripts` →
   an agnostic install-time-code-execution name; settle `AllowScope`/namespace.
   Touches `Ecluse.Rules.Types`, `Ecluse.Rules`, `Ecluse.Config` wire `type`s, the
   tests, and the rule-name references in docs. Cheap pre-launch — do it before more
   rules accrete. _(D3)_
2. **Ecosystem as a value on the mount.** Add `Ecosystem` to the declarative config
   mount; re-key `MountMap` / the document `mounts` by ecosystem; derive
   `bindingPrefix`; default the env-only mount to npm. Rename `RegistryTuple` → a
   structural name (e.g. `MountRegistries`). _(D1, D2, D5)_
3. **Global credential providers.** Hoist `mtCredential` off `MirrorTarget` into a
   process-global provider registry the mount references; add the boot-time
   "credential references must resolve" check. _(D4)_
4. **Composition-root wiring.** Resolve `ecosystem → RegistryClient + classifier +
   bindingPrefix` into one `MountBinding` per ecosystem — the S20-flagged
   Config → Server wiring. _(D5)_
5. **Reader over the request path.** Introduce `RequestCtx`, run handlers in
   `ReaderT RequestCtx IO`, retire explicit `Env`+deps threading. _(D6)_

Dispatched as slices on the architect's kickoff, per the standing process.
