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

Threads are worked **one at a time**. **All threads (D1–D6) are resolved, and all of
the conforming code has landed** — see [Resolved](#resolved). The pre-S15 hardening
track is [complete](#code--the-pre-s15-hardening-track), so this epic is closed: the
tarball path ([S15](slices/S15-tarball-path.md)) and M4 now build on the new base.

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
**Code: landed** _(#140 `refactor/per-ecosystem-mounts`; serving wired in
`feat/composition-root-config-creds`)_. The value-level `Ecosystem` sits on the
declarative mount, the `MountMap` is keyed by ecosystem, and `bindingPrefix` is derived
(npm → `/npm`); `MountPrefix` is retired. The composition root now derives and serves
the npm mount from config rather than hard-coding it.

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
**Code: landed** _(#140 `refactor/per-ecosystem-mounts`)_. `RegistryTuple` is now the
record `MountRegistries`, its named roles `regPrivateUpstream` / `regPublicUpstream` /
`regMirrorTarget`.

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
**Code: landed** _(`feat/composition-root-config-creds`)_. Credential providers are built
**once** at the composition root (`Ecluse.Composition.initCredentialProviders`), keyed by
backend; a mount keeps `mtCredential` as the **name** it references, and the boot-time
check (`UnresolvedCredential`) rejects any reference with no initialized provider,
aggregated with the other boot errors. Only the `static` leaf (`MIRROR_TARGET_TOKEN`) is
built in this build; `codeartifact` / `adc` have no leaf yet, so naming one is an honest
boot failure until the cloud-backend slices land.

**D3 — Rule vocabulary.** _(resolved 2026-06-23)_
**Decision:** rules are **ecosystem-agnostic by design** — they reason only over the
agnostic `PackageDetails`; **ecosystem-specific rules are out of scope**. A rule
whose signal is absent for an ecosystem (e.g. a declared scope on a scopeless
ecosystem) **abstains** — under deny-by-default that is the sensible default, not a
per-ecosystem error. No per-ecosystem rule vocabulary, applicability tagging, or
ecosystem-gated validation (the machinery earlier proposed is **dropped**). Rule
**names** track the agnostic concept, normalized **early**.
**Rendered into:** [`rules-engine.md` → Rules Engine](../docs/architecture/rules-engine.md#rules-engine).
**Code: landed** _(refactor/rename-install-rule)_. Renamed `DenyHasInstallScripts`
→ `DenyInstallTimeExecution` — the `Rule` constructor, the wire `type`,
`defaultDenyInstallTimeExecutionPrecedence`, the tests, and the references in
`configuration.md` / `rules-engine.md` / `observability.md` / `STYLE.md` /
`USAGE.md` / research. `AllowScope` kept as the agnostic "namespace" concept the
domain model already carries (abstaining where absent).

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
**Code: landed** _(#140 `refactor/per-ecosystem-mounts` + `feat/composition-root-config-creds`)_.
`Ecluse.Config` keys the `MountMap` by ecosystem, carries the value-level `Ecosystem` on
the mount, derives `bindingPrefix`, and defaults the env-only mount to npm (#140); the
composition root now loads config and resolves each ecosystem to a served `MountBinding`,
failing fast at boot on an unresolved policy or a configured mount with no adapter
(`feat/composition-root-config-creds`).
**As-built reconciliation:** the serve-side endpoints flow on the mount's `PackumentDeps`
/ `MountBinding` (per #133 / #139), **not** a per-mount `RegistryClient` — the serve path
builds an `NpmClientConfig` per leg from the shared `Manager`, so the
"ecosystem → `RegistryClient`" wording predates that refactor. The single global
`envRegistry` is therefore left in place (vestigial on the serve path); resolving a
per-ecosystem publish `RegistryClient` and retiring `envRegistry` belongs with the
**worker slice** — its only consumer, currently a stub.

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
**Code: landed** _(#139 `refactor/reader-request-ctx`)_. `RequestCtx` and the `Handler`
reader over it live in `Ecluse.Server.Context` (deriving `Katip` / `KatipContext`);
dispatch builds the context and the packument handler reads its mount wiring from it
rather than from threaded `Env` + deps arguments.

---

## Code — the pre-S15 hardening track

All six threads are resolved in the docs **and the conforming code has landed**, so the
tarball path ([S15](slices/S15-tarball-path.md)) now builds on the new base. The track,
in the order it was built:

1. ~~**Rule-name normalization (early, standalone).**~~ **Done** — `DenyHasInstallScripts`
   → `DenyInstallTimeExecution` (constructor, wire `type`, precedence helper, tests,
   docs); `AllowScope` kept. _(D3 — `refactor/rename-install-rule`)_
2. ~~**Ecosystem as a value on the mount.**~~ **Done** — `Ecosystem` on the declarative
   mount; `MountMap` / document `mounts` keyed by ecosystem; `bindingPrefix` derived;
   env-only mount defaults to npm; `RegistryTuple` → `MountRegistries`.
   _(D1, D2, D5 — #140 `refactor/per-ecosystem-mounts`)_
3. ~~**Global credential providers.**~~ **Done** — providers built once at the composition
   root, keyed by backend; the mount references one by name (`mtCredential`); the
   boot-time "credential references must resolve" check aggregates with the other boot
   errors. _(D4 — `feat/composition-root-config-creds`)_
4. ~~**Composition-root wiring.**~~ **Done** — `run` loads config and resolves each
   ecosystem to a served `MountBinding` (with real `PackumentDeps`, so packuments are
   merged rather than the `501` stub), failing fast at boot. The serve endpoints flow on
   `PackumentDeps`, not a per-mount `RegistryClient` (see the D5 as-built reconciliation
   above). _(D5 — `feat/composition-root-config-creds`)_
5. ~~**Reader over the request path.**~~ **Done** — `RequestCtx` introduced, handlers run
   in a `Handler` reader over it (`Ecluse.Server.Context`); the packument handler reads
   its mount wiring from context rather than threaded arguments.
   _(D6 — #139 `refactor/reader-request-ctx`)_

Delivered as slices on the architect's kickoffs, per the standing process; with item 4
landed the track — and this epic — is closed.
