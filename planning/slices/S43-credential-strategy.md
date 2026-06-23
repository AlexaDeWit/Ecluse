---
id: S43
title: Credential strategy & edge authentication
milestone: M3 — Request pipeline (walking skeleton)
status: not-started
depends-on: [S03, S12, S14]
test-tier: [unit]
arch-refs:
  - docs/architecture/access-model.md
  - docs/architecture/access-model.md#credential-strategies-per-mount
  - docs/architecture/access-model.md#edge-authentication
  - docs/architecture/access-model.md#safe-defaults-and-unrepresentable-unsafe-combinations
pr: null
---

# S43 — Credential strategy & edge authentication

> Milestone **M3** · depends on: [S03](S03-config-loader.md), [S12](S12-wai-app-middleware.md), [S14](S14-packument-path.md) · tier: unit
>
> _Access-model enhancement; **off the launch critical path** — the `passthrough`
> default is what S14/S15 already ship. This slice adds the framework and the
> non-default strategies' selection + edge-auth, ahead of S44/S45 wiring them._

**Goal.** Introduce the per-mount **credential strategy** as a first-class config
choice — `passthrough` (default) | `service` | `delegated-cache` — and the **edge
authentication** modes that feed it (`open` | `static` | `trusted-edge`). Make the
safe default the floor and the **unsafe combination unrepresentable**. No upstream
read credential is wired yet (that is S44); this slice is the model, the config, the
validation, and the edge-identity extraction.

**Acceptance criteria.**
- [ ] A per-mount `credentialStrategy` (default `passthrough`) decodes in the mount
  map (extending S03's per-endpoint provider model), with strict validation. —
  _access-model.md#credential-strategies-per-mount, configuration.md#client-authentication_
- [ ] Edge authentication modes parse and validate: `open`, `static`
  (`PROXY_AUTH_TOKEN`), and `trusted-edge` (a configured, signed identity header /
  asserted principal). `trusted-edge` records the **reachable-only-via-edge**
  precondition in docs. — _access-model.md#edge-authentication_
- [ ] **Unrepresentable unsafe combination**: a shared private-leg cache entry is
  reachable **only** under `service`/`delegated-cache`; the types/config make
  "`passthrough` + shared private cache" impossible to express, not merely rejected
  at runtime. — _access-model.md#safe-defaults-and-unrepresentable-unsafe-combinations_
- [ ] `service` requires an explicit "the edge authorizes callers" acknowledgement in
  config; omitting it is a fail-fast startup error. — _access-model.md#safe-defaults-and-unrepresentable-unsafe-combinations_
- [ ] The caller's edge identity (when present) is available to the request pipeline
  for audit/logging without becoming a metric label (bounded-cardinality rule). —
  _observability.md#cardinality-and-attributes_
- [ ] Unit tests: strategy/edge-mode decode (valid/invalid/missing), the
  fail-fast `service`-without-assertion case, and a type-level/​property check that the
  unsafe cache combination cannot be constructed.

**File scope.**
- `src/Ecluse/Config.hs` — `credentialStrategy` + edge-auth fields (additive to S03).
- `src/Ecluse/Access.hs` — the strategy/edge model + the cache-admission witness that
  makes the unsafe combination unrepresentable.
- `test/unit/Ecluse/AccessSpec.hs` — decode, validation, and the unrepresentable-combo tests.
- `docs/architecture/access-model.md`, `docs/architecture/configuration.md`, `USAGE.md` — keep in sync.

**Test tier.** Unit — config decode/validation and the cache-admission witness are pure.

**Notes / risks.** This formalises what `passthrough` (S14) already does as the
default and opens the door for S44 (`service` read path) and S45 (`delegated-cache`
probe). Keep `passthrough` behaviour **byte-identical** to today — this slice must
not change the default path, only add the selection and guard. The
"reachable-only-via-edge" precondition for `trusted-edge` is a deployment invariant
Écluse cannot enforce alone; document it loudly and **escalate** if a code-level
enforcement is expected.
