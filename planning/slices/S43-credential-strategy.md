---
id: S43
title: Credential strategy & edge authentication
milestone: M3, Request pipeline (walking skeleton)
status: not-started
depends-on: [S03, S12, S14]
test-tier: [unit]
arch-refs:
  - docs/architecture/access-model.md
  - docs/architecture/access-model.md#credential-strategies-per-mount
  - docs/architecture/access-model.md#edge-authentication
  - docs/architecture/access-model.md#safe-defaults-and-unrepresentable-unsafe-combinations
  - docs/architecture/security.md#trust-assumptions--credential-posture
pr: null
---

# S43, Credential strategy & edge authentication

> Milestone **M3** · depends on: [S03](S03-config-loader.md), [S12](S12-wai-app-middleware.md), [S14](S14-packument-path.md) · tier: unit
>
> _Access-model enhancement; **off the launch critical path**, the `passthrough`
> default is what S14/S15 already ship. This slice adds the framework and the
> non-default `service` strategy's selection + edge-auth, ahead of S44 wiring it._

**Goal.** Introduce the per-mount **credential strategy** as a first-class config
choice, `passthrough` (default) | `service`, and the **edge
authentication** modes that feed it (`open` | `static` | `trusted-edge`). Make the
safe default the floor and the **unsafe combinations unrepresentable**, including
`trusted-edge`, which Écluse accepts **only over a verifiable binding to the edge**
(mutual TLS, or a shared secret / HMAC on the assertion) and otherwise rejects
fail-fast, since a bare trusted header is forgeable into granted access. No upstream
read credential is wired yet (that is S44); this slice is the model, the config, the
validation, and the verified edge-identity extraction.

**Acceptance criteria.**
- [ ] A per-mount `credentialStrategy` (default `passthrough`) decodes in the mount
  map (alongside the mount's reference to a **process-global** credential provider,  base-hardening D4), with strict validation.,  _access-model.md#credential-strategies-per-mount, configuration.md#client-authentication_
- [ ] Edge authentication modes parse and validate: `open`, `static`
  (`PROXY_AUTH_TOKEN`), and `trusted-edge` (a configured, signed identity header /
  asserted principal **plus its verifiable binding**, a configured mutual-TLS peer
  identity, or a shared secret / HMAC key the edge signs the assertion with). The
  **reachable-only-via-edge** network precondition is recorded in docs as the
  deployment's part., _access-model.md#edge-authentication, security.md#trust-assumptions--credential-posture_
- [ ] **`trusted-edge` without a verifiable binding is unrepresentable / fails fast.**
  A `trusted-edge` mount configured with neither an mTLS-peer identity nor a
  shared-secret/HMAC key is **rejected at startup**, and at request time an assertion
  lacking a valid binding is **not honoured** (treated as unauthenticated). A bare
  trusted header is forgeable into granted access wherever Écluse is reachable other
  than through the edge, so this is closed in code, not left to a deployment hope.,  _access-model.md#safe-defaults-and-unrepresentable-unsafe-combinations, security.md#trust-assumptions--credential-posture_
- [ ] **Unrepresentable unsafe combination**: **no** strategy admits a shared
  private-leg cache entry; the types/config make a shared private cache impossible to
  express under **any** strategy (`passthrough` or `service`), not merely rejected at
  runtime, Écluse forbids a private cache outright.,  _access-model.md#safe-defaults-and-unrepresentable-unsafe-combinations_
- [ ] `service` requires an explicit "the edge authorises callers" acknowledgement in
  config; omitting it is a fail-fast startup error., _access-model.md#safe-defaults-and-unrepresentable-unsafe-combinations_
- [ ] The caller's edge identity (when present) is available to the request pipeline
  for audit/logging without becoming a metric label (bounded-cardinality rule).,  _observability.md#cardinality-and-attributes_
- [ ] Unit tests: strategy/edge-mode decode (valid/invalid/missing), the
  fail-fast `service`-without-assertion case, and a type-level/​property check that the
  unsafe cache combination cannot be constructed.

**File scope.**
- `src/Ecluse/Config.hs`, `credentialStrategy` + edge-auth fields, incl. the
  `trusted-edge` binding config (mTLS-peer identity / shared-secret reference) (additive to S03).
- `src/Ecluse/Access.hs`, the strategy/edge model + the cache-admission witness that
  makes the unsafe combination unrepresentable, **and the `trusted-edge`-needs-a-binding
  witness** (a bound-assertion type the request path consumes; an unbound `trusted-edge`
  config cannot be constructed).
- the inbound **edge-assertion verification** at the request boundary (the WAI
  app/middleware from [S12](S12-wai-app-middleware.md)), honour an assertion only when
  its binding (mTLS peer / HMAC) checks out.
- `test/unit/Ecluse/AccessSpec.hs`, decode, validation, the unrepresentable-combo tests,
  **and the `trusted-edge`-without-binding fail-fast + unbound-assertion-not-honoured tests**.
- `docs/architecture/access-model.md`, `docs/architecture/configuration.md`,
  `docs/architecture/security.md`, `USAGE.md`, keep in sync.

**Test tier.** Unit, config decode/validation and the cache-admission witness are pure.

**Notes / risks.** This formalises what `passthrough` (S14) already does as the
default and opens the door for S44 (`service` read path). (S45/`delegated-cache` is
**superseded**, Écluse forbids a shared private cache.) Keep `passthrough` behaviour
**byte-identical** to today, this slice must not change the default path, only add the
selection and guard. The
"reachable-only-via-edge" network topology for `trusted-edge` remains a deployment
invariant Écluse cannot enforce alone, but the **escalation it once flagged is
resolved (DECIDED 2026-06-27): code-level enforcement _is_ expected.** Écluse must not
honour a `trusted-edge` assertion that is not cryptographically bound to the edge
(mutual TLS, or a shared secret / HMAC), and must reject such a mount fail-fast at
startup, the same "make the unsafe combination unrepresentable" discipline this slice
applies to the cache. This closes the forgeable-bare-header footgun; the security
rationale lives in _security.md#trust-assumptions--credential-posture_.
