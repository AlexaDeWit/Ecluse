---
id: S11
title: Error model + denial responses
milestone: M2 — Web front door
status: merged
depends-on: [S05, S10]
test-tier: [unit]
arch-refs:
  - docs/architecture/web-layer.md#error-model
  - docs/architecture/rules-engine.md#denial-responses
pr: null
---

# S11 — Error model + denial responses

> Milestone **M2** · depends on: [S05](S05-rules-precedence.md), [S10](S10-router.md) · tier: unit

**Goal.** The serve-outcome type and its rendering to npm-shaped responses, so every
client-facing error maps to the right status and an intuitive body — not a generic
403/500.

**Acceptance criteria.**
- [ ] `ServeDecision = Admit | Reject Rejection`; `Rejection { reason, message }`;
  `RejectReason = ByPolicy RuleName | Unavailable Transience`;
  `Transience = WillResolve (Maybe Seconds) | WontResolve`. — _web-layer.md#error-model_
- [ ] Concrete-artifact mapping: `Admit`→200(stream), `ByPolicy`→403+denial body,
  `Unavailable WillResolve`→503+`Retry-After`, `Unavailable WontResolve`→500,
  upstream miss→404. The rule: **503 only when we believe it will resolve**, else
  500. — _web-layer.md#error-model_
- [ ] Denial body is the npm `{"error": "…"}` shape including which rule decided and
  why; `PROXY_HELP_MESSAGE` (config, S03) appended to every denial. —
  _rules-engine.md#denial-responses_
- [ ] Renders a `Decision` (from S05) into a `Rejection` message via the existing
  `renderDecision`/`renderDuration`.

**File scope.**
- `src/Ecluse/Server/Response.hs` — the types, status mapping, npm error-body encoder, help-message append.
- `ecluse.cabal` — register module (`aeson` already present from S03/S06).
- `test/unit/Ecluse/Server/ResponseSpec.hs` — outcome→status table; denial-body shape; help-message appending.

**Test tier.** Unit — the outcome→status mapping and body shape, table-driven.

**Notes / risks.** This module is referenced by both the serve path (S14/S15) and the
no-survivors packument case (S09→S14). Keep the `Unavailable`/`Transience` arm
present from the start even though it only becomes reachable when S21 (effectful
tier) lands — it is part of the error model now, not a stub. Packument requests have
no single status (S14 chooses 403-vs-503 over the filtered set).
