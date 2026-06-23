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

**As-built notes (PR #47).**
- **HTTP status modelled as a domain sum type, not `http-types`.** The
  concrete-artifact status is its own total sum `ArtifactStatus = Ok | Forbidden |
  Unavailable' (Maybe RetryAfter) | ServerError | NotFound`, with
  `artifactStatus :: ServeDecision -> ArtifactStatus` and `artifactStatusCode ::
  ArtifactStatus -> Int` (200/403/503/500/404). Modelling the status as a closed
  domain type — rather than a raw `http-types` `Status` — keeps the outcome→status
  mapping exhaustive and lets the WAI layer read off a finite set. (`NotFound`/404 is
  carried for completeness but is not produced by `artifactStatus`: an upstream miss
  is a forwarded status, not a serve *decision*.)
- **`Seconds` realised as a `RetryAfter` newtype.** The architecture sketch's
  `WillResolve (Maybe Seconds)` shipped as `WillResolve (Maybe RetryAfter)`, where
  `newtype RetryAfter = RetryAfter Int` (whole seconds) keeps a raw retry count from
  being confused with any other integer when it becomes the `Retry-After` header.
  (`Seconds` from S02/`Ecluse.Queue` is a queue-visibility unit; the response model
  uses its own newtype rather than borrowing it.)
- **`RuleName` and `HelpMessage` newtypes.** `RejectReason`'s `ByPolicy` carries a
  `newtype RuleName = RuleName Text` (over `Ecluse.Rules.ruleName`), not a bare
  `Text`. The denial body's operator help message is a `newtype HelpMessage` with a
  `mkHelpMessage` smart constructor that trims surrounding whitespace, so a blank or
  all-space `PROXY_HELP_MESSAGE` contributes nothing rather than appending an empty
  span.
- **`Rejection` field names.** The record fields are `rejectionReason` /
  `rejectionMessage` (type-tagged per STYLE.md §6.3), and a rules `Decision` is
  projected to a `ServeDecision` via `serveDecisionOf` reusing `renderDecision` for
  the message. The `Unavailable`/`Transience` arm is present from the start (only
  reachable once S21 lands), exactly as the note above requires.

**Reconciliation (post-merge).** The npm `{"error": …}` body shape encoded here was
**moved out of the agnostic layer by #122 / #133**: `Ecluse.Server.Response` now
decides an error's *status* but holds **no body shape of its own**, and each mount
supplies a `MountRenderer` (returning a `RenderedBody`) — npm's object lives in
`Ecluse.Registry.Npm.Serve`. Rendering is two-tier: a request matching **no mount**
is a neutral `text/plain` 404; every in-mount error renders through that mount's
renderer. See [web-layer.md → Error model](../../docs/architecture/web-layer.md#error-model)
and [rules-engine.md → Denial Responses](../../docs/architecture/rules-engine.md#denial-responses).
