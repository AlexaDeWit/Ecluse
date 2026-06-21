---
id: S04
title: katip logging scaffold (json/console)
milestone: M0 — Shell, seams & foundations
status: not-started
depends-on: [S01]
test-tier: [unit]
arch-refs:
  - docs/architecture/observability.md#logs
  - docs/architecture/technology-stack.md
pr: null
---

# S04 — `katip` logging scaffold (json/console)

> Milestone **M0** · depends on: [S01](S01-app-env-scaffold.md) · tier: unit

**Goal.** Stand up the structured-logging pipeline (`katip`) with a switchable
format — one-line JSONL to stdout (the in-container default) or human-readable
console (dev) — wired into `Env`. This is the base log stream every later slice
attaches context to; trace-ID/`dd`-object correlation is added in M6 (S26).

**Acceptance criteria.**
- [ ] A `katip` `LogEnv`/namespace is created and stored in `Env` (filling the S01
  logger slot). — _technology-stack.md_
- [ ] `PROXY_LOG_FORMAT=json` produces **exactly one** compact JSON object per line
  to stdout (no pretty-print, embedded newlines escaped as `\n`, no prefix outside
  the object); `console` produces the human-readable dev format. — _observability.md#logs_
- [ ] A small structured-context helper so denials/audit events can attach
  `package`/`version`/`rule` fields (the audit-trail use the rules engine feeds).
- [ ] The JSONL scribe is unit-tested table-driven: a record serialises to one line
  with the expected keys and escaped newlines.

**File fence.**
- `src/Ecluse/Log.hs` — scribe construction, format switch, context helpers.
- `src/Ecluse/Env.hs` — fill the logger field (additive).
- `src/Ecluse/Config.hs` — `PROXY_LOG_FORMAT` (additive; coordinate with S03).
- `ecluse.cabal` — add `katip`.
- `test/unit/Ecluse/LogSpec.hs` — JSONL one-line/escaping; format selection.

**Test tier.** Unit — serialise-and-assert on the scribe output; no real stdout dependency.

**Notes / risks.** Do **not** add the `dd` object or trace-ID injection here — that
is S26 and depends on the OTel substrate (S24). Keep this scribe additive so S26
layers correlation on without rework. Secrets must never reach a log field (assert
nothing logs a token).
