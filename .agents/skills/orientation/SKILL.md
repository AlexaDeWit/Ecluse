---
name: orientation
description: >-
  Bootstrap a fresh agent on the Écluse project: orient by reading the
  design-of-record and process canon, syncing ground truth from git/gh and agent
  memory, and forming a picture before acting. A map to where knowledge lives, not a
  copy of it, invoke it first in a fresh session.
---

# Orientation

You are picking up work on **Écluse**, a Haskell npm-registry resilience proxy
(package `ecluse`, modules `Ecluse.*`). This routine takes you from a cold start to
working context.

It is deliberately a **map to where knowledge lives, not a copy of it.** The durable
state of this project lives in **files**, the repo docs, the planning DAG, and agent
memory, and those are authoritative over any conversation or summary. Read the
sources; don't trust a paraphrase (including this one, if the map and a file
disagree, the file wins, and fix the map).

Two facts to hold before you start:

- **The design docs describe the _target_ (the design of record), not necessarily the
  current code.** Implementation tracks toward them slice by slice; `docs/architecture.md`
  says exactly this in its header. Read them for *intent*, then cross-check **git** and
  the **planning DAG** for what has actually shipped.
- **The governing rule is _escalate, don't guess._** Facing ambiguous, missing, or
  contradictory spec, stop and surface it rather than inventing a way past it
  (`planning/orchestration-strategy.md`).

## 1. Read the canon, for your role, not all of it

Start at the two entry points, then follow them to what your task touches:

- **`AGENTS.md`**, the agent entry point: documentation policy, the multi-agent
  coordination model, project structure, code conventions (incl. **Canadian
  spelling**), build & tooling, and CI/security. **Read this first.**
- **`CONTRIBUTING.md`**, the process index: setup, testing, style, design, and the
  commit rules (**Conventional Commits**, **GPG-signed**, **DCO `Signed-off-by`**, and
  the `Assisted-by:` AI-disclosure trailer).

**What Écluse is & why**, `README.md`; then `docs/architecture.md` (the index: vision,
request lifecycle, and a document map) into the per-concern docs under
`docs/architecture/` (registry model, web layer, rules engine, domain model,
access/credential model, cloud backends, hosting, configuration, security,
observability, API surface, technology stack, release supply chain, diagrams). The
domain model is synthesised from the protocol studies in `docs/research/`. Operators
read `USAGE.md`.

**How it gets built**, `planning/`:

- `planning/orchestration-strategy.md`, roles, the per-PR build → evaluate → gate →
  handoff loop, draft-until-ready, the Definition of Done, the guardrails.
- `planning/delivery-plan.md`, the slice DAG index (milestones, current state).
- `planning/slices/SNN-*.md`, **one file per slice**, the authoritative goal,
  acceptance criteria, file scope, and **live `status:`**. Read your slice's file
  before touching its code.
- `docs/architecture/`, the design-of-record and the "why" behind the current
  shape (where resolved design decisions land).

**Before writing code or prose**, `STYLE.md` (Haskell style, module organisation,
flags) and `HADDOCK.md` (what/how to document; §11: no slice/PR/roadmap narration in
source Haddock). Build/test/run lives in `docs/getting-started.md` and
`docs/testing.md`: everything goes through **`make`** from the Nix dev shell, run
`make help` rather than recalling commands, and use the dev-shell tools (`hoogle`,
HLS, the MCP bridge) per AGENTS.md → Build & Tooling.

## 2. Sync ground truth (trust this over any summary)

Status, milestones, and "what shipped" **drift**; derive them live, never from a doc:

- `git checkout main && git pull --ff-only`
- `git log --oneline -20` · `git worktree list`
- `gh pr list --state open --json number,title,headRefName,isDraft` · `gh issue list --state open`

The real per-slice status is each slice file's `status:` frontmatter reconciled against
the git log, not the delivery-plan prose, which is a single-writer summary that can
lag.

## 3. Read agent memory

Read **`MEMORY.md`** (the index) and the memory files it points to, durable facts
about the user, prior feedback, and project context that outlive a session. Each
reflects what was *true when written*: if one names a file, flag, or rule, verify it
still exists in the synced tree before relying on it.

## 4. Form a picture, then proceed by role

Summarise what you now know, what's merged vs in flight, and where your task fits,and only then act:

- **Implementing a slice** → follow its slice file's scope and acceptance criteria;
  TDD; keep changes in scope; reproduce the gate before handoff (orchestration-strategy
  → the per-PR loop and Definition of Done).
- **Leading the multi-agent build (team lead)** → use **`/resume-orchestration`**
  instead, the role-specific resume (the orchestration loop, fix routing, and the
  `/compact` template). This skill is the general front door; that one is the team-lead
  seat. Other project skills live in `.agents/skills/`.
- **Anything large, ambiguous, or outward-facing** → surface a short plan or a
  decision-ready question first. *Escalate, don't guess.*

This routine is read-only: it builds context and hands back. It changes nothing.
