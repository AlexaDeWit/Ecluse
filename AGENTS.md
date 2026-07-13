# Agent Instructions

This file is the small, always-loaded constitution for agents working on Écluse. The
repository is durable memory: load detailed guidance only when the current task needs it.
See [`.agents/context-management.md`](.agents/context-management.md) for the context-routing
procedure.

## Start here

- **Always read [`README.md`](README.md) before starting a task.** It describes the current
  architecture, design decisions, and module responsibilities.
- Identify the task type and authoritative source before reading further. Do not preload the
  entire process and design canon.
- The design documents describe the target; git, the implementation, and per-slice status
  describe what has shipped. Reconcile them rather than treating either as interchangeable.
- **Escalate, don't guess.** Stop on ambiguous, missing, or contradictory requirements rather
  than inventing a way through them.
- **Review the plan before you build.** For any non-trivial change, present the proposed approach
  (the strategy, the files you intend to touch, and the notable trade-offs or alternatives) to the
  repo owner and get their agreement *before* writing the implementation. Do not run ahead and hand
  back a finished result whose strategy the owner never saw. A trivial or already-scoped change is
  exempt; when unsure whether it qualifies, surface the plan and wait.

| Work | Read next |
|---|---|
| Implement or change Haskell | Active slice or issue, relevant architecture section, [`STYLE.md`](STYLE.md), then applicable sections of [`HADDOCK.md`](HADDOCK.md) |
| Change architecture or module boundaries | [`docs/architecture.md`](docs/architecture.md) and only the linked concern documents affected |
| Change operator behaviour or configuration | [`USAGE.md`](USAGE.md) and the relevant architecture document |
| Add or change tests | Applicable sections of [`docs/testing.md`](docs/testing.md) |
| Build, debug, or navigate Haskell | Applicable sections of [`docs/getting-started.md`](docs/getting-started.md) |
| Change CI, releases, supply chain, or security tooling | [`CONTRIBUTING.md`](CONTRIBUTING.md) and the relevant testing or release-supply-chain sections |
| Coordinate implementation slices | [`planning/orchestration-strategy.md`](planning/orchestration-strategy.md) |
| Commit or open a PR | [`CONTRIBUTING.md`](CONTRIBUTING.md), the PR template, and the `open-pull-request` skill |
| Run in a hosted/web execution environment | [`.agents/remote-execution.md`](.agents/remote-execution.md) |

## Documentation policy

- Read architecture documents before structural changes.
- Update documentation in the same PR whenever behaviour, architecture, public interfaces, or
  configuration changes. Do not defer it.
- [`USAGE.md`](USAGE.md) is the operator manual. Update it for anything an operator configures or
  must do to run Écluse safely: environment variables, config schema, egress, client auth, and
  health or observability endpoints.
- `docs/architecture/` owns the *why*; `USAGE.md` owns the *how*. Keep them aligned.
- Update the architecture section of `README.md` when adding a module or significantly changing
  a module's responsibility.

## Implementation coordination

The repo owner is the **principal architect** and owns design and requirements. The lead agent
coordinates PR-sized work, independent evaluation, and handoff. The full workflow lives in
[`planning/orchestration-strategy.md`](planning/orchestration-strategy.md).


- Use one isolated worktree per implementation agent. Never let concurrent agents edit the same
  checkout.
- Dispatch implementation only after explicit architect kickoff. The lead agent never merges or
  pushes to `main`.

## Project structure and code conventions

```text
core/     ecluse-core: pure, ecosystem-agnostic capability core (Ecluse.Core.*)
runtime/  ecluse-runtime: effectful edge — OTel SDK, warp, scribes, cloud adapters (Ecluse.Runtime.*)
src/      ecluse: composition shell that assembles and runs the tiers (Ecluse.*)
app/      executable entry point (keep Main.hs thin)
test/     unit and integration tests mirroring the library hierarchy
docs/     architecture and design documents
```

- Follow [`STYLE.md`](STYLE.md) for Haskell and [`HADDOCK.md`](HADDOCK.md) for documentation.
- Organise vertically by capability. Keep effects at the application edge and avoid generic
  `.Types` or `.Helpers` modules unless the split is earned.
- Tests mirror library paths (`src/Foo/Bar.hs` -> `test/Foo/BarSpec.hs`).
- Write prose, comments, commits, and PRs in Canadian English, including `behaviour`, `colour`,
  `licence` as a noun, and `-ise` endings. Do not rewrite a human contributor's spelling.
- Repository diagrams are Mermaid, never ASCII art.
- **Agent Workspace Hygiene:** Do not litter the repository with temporary scratch scripts (e.g., `inspect.hs`, `test-ministack.sh`) or blindly run `git add -A`. Always place temporary working files in `scratchpad/` or `ai-notepad/`, and review your staged files carefully before committing.

## Build and tooling

- Nix with flakes is mandatory. Use the pinned dev shell; system GHC/Cabal is unsupported.
- Run work through `task` inside `nix develop`. When the ambient shell may be stale, use:

  ```bash
  env -u IN_NIX_SHELL nix develop --command task <target>
  ```

- Run `task --list` to discover targets. `task check` is the pre-push suite: a subset of the full
  `task gate`, but not a quick check (a full `-Werror` build of every component that runs 10+ minutes
  on a cold checkout or under CPU contention). CI remains the authoritative gate. **Always run `task format` before `task check` to auto-fix code styling.** Never ignore a failing `task check` exit code; fix the issue before opening a PR. **Never assume a fix worked without re-running the verification command locally and observing a 0 exit code.** Run `task sast` before pushing. Web-based agents without Nix access must
  not skip local verification; instead, use `scripts/setup-jules.sh` to bootstrap the environment.
- The integration and e2e suites (`task test-integration`, `task test-e2e`) start Docker
  containers. Those targets reap **this worktree's own** containers before and after each run, but
  a hard kill (SIGKILL, an OOM, a timed-out command) can still leave containers behind, and they
  pile up fast across repeated runs. If you interrupt or kill a suite, run **`task test-clean`**
  (scoped to your worktree by the `com.ecluse.test.scope` label, so it is safe to run while other
  agents or worktrees have suites running) and confirm none are left with
  `docker ps --filter label=com.ecluse.test`. `task test-clean-all` removes *every* worktree's
  test containers at once, so only reach for it when you know no other suite is running.
- Automation scripts are Bash in `scripts/`, with `#!/usr/bin/env bash` and
  `set -euo pipefail`; keep workflow `run:` blocks trivial and scripts shellcheck-clean. A new
  Python or Node build-time dependency needs an explicit justification.
- Use `hoogle`, HLS, `cabal-plan`, and `ghcid` to discover types and behaviour instead of
  guessing. Start the HLS MCP bridge with the worktree root before semantic requests.
- Create agent worktrees with `task new-worktree BRANCH=<branch>` so the local HLS index warms in
  isolation. Keep cold HLS concurrency to two or three worktrees.
- Retire a worktree with **`task rm-worktree BRANCH=<branch>`**, not a bare `git worktree remove`.
  Creation warms a roughly 1 GB HLS index *outside* the checkout (under the hie-bios cache), and
  only `rm-worktree` reclaims both halves; removing the checkout alone strands the gigabyte. The
  branch is kept, so retiring a worktree never discards work. If worktrees have already been
  removed by hand, **`task worktree-clean`** sweeps up after them: it reaps the stranded caches and
  prunes stale registrations. It is scoped to caches this repository positively owns, so it is safe
  to run while other worktrees are building and it never touches another project's cache. Add
  `-- --dry-run` to preview.

## CI, security, and repository gates

- The unified CI workflow and tier semantics are documented in [`docs/testing.md`](docs/testing.md).
  The terminal `gate` job is the branch-protection authority; the weeder and Stan analysis jobs
  gate through it, while smoke, vulnerability scanning, and Codecov's server-side statuses have
  their documented non-gating roles.
- Pin every GitHub Action to a full commit SHA and follow the cache rules in
  [`CONTRIBUTING.md`](CONTRIBUTING.md).
- Do not add `.semgrepignore` entries or `nosemgrep` comments without repo-owner approval.
- Do not add Stan `[[ignore]]` entries without repo-owner approval. Prefer fixing findings.
- Keep the threat model in `threat-modelling/ecluse.json`; do not create a competing prose risk
  register.
- The version authority is `ecluse.cabal`'s `version:` field. Release and supply-chain procedures
  live in [`docs/architecture/release-supply-chain.md`](docs/architecture/release-supply-chain.md).
- Every commit must be Conventional-Commit formatted, GPG-signed, DCO-signed off as the human
  author, and disclose non-trivial AI help with `Assisted-by:` rather than `Co-Authored-By:`.

## Skills

- Reusable procedures live in [`.agents/skills/`](.agents/skills/), one directory per skill with a
  `SKILL.md`, following the [Agent Skills](https://agentskills.io/specification) open standard.
  Codex, Gemini CLI, and GitHub Copilot discover this location natively.
- Claude Code discovers project skills only from `.claude/skills/`, so each skill is bridged there
  by a tracked relative symlink. When adding, renaming, or removing a skill, update the matching
  symlink in the same commit.
- `CLAUDE.md` (an `@AGENTS.md` import) and `.gemini/settings.json` (`context.fileName`) exist only
  to point those harnesses at this file. Keep shared guidance here, never in per-agent files.

## Context discipline

- Keep stable rules in files and volatile decisions in the current task or compaction summary.
- Read precise sections and files required by the task; avoid whole-canon rereads.
- Keep command output bounded. Save large logs as files and inspect the failing portion.
- Use a fresh thread for a bounded implementation or review. Keep the orchestration thread focused
  on requirements, decisions, PR state, and handoff.
- Use `orientation` for a cold task session and `resume-orchestration` for the team-lead seat after
  compaction or restart. Do not run both for the same startup.
