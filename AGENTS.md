# Agent Instructions

This file is the small, always-loaded constitution for agents working on Écluse. The repository is
durable memory: load detailed guidance only when the task needs it. See
[`.agents/context-management.md`](.agents/context-management.md) for the context-routing procedure.

## Start here

- **Always read [`README.md`](README.md) first**. It describes the current architecture, design
  decisions, and module responsibilities.
- Identify the task type and its authoritative source before reading further. Do not preload the
  whole process and design canon.
- The design documents describe the target. Git, the implementation, and the per-slice status
  describe what has shipped. Reconcile the two. Neither is interchangeable with the other.
- **Escalate, don't guess**. Stop on an ambiguous, missing, or contradictory requirement instead of
  inventing a way through it.
- **Review the plan before you build**. For any non-trivial change, put the approach (strategy,
  files, notable trade-offs) to the repo owner and get agreement before you write code. A trivial or
  already-scoped change is exempt. When you are unsure, surface the plan and wait.

| Work | Read next |
|---|---|
| Implement or change Haskell | Active slice or issue, relevant architecture section, [`docs/style.md`](docs/style.md), then applicable sections of [`docs/haddock.md`](docs/haddock.md) |
| Change architecture or module boundaries | [`docs/architecture.md`](docs/architecture.md) and only the linked concern documents affected |
| Change operator behaviour or configuration | [`USAGE.md`](USAGE.md), [`config/default.yaml`](config/default.yaml), and the relevant architecture document |
| Add or change tests | Applicable sections of [`docs/testing.md`](docs/testing.md) |
| Build, debug, or navigate Haskell | Applicable sections of [`docs/getting-started.md`](docs/getting-started.md) |
| Change CI, releases, supply chain, or security tooling | [`CONTRIBUTING.md`](CONTRIBUTING.md) and the relevant testing or release-supply-chain sections |
| Coordinate implementation slices | the `orchestrate-implementation` skill and [`.agents/orchestration-strategy.md`](.agents/orchestration-strategy.md) |
| Commit or open a PR | [`CONTRIBUTING.md`](CONTRIBUTING.md), the PR template, and the `open-pull-request` skill |

## Documentation policy

- Read architecture documents before structural changes, and update documentation in the same PR
  whenever behaviour, architecture, public interfaces, or configuration changes.
- One fact, one home. [`USAGE.md`](USAGE.md) is the operator manual and
  [`config/default.yaml`](config/default.yaml) is the configuration reference: every key, its
  default, and its meaning live there as comments. `docs/architecture/` owns the *why*. A section
  there earns its place only by answering an operator or adoption question. Contributors read the
  code and the Haddock, so never re-home cut prose in either.
- Update the architecture section of `README.md` when adding a module or significantly changing a
  module's responsibility.

## Implementation coordination

The repo owner is the **principal architect** and owns the design and the requirements. The team
lead coordinates PR-sized work, independent evaluation, and hand-off. Roles, worktree isolation,
dispatch, and the hand-off gate are in
[`.agents/orchestration-strategy.md`](.agents/orchestration-strategy.md). Dispatch implementation
only after explicit architect kickoff. The team lead never merges or pushes to `main`.

## Project structure and code conventions

- The layout is in [`README.md`, Project structure](README.md#project-structure). Tests mirror
  library paths (`src/Foo/Bar.hs` -> `test/Foo/BarSpec.hs`).
- Follow [`docs/style.md`](docs/style.md) for Haskell and [`docs/haddock.md`](docs/haddock.md) for
  Haddock. Organise vertically by capability. Keep effects at the application edge.
- Write prose, comments, commits, and PRs in Canadian English (`behaviour`, `colour`, `licence` as a
  noun, `-ise` endings). Do not rewrite a human contributor's spelling.
- **Workspace hygiene:** keep temporary scratch files in `scratchpad/`, never loose in the tree.
  Review staged files before committing, rather than running `git add -A`.

## Build and tooling

- Nix with flakes is mandatory in every environment, hosted agents included. There is no Nix-less
  path ([README, Development](README.md#development)). Run work through `task` inside the flake.
  When the ambient shell may be stale, invoke it as:

  ```bash
  env -u IN_NIX_SHELL nix develop --command task <target>
  ```

- `task --list` discovers targets. What `task check` and `task gate` run, what only CI runs, and
  how the container-backed suites are scoped and cleaned up are in
  [`docs/testing.md`](docs/testing.md). Never ignore a failing exit code. CI is the authoritative
  gate.
- One worktree per agent: `task new-worktree BRANCH=<branch>` to create, `task rm-worktree
  BRANCH=<branch>` to retire. The lifecycle detail lives in
  [`.agents/orchestration-strategy.md`](.agents/orchestration-strategy.md).
- Automation scripts are Bash in `scripts/`
  ([CONTRIBUTING, Automation scripting](CONTRIBUTING.md#automation-scripting)).
- Use `hoogle`, HLS, `cabal-plan`, and `ghcid` to discover types and behaviour instead of guessing.
  Start the HLS MCP bridge with the worktree root before semantic requests.

## CI, security, and repository gates

- The CI workflow and tier semantics live in [`docs/testing.md`](docs/testing.md). The terminal
  `gate` job is the branch-protection authority.
- The repository rules (SHA-pinned Actions, injection-free workflows, no Semgrep or Stan ignores
  without repo-owner approval, SPDX headers, Mermaid diagrams) are in
  [CONTRIBUTING, Repository requirements](CONTRIBUTING.md#repository-requirements).
- Keep the threat model in `threat-modelling/ecluse.json`. Do not create a competing prose risk
  register.
- The version authority is `ecluse.cabal`'s `version:` field ([`VERSIONING.md`](VERSIONING.md)).
- Every commit is Conventional-Commit formatted, GPG-signed, DCO-signed off as the human author, and
  discloses non-trivial AI help with `Assisted-by:` (not `Co-Authored-By:`). The `open-pull-request`
  skill is the recipe.

## Skills

- Reusable procedures live in [`.agents/skills/`](.agents/skills/), one directory per skill with a
  `SKILL.md`, following the [Agent Skills](https://agentskills.io/specification) standard. Codex and
  GitHub Copilot discover this location natively.
- Claude Code discovers project skills only from `.claude/skills/`, so a tracked relative symlink
  bridges each skill there. Update the matching symlink in the same commit as any skill add,
  rename, or removal.
- `CLAUDE.md` (an `@AGENTS.md` import) exists only to point that harness at this file. Keep shared
  guidance here, never in per-agent files.

## Context discipline

- Keep stable rules in files and volatile decisions in the current task or compaction summary. The
  context layers, the per-phase retrieval table, and the compaction contract are in
  [`.agents/context-management.md`](.agents/context-management.md).
- Use `orientation` for a cold task session and `resume-orchestration` for the team-lead seat after
  compaction or restart, not both.
