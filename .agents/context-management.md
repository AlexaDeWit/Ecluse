# Agent Context Management

This guide keeps an agent's limited context focused on evidence needed for the current decision.
Thorough project understanding comes from reliable retrieval from repository sources, not from
loading every source at once.

## Context layers

1. **Always loaded:** `AGENTS.md`, containing only repository-wide invariants and routing.
2. **Task contract:** the user's request, active issue or slice, acceptance criteria, and file
   scope.
3. **Decision evidence:** only the architecture, style, testing, or operational sections needed
   for the current decision.
4. **Volatile state:** branch, diff, open PRs, test results, decisions, blockers, and next action.

Layers 2-4 are replaceable. Do not copy layer 1 or whole design documents into checkpoints.

## Startup procedure

1. Read `README.md` and classify the task.
2. Read the task contract. For slice work, this is the one active slice file.
3. Use the routing table in `AGENTS.md` to select additional sources.
4. Inspect live git or GitHub state only when it affects the task.
5. Read targeted sections. Use heading searches or bounded ranges rather than emitting an entire
   large document into the transcript.
6. State the objective, applicable invariants, evidence sources, open questions, and next action.

Do not read `AGENTS.md` again in a session: the harness injects it at startup. Do not read
`CONTRIBUTING.md`, all of `STYLE.md`, all architecture documents, or all memory files merely
because they exist.

## Phase-specific retrieval

| Phase | Keep in working context |
|---|---|
| Plan | Task contract, relevant architecture sections, unresolved design questions |
| Implement | Acceptance criteria, target modules, applicable style/Haddock sections, current diagnostics |
| Review | Diff, acceptance criteria, affected invariants, focused verification evidence |
| Gate and handoff | Checks run, CI/PR state, known limitations, commit and PR requirements |
| Orchestrate | Active slices, agents/worktrees, decisions, blockers, PR state, next dispatchable work |

Start a fresh thread when the objective or phase changes enough that most accumulated evidence is
no longer relevant. Do not use a permanent implementation thread as project memory.

## Tool-output hygiene

- Prefer `rg` and targeted semantic navigation to recursive file dumps.
- Limit git logs, issue lists, test output, and generated artefacts to the portion needed.
- For a noisy command, write its normal artefact or log on disk, then inspect the error summary and
  nearby lines. A successful full log has no decision value.
- Ask workers to return conclusions, evidence locations, and blockers rather than raw exploration
  transcripts.

## Compaction contract

Compact at a phase boundary or while enough context remains to produce an accurate checkpoint.
Use [`.agents/compact-prompt.md`](compact-prompt.md) as the compaction instruction.

The checkpoint preserves only facts that cannot be cheaply reconstructed:

- objective and acceptance criteria;
- active branch, worktree, PR, and phase;
- decisions and rationale;
- files changed;
- verification run and failures still relevant;
- blockers, open questions, and exact next action;
- precise source paths or headings needed next.

It omits copied repository policy, copied design prose, successful logs, abandoned hypotheses, and
chronological narration. On resume, verify volatile state live and retrieve referenced sources only
when the next decision needs them.

### Per-agent compaction wiring

- **Claude Code:** `/compact` accepts inline instructions; direct it to follow
  `.agents/compact-prompt.md`.
- **Gemini CLI:** `/compress` uses a fixed summariser; write the checkpoint per
  `.agents/compact-prompt.md` into the conversation before compressing.
- **Codex CLI:** a trusted project may point automatic compaction at the prompt from
  `.codex/config.toml` (relative paths resolve from `.codex/`):

  ```toml
  tool_output_token_limit = 6000
  experimental_compact_prompt_file = "../.agents/compact-prompt.md"

  [tui]
  status_line = ["model-with-reasoning", "context-remaining", "git-branch", "current-dir"]
  ```
