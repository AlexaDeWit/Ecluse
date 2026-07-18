# Agent context management

This guide keeps an agent's limited context on the evidence the current decision needs. Thorough
understanding comes from reliable retrieval, not from loading every source at once.

## Context layers

1. **Always loaded:** `AGENTS.md`, holding only repository-wide invariants and routing.
2. **Task contract:** the request, active issue or slice, acceptance criteria, and file scope.
3. **Decision evidence:** only the architecture, style, testing, or operational sections the current
   decision needs.
4. **Volatile state:** branch, diff, open PRs, test results, decisions, blockers, and next action.

Layers 2-4 are replaceable. Do not copy layer 1 or whole design documents into a checkpoint.

## Startup procedure

1. Read `README.md` and classify the task.
2. Read the task contract. For slice work, that is the one active slice file.
3. Route through the `AGENTS.md` table to select further sources.
4. Inspect live git or GitHub state only when it affects the task.
5. Read targeted sections: heading searches or bounded ranges, not a whole large document.
6. State the objective, applicable invariants, evidence sources, open questions, and next action.

Do not read `AGENTS.md` again in a session: the harness injects it at startup. Do not read
`CONTRIBUTING.md`, all of `STYLE.md`, every architecture document, or every memory file merely
because it exists.

## Phase-specific retrieval

| Phase | Keep in working context |
|---|---|
| Plan | Task contract, relevant architecture sections, unresolved design questions |
| Implement | Acceptance criteria, target modules, applicable style/Haddock sections, current diagnostics |
| Review | Diff, acceptance criteria, affected invariants, focused verification evidence |
| Gate and handoff | Checks run, CI/PR state, known limitations, commit and PR requirements |
| Orchestrate | Active slices, agents/worktrees, decisions, blockers, PR state, next dispatchable work |

Start a fresh thread when the objective or phase changes enough that most accumulated evidence no
longer applies. Do not use a permanent implementation thread as project memory.

## Tool-output hygiene

- Prefer `rg` and targeted semantic navigation over recursive file dumps; bound git logs, issue
  lists, test output, and generated artefacts to the portion needed.
- For a noisy command, write its log to disk and inspect the error summary; a successful full log has
  no decision value. Ask workers for conclusions, evidence locations, and blockers, not transcripts.

## Compaction contract

Compact at a phase boundary, while enough context remains to write an accurate checkpoint. Use
[`.agents/compact-prompt.md`](compact-prompt.md) as the instruction: it is the literal prompt and
defines what the checkpoint keeps (only facts that cannot be cheaply reconstructed) and drops (copied
policy, design prose, successful logs, narration). On resume, verify volatile state live and
re-retrieve referenced sources only when the next decision needs them.

### Per-agent compaction wiring

- **Claude Code:** `/compact` takes inline instructions; direct it to follow
  `.agents/compact-prompt.md`.
- **Gemini CLI:** `/compress` uses a fixed summariser; write the checkpoint per
  `.agents/compact-prompt.md` into the conversation before compressing.
- **Codex CLI:** this repo ships no tracked Codex config. A Codex user can point automatic
  compaction at the prompt with their own `.codex/config.toml` (relative paths resolve from
  `.codex/`):

  ```toml
  tool_output_token_limit = 6000
  experimental_compact_prompt_file = "../.agents/compact-prompt.md"
  ```
