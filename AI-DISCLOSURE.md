# Built with AI — and how to verify it

> **In short.** Most of Écluse's *implementation* has been written with heavy AI
> assistance during this bootstrapping phase, under a documented review process and a
> hard CI gate. The *architecture* — the three-registry origin model, the deny-by-default
> invariants, the security posture — is the product of months of deliberate human design.
> Nothing will be released until I have audited the codebase myself. And you don't have to
> take any of that on faith: the build is reproducible and attested, and the
> safety-critical behaviour is encoded in types and pinned by tests. **Verify it, don't
> trust it.** This is shared *pre-launch, on purpose* — to invite scrutiny of the design
> while it is still cheap to change.

Écluse is a supply-chain *security* tool, and it has been built with heavy AI assistance.
Those two facts together should make a careful person pause — so here is exactly how it is
built, what is and isn't mine, and how you can check the result rather than trust the
process.

## What is human, and what is AI

The line matters, so I will be precise about it.

- **Human-owned — the design.** The architecture, the three-registry origin model, the
  rules engine's deny-by-default posture, the security invariants (outbound-egress / SSRF,
  identifier canonicalisation, response bounds), and the planning are mine, and
  deliberately so. They are the product of months of on-and-off thought — the dead ends
  in [`MOTIVATION.md`](MOTIVATION.md) are real — about how a small team gets this
  protection without enterprise licences or running its own registry. I am an experienced
  engineer with years of professional work in typed functional programming (fp-ts, Scala,
  category-theory-patterned ecosystems); the paradigm Haskell lives in is long familiar to
  me. I own, and can explain, the design and the safety-critical paths.
- **AI-assisted — the implementation.** The Haskell itself — turning that design into
  code, and the specific libraries and idioms I am refreshing along the way — is written
  largely with an LLM, at this stage, for velocity. I review every PR for smells; I am not,
  *yet*, doing an exhaustive line-by-line review of all of it.

## How that is kept honest

The implementation runs through a process built specifically so that "the AI wrote it"
does not get to mean "nobody checked it":

- **A human is the final authority.** I act as the principal architect: I own the
  requirements and review and merge *every* PR. The agent decomposes and implements but
  **never merges and never pushes to `main`**
  ([`planning/orchestration-strategy.md`](planning/orchestration-strategy.md)).
- **"Escalate, don't guess."** The governing rule: an agent facing anything ambiguous,
  missing, or contradictory **stops and surfaces it** rather than inventing a way past it.
  No fabricated config or API behaviour, no silently-weakened tests, no leftover
  `undefined` / stub passed off as done — and review scans for exactly that.
- **Independent review passes.** Each slice is evaluated by reviewer agents with fresh
  context (requirements traceability, then security and code quality), on top of my own
  pass.
- **Correctness by construction.** Haskell totality, `-Werror`, parse-don't-validate, and
  invariants encoded in *types* (deny-by-default; a root mount is unrepresentable) kill a
  whole class of plausible-but-wrong AI output before it is even reviewed.
- **A gate that does not accept "it works" as evidence.** Every acceptance criterion is
  backed by a deterministic test; the rules engine's deny-precedence is **property-tested**
  (Hedgehog); changed lines must clear 95% coverage; Semgrep, lint, and a hermetic Nix
  build all gate. See [`docs/testing.md`](docs/testing.md).

## Before anything is released

**Nothing here will be cut as a release until I have thoroughly audited the codebase
myself.** Écluse is **pre-launch by design** — not yet something to put in front of a
build, and you are not being asked to trust an AI-written security tool in production. The
"understand and be able to explain every line" bar that
[`CONTRIBUTING.md`](CONTRIBUTING.md) holds contributors to is the **release** bar; that
pre-release audit is the mechanism by which the codebase reaches it before anyone relies on
it.

## You do not have to trust this — verify it

This is the part that actually matters for a security tool. You do not have to trust me,
the process, or the model:

- The image is **bit-for-bit reproducible** — rebuild it from pinned source and diff it
  against what is published.
- Every release carries **keyless SLSA provenance and an SBOM**, recorded in a public
  transparency log.
- The code is **small, typed, and tested**, and the security invariants are explicit and
  property-checked — read the deny-by-default property; read the egress guards.

Verification beats trust, and here it is on offer. (See
[Release & Supply-Chain Operations](docs/architecture/release-supply-chain.md).)

## Why I am telling you this now

I am sharing Écluse *pre-launch, deliberately* — not to ask for adoption, but to ask for
**scrutiny of the design while it is still cheap to change.** I do not yet have a community
or a budget for an independent security review; part of why this is public is to start
attracting exactly that kind of attention.

So: **please try to break the design.** The origin model, the cross-upstream merge, the
deny-by-default rules, the egress story — if something is wrong, I would far rather hear it
now. Start with [`MOTIVATION.md`](MOTIVATION.md) and the
[architecture](docs/architecture.md), and tell me where it falls down.

## The shape of this going forward

This heavy reliance on AI is a property of the **bootstrapping phase**, not a permanent
engineering philosophy; I expect it to taper as the project — and my own line-by-line
familiarity with the code — matures. The conviction behind the design is not going
anywhere.
