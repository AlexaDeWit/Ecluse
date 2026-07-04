# Built with AI, and how to verify it

Écluse is a supply-chain *security* tool, and I built much of it leaning on an LLM harder than
I ever have. If that makes you nervous, good. Here's the honest version: what's mine, what the
AI did, and why you don't have to trust either of us to use it.

## What's mine, and what's the AI's

- **Mine: the design.** The architecture, the three-registry origin model, the deny-by-default
  rules engine, the security invariants (outbound egress / SSRF, identifier canonicalisation,
  response bounds), and the planning. That's the result of months of thinking about how a small
  team gets this protection without enterprise licence fees or a registry of its own. I've
  worked in typed functional programming for years, so Haskell is familiar ground. I own the
  design and the safety-critical paths, and I can explain them.
- **The AI's: the implementation.** Writing the Haskell that turns the design into code is
  mostly done with an LLM right now, for speed. I'm not hands-off: I read every PR and go
  through the code line by line, just not as tightly as I will near release, since it's still
  under frequent revision. The release-grade pass comes before release.

## How I keep that honest

- **I decide.** I act as the architect, own the requirements, and review and merge every PR.
  The agent does the work but **never merges and never pushes to `main`**. The process is
  public, in [`planning/orchestration-strategy.md`](planning/orchestration-strategy.md).
- **Escalate, don't guess.** On anything ambiguous, missing, or contradictory, the agent stops
  and asks rather than inventing its way past it. No made-up config or API behaviour, no
  quietly-weakened tests, no `undefined` left in.
- **A second set of eyes.** Each slice is reviewed by separate agents with fresh context: first
  for whether it does what was asked, then for security and code quality.
- **The compiler does a lot of the work.** Totality, `-Werror`, parse-don't-validate, and
  invariants baked into the *types* (you literally can't represent a mount at the root) throw
  out a category of confident-but-wrong output before anyone reads it.
- **Tests, not "it works."** Every acceptance criterion has a deterministic test; the rules
  engine's deny-precedence is property-tested with Hedgehog; changed lines clear 85% coverage;
  Semgrep, lint, and a hermetic Nix build must pass. Details in
  [`docs/testing.md`](docs/testing.md).

## Nothing ships until I've audited it

I'm not cutting a release until I've been through the whole codebase line by line, the way you
read code you're about to hand someone else to run. Écluse is pre-launch on purpose: don't put
it in front of a build yet. The "understand and explain every line" bar that
[`CONTRIBUTING.md`](CONTRIBUTING.md) sets for contributors is the bar for release.

## You don't have to trust me. Check it

- The image is bit-for-bit reproducible: rebuild from pinned source and diff it against what's
  published.
- Every release ships keyless SLSA provenance and an SBOM in a public transparency log.
- The security-critical behaviour is in the types and the property tests: read the
  deny-by-default property and the egress guards.

If you can verify the output, you don't have to trust how it was made. (More in
[Release & Supply-Chain Operations](docs/architecture/release-supply-chain.md).)

## Why this is public now

I'm sharing Écluse pre-launch to get the design picked apart while changing it is still cheap;
I don't have a budget for an outside security review yet. So please try to break it: the origin
model, the upstream merge, the deny-by-default rules, the egress story. Start with
[`MOTIVATION.md`](MOTIVATION.md) and the [architecture](docs/architecture.md). The heavy LLM
use is a property of bootstrapping and will taper; the design is what I'm not backing off.
