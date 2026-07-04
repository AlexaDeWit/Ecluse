# Built with AI, and how to verify it

Écluse is a supply-chain *security* tool, and I built a lot of it leaning on an LLM harder
than I ever have in my career. If that combination makes you nervous, good. It should. So
here's the honest version of what it means: what's mine, what the AI did, and why you don't
have to trust either of us to use this.

## What's mine, and what's the AI's

I want to be precise about the line, because it's the whole point.

- **Mine: the design.** The architecture, the three-registry origin model, the
  deny-by-default rules engine, the security invariants (outbound egress / SSRF, identifier
  canonicalisation, response bounds), the planning. That's all me, and deliberately so. It's
  the result of months of on-and-off thinking about a problem I kept hitting: how a small team
  gets this kind of protection without paying enterprise licence fees or standing up a whole
  registry of its own. The dead ends are written up in [`MOTIVATION.md`](MOTIVATION.md), and they
  were real. I'm a developer who's worked in typed functional programming for years (fp-ts,
  Scala, that family of category-theory-flavoured ecosystems), so the way Haskell thinks is
  familiar ground. I own the design and the safety-critical paths, and I can explain them.
- **The AI's: the implementation.** Writing the Haskell that turns the design into code, and
  re-learning the specific libraries and idioms as I go, is mostly done with an LLM right now,
  for speed. But I'm not hands-off: I read every PR, and I go through the code itself, every file
  and every line. I'm just not doing it as tightly as I would if this were at or near release,
  mainly because everything is still subject to frequent revision, and combing a line closely when
  it's about to be rewritten is wasted effort. The tight, release-grade pass comes before release
  (more on that below).

## How I keep that honest

The process is built so that "the AI wrote it" can't quietly turn into "nobody checked it":

- **I'm the one who decides.** I act as the architect. I own the requirements, and I review and
  merge every single PR. The agent does the work but **never merges and never pushes to
  `main`.** The whole process is public, in
  [`planning/orchestration-strategy.md`](planning/orchestration-strategy.md).
- **The rule is "escalate, don't guess."** If the agent hits anything ambiguous, missing, or
  contradictory, it has to stop and ask instead of inventing its way past it. No made-up config
  or API behaviour, no quietly-weakened tests, no `undefined` left in and called done. Review
  looks for exactly that.
- **A second set of eyes that isn't the author.** Each slice gets reviewed by separate agents
  with fresh context: first for whether it does what was asked, then for security and code
  quality, before it ever gets to me.
- **The compiler does a lot of the work.** Haskell totality, `-Werror`, parse-don't-validate, and
  invariants baked into the *types* (deny-by-default; you literally can't represent a mount at the
  root) throw out a whole category of confident-but-wrong AI output before anyone reads it.
- **"It works" doesn't count. Tests do.** Every acceptance criterion has a deterministic test
  behind it; the rules engine's deny-precedence is property-tested with Hedgehog; changed
  lines have to clear 85% coverage; Semgrep, lint, and a hermetic Nix build all have to pass.
  Details in [`docs/testing.md`](docs/testing.md).

## Nothing ships until I've audited it

I'm not cutting any of this as a release until I've been through the whole codebase closely, line
by line, the way you go through code you're about to hand someone else to run. Écluse is
pre-launch on purpose. It isn't something to put in front of a build yet, and I'm not asking
anyone to run an AI-written security tool in production. The "understand and explain every line"
bar that [`CONTRIBUTING.md`](CONTRIBUTING.md) sets for contributors is the bar for *release*; that
audit is how the code gets there before anyone leans on it.

## You don't have to trust me. Check it

This is the part that actually matters for a security tool. You don't have to trust me, the
process, or the model:

- The image is bit-for-bit reproducible. Build it yourself from pinned source and diff it
  against what's published.
- Every release ships keyless SLSA provenance and an SBOM, recorded in a public transparency
  log.
- The code is small, typed, and tested, and the security-critical behaviour is right there in the
  types and the property tests. Go read the deny-by-default property, go read the egress guards.

If you can verify the output, you don't have to trust how it was made. Here, you can. (More in
[Release & Supply-Chain Operations](docs/architecture/release-supply-chain.md).)

## Why I'm posting this now

I'm sharing Écluse pre-launch, on purpose. Not to get anyone to adopt it, but to get the design
picked apart while changing it is still cheap. I don't have a community or a budget for an outside
security review yet; honestly, part of why this is public is to start drawing that kind of
attention.

So please, try to break it. The origin model, the way the two upstreams get merged, the
deny-by-default rules, the egress story: if something's wrong, I would much rather find out now.
Start with [`MOTIVATION.md`](MOTIVATION.md) and the [architecture](docs/architecture.md), and tell
me where it falls apart.

## Where this goes

The heavy LLM use is a property of *bootstrapping*, not how I plan to work forever; I expect it to
taper as the project (and my own familiarity with the code, line by line) catches up. What I'm
not backing off is the design. I spent a long time on it, and I believe in it.
