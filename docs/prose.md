# Prose style guide

How to write prose for Écluse: the operator manual (`web/content/docs/`), the repository's
Markdown, PR bodies, and issues. [`docs/haddock.md`](haddock.md) owns in-source documentation, and
[CONTRIBUTING](../CONTRIBUTING.md#pull-requests) owns the PR-body shape. When the floor and the
register below pull against each other, structure wins: move the facts into a table or diagram and
let the prose shrink.

## The floor

Plain language is the baseline on every surface.

- Active voice, with the actor named: "the parser drops the header", never "the header is
  dropped".
- One idea per sentence. Prefer the short common word, and define an unavoidable term in a
  clause.
- No filler adjectives, no marketing, no reassurance ("robust", "safely", "with confidence").
- Reproduce commands, paths, defaults, numbers, and error text exactly.
- Canadian English. No em-dashes or en-dashes.

## The register

Write like an experienced operator explaining to a peer, not like a specification. The floor above
caps sentence complexity. It does not mandate a metronome.

- Open every page and section by orienting the reader: when they need it, or what it decides.
  Never the table-of-contents formula ("This page covers X, Y, and Z").
- Vary sentence length. After two sentences under ten words, the next one connects or expands.
  Let "so", "because", "then", and "instead" carry the logic between facts.
- Address the operator as "you" when the operator acts. Keep Écluse the actor when Écluse acts.
- No verbless fragments as connective tissue ("The trade:"). No gnomic closers ("Either way you
  lose nothing."). A pithy line must still carry a fact.
- No sentence repeats verbatim on a page.

## Structure over compression

Density that comes from packing facts into prose is a defect, not a virtue.

- A paragraph holding three or more parallel facts becomes a list or a table.
- Enumerable facts (variables, modes, exit codes, endpoints, pod shapes) live in tables. The
  prose around a table says why it matters, not what the rows already say.
- A flow, a topology, or a precedence order lives in a diagram, and the prose keeps only what
  the picture cannot say.

## Diagrams

- System diagrams come from `threat-modelling/ecluse.yaml`. Share elements across its focused
  Saerskriven views so names, relationships and threat links have one home.
- The site build generates SVGs under the ignored `web/static/diagrams/` directory.
  Run `task diagrams` inside the flake to render them locally. Never commit generated images.
- Repository Markdown links to the relevant diagram sections on the published threat-model page.
- Site pages use the `diagram` shortcode with the diagram ID and an accessible description.
  Each image links to its full-size SVG and keeps its contrast in both colour schemes.
- Contributor workflow diagrams can use Mermaid when they describe no system or threat-model view.
