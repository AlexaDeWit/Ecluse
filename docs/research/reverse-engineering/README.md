# Reverse-engineering research

Protocol references for the package registries Écluse proxies or plans to add.
The core is ecosystem-agnostic, and Écluse wires npm first. PyPI and RubyGems
are research only. Each document reverse-engineers one registry's wire protocol,
the HTTP surface, the JSON shapes, and the resolution and auth behaviours. It
carries the detail needed to build both halves of the proxy:

- **Client behaviour**, fetching metadata and artifacts from upstream
  registries the way each ecosystem's installer does.
- **Server behaviour**, answering an npm, pip, or gem client well enough that
  the client believes it is talking to a real registry.

Each document also gives a type model of the wire format, in its "Type model"
section, so a request round-trips faithfully in either direction.

## Documents

| Ecosystem | Document | Status |
|-----------|----------|--------|
| npm       | [`npm.md`](npm.md) | Complete, read path, version resolution, auth (theory), type model |
| pip / PyPI | [`pypi.md`](pypi.md) | Complete, Simple + JSON APIs, version resolution, auth (theory), type model. Includes an npm↔PyPI correspondence table. |
| RubyGems (Ruby) | [`rubygems.md`](rubygems.md) | Complete, Compact Index + JSON APIs, `.gem` anatomy, version resolution, auth (theory), type model. Extends the correspondence table to npm↔PyPI↔RubyGems. |

## Method

Each document rests on two kinds of evidence and flags which one backs a claim:

1. **Live probes** against the public registry (`curl` + `jq`), dated and
   reproducible. The commands live in each document's "Reproducing the probes"
   appendix.
2. **Official documentation**, quoted and linked, for behaviour we cannot or
   should not exercise anonymously: publish, token lifecycle, and 2FA.

The public npm registry sits behind Cloudflare and drifts from the spec. Where
live behaviour and the documentation disagree, the document records both and
prefers what it observed.
