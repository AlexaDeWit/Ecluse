# Reverse-engineering research

Protocol references for the package registries Écluse proxies or plans to add.
The core is ecosystem-agnostic: npm is wired first; PyPI and RubyGems are roadmap
research. Each document reverse-engineers a registry's wire protocol, the HTTP
surface, JSON shapes, and resolution and auth behaviours, to the detail needed to
implement both halves of the proxy:

- **Client behaviour**, fetching metadata and artifacts from upstream
  registries the way each ecosystem's installer does.
- **Server behaviour**, answering an npm/pip/gem client well enough that the
  client believes it is talking to a real registry.

Each also gives a type model of the wire format (its "Type model" section), so a
request round-trips faithfully in either direction.

## Documents

| Ecosystem | Document | Status |
|-----------|----------|--------|
| npm       | [`npm.md`](npm.md) | Complete, read path, version resolution, auth (theory), type model |
| pip / PyPI | [`pypi.md`](pypi.md) | Complete, Simple + JSON APIs, version resolution, auth (theory), type model. Includes an npm↔PyPI correspondence table. |
| RubyGems (Ruby) | [`rubygems.md`](rubygems.md) | Complete, Compact Index + JSON APIs, `.gem` anatomy, version resolution, auth (theory), type model. Extends the correspondence table to npm↔PyPI↔RubyGems. |

## Method

Each document is grounded two ways and flags which:

1. **Live probes** against the public registry (`curl` + `jq`), dated and
   reproducible; the commands live in each document's "Reproducing the probes"
   appendix.
2. **Official documentation**, quoted and linked, for behaviour we cannot or
   should not exercise anonymously (publish, token lifecycle, 2FA).

Where live behaviour and docs disagree (the public registry sits behind
Cloudflare and has drifted from the spec), the document notes both and prefers
observed behaviour.
