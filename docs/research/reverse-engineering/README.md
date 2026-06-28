# Reverse-Engineering Research

Protocol references for the package ecosystems Écluse proxies. Each document
reverse-engineers a registry's **wire protocol**, the HTTP surface, the JSON
shapes, the resolution and auth behaviours, to the level of detail needed to
implement *both halves* of the proxy:

- **Client behaviour**, fetching metadata and artifacts from upstream
  registries the way each ecosystem's installer does.
- **Server behaviour**, answering an npm/pip/gem client well enough that the
  client believes it is talking to a real registry.

Each document also gives a precise **type model of the wire format** (see its
"Type model" section), so a request can be decoded and re-encoded faithfully in
either direction.

## Documents

| Ecosystem | Document | Status |
|-----------|----------|--------|
| npm       | [`npm.md`](npm.md) | Complete, read path, version resolution, auth (theory), type model |
| pip / PyPI | [`pypi.md`](pypi.md) | Complete, Simple + JSON APIs, version resolution, auth (theory), type model. Includes an npm↔PyPI correspondence table. |
| RubyGems (Ruby) | [`rubygems.md`](rubygems.md) | Complete, Compact Index + JSON APIs, `.gem` anatomy, version resolution, auth (theory), type model. Extends the correspondence table to npm↔PyPI↔RubyGems. |

## Method

Each document is grounded two ways, and says which claim came from which:

1. **Live probes** against the public registry (`curl` + `jq`), captured with a
   date. These are reproducible, the exact commands are kept in each
   document's "Reproducing the probes" appendix.
2. **Official documentation**, quoted and linked, for behaviour we cannot or
   should not exercise anonymously (publish, token lifecycle, 2FA).

Where live behaviour and documentation disagree (it happens, the public
registry is fronted by Cloudflare and has drifted from the spec), the document
notes both and prefers the observed behaviour for client/server implementation.
