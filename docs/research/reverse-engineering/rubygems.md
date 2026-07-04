# The RubyGems registry protocol (Ruby / gem / Bundler)

Reverse-engineering reference for the Ruby package registry, third after
[`npm.md`](npm.md) and [`pypi.md`](pypi.md). Same goal: a JSON/text type model
(see [Type model](#11-type-model)) that lets Écluse act as both a Ruby client
(fetching the way `gem`/Bundler do) and a Ruby server (an index `bundle install`
resolves against).

> **Terminology.** `gem` is the CLI, **Bundler** the dependency manager; the
> registry is **RubyGems.org** (`rubygems.org`). The install-facing protocol is
> the **Compact Index** (`/versions`, `/info`, `/names`): plain text, append-only,
> Range-fetched, the analogue of npm's abbreviated packument / PyPI's Simple API.
> A richer **JSON API** (`/api/v1/...`, `/api/v2/...`) is the packument-style
> view. Artifacts are `.gem` files under `/gems/`.

> **Provenance.** Live examples captured 2026-06-21 against `rubygems.org` with
> `curl`/`jq`/`tar` (see [Reproducing the probes](#13-reproducing-the-probes));
> normative claims cite the
> [RubyGems guides](https://guides.rubygems.org/). Where live and docs differ,
> observed behaviour wins.

---

## Table of contents

1. [Mental model & cross-ecosystem correspondence](#1-mental-model--cross-ecosystem-correspondence)
2. [Transport & conventions](#2-transport--conventions)
3. [Endpoint catalogue](#3-endpoint-catalogue)
4. [Project metadata, the JSON API (rich)](#4-project-metadata--the-json-api-rich)
5. [The Compact Index (installer-facing)](#5-the-compact-index-installer-facing)
6. [Package details, a version & its gemspec](#6-package-details--a-version--its-gemspec)
7. [The `.gem` artifact & integrity](#7-the-gem-artifact--integrity)
8. [Version & availability resolution](#8-version--availability-resolution)
9. [Authentication (in theory)](#9-authentication-in-theory)
10. [Write path (for completeness)](#10-write-path-for-completeness)
11. [Type model](#11-type-model)
12. [Implementing the protocol](#12-implementing-the-protocol)
13. [Reproducing the probes](#13-reproducing-the-probes)
14. [References](#14-references)

---

## 1. Mental model & cross-ecosystem correspondence

RubyGems sits between npm and PyPI in shape, with one transport twist:

- **Like PyPI**, a version can have **multiple files**, one per platform (`ruby`
  = pure, plus `java`, `x86_64-linux`, `arm64-darwin`, …). Live: `bcrypt 3.1.22`
  ships both `3.1.22` (MRI) and `3.1.22-java` (JRuby).
- **Like npm**, install can execute arbitrary code: a gem with **native
  extensions** compiles them at `gem install` time (`extconf.rb`, `make`), the
  install-time RCE surface analogous to npm's install scripts.
- **Unlike either**, the Compact Index is **plain text, append-only, and fetched
  incrementally via HTTP Range**, not a JSON document re-downloaded each time.

One finding dominates the security design:

- **The native-extension signal is in no metadata API.** `extensions` lives only
  in the gemspec (the `.gem`'s `metadata.gz`, or the legacy `quick` Marshal
  spec), not the Compact Index and not the JSON API (captured: `extensions: null`
  for `bcrypt`, which has one). So "does it run code on install?" is a
  fetch-and-parse, not a free field read, a real divergence from npm (§6).

### npm ↔ PyPI ↔ RubyGems

| Concept | npm | PyPI | RubyGems |
|---------|-----|------|----------|
| Registry host | `registry.npmjs.org` | `pypi.org` | `rubygems.org` |
| Artifact host | same | `files.pythonhosted.org` | `rubygems.org` (`/gems/…`, CDN) |
| Client | `npm` | `pip` | `gem` / Bundler |
| Rich metadata | full packument | JSON API `/pypi/{p}/json` | JSON API `/api/v1/gems/{n}.json` |
| Install-facing | abbreviated packument | Simple API | **Compact Index** (`/info/{n}`) |
| One version's detail | `/{pkg}/{v}` | `/pypi/{p}/{v}/json` | `/api/v2/rubygems/{n}/versions/{v}.json` |
| Files per version | one `.tgz` | many (sdist + wheels) | many (one per **platform**) |
| Integrity | `dist.integrity` (SRI) | `hashes.sha256` | `checksum` (SHA256 of `.gem`) in `/info` |
| "current" pointer | `dist-tags.latest` | none (client computes) | none; `/latest.json` or JSON `version` |
| Name identity | case-sensitive, scopes | normalised (PEP 503) | **verbatim** (no normalisation step) |
| Dependency spec | semver range | PEP 508 | `Gem::Requirement` (`~>`, `&`-joined) |
| Version grammar | semver | PEP 440 | `Gem::Version` (e.g. `1.0.0.beta1`) |
| "don't use" marker | `deprecated` (advisory) | `yanked` (file kept) | `yanked` (**file removed**, line in `/versions`) |
| Install-time code | pre/post-install scripts | sdist build backend | **native extensions** (`extconf.rb`) |
| Code-exec signal location | abbreviated `hasInstallScript` (free) | file `packagetype == sdist` (free) | **gemspec `extensions`** (fetch required) |
| Advisories | npm advisories endpoint | `vulnerabilities[]` (OSV) | RubyAdvisory DB / OSV (out-of-band) |

Three request shapes cover ~all install traffic:

| Intent | Request |
|--------|---------|
| "What versions of X exist + their deps + checksums?" | `GET /info/{gem}` (Compact Index) |
| "What does X-1.2.3 depend on / does it build native code?" | `/info` for deps; **gemspec** for `extensions` |
| "Give me the bytes" | `GET /gems/{gem}-{version}[-{platform}].gem` |

---

## 2. Transport & conventions

### Hosts & scheme

- One host does it all: `https://rubygems.org` serves the Compact Index, JSON
  APIs, and `.gem` artifacts (`/gems/…`), fronted by Fastly. No separate artifact
  host (simpler than PyPI). Always HTTPS, HTTP/2.

### Gem-name identity

Gem names are **verbatim**, no PEP 503-style normalisation. `/names` is the
authoritative set of exact names. Names are conventionally lowercase with
`-`/`_`, but the registry folds nothing: treat the name as opaque and exact.

### The Compact Index is plain text + Range-incremental

The install path is not JSON. `/versions` and `/info/{gem}` are UTF-8 text files
that only grow (append-only), letting a client fetch just the new tail:

| Mechanism | Observed |
|-----------|----------|
| `ETag` | `"638dc9b8…"`, replay as `If-None-Match` → `304` |
| `Accept-Ranges: bytes` + `Range: bytes=N-` | `206 Partial Content` with `Content-Range: bytes 0-400/22829133` |
| `Repr-Digest: sha-256="…"` | digest of the *full* representation, so a client that appended a tail can verify the whole file |
| `Cache-Control` | `max-age=60` (`/versions`), short, the index changes constantly |

A client appends partial content, computes SHA256, and verifies against
`Repr-Digest`. A server **must** support `ETag`, `Range`/`206`, and a correct
`Repr-Digest` (or `Digest`), or Bundler's incremental fetch breaks: a heavier
server contract than npm/PyPI's plain JSON.

### Artifacts are immutable

`.gem` files cache hard: `Cache-Control: max-age=31536000`,
`Content-Type: application/octet-stream`. A published `name-version[-platform].gem`
never changes (until yanked, when it disappears, §8).

### Compression

`/versions` and `.gem` files are already compact/binary; `Accept-Encoding: gzip`
is honoured on text endpoints. The legacy full indexes are gzipped Marshal
(`*.4.8.gz`).

### Errors

| Situation | Status | Body |
|-----------|--------|------|
| Unknown gem, JSON API | `404` | (JSON error) |
| Unknown gem, `/info/{gem}` | `404` | `This gem could not be found` (plain text) |
| Unknown `.gem` file | `403` | (Fastly/object-store denies a missing key) |

Note the `.gem` 403 (not 404) for a missing artifact, an object-store quirk. For
the Compact Index the natural denial is to omit the version line (§8) or `403`.

---

## 3. Endpoint catalogue

`✓` = exercised live on 2026-06-21; `▢` = documented / theory only.

### Read path (the proxy's hot path)

| Method | Path | Purpose | Auth |
|--------|------|---------|------|
| `GET` ✓ | `/versions` | Compact Index master: every gem → versions + `/info` checksum | none |
| `GET` ✓ | `/info/{gem}` | Compact Index per-gem: versions, deps, checksum, ruby req | none |
| `GET` ✓ | `/names` | Compact Index: all gem names | none |
| `GET` ✓ | `/api/v1/gems/{name}.json` | Rich metadata for the **latest** version | none |
| `GET` ✓ | `/api/v1/versions/{name}.json` | All versions (array) with `sha`, platform, etc. | none |
| `GET` ▢ | `/api/v1/versions/{name}/latest.json` | Latest version number only | none |
| `GET` ✓ | `/api/v2/rubygems/{name}/versions/{version}.json` | Rich metadata for one version | none |
| `GET` ✓ | `/gems/{name}-{version}[-{platform}].gem` | Artifact bytes (`application/octet-stream`) | none |
| `GET` ✓ | `/quick/Marshal.4.8/{name}-{version}.gemspec.rz` | One gemspec (zlib Marshal), **carries `extensions`** | none |
| `GET` ▢ | `/specs.4.8.gz`, `/latest_specs.4.8.gz`, `/prerelease_specs.4.8.gz` | Legacy full Marshal indexes | none |

> The legacy Marshal **dependency API** (`/api/v1/dependencies?gems=…`) was
> superseded by the Compact Index and should not be used by new integrations.

### Auth & write path (theory, no token)

| Method | Path | Purpose |
|--------|------|---------|
| `GET` ▢ | `/api/v1/api_key` | Retrieve API key (Basic auth + `OTP`) |
| `POST` ▢ | `/api/v1/gems` | Push a built `.gem` |
| `DELETE` ▢ | `/api/v1/gems/yank` | Yank a version |

---

## 4. Project metadata, the JSON API (rich)

The packument-style view. Two generations coexist.

### `GET /api/v1/gems/{name}.json`, latest version

Describes the gem at its **latest** version. Captured keys (`sinatra`):
`name`, `version`, `platform`, `authors`, `info` (description), `licenses`,
`sha` (SHA256 of the `.gem`), `spec_sha`, `dependencies` (`{development,
runtime}`), `metadata` (free-form hash), `downloads`, `version_downloads`,
`version_created_at`, `yanked`, plus URI fields (`gem_uri`, `homepage_uri`,
`source_code_uri`, `documentation_uri`, `changelog_uri`, `bug_tracker_uri`,
`funding_uri`, `project_uri`).

`dependencies` here is `{ "runtime": [{name, requirements}], "development":
[{name, requirements}] }`, the **runtime** list is what matters for resolution.

The `metadata` hash is gem-author-supplied and can include policy-relevant keys,
notably **`rubygems_mfa_required`** (published under enforced MFA?), plus
`source_code_uri`, `funding_uri`, `changelog_uri`. Live on `sinatra`:
`metadata.rubygems_mfa_required`.

### `GET /api/v1/versions/{name}.json`, all versions

An array, newest first; the analogue of npm's `versions` map / PyPI `releases`.
Each entry (captured, `bcrypt`):

| Field | Type | Notes |
|-------|------|-------|
| `number` | string | The version, e.g. `3.1.22`. |
| `platform` | string | `ruby`, `java`, `x86_64-linux`, …, **one entry per platform per version**. |
| `sha` | string | SHA256 of the `.gem`. |
| `ruby_version` / `rubygems_version` | string | Required-version constraints (despite the names). |
| `prerelease` | bool | A version with a letter segment (`1.0.0.beta`). |
| `licenses` | string[] | |
| `created_at` / `built_at` | ISO date | Publish / build time, the **age signal**. |
| `requirements`, `authors`, `summary`, `description`, `metadata`, `spec_sha`, `downloads_count` | … | |

This is where you enumerate platform variants: `bcrypt 3.1.22` appears as both
`platform: "ruby"` and `platform: "java"`.

### Real example (trimmed, `sinatra` latest)

```json
{
  "name": "sinatra",
  "version": "4.2.1",
  "platform": "ruby",
  "licenses": ["MIT"],
  "sha": "…sha256 of the .gem…",
  "dependencies": {
    "runtime": [
      { "name": "rack", "requirements": ">= 3.0.0, < 4" },
      { "name": "mustermann", "requirements": "~> 3.0" }
    ],
    "development": []
  },
  "metadata": { "rubygems_mfa_required": "true", "source_code_uri": "…" },
  "yanked": false
}
```

---

## 5. The Compact Index (installer-facing)

The endpoint Bundler resolves against and the proxy's primary. Three plain-text
files.

### `GET /versions`, the master list

```
created_at: 2026-05-29T01:10:37Z
---
RUBYGEM [-]VERSION[,VERSION,...] MD5
```

- A `created_at` line, then `---`, then one line per gem; later publishes append
  new lines.
- Each line: `name`, a comma-separated version list, and the **MD5 of that gem's
  `/info` file** (a cheap "did /info change?" check).
- **Yanks** append a line with a leading dash on the version (`somegem -2.0.0
  <md5>`); only the last MD5 per gem is authoritative. Live (first lines, 22 MB
  file):

```
- 1 419d8a97f5fa53e83192b142e0fd648b
.cat 0.0.1 5f7099c9095197710c7a06b448708688
.omghi 1,2 254204b8529d97f9655fbb2fc26ec4f5
```

### `GET /info/{gem}`, per-gem versions, deps, integrity

```
---
VERSION[-PLATFORM] [DEP:REQ[&REQ],DEP:REQ,...]|checksum:SHA256,ruby:REQ,rubygems:REQ,created_at:ISO
```

Captured live (`sinatra 4.2.1`):

```
4.2.1 logger:>= 1.6.0,mustermann:~> 3.0,rack:< 4&>= 3.0.0,rack-protection:= 4.2.1,rack-session:< 3&>= 2.0.0,tilt:~> 2.0|checksum:b7aeb9b1…,ruby:>= 2.7.8,created_at:2025-10-10T15:20:36Z
```

and a platform-split, dependency-free gem (`bcrypt`):

```
3.1.22 |checksum:1f0072e8…,created_at:2026-03-18T22:48:51Z
3.1.22-java |checksum:c4e3d8ac…,created_at:2026-03-18T22:52:31Z
```

| Token | Meaning |
|-------|---------|
| `VERSION[-PLATFORM]` | The version; platform suffix when not pure `ruby`. |
| before `\|` | **Runtime** dependencies, `name:requirement`; multiple constraints joined by `&` (`rack:< 4&>= 3.0.0`). Development deps are **not** here. |
| `checksum:` | **SHA256 of the `.gem`**, the integrity value Bundler pins. |
| `ruby:` / `rubygems:` | `required_ruby_version` / `required_rubygems_version`. |
| `created_at:` | Per-version publish timestamp (the age signal). |

⚠️ **No `extensions` token.** The Compact Index carries deps, checksum, ruby
requirement, and timestamp, but *nothing* about whether the gem builds native
code. That signal requires the gemspec (§6).

### `GET /names`

```
---
RUBYGEM
```
Newline-delimited, one exact name per line (`_`, `-`, `023_solver_…`). The basis
of name completion / existence checks.

---

## 6. Package details, a version & its gemspec

"Package details" for one version come from three places, in increasing cost:

### (a) `GET /api/v2/rubygems/{name}/versions/{version}.json`

The richest JSON for one version: `dependencies` (`{runtime, development}`),
`requirements`, `ruby_version`, `rubygems_version`, `sha`, `spec_sha`,
`metadata`, `licenses`, `authors`, `description`, `created_at`, `yanked`, URIs.
Good for display and most rules.

### (b) The Compact Index `/info` line (§5)

Cheapest source of the resolver essentials: deps, `checksum`, `ruby` req,
timestamp. This is what Bundler uses.

### (c) The gemspec, the **only** source of `extensions`

Neither (a) nor (b) says whether install will **compile and run code**; the
gemspec does. Two ways to get it without running the gem:

1. **`/quick/Marshal.4.8/{name}-{version}.gemspec.rz`**, a single version's
   gemspec, zlib Ruby Marshal. Cheapest read of `extensions` without the full
   artifact (captured: `200`, `application/octet-stream`).
2. **Inside the `.gem`**, `metadata.gz` is the YAML `Gem::Specification`. Live
   (`bcrypt`):

```yaml
name: bcrypt
platform: ruby
extensions:
- ext/mri/extconf.rb
- ext/jruby/bcrypt_jruby/BCrypt.java
- ext/mri/bcrypt_ext.c
  …
required_ruby_version: !ruby/object:Gem::Requirement
```

A non-empty `extensions:` list is the install-time-RCE signal. Both formats are
Ruby-native serialisations (Marshal, or YAML with `!ruby/object:` tags), so a
non-Ruby implementation parses them directly or shells to Ruby. In the YAML form
`extensions:` is a plain string list, readable without instantiating the objects.

### Dependencies (`Gem::Requirement`)

Operators are `=`, `!=`, `>`, `<`, `>=`, `<=`, and the pessimistic `~>`
(twiddle-wakka: `~> 3.0` ≈ `>= 3.0, < 4.0`). In the Compact Index multiple
constraints are `&`-joined; in JSON they are one comma-separated `requirements`
string. Keep the raw string plus the parsed `{name, constraints}`.

---

## 7. The `.gem` artifact & integrity

### Anatomy (captured)

A `.gem` is a POSIX **tar** containing three gzipped members:

| Member | Contents |
|--------|----------|
| `metadata.gz` | YAML `Gem::Specification` (name, version, **`extensions`**, deps, `required_ruby_version`, licenses, …). |
| `data.tar.gz` | The actual files (libs, and any `ext/…/extconf.rb` + C sources). |
| `checksums.yaml.gz` | SHA256 **and** SHA512 of `metadata.gz` and `data.tar.gz`. |

Captured `checksums.yaml`:

```yaml
SHA256:
  metadata.gz: 521c5039…
  data.tar.gz: 9abdb876…
SHA512:
  metadata.gz: 6ce98e4f…
  data.tar.gz: 9a2eddcb…
```

So there are **two integrity layers**: the outer `checksum:` (SHA256 of the
whole `.gem`, in `/info` and JSON `sha`) that Bundler verifies on download, and
the inner `checksums.yaml.gz` covering the two tar members.

### Download URL & platforms

`GET /gems/{name}-{version}[-{platform}].gem`. The pure build omits the platform
(`bcrypt-3.1.22.gem`), variants carry it (`bcrypt-3.1.22-java.gem`). As with PyPI
wheels, a version maps to a **set of files** keyed by platform. Bundler picks the
file matching the running platform, falling back to the pure `ruby` gem (which
may then build a native extension).

### Signing

RubyGems supports **signed gems** (`gem cert`, X.509), but adoption is rare and
not enforced. Rely on the per-download `checksum` (SHA256), which is ubiquitous.

---

## 8. Version & availability resolution

As with npm and pip, the registry resolves no requirements; Bundler does,
client-side.

### What the server resolves vs. the client

No endpoint accepts a requirement (`~> 3.0`). The server offers only the per-gem
version list (`/info/{gem}`, `/api/v1/versions/{name}.json`), the latest pointer
(`/api/v1/versions/{name}/latest.json`), and exact-file download. Unknown gem →
`404`, unknown `.gem` → `403`.

### What `bundle install` / `gem install foo` actually does

1. **Fetch the Compact Index**, `/versions` (incrementally via Range) for what
   changed, then `/info/{gem}` per gem in play. One `/info` fetch yields every
   version's deps, checksum, and ruby requirement.
2. **Resolve locally** with Bundler's resolver (**PubGrub**, formerly Molinillo)
   over all `Gem::Requirement`s:
   - bare `gem install foo` ⇒ highest non-prerelease version.
   - prereleases excluded unless requested (a letter segment marks one).
   - platform: prefer a precompiled platform gem, else the pure `ruby` gem.
   - yanked versions are absent from `/info`, never considered.
3. **Recurse** over runtime dependencies (from the same `/info` data).
4. **Download & verify**, fetch each `/gems/…​.gem`, verify against the
   `checksum:` SHA256, write resolved versions (and, in modern Bundler, a
   `CHECKSUMS` block) to `Gemfile.lock`.

So availability is a non-yanked version line in `/info`; presence in the Compact
Index is availability.

### Yank semantics (sharper than PyPI)

`gem yank` removes the version: its line disappears from `/info`, a dash-prefixed
line is appended to `/versions`, and the `.gem` stops resolving (403/404). Unlike
PyPI, where a yanked file stays downloadable for exact pins, a RubyGems yank is
closer to a soft-delete.

### Consequences for a proxy (both directions)

- **As a client**, fetch availability via the Compact Index (incrementally,
  honouring `Range`/`ETag`), read deps/checksums from `/info`, resolve
  `Gem::Requirement` locally. For an install-script policy, also fetch the
  gemspec (`/quick/...gemspec.rz` or the `.gem`'s `metadata.gz`), the one signal
  the index withholds.
- **As a server**, serve a coherent Compact Index: a `/versions` whose per-gem
  MD5 matches the served `/info`, `/info` lines with correct `checksum:` and
  `ruby:`, and working `ETag`/`Range`/`Repr-Digest`. Also serve `/names` and the
  `.gem` files.
- **Policy shapes availability.** To deny a version, omit its `/info` line and
  update the `/versions` MD5; to hard-block, `403` the `.gem`. A deny-by-default
  index is a **filtered projection** of upstream, but here it must keep the
  append-only/checksum invariants, more delicate than rewriting JSON.
- **`created_at`** (per version, in `/info` and JSON) is the age signal.
- **`rubygems_mfa_required`** (JSON `metadata`) is a Ruby-specific trust signal:
  prefer gems published under enforced MFA.

---

## 9. Authentication (in theory)

No token available. All read endpoints above are anonymous (every probe
succeeded with no credentials); auth gates only writes and account actions.

### Reading

Public RubyGems.org needs no auth to read. Private gem servers (Gemfury,
Artifactory, GitHub Packages, self-hosted Geminabox/`gem server`) use HTTP Basic,
typically credentials in the source URL (`https://KEY@gems.example.com`) or in
Bundler config (`bundle config set --global https://gems.example.com KEY`).
`gem`/Bundler send `Authorization: Basic …`.

### Writing (the API-key model)

Unlike npm's `Bearer`, RubyGems sends the **raw API key** in `Authorization`:

| Aspect | Value |
|--------|-------|
| Header | `Authorization: <api_key>` (the key itself, **not** `Bearer <key>`) |
| Storage | `~/.gem/credentials` (YAML) |
| 2FA / MFA | `OTP: <one-time-passcode>` header alongside the key |
| Retrieve key | `GET /api/v1/api_key` with Basic (username:password) + `OTP` |
| Push | `POST /api/v1/gems`, the built `.gem` as the raw request body |
| Yank | `DELETE /api/v1/gems/yank` with `gem_name`, `version`, optional `platform` |

Modern refinements (parallel to npm/PyPI):

- **Scoped API keys**, limited to specific actions (push / yank / add-owner) and
  even to a single gem.
- **MFA enforcement**: popular gems can require MFA to publish, surfaced by the
  `rubygems_mfa_required` gemspec metadata (§4).
- **Trusted Publishing (OIDC)**: a CI workflow exchanges an OIDC identity for a
  short-lived scoped key, no long-lived secret. The PyPI-style path.

### Implications for a proxy

- **Read proxy to public RubyGems needs no credentials**, like PyPI.
- **Private upstream**: attach `Authorization: Basic …` (or the raw API key if
  the upstream expects it). CodeArtifact's RubyGems endpoint uses an AWS-issued
  token, handled like its npm endpoint.
- **Mirror/push** (if the proxy publishes): `POST /api/v1/gems` with the raw-key
  `Authorization`, or OIDC.
- Note the wire difference: an npm client sends `Bearer`, a Ruby client a raw key
  or Basic. The edge auth check must accept the right form per mount.

---

## 10. Write path (for completeness)

Off the proxy's critical path (Écluse delegates storage), here for completeness.

- **Push**, `POST /api/v1/gems`, `Authorization: <key>` (+ `OTP`), body the raw
  `.gem`. Re-pushing an existing `name-version[-platform]` → rejected (immutable).
- **Yank**, `DELETE /api/v1/gems/yank` (`gem_name`, `version`, `platform?`);
  removes the version from the index and stops serving the `.gem` (§8).
- **Owners**, `GET/POST/DELETE /api/v1/gems/{name}/owners` manage gem owners.
- **Trusted Publishing**, CI exchanges OIDC for a short-lived key, then pushes
  as above.

---

## 11. Type model

A wire type model, sharing vocabulary with [`npm.md` §11](npm.md#11-type-model)
and [`pypi.md` §11](pypi.md#11-type-model) for easy comparison. Lenient on input
(ignore unknown keys; tolerate the plain-text Compact Index, JSON, and
Ruby-serialised gemspec forms), strict on output. ⚠️ = a shape that differs
materially from the npm wire model.

### Shared scalars

```
GemName        = string   -- verbatim, no normalisation (≠ PyPI)
GemVersion     = string   -- Gem::Version, e.g. "3.1.22", "1.0.0.beta1"; opaque
Platform       = string   -- "ruby" | "java" | "x86_64-linux" | …
GemRequirement = string   -- Gem::Requirement, e.g. "~> 3.0", "< 4&>= 3.0.0"
ISODate        = string   -- created_at / built_at
```

### `GemFile` (⚠️ npm's single `Dist` → a set, keyed by platform)

```
GemFile = {
  name:        GemName,
  version:     GemVersion,
  platform:    Platform,            -- part of the file identity
  url:         string,              -- /gems/{name}-{version}[-{platform}].gem
  checksum:    string,              -- SHA256 of the .gem (from /info or JSON sha)
  uploadTime?: ISODate,             -- created_at (age signal)
  yanked:      boolean              -- (and: yanked ⇒ file removed)
}
```

### `GemVersionDetails` (per-version)

Assembled from `/info` + JSON + (for `extensions`) the gemspec:

```
GemVersionDetails = {
  name:              GemName,
  version:           GemVersion,
  platform:          Platform,
  runtimeDeps:       [{ name: GemName, req: GemRequirement }],  -- ⚠️ ~> & &-joined, not name→range (cf. npm)
  requiredRuby?:     GemRequirement,      -- "ruby:" in /info
  requiredRubygems?: GemRequirement,
  checksum:          string,              -- SHA256 of .gem
  createdAt:         ISODate,             -- publish timestamp (present in /info, unlike PyPI/npm, no separate time map)
  licenses?:         [string],
  authors?:          [string],           -- author names; emails not in index
  yanked:            boolean,             -- closest analogue of npm's deprecated (but = removal)
  mfaRequired?:      boolean,             -- metadata.rubygems_mfa_required, Ruby-specific trust signal
  extensions?:       [string]            -- ⚠️ install-time-RCE signal; NOT in any API, from gemspec only
  -- install-script signal ≡ (extensions is non-empty)
}
```

> The install-script signal (`extensions` non-empty) is not in the metadata
> responses, unlike npm's `hasInstallScript` and PyPI's `packagetype`: it must be
> fetched from the gemspec, an effectful input rather than a free field.

### Compact Index & JSON shapes

```
CompactVersions = [ { name: GemName, versions: [GemVersion|("-"+GemVersion)], infoMd5: string } ]
                                                    -- GET /versions (append-only; last md5 wins; "-" = yank)

CompactInfo     = [ { version: GemVersion, platform?: Platform,
                      deps: [{name, req}], checksum: string,
                      ruby?: GemRequirement, rubygems?: GemRequirement, createdAt?: ISODate } ]
                                                    -- GET /info/{gem}

GemJsonLatest   = { name, version, platform, sha, dependencies:{runtime,development},
                    licenses, metadata, yanked, ...uris }      -- GET /api/v1/gems/{n}.json
GemJsonVersions = [ { number, platform, sha, ruby_version, prerelease, created_at, ... } ]
                                                    -- GET /api/v1/versions/{n}.json
GemJsonVersion  = GemJsonLatest-shaped, one version -- GET /api/v2/rubygems/{n}/versions/{v}.json
```

> **Cross-ecosystem note.** RubyGems echoes the same shape differences PyPI
> surfaced versus npm, (a) a version owns **N** artifacts (`Dist` → list, keyed
> by platform); (b) the publish timestamp is per-file/per-version (here right
> inside `/info`, no separate time map); (c) the install-risk signal is
> ecosystem-specific (`hasInstallScript` / `packagetype==sdist` / `extensions`),
> and adds a fourth: that signal may live **outside the metadata API**, so a
> resolver needs a way to *fetch* per-version detail rather than read it from a
> field.

---

## 12. Implementing the protocol

### To be a believable gem **server** (answer `bundle install`)

- [ ] Compact Index: `GET /versions`, `/info/{gem}`, `/names` as plain text,
      with **`ETag`, `Range`/`206`, and a correct `Repr-Digest`** so incremental
      fetch works.
- [ ] Keep the invariants: `/versions` per-gem MD5 == MD5 of the served `/info`;
      append-only ordering; dash-prefixed lines for yanks.
- [ ] `/info` lines with correct `checksum:` (SHA256), `ruby:`, `created_at:`.
- [ ] Serve `.gem` artifacts at `/gems/{name}-{version}[-{platform}].gem`
      (immutable cache), and `/quick/Marshal.4.8/…gemspec.rz`.
- [ ] Optionally serve the JSON APIs (`/api/v1/gems`, `/api/v1/versions`,
      `/api/v2/...`).
- [ ] `404` for unknown gem/metadata; policy denials by **omitting** the version
      line (or `403` on the `.gem`).

### To be a correct gem **client** (fetch from upstreams)

- [ ] Fetch the Compact Index **incrementally** (Range + ETag + Repr-Digest
      verification); resolve `Gem::Requirement` (PubGrub semantics) **locally**.
- [ ] Select the right **platform** file; exclude **yanked** and (by default)
      prereleases.
- [ ] Read deps/checksum/ruby-req from `/info`; **fetch the gemspec** to learn
      `extensions` for the install-script signal.
- [ ] For private upstreams attach `Authorization: Basic …` / raw key.
- [ ] Verify each `.gem` against its SHA256 `checksum` before mirroring.
- [ ] Project upstream into a **filtered** Compact Index reflecting policy
      decisions, preserving the append-only/MD5/checksum invariants.

---

## 13. Reproducing the probes

All captures 2026-06-21 against `rubygems.org`.

```bash
# Compact Index: Range/incremental (206) + format
curl -s -D - -r 0-400 https://rubygems.org/versions | grep -iE '^(HTTP|content-range|repr-digest|etag)'
curl -s -r 0-400 https://rubygems.org/versions                       # created_at, ---, name versions md5

# /info: deps (&-joined) + checksum + ruby req  (deps-bearing vs platform-split)
curl -s https://rubygems.org/info/sinatra | tail -2
curl -s https://rubygems.org/info/bcrypt  | tail -2                  # 3.1.22 and 3.1.22-java

# JSON APIs
curl -s https://rubygems.org/api/v1/gems/sinatra.json | jq 'keys'
curl -s https://rubygems.org/api/v1/versions/bcrypt.json | jq '.[0]|{number,platform,sha,prerelease}'
curl -s https://rubygems.org/api/v2/rubygems/bcrypt/versions/3.1.22.json | jq '{extensions}'   # → null (!)

# extensions ONLY in the gemspec, crack the .gem
curl -s -o bcrypt.gem https://rubygems.org/gems/bcrypt-3.1.22.gem
tar tf bcrypt.gem                                                    # metadata.gz data.tar.gz checksums.yaml.gz
tar xf bcrypt.gem && gunzip -c metadata.gz | grep -A3 '^extensions:'
gunzip -c checksums.yaml.gz                                          # SHA256/SHA512 of inner tars

# Artifact headers + 404/403 behaviour
curl -sI https://rubygems.org/gems/bcrypt-3.1.22.gem | grep -iE '^(content-type|cache-control)'
curl -s -o /dev/null -w "missing .gem -> %{http_code}\n" https://rubygems.org/gems/zzz-1.0.0.gem   # 403
curl -s -o /dev/null -w "missing /info -> %{http_code}\n" https://rubygems.org/info/zzz-no-such    # 404
```

---

## 14. References

- RubyGems.org API v1: <https://guides.rubygems.org/rubygems-org-api/>
- RubyGems.org API v2: <https://guides.rubygems.org/rubygems-org-api-v2/>
- Compact Index API: <https://guides.rubygems.org/rubygems-org-compact-index-api/>
  · server lib: <https://github.com/rubygems/compact_index>
- Publishing, API keys, scopes, MFA:
  <https://guides.rubygems.org/publishing/> ·
  <https://guides.rubygems.org/api-key/> ·
  <https://guides.rubygems.org/mfa-requirement-opt-in/>
- Trusted Publishing (OIDC): <https://guides.rubygems.org/trusted-publishing/>
- The `.gem` format & specification reference:
  <https://guides.rubygems.org/specification-reference/>
- Bundler resolver (PubGrub): <https://github.com/jhawthorn/pub_grub>
- OSV (advisory source covering RubyGems): <https://osv.dev/>
- Internal: [`npm.md`](npm.md), [`pypi.md`](pypi.md) (parallel references),
  [`../../architecture.md`](../../architecture.md).
```