# The RubyGems Registry Protocol (Ruby / gem / Bundler)

A reverse-engineering reference for the Ruby package registry, the third in the
series after [`npm.md`](npm.md) and [`pypi.md`](pypi.md). Same goal: a faithful
JSON/text type model (see [Type model](#11-type-model)) that lets Écluse act as
both a Ruby **client** (fetching the way `gem`/Bundler do) and a Ruby **server**
(an index `bundle install` will resolve against).

> **Terminology.** `gem` is the CLI and **Bundler** the dependency manager;
> neither is the registry. The registry is **RubyGems.org** (`rubygems.org`).
> The install-facing protocol is the **Compact Index** (`/versions`, `/info`,
> `/names`), plain text, append-only, Range-fetched; it is the analogue of npm's
> abbreviated packument / PyPI's Simple API. A richer **JSON API**
> (`/api/v1/...`, `/api/v2/...`) is the packument-style metadata view. Artifacts
> are `.gem` files under `/gems/`.

> **Provenance.** Live examples captured on **2026-06-21** against `rubygems.org`
> with `curl`/`jq`/`tar` (see [Reproducing the probes](#13-reproducing-the-probes)).
> Normative claims are backed by the RubyGems guides
> ([guides.rubygems.org](https://guides.rubygems.org/)), quoted inline. Where
> live behaviour and docs differ, the observed behaviour wins for implementation.

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

RubyGems sits between npm and PyPI in shape, with one transport twist of its own:

- **Like PyPI**, a version can have **multiple files**, one per *platform*
  (`ruby` = pure, plus `java`, `x86_64-linux`, `arm64-darwin`, …). Captured live:
  `bcrypt 3.1.22` ships both `3.1.22` (native/MRI) and `3.1.22-java` (JRuby).
- **Like npm**, install can **execute arbitrary code**: a gem with **native
  extensions** compiles them at `gem install` time, running `extconf.rb` (and
  `make`). This is the install-time RCE surface, the analogue of npm's
  install scripts.
- **Unlike either**, the primary install protocol (Compact Index) is **plain
  text, append-only, and fetched incrementally with HTTP Range requests**, not a
  JSON document re-downloaded each time.

And one finding that dominates the security design:

- **The native-extension signal is not in any metadata API.** `extensions` lives
  only inside the gem's gemspec (the `metadata.gz` in the `.gem`, or the legacy
  `quick` Marshal spec), **not** in the Compact Index and **not** in the JSON
  API (captured: `extensions: null` for `bcrypt`, which plainly has one). See §6.
  This makes the "does it run code on install?" signal a *fetch-and-parse*
  operation, not a free field read, a real divergence from npm.

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
| Name identity | case-sensitive, scopes | normalized (PEP 503) | **verbatim** (no normalization step) |
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
  APIs, **and** `.gem` artifacts (`/gems/…`), all fronted by Fastly. No separate
  artifact host to proxy (simpler than PyPI). Always HTTPS / HTTP-2.

### Gem-name identity

Gem names are used **verbatim**, there is no PEP 503-style normalization step.
The Compact Index `/names` file is the authoritative set of exact names. (Names
are conventionally lowercase with `-`/`_`, but the registry does not fold case or
punctuation for you; treat the name as opaque and exact.)

### The Compact Index is plain text + Range-incremental

The install path is **not** JSON. `/versions` and `/info/{gem}` are UTF-8 text
files that only ever **grow** (append-only), which lets a client fetch just the
new tail:

| Mechanism | Observed |
|-----------|----------|
| `ETag` | `"638dc9b8…"`, replay as `If-None-Match` → `304` |
| `Accept-Ranges: bytes` + `Range: bytes=N-` | `206 Partial Content` with `Content-Range: bytes 0-400/22829133` |
| `Repr-Digest: sha-256="…"` | digest of the *full* representation, so a client that appended a tail can verify the whole file |
| `Cache-Control` | `max-age=60` (`/versions`), short, the index changes constantly |

> "The compact index is designed to be fetched using the HTTP `Range` header.
> When a previously fetched copy is present, a ranged request [takes] advantage
> of the appended-line pattern." Clients append partial content, compute SHA256,
> and verify against `Repr-Digest`., guides.rubygems.org

A server implementation **must** support `ETag`, `Range`/`206`, and a correct
`Repr-Digest` (or `Digest`) for the compact index, or Bundler's incremental
fetch breaks. This is a heavier server contract than npm/PyPI's plain JSON.

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

Note the `.gem` **403** (not 404) for a non-existent artifact, an object-store
quirk worth handling. A proxy's own denials should be explicit; for the compact
index the natural denial is to **omit** the version line (§8) or `403`.

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

The `metadata` hash is gem-author-supplied and can include resilience-relevant
keys, notably **`rubygems_mfa_required`** (was this gem published under
mandatory MFA?), plus `source_code_uri`, `funding_uri`, `changelog_uri`. (Seen
live on `sinatra`: `metadata.rubygems_mfa_required`.)

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

Note: this is where you enumerate platform variants, `bcrypt 3.1.22` appears
both as `platform: "ruby"` and `platform: "java"`.

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

The endpoint Bundler actually resolves against, and the one Écluse should treat
as primary. Three plain-text files.

### `GET /versions`, the master list

```
created_at: 2026-05-29T01:10:37Z
---
RUBYGEM [-]VERSION[,VERSION,...] MD5
```

- The `created_at` line, then `---`, then one starting line per gem; later
  publishes **append** new lines.
- Each line: `name`, a comma-separated version list, and the **MD5 of that gem's
  `/info` file** (a cheap "did /info change?" check).
- **Yanks** appear as an appended line with a **leading dash** on the version
  (e.g. `somegem -2.0.0 <md5>`); "only the last MD5 for each gem name is
  authoritative." Captured live (first data lines, 22 MB file):

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
Newline-delimited, one exact name per line (captured: `_`, `-`,
`023_solver_…`). The basis of `gem` name completion / existence checks.

---

## 6. Package details, a version & its gemspec

"Package details" for a single version come from one of three places, in
increasing cost and completeness:

### (a) `GET /api/v2/rubygems/{name}/versions/{version}.json`

The richest JSON for one version: `dependencies` (`{runtime, development}`),
`requirements`, `ruby_version`, `rubygems_version`, `sha`, `spec_sha`,
`metadata`, `licenses`, `authors`, `description`, `created_at`, `yanked`, URIs.
Good for display and most rules.

### (b) The Compact Index `/info` line (§5)

Cheapest source of the resolver essentials: deps, `checksum`, `ruby` req,
timestamp. This is what Bundler uses.

### (c) The gemspec, the **only** source of `extensions`

Neither (a) nor (b) tells you whether installing the gem will **compile and run
code**. The authoritative gemspec does, and there are two ways to get it without
running the gem:

1. **`/quick/Marshal.4.8/{name}-{version}.gemspec.rz`**, a single version's
   gemspec, zlib-compressed Ruby Marshal. Cheapest way to read `extensions`
   without the full artifact (captured: `200`, `application/octet-stream`).
2. **Inside the `.gem`**, `metadata.gz` is the YAML-serialised
   `Gem::Specification`. Captured live (`bcrypt`):

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

The presence of a non-empty `extensions:` list is the install-time-RCE signal.
Both formats are **Ruby-native serializations** (Marshal / a YAML dialect with
`!ruby/object:` tags), so a non-Ruby implementation must either parse them
directly or shell to a Ruby helper. For the YAML form, the `extensions:` key is a
plain string list and is readable without instantiating the Ruby objects.

### Dependencies (`Gem::Requirement`)

Requirements use the operators `=`, `!=`, `>`, `<`, `>=`, `<=`, and the
**pessimistic** `~>` (twiddle-wakka: `~> 3.0` ≈ `>= 3.0, < 4.0`). In the Compact
Index, multiple constraints on one dependency are `&`-joined; in JSON they are a
single comma-separated `requirements` string. A faithful model keeps the raw
string plus the parsed `{name, constraints}`.

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
(`bcrypt-3.1.22.gem`); variants carry it (`bcrypt-3.1.22-java.gem`). As with PyPI
wheels, **a version maps to a set of files** keyed by platform, not a single
artifact. Bundler picks the file matching the running platform, falling back to
the pure-`ruby` gem (which may then build a native extension).

### Signing

RubyGems supports cryptographically **signed gems** (`gem cert`, X.509), but
adoption is rare and not enforced by default, unlike the per-download `checksum`
(SHA256), which is ubiquitous and is the integrity guarantee to rely on.

---

## 8. Version & availability resolution

> The recurring requirement: handling *"someone installs a gem without
> specifying a version."* As with npm and pip, **the registry resolves no
> requirements**, Bundler does, client-side.

### What the server resolves vs. the client

There is **no endpoint that accepts a requirement** (`~> 3.0`). The server
offers only: the full per-gem version list (`/info/{gem}`,
`/api/v1/versions/{name}.json`), the latest pointer
(`/api/v1/versions/{name}/latest.json`), and exact-file download. Unknown gem →
`404`; unknown `.gem` → `403`.

### What `bundle install` / `gem install foo` actually does

1. **Fetch the Compact Index**, `/versions` (incrementally, via Range) to learn
   what changed, then `/info/{gem}` for each gem in play. One `/info` fetch
   yields every version's deps, checksum, and ruby requirement.
2. **Resolve locally**, Bundler's resolver (**PubGrub**, formerly Molinillo)
   computes a version set satisfying all `Gem::Requirement`s across the graph:
   - bare `gem install foo` ⇒ highest non-prerelease version.
   - **prereleases excluded** unless explicitly requested (a letter segment marks
     a prerelease).
   - **platform** selection: prefer a precompiled platform gem, else the pure
     `ruby` gem.
   - **yanked** versions are absent from `/info` and so never considered.
3. **Recurse** over runtime dependencies (from the same `/info` data).
4. **Download & verify**, fetch each `/gems/…​.gem`, verify against the
   `checksum:` SHA256, write resolved versions (and, in modern Bundler, a
   `CHECKSUMS` block) to `Gemfile.lock`.

So **availability = a non-yanked version line present in `/info`**; presence in
the Compact Index *is* availability.

### Yank semantics (sharper than PyPI)

`gem yank` **removes** the version: its line disappears from `/info`, a
dash-prefixed line is appended to `/versions`, and the `.gem` download stops
resolving (403/404). Contrast PyPI, where a yanked file stays downloadable for
exact pins. So in RubyGems a yank is closer to a soft-delete than PyPI's
"hidden-from-ranges."

### Consequences for a proxy (both directions)

- **As a client**, fetch availability via the Compact Index (incrementally,  honour `Range`/`ETag`), read deps/checksums from `/info`, resolve
  `Gem::Requirement` locally. **For an install-script policy, additionally fetch
  the gemspec** (`/quick/...gemspec.rz` or the `.gem`'s `metadata.gz`), the one
  signal the index withholds.
- **As a server**, serve a coherent Compact Index: a `/versions` whose per-gem
  MD5 matches the served `/info`, `/info` lines with correct `checksum:` and
  `ruby:`, and **working `ETag`/`Range`/`Repr-Digest`** so Bundler's incremental
  fetch holds. Also serve `/names` and the `.gem` files.
- **Policy shapes availability.** To deny a version, **omit its `/info` line**
  (Bundler simply never sees it) and update the `/versions` MD5 accordingly; to
  hard-block, serve the index but `403` the `.gem`. A deny-by-default served
  index is, as elsewhere, a **filtered projection** of upstream, but here the
  projection must also keep the append-only/checksum invariants intact, which is
  more delicate than rewriting a JSON blob.
- **`created_at` (per version, in `/info` and JSON) is the age signal** for an
  age-based policy.
- **`rubygems_mfa_required`** (JSON `metadata`) is a Ruby-specific trust signal
  worth a policy: prefer gems published under enforced MFA.

---

## 9. Authentication (in theory)

No token is available; grounded in the RubyGems guides plus the fact that **all
read endpoints above are anonymous** (every probe succeeded with no credentials).
Auth gates only writes and account actions.

### Reading

Public RubyGems.org requires **no authentication** to read. Private gem servers
(Gemfury, Artifactory, GitHub Packages, a self-hosted Geminabox/`gem server`)
use **HTTP Basic**, typically credentials embedded in the source URL
(`https://KEY@gems.example.com`) or in Bundler config
(`bundle config set --global https://gems.example.com KEY`). `gem`/Bundler send
`Authorization: Basic …`.

### Writing (the API-key model)

Unlike npm's `Bearer`, RubyGems sends the **raw API key** in `Authorization`:

> ```
> curl -H 'Authorization:YOUR_API_KEY' \
>      -H 'OTP:YOUR_ONE_TIME_PASSCODE' \
>      https://rubygems.org/api/v1/...
> ```
>, guides.rubygems.org

| Aspect | Value |
|--------|-------|
| Header | `Authorization: <api_key>` (the key itself, **not** `Bearer <key>`) |
| Storage | `~/.gem/credentials` (YAML) |
| 2FA / MFA | `OTP: <one-time-passcode>` header alongside the key |
| Retrieve key | `GET /api/v1/api_key` with Basic (username:password) + `OTP` |
| Push | `POST /api/v1/gems`, the built `.gem` as the raw request body |
| Yank | `DELETE /api/v1/gems/yank` with `gem_name`, `version`, optional `platform` |

Modern refinements (parallel to npm/PyPI):

- **Scoped API keys**, a key can be limited to specific actions (push / yank /
  add-owner) and even to a single gem.
- **MFA enforcement**, popular gems can require MFA to publish; the
  `rubygems_mfa_required` gemspec metadata surfaces it (§4).
- **Trusted Publishing (OIDC)**, RubyGems added trusted publishing in 2024: a CI
  workflow exchanges an OIDC identity for a short-lived scoped key, so no
  long-lived secret is stored. The PyPI-style modern path.

### Implications for a proxy

- **Read proxy to public RubyGems needs no credentials**, like PyPI, simpler
  than npm.
- **Private upstream**: forward/attach `Authorization: Basic …` (or the raw API
  key shape if the upstream expects it). CodeArtifact's RubyGems endpoint uses an
  AWS-issued bearer/token, handled the same way as its npm endpoint.
- **Mirror/push request** (if the proxy ever publishes): `POST /api/v1/gems` with the
  raw-key `Authorization`, or OIDC trusted publishing.
- A proxy's **own** client gate is a separate concern. Note the wire difference:
  an npm client sends `Bearer`, a Ruby client sends a raw key or Basic, the edge
  auth check must accept the relevant ecosystem's form per mount.

---

## 10. Write path (for completeness)

Not on the proxy's critical path (Écluse delegates storage; mirror writes are a
separate concern), but documented so "act as a gem server" is complete.

- **Push**, `POST /api/v1/gems`, `Authorization: <key>` (+ `OTP`), body is the
  raw `.gem`. Re-pushing an existing `name-version[-platform]` is rejected
  (versions are immutable).
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
GemName        = string   -- verbatim, no normalization (≠ PyPI)
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

> The install-script signal (`extensions` non-empty) is, unlike npm (free in the
> abbreviated packument) and PyPI (free from `packagetype`), **not in the
> metadata responses**, it must be fetched from the gemspec. So it behaves like
> an *effectful* input (a fetch), not a field that can be read for free.

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
> ecosystem-specific (`hasInstallScript` / `packagetype==sdist` / `extensions`)
>, and adds a fourth: that signal may live **outside the metadata API**, so a
> resolver needs a path to *fetch* per-version detail, not just read it.

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

All captures on 2026-06-21 against `rubygems.org`.

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