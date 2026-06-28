# The PyPI Registry Protocol (Python / pip)

A reverse-engineering reference for the Python package registry, the companion to
[`npm.md`](npm.md). Same goal: a faithful JSON type model (see
[Type model](#11-type-model)) that lets Écluse act as both a Python **client**
(fetching from upstreams the way `pip` does) and a Python **server** (an index
`pip` will install from).

> **Terminology first.** `pip` is the *client* (installer), not a registry or a
> protocol. The registry is **PyPI** (the Python Package Index), at
> `https://pypi.org`. Two protocols matter:
> - the **Simple / Index API**, the installer-facing index (PEP 503 HTML,
>   PEP 691 JSON, PEP 700 extensions). This is what `pip` actually uses; it is
>   the analogue of npm's *abbreviated packument*.
> - the **JSON API**, Warehouse's richer metadata (`/pypi/{p}/json`), the
>   analogue of npm's *full packument*.
>
> Artifacts live on a **separate host**, `https://files.pythonhosted.org`, not
> on `pypi.org`.

> **Provenance.** Live examples captured on **2026-06-21** against `pypi.org` /
> `files.pythonhosted.org` with `curl`/`jq` (see
> [Reproducing the probes](#13-reproducing-the-probes)). Normative claims are
> backed by the PyPI docs ([docs.pypi.org](https://docs.pypi.org/)) and the
> packaging PEPs, quoted inline. Where live behaviour and spec differ, the
> observed behaviour wins for implementation.

---

## Table of contents

1. [Mental model & npm correspondence](#1-mental-model--npm-correspondence)
2. [Transport & conventions](#2-transport--conventions)
3. [Endpoint catalogue](#3-endpoint-catalogue)
4. [Project metadata, the JSON API (rich)](#4-project-metadata--the-json-api-rich)
5. [The Simple / Index API (installer-facing)](#5-the-simple--index-api-installer-facing)
6. [Package details, a release & its core metadata](#6-package-details--a-release--its-core-metadata)
7. [The file (distribution) object](#7-the-file-distribution-object)
8. [Version & availability resolution](#8-version--availability-resolution)
9. [Authentication (in theory)](#9-authentication-in-theory)
10. [Write path (for completeness)](#10-write-path-for-completeness)
11. [Type model](#11-type-model)
12. [Implementing the protocol](#12-implementing-the-protocol)
13. [Reproducing the probes](#13-reproducing-the-probes)
14. [References](#14-references)

---

## 1. Mental model & npm correspondence

PyPI is **not** a document store like npm's CouchDB. It is fundamentally a
**file index**: a project is a normalized name pointing at a flat list of
**distribution files**, and a "version" is just the set of files that share a
release number. Two consequences dominate the design:

- **A version has *many* files, not one tarball.** Each release offers at most
  one **sdist** (`*.tar.gz`, a source distribution) and *zero or more*
  **wheels** (`*.whl`, pre-built, one per Python-version × platform combination).
  Captured live: `markupsafe 3.0.3` ships **89 files** for the single version,  one sdist plus 88 platform/Python-tagged wheels. npm's single `dist.tarball`
  has no equivalent; the npm `Dist` becomes a **list** here.
- **Resolution is the client's job** (as in npm). The index returns every file;
  `pip` parses versions (PEP 440), filters by Python compatibility, wheel tags,
  and yank status, then picks. PyPI resolves *no* version specifiers server-side
 ; see §8.

### npm ↔ PyPI correspondence

| Concept | npm | PyPI |
|---------|-----|------|
| Registry host | `registry.npmjs.org` | `pypi.org` |
| Artifact host | same registry | **`files.pythonhosted.org`** (separate) |
| Installer (client) | `npm` | `pip` |
| "Give me everything" (rich) | full packument `GET /{pkg}` | JSON API `GET /pypi/{p}/json` |
| "Give me what install needs" | abbreviated packument (`Accept: …install-v1+json`) | Simple API `GET /simple/{p}/` (HTML or `…simple.v1+json`) |
| One version's metadata | `GET /{pkg}/{version}` | `GET /pypi/{p}/{v}/json` |
| The artifact | one `.tgz` per version | **N** `.whl` + one `.tar.gz` per version |
| Integrity | `dist.integrity` (SRI), `shasum` | `hashes.sha256` (+ `md5`, `blake2b`) |
| "Current" pointer | `dist-tags.latest` (server-set) | **none on the wire**, client computes; `info.version` (JSON API) is latest |
| Name identity | case-sensitive, scopes (`@s/n`) | **normalized** (PEP 503): lowercase, `[-_.]+`→`-` |
| Dependency spec | semver range string | PEP 508 string (markers, extras) |
| Version grammar | semver | PEP 440 |
| "Don't use this" marker | `deprecated` (advisory) | `yanked` (PEP 592, removed from resolution) |
| Install-time code execution | `pre/post-install` scripts | sdist build (`setup.py`); **wheels run no code** |
| Advisories source | npm advisories endpoint | `vulnerabilities[]` (OSV) inline in JSON API |

Three request shapes cover ~all install traffic:

| Intent | Request |
|--------|---------|
| "What versions/files of X exist?" | `GET /simple/{p}/` (JSON preferred) |
| "What does X@1.2.3 depend on?" | `<file-url>.metadata` (PEP 658) or `GET /pypi/{p}/{v}/json` |
| "Give me the bytes" | `GET https://files.pythonhosted.org/packages/…/X-1.2.3-*.whl` |

---

## 2. Transport & conventions

### Hosts & scheme

- Metadata APIs: `https://pypi.org` (Simple at `/simple/…`, JSON at `/pypi/…`).
- Artifacts **and** PEP 658 metadata files: `https://files.pythonhosted.org`.
  A proxy must front (or rewrite to) **both** hosts, resolving metadata through
  Écluse but pulling bytes straight from `files.pythonhosted.org` would bypass
  the gate.
- Always HTTPS; the public endpoints negotiate HTTP/2.

### Project-name normalization (PEP 503)

> "The project is matched case-insensitively with the `_`, `-` and `.`
> characters considered equal.", docs.pypi.org / PEP 503

The normalization function is:

```python
re.sub(r"[-_.]+", "-", name).lower()
```

The registry **301-redirects** non-canonical names to the normalized path.
Captured live:

| Request | → |
|---------|---|
| `GET /simple/Flask/` | `301` → `/simple/flask/` |
| `GET /simple/zope.interface/` | `301` → `/simple/zope-interface/` |
| `GET /simple/typing_extensions/` | `301` → `/simple/typing-extensions/` |

A server implementation **must** normalize and redirect; a client **must**
normalize before requesting (and follow the redirect).

### Content negotiation (Simple API)

The Simple API speaks three media types, chosen by `Accept`:

| `Accept` | Response `Content-Type` | Format |
|----------|------------------------|--------|
| _absent_ / `text/html` / `application/vnd.pypi.simple.v1+html` | `…v1+html` | PEP 503 HTML |
| `application/vnd.pypi.simple.v1+json` | `application/vnd.pypi.simple.v1+json` | PEP 691 JSON |

> "We recommend new integrations use the JSON version." Modern `pip` sends
> `Accept: application/vnd.pypi.simple.v1+json, application/vnd.pypi.simple.v1+html;q=0.1, text/html;q=0.01`.

The **JSON API** (`/pypi/…/json`) is a *separate* endpoint, always
`application/json`, not content-negotiated.

### Caching & conditional requests

| Header | Observed | Meaning |
|--------|----------|---------|
| `ETag` | `"OUJZOTBg4VfJPFsr2lGD1Q"` | entity tag; replay as `If-None-Match` |
| `Cache-Control` | `max-age=600, public` (Simple), `max-age=900` (JSON) | cacheable minutes |
| `X-PyPI-Last-Serial` | `37059094` | monotonic serial of the last event affecting this project; also `meta._last-serial` / `last_serial` in bodies |
| `X-Cache` / `X-Cache-Hits` | CDN (Fastly) markers | cache diagnostics |

`X-PyPI-Last-Serial` is PyPI's cheap change-detector: a mirror compares the
serial it last saw against the header to know whether to refetch. Artifacts on
`files.pythonhosted.org` are **immutable** (a published file's content never
changes, that's the hash guarantee) and cache effectively forever.

### Compression & User-Agent

`Accept-Encoding: gzip` is honoured. PyPI explicitly asks clients to **set a
descriptive `User-Agent`** (with contact info) and to prefer a mirror/cache for
high volume, relevant for a proxy's upstream fetch.

### Errors

Unknown project / version → `404` (plain, no structured body). There is no
rich error envelope like npm's `{error}`. A proxy's own denial responses should
still be explicit; for a Simple-API surface the natural denial is to **omit**
the file/version (§8) or return `403`.

---

## 3. Endpoint catalogue

`✓` = exercised live on 2026-06-21; `▢` = documented / theory only.

### Read path (the proxy's hot path)

| Method | Path (host) | Purpose | Auth |
|--------|-------------|---------|------|
| `GET` ✓ | `/simple/` (pypi.org) | Root index, every project name | none |
| `GET` ✓ | `/simple/{project}/` (pypi.org) | Project's files (HTML or JSON by `Accept`) | none |
| `GET` ✓ | `/pypi/{project}/json` (pypi.org) | Rich metadata, all releases | none |
| `GET` ✓ | `/pypi/{project}/{version}/json` (pypi.org) | Rich metadata, one release | none |
| `GET` ✓ | `/packages/…/{file}` (files.pythonhosted.org) | Distribution bytes | none |
| `GET` ✓ | `{file-url}.metadata` (files.pythonhosted.org) | Wheel core metadata (PEP 658) | none |
| `GET` ▢ | `/integrity/{p}/{v}/{file}/provenance` (pypi.org) | PEP 740 attestations | none |

### Auth & write path (theory, no token)

| Method | Path | Purpose |
|--------|------|---------|
| `POST` ▢ | `https://upload.pypi.org/legacy/` | Upload a distribution (twine) |
| `POST` ▢ | `/_/oidc/mint-token` | Trusted Publishing: OIDC → short-lived token |

There is **no** login/whoami/token-lifecycle API on the wire, API tokens are
minted in the web UI, and Trusted Publishing mints them from a CI OIDC identity
(§9). This is a real divergence from npm.

---

## 4. Project metadata, the JSON API (rich)

`GET /pypi/{project}/json` → `application/json`. The analogue of npm's full
packument: one document covering the project and **all** its releases.

### Top-level keys

| Key | Type | Notes |
|-----|------|-------|
| `info` | object | Project metadata (see below), describes the **latest** release. |
| `last_serial` | number | Serial of the last event; mirrors `X-PyPI-Last-Serial`. |
| `releases` | object`<version, `[File](#7-the-file-distribution-object)`[]>` | **All** versions → their files. ⚠️ **Deprecated** (see below). |
| `urls` | [File](#7-the-file-distribution-object)`[]` | Files for the **latest** release only. |
| `vulnerabilities` | [Vulnerability](#vulnerability)`[]` | OSV advisories affecting this version. |
| `ownership` | object | Project roles / org membership. |

> **Deprecations** (per docs.pypi.org, treat as read-only relics):
> - **`releases`**, "projects should shift to using the Index API"; may be
>   removed. Prefer the Simple API for the version/file list.
> - **`downloads`**, always `-1`.
> - **`has_sig`**, always `false`.
> - **`bugtrack_url`**, always `null`.

### The `info` object

Captured keys (live, `requests`): `name`, `version`, `summary`, `description`,
`description_content_type`, `author`, `author_email`, `maintainer`,
`maintainer_email`, `license`, `license_expression`, `license_files`,
`keywords`, `classifiers`, `home_page`, `project_urls`, `download_url`,
`platform`, `requires_python`, `requires_dist`, `provides_extra`, `dynamic`,
`yanked`, `yanked_reason`, `package_url`, `project_url`, `release_url`,
`docs_url`, `bugtrack_url`, `downloads`.

The install-critical ones:

| Field | Type | Meaning |
|-------|------|---------|
| `name` / `version` | string | Latest version (`info.version` is the closest thing to npm's `latest`). |
| `requires_python` | string (PEP 440) | e.g. `">=3.10"`, gates which interpreters may install. |
| `requires_dist` | string[] (PEP 508) | Dependencies *with markers/extras*, e.g. `"PySocks!=1.5.7,>=1.5.6; extra == \"socks\""`. |
| `provides_extra` | string[] | Declared extras (e.g. `socks`). |
| `license` / `license_expression` | string | SPDX expression (`license_expression`) is the modern PEP 639 form. |
| `classifiers` | string[] | Trove classifiers (incl. `Development Status`, license, supported Pythons). |
| `yanked` / `yanked_reason` | bool / string | PEP 592; see §8. |

### Real example (trimmed, `requests`)

```json
{
  "info": {
    "name": "requests",
    "version": "2.34.2",
    "requires_python": ">=3.10",
    "requires_dist": [
      "charset_normalizer<4,>=2", "idna<4,>=2.5",
      "urllib3<3,>=1.26", "certifi>=2023.5.7",
      "PySocks!=1.5.7,>=1.5.6; extra == \"socks\""
    ],
    "license": "Apache-2.0",
    "yanked": false
  },
  "last_serial": 37059094,
  "releases": { "2.34.2": [ /* File[] */ ] },
  "urls": [ /* File[] for 2.34.2 */ ],
  "vulnerabilities": []
}
```

---

## 5. The Simple / Index API (installer-facing)

`GET /simple/{project}/`. The endpoint `pip` actually uses, and the one Écluse
should treat as primary. Two equivalent representations.

### PEP 503 HTML

Each file is an `<a>` whose `href` carries the hash in the URL fragment and
whose `data-*` attributes carry install hints. Captured live (`requests`):

```html
<a href="https://files.pythonhosted.org/packages/a0/f4/…/requests-2.34.2-py3-none-any.whl#sha256=2a0d60c1…"
   data-requires-python="&gt;=3.10"
   data-dist-info-metadata="sha256=8c384ba3…"
   data-core-metadata="sha256=8c384ba3…"
   data-provenance="https://pypi.org/integrity/requests/2.34.2/requests-2.34.2-py3-none-any.whl/provenance">requests-2.34.2-py3-none-any.whl</a><br />
<a href="https://files.pythonhosted.org/packages/ac/c3/…/requests-2.34.2.tar.gz#sha256=f288924c…"
   data-requires-python="&gt;=3.10"
   data-provenance="…/requests-2.34.2.tar.gz/provenance">requests-2.34.2.tar.gz</a><br />
```

Note the **sdist has no `data-core-metadata`**, only wheels carry a METADATA
file. `data-yanked` (PEP 592), when present, marks a yanked file.

### PEP 691 JSON (preferred)

`Accept: application/vnd.pypi.simple.v1+json` → the same information, machine-readable.

```json
{
  "meta": { "api-version": "1.4", "_last-serial": 37059094 },
  "name": "requests",
  "versions": ["…", "2.34.1", "2.34.2"],
  "files": [
    {
      "filename": "requests-2.34.2-py3-none-any.whl",
      "url": "https://files.pythonhosted.org/packages/a0/f4/…/requests-2.34.2-py3-none-any.whl",
      "hashes": { "sha256": "2a0d60c1…" },
      "requires-python": ">=3.10",
      "core-metadata": { "sha256": "8c384ba3…" },
      "data-dist-info-metadata": { "sha256": "8c384ba3…" },
      "provenance": "https://pypi.org/integrity/requests/2.34.2/requests-2.34.2-py3-none-any.whl/provenance",
      "size": 73075,
      "upload-time": "2026-05-14T19:25:26.443Z",
      "yanked": false
    }
  ]
}
```

| Field | Type | Source PEP | Notes |
|-------|------|-----------|-------|
| `meta.api-version` | string | 691 | e.g. `"1.4"`. |
| `meta._last-serial` | number | (Warehouse) | == `X-PyPI-Last-Serial`. |
| `name` | string | 691 | Normalized project name. |
| `versions` | string[] | 700 | All version strings present (PEP 700 addition). |
| `files[].filename` | string | 691 | The distribution filename (encodes tags, §7). |
| `files[].url` | string | 691 | Absolute, on `files.pythonhosted.org`. |
| `files[].hashes` | object`<alg,hex>` | 691 | Keyed by algorithm; **`sha256` always present**. |
| `files[].requires-python` | string\|null | 691 | PEP 440 gate. |
| `files[].core-metadata` | bool \| `{sha256}` | 658/714 | If truthy, a `.metadata` file exists (see §6). `data-dist-info-metadata` is the **old name** for the same thing, both emitted for compat (PEP 714). |
| `files[].provenance` | string\|null | 740 | URL of attestation bundle, if any. |
| `files[].size` | number | 700 | Bytes. |
| `files[].upload-time` | ISO-8601 | 700 | The **publish timestamp** (the age signal). |
| `files[].yanked` | bool \| string | 592 | `true` or a reason string. |

This is the richer, modern surface, prefer JSON, fall back to HTML only for
ancient mirrors.

---

## 6. Package details, a release & its core metadata

"Package details" for Python splits across two places, because dependency
metadata lives **inside the artifact**, not in the index:

### (a) The version JSON, `GET /pypi/{project}/{version}/json`

Identical to the project JSON **minus the `releases` key**: `{ info, last_serial,
urls, vulnerabilities, ownership }`. `urls` is the file list for *that* version;
`info` is that version's metadata. Captured live: `requests 2.32.3` →
`urls` had 2 files (`bdist_wheel`, `sdist`). Unknown version → `404`.

### (b) Core metadata (the METADATA file), PEP 658/714

The authoritative per-version dependency data is the **core metadata** (PEP 566/
621/643) embedded in each wheel's `*.dist-info/METADATA`. PyPI exposes it
**without downloading the wheel**: append `.metadata` to the file URL.

Captured live:

```
GET https://files.pythonhosted.org/packages/…/requests-2.34.2-py3-none-any.whl.metadata
→ 200, content-type: binary/octet-stream, 4806 bytes
```

The body is RFC-822-style key:value text (`Name`, `Version`, `Requires-Python`,
`Requires-Dist: …`, `Provides-Extra: …`, …). This is the **direct analogue of
npm's abbreviated manifest**: it lets the resolver read dependencies cheaply.
The index advertises its availability and hash via `core-metadata` (§5), so a
client can fetch and verify it before committing to the full download.

### Dependencies (PEP 508)

`requires_dist` entries are PEP 508 specifiers, richer than npm ranges:

```
PySocks!=1.5.7,>=1.5.6 ; extra == "socks"
chardet<8,>=3.0.2      ; extra == "use-chardet-on-py3"
charset_normalizer<4,>=2
```

They carry version sets, **environment markers** (`python_version`, `sys_platform`,
`extra == …`), and extras. A faithful model keeps the raw string and the parsed
`{name, specifier, marker, extras}`.

### Install-time code execution (the npm-install-script analogue)

PyPI has no `hasInstallScript`, but the risk exists in a different shape:

- **Wheels run no code on install**, they are unpacked, not executed. Installing
  a wheel is inert.
- **sdists execute a build backend** (`setup.py` / PEP 517 backend) at *install/
  build* time, arbitrary code. For PyPI, the install-time-execution signal is
  therefore **"prefer/require wheels; treat sdist-only releases as higher
  risk."** This is derivable purely from the file list (`packagetype`), no
  download needed.

---

## 7. The file (distribution) object

The security-critical unit, the analogue of npm's `dist`, but there are **many
per version**, and its shape differs slightly between the JSON API and the
Simple API.

### JSON API file (in `urls` / `releases[v]`)

Captured live (`requests` wheel):

| Field | Type | Meaning |
|-------|------|---------|
| `filename` | string | e.g. `requests-2.34.2-py3-none-any.whl`. |
| `url` | string | On `files.pythonhosted.org`. |
| `packagetype` | string | `"bdist_wheel"` or `"sdist"`. |
| `python_version` | string | Wheel: tag like `py3`, `cp310`; sdist: `source`. |
| `requires_python` | string\|null | PEP 440 gate. |
| `size` | number | Bytes. |
| `upload_time_iso_8601` | ISO-8601 | Publish timestamp. |
| `digests` | object | `{ sha256, md5, blake2b_256 }`, **`sha256` is the one to verify**. |
| `yanked` / `yanked_reason` | bool / string\|null | PEP 592. |
| `has_sig` | bool | Legacy GPG flag, **always `false`** now (deprecated). |
| `comment_text` | string | Uploader note (usually empty). |

### Simple API file

The same artifact, leaner fields: `filename`, `url`, `hashes`,
`requires-python`, `core-metadata`, `provenance`, `size`, `upload-time`,
`yanked` (§5). The Simple form is what the installer resolves against.

### Filename encodes compatibility (PEP 425/427)

A wheel filename is structured:
`{distribution}-{version}(-{build})?-{python tag}-{abi tag}-{platform tag}.whl`.
Captured live (`markupsafe 3.0.3`, one of 89 files):

```
markupsafe-3.0.3-cp310-cp310-manylinux_2_17_x86_64.manylinux2014_x86_64.whl
            │     │     │     └ platform tag(s)
            │     │     └ abi tag
            │     └ python tag (CPython 3.10)
            └ version
```

`pip` selects files whose tags match the running interpreter/platform. A pure
wheel uses `py3-none-any`. **Implication:** a version maps to a *set* of files,
each with its own hash, size, tags, and metadata, it cannot be collapsed to a
single artifact the way npm's `dist` can.

### Integrity & provenance

- **Hash**: verify the downloaded file against `hashes.sha256` (Simple) /
  `digests.sha256` (JSON). In HTML it's the `#sha256=…` URL fragment. Any
  mirror/rewrite must preserve bytes exactly.
- **Core-metadata hash**: `core-metadata.sha256` lets a client verify the
  `.metadata` companion independently.
- **Provenance** (PEP 740): `provenance` / `data-provenance` points at an
  attestation bundle (`/integrity/{p}/{v}/{file}/provenance`), Sigstore-backed
  signatures binding the file to a Trusted Publisher. The modern integrity story,
  analogous to npm's `dist.signatures`.

---

## 8. Version & availability resolution

> The explicit requirement again: handling *"someone installs a package without
> specifying a version"*. As with npm, **the registry does not resolve
> specifiers**, `pip` does, client-side.

### What the server resolves vs. what the client resolves

| Spec | Server (PyPI) |
|------|---------------|
| Exact version, JSON API (`/pypi/requests/2.32.3/json`) | `200` |
| Unknown version | `404` (captured: `/pypi/requests/0.0.0/json` → `404`) |
| A **specifier** (`requests>=2`, `requests==2.*`) | **no endpoint accepts it**, there is no range URL at all |

The Simple index has **no per-version URL**, it returns *all* files for the
project, full stop. There is also **no `latest` pointer on the wire** (unlike
npm's `dist-tags.latest`); `info.version` in the JSON API is the only
server-provided "current", and the installer doesn't rely on it.

### What `pip install requests` actually does

1. **Normalize** the name (PEP 503) and `GET /simple/requests/` (JSON preferred).
2. **Enumerate** candidate files from `files[]` / `versions[]`.
3. **Filter** each candidate:
   - **PEP 440** version parse + match against the requirement specifier (bare
     `pip install requests` ⇒ "any", which means *highest compatible*).
   - **`requires-python`** must admit the running interpreter.
   - **wheel tags** (PEP 425) must match the platform; else fall back to the
     **sdist** (and build it).
   - **yanked** files (PEP 592) are **excluded** unless the requirement pins that
     exact version (`==`). Yanked ≠ deleted: still downloadable, just invisible
     to range resolution.
   - **pre-releases** excluded by default (PEP 440) unless `--pre` or pinned.
4. **Pick** the highest remaining version; choose the best-matching wheel (or
   sdist).
5. **Resolve transitive deps** by reading each candidate's core metadata,   cheaply via the **`.metadata`** companion (§6), else by downloading.
6. **Download** from `files.pythonhosted.org`, verify `sha256`, install.

So **availability of a version = "a usable, non-yanked file for it exists in the
index"**, there is no separate availability API; presence in the Simple index
*is* availability, modulo yank/compat filtering.

### Consequences for a proxy (both directions)

- **As a client**, fetch version/availability info = `GET /simple/{p}/` (JSON)
  and read `versions`/`files`. Don't ask upstream to resolve a specifier;
  resolve PEP 440 yourself (or forward the whole index). Read dependencies from
  `.metadata` rather than downloading wheels.
- **As a server**, to let `pip` resolve, the proxy must serve a **coherent Simple
  index**: every offered file with a correct `sha256`, `requires-python`, and
  `yanked` flag, plus `versions` (PEP 700). Names **must** be normalized and
  non-canonical requests 301-redirected.
- **Policy shapes availability.** A per-file/version policy decides what to
  serve. To *hide* a denied release, **omit its files** from the served index
  (the cleanest experience, `pip` simply never considers it). To *soft-block*,
  mark files `yanked` (keeps `==` pins working but drops them from ranges). To
  *hard-block*, serve the index but `403` the artifact fetch (a mid-resolution
  failure). A deny-by-default served index is, as with npm, a **filtered
  projection** of the upstream index.
- **`upload-time` is the age signal** for an age-based policy, it's in the
  Simple JSON (PEP 700) and the JSON API (`upload_time_iso_8601`), per file. Note
  age is **per file**, not per version (a version's wheels can be uploaded at
  different times).

---

## 9. Authentication (in theory)

No token is available, so this is grounded in docs.pypi.org plus the
observable fact that **all read endpoints are anonymous** (every probe above
succeeded with no credentials). Auth only gates *writes*.

### Reading

Public PyPI requires **no authentication** to read. Private indexes (e.g.
`devpi`, Artifactory, AWS CodeArtifact's PyPI endpoint, GitLab/GitHub package
registries) use **HTTP Basic** auth on the index URL. `pip` sources those
credentials from, in order: the URL itself
(`https://user:pass@host/simple/`), `keyring`, and `~/.netrc`, emitting
`Authorization: Basic <base64(user:pass)>`. There is **no** bearer/OTP/login
handshake for reads.

### Writing (upload)

> Endpoint: `POST https://upload.pypi.org/legacy/` (the "legacy" name is
> historical; it is the current upload API). `twine` is the reference client.

Two mechanisms:

1. **API tokens**, HTTP Basic auth with a fixed username and the token as
   password:
   ```
   username = __token__
   password = pypi-AgEIcHlwaS5vcmc…        # tokens are prefixed "pypi-"
   ```
   Tokens are created in the PyPI web UI (account- or project-scoped). Legacy
   username/password upload has been **removed**, `__token__` (or Trusted
   Publishing) is mandatory.
2. **Trusted Publishing (OIDC)**, a CI workflow (GitHub Actions, GitLab CI, …)
   presents an OIDC identity token to `POST /_/oidc/mint-token`, which returns a
   short-lived, tightly-scoped API token used for that one upload. No long-lived
   secret stored. This is the recommended modern path.

In `.pypirc` / `pip.conf`, credentials are configured per index URL (the analogue
of npm's per-registry nerf-dart), e.g.:

```ini
[pypi]
  username = __token__
  password = pypi-AgEIcHlwaS5vcmc…
```

### Implications for a proxy

- **Read proxy to public PyPI needs no credentials at all**, simpler than npm.
- **Private upstream**: forward/attach `Authorization: Basic …` (static token,
  or CodeArtifact's IAM-derived credential, same Basic shape, AWS-issued).
- **Mirror/upload request** (if the proxy ever publishes): `POST .../legacy/` with
  `__token__` Basic, or mint via OIDC.
- A proxy's **own** client gate is a separate concern; it need implement none of
  PyPI's write/auth endpoints to be a functional install index.
- Set a descriptive `User-Agent` on upstream requests (PyPI asks for it).

---

## 10. Write path (for completeness)

Not on the proxy's critical path (Écluse delegates storage; mirror writes are a
separate concern), but documented so "act as a Python index" is complete.

- **Upload**, `POST https://upload.pypi.org/legacy/`, `multipart/form-data`
  with `:action=file_upload`, `protocol_version=1`, the core-metadata fields
  (`name`, `version`, `metadata_version`, `requires_dist[]`, `requires_python`,
  …), the file `content`, and its `sha256_digest`. One request per file (sdist
  and each wheel uploaded separately). Re-uploading an existing filename is
  rejected (`400`, PyPI files are immutable; you cannot overwrite).
- **Yank/unyank**, done via the web UI / API, sets the PEP 592 flag (no public
  wire endpoint comparable to npm dist-tags).
- **Provenance**, attestations (PEP 740) are submitted alongside upload under
  Trusted Publishing and surfaced at `/integrity/{p}/{v}/{file}/provenance`.

---

## 11. Type model

A JSON type model for the wire format, sharing vocabulary with [`npm.md`
§11](npm.md#11-type-model) for easy comparison. Lenient on input (ignore unknown
keys, tolerate the HTML/JSON dual forms and the `core-metadata` /
`data-dist-info-metadata` alias), strict on output. ⚠️ marks a shape that
differs materially from the npm wire model.

### Shared scalars

```
NormalizedName  = string   -- PEP 503 normalized (lowercase, [-_.]+ → -)  ⚠️ npm has Scope+base instead
Pep440Version   = string   -- exact, e.g. "2.34.2"; opaque, never resolved server-side
Pep440Specifier = string   -- a requirement, e.g. ">=2,<4"; never resolved server-side
Pep508Req       = string   -- full dependency: name + specifier + extras + marker
ISODate         = string   -- ISO-8601 UTC (upload-time)
Hashes          = { sha256: string, md5?: string, blake2b_256?: string }
```

### `File` (⚠️ npm's single `Dist` becomes a list of these)

```
File = {
  filename:        string,          -- encodes tags (PEP 425/427)
  url:             string,          -- on files.pythonhosted.org
  packagetype:     "sdist" | "bdist_wheel",   -- drives the "prefer wheels" signal
  hashes:          Hashes,          -- integrity ← hashes.sha256
  requiresPython?: Pep440Specifier, -- interpreter gate
  size?:           number,
  uploadTime?:     ISODate,         -- the age signal (per file!)
  yanked:          boolean | string,-- closest analogue of npm's deprecated
  coreMetadata?:   boolean | { sha256: string },  -- PEP 658/714, .metadata exists
  provenance?:     string           -- PEP 740 attestation URL (≈ npm dist.signatures)
}
```

### `CoreMetadata` (the per-version manifest)

Parsed from the `.metadata` file or the JSON `info`:

```
CoreMetadata = {
  name:            NormalizedName,
  version:         Pep440Version,
  requiresPython?: Pep440Specifier,
  requiresDist:    [Pep508Req],     -- ⚠️ ranges carry markers/extras, not a flat name→range map (cf. npm)
  providesExtra?:  [string],
  license?:        string,          -- license_expression preferred (PEP 639)
  summary?:        string,
  classifiers?:    [string],        -- trove (license, status, supported Pythons)
  authorEmail?:    string           -- from author/maintainer/author_email
  -- no direct install-script field: derive risk from any file's packagetype == "sdist"
  -- the publish timestamp is NOT here, it comes from the File.uploadTime
}
```

### `SimpleIndex` (abbreviated analogue) and `ProjectJson` (full analogue)

```
SimpleIndex = {                 -- GET /simple/{p}/  (PEP 691/700)
  meta: { "api-version": string, "_last-serial"?: number },
  name: NormalizedName,
  versions: [Pep440Version],
  files: [File]
}

ProjectJson = {                 -- GET /pypi/{p}/json  (rich; releases deprecated)
  info: CoreMetadata & { yanked, yanked_reason, project_urls, ... },
  last_serial: number,
  releases?: { [Pep440Version]: [File] },   -- deprecated; prefer SimpleIndex
  urls: [File],                              -- latest version's files
  vulnerabilities: [Vulnerability],
  ownership?: object
}

VersionJson = ProjectJson without `releases`   -- GET /pypi/{p}/{v}/json
```

### Ancillary types

```
Vulnerability = {               -- OSV, inline in the JSON API
  id: string,                   -- e.g. "PYSEC-2018-28"
  aliases: [string],            -- ["CVE-…","GHSA-…"]
  fixed_in: [Pep440Version],
  link: string,                 -- osv.dev URL
  source: string,               -- "osv"
  withdrawn: ISODate | null
}
-- errors: PyPI returns bare 404 with no JSON envelope; model as HTTP status only.
-- auth (theory): API token = Basic("__token__", "pypi-…"); no token object on the wire.
```

> **Cross-ecosystem note.** The main shape differences from npm: (a) a version
> owns **N** artifacts here, not one (`Dist` → `[File]`); (b) names are PEP 503
> *normalized* rather than scoped; (c) the publish timestamp is per-file here
> vs. the packument `time` map in npm; and (d) the install-time-execution signal
> is `packagetype == sdist` rather than npm's `hasInstallScript`.

---

## 12. Implementing the protocol

### To be a believable Python **index** (answer `pip install`)

- [ ] `GET /simple/{p}/` honouring `Accept` → PEP 503 HTML **and** PEP 691 JSON,
      correct `Content-Type`.
- [ ] **Normalize** project names and **301-redirect** non-canonical requests.
- [ ] Coherent `files`/`versions` so the client can run PEP 440 + tag selection
      locally: correct `sha256`, `requires-python`, `yanked`, `size`,
      `upload-time` per file (§8).
- [ ] Serve (or rewrite to) artifacts on the **files host**, plus the
      **`.metadata`** companions, with immutable cache headers.
- [ ] Optionally serve the JSON API (`/pypi/{p}/json`, `/pypi/{p}/{v}/json`) and
      `vulnerabilities`.
- [ ] Plain `404` for unknown project/version; policy denials by **omitting**
      files (or `403` on the artifact).
- [ ] ETag / `X-PyPI-Last-Serial` so clients & mirrors revalidate cheaply.

### To be a correct Python **client** (fetch from upstreams)

- [ ] Normalize names (PEP 503); request Simple **JSON** + `Accept-Encoding: gzip`;
      set a descriptive `User-Agent`.
- [ ] Resolve PEP 440 specifiers, wheel tags (PEP 425), `requires-python`, and
      **exclude yanked** (PEP 592), all **locally**; never expect upstream to.
- [ ] Read dependencies from the **`.metadata`** companion (PEP 658) before
      downloading whole wheels.
- [ ] For private upstreams attach `Authorization: Basic …` (static or
      CodeArtifact-issued).
- [ ] Verify downloaded files against `sha256` before mirroring.
- [ ] Project upstream indexes into a **filtered** index reflecting policy
      decisions (omit/yank denied files, preserve `upload-time`).

---

## 13. Reproducing the probes

All captures on 2026-06-21 against `pypi.org` / `files.pythonhosted.org`.

```bash
# JSON API: project (rich) and one version
curl -s https://pypi.org/pypi/requests/json | jq '{top:keys, info_keys:(.info|keys), n_releases:(.releases|length)}'
curl -s https://pypi.org/pypi/requests/2.32.3/json | jq '{top:keys, packagetypes:[.urls[].packagetype]}'

# Simple API: HTML (default) vs PEP 691 JSON (content negotiation)
curl -sD - -o /dev/null https://pypi.org/simple/requests/ | grep -i '^content-type'         # text/html
curl -sD - -o s.json -H 'Accept: application/vnd.pypi.simple.v1+json' https://pypi.org/simple/requests/ | grep -i '^content-type'
jq '{meta, n_versions:(.versions|length), last_file:(.files[-1])}' s.json

# Registry resolves NO specifiers / unknown version → 404
curl -s -o /dev/null -w "%{http_code}\n" https://pypi.org/pypi/requests/0.0.0/json          # 404

# PEP 503 name normalization → 301 redirect
for n in Flask zope.interface typing_extensions; do
  curl -s -o /dev/null -w "$n -> %{http_code} %{redirect_url}\n" "https://pypi.org/simple/$n/"
done

# PEP 658/714 companion METADATA file (no wheel download)
W=$(curl -s https://pypi.org/pypi/requests/json | jq -r '[.urls[]|select(.packagetype=="bdist_wheel")][0].url')
curl -sI "${W}.metadata" | grep -iE '^(HTTP|content-length)'

# OSV vulnerabilities inline (an old, known-vulnerable pin)
curl -s https://pypi.org/pypi/requests/2.19.1/json | jq '.vulnerabilities[0]|{id,aliases,fixed_in,source}'

# Many files per version (89 for markupsafe 3.0.3): the multi-wheel reality
curl -s https://pypi.org/pypi/markupsafe/json | jq '{version:.info.version, n_files:(.urls|length)}'
```

---

## 14. References

- PyPI API overview / JSON / Index / Upload docs:
  <https://docs.pypi.org/api/>
- PEP 503, Simple Repository API (HTML, normalization):
  <https://peps.python.org/pep-0503/>
- PEP 691, JSON Simple API: <https://peps.python.org/pep-0691/>
- PEP 700, Additional fields for the JSON Simple API (`versions`, `size`,
  `upload-time`): <https://peps.python.org/pep-0700/>
- PEP 592, Yanked releases: <https://peps.python.org/pep-0592/>
- PEP 658 / PEP 714, serving & renaming the metadata file (`.metadata`,
  `data-core-metadata`): <https://peps.python.org/pep-0658/> ·
  <https://peps.python.org/pep-0714/>
- PEP 740, provenance / attestations: <https://peps.python.org/pep-0740/>
- PEP 440 (versions) · PEP 508 (dependency specifiers) · PEP 425/427 (wheel tags
  & format) · PEP 639 (license expression).
- API tokens & Trusted Publishing:
  <https://docs.pypi.org/trusted-publishers/>
- OSV (the `vulnerabilities` source): <https://osv.dev/>
- Internal: [`npm.md`](npm.md) (the parallel npm reference),
  [`../../architecture.md`](../../architecture.md).
```