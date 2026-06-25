# The npm Registry Protocol

A reverse-engineering reference for the npm registry HTTP API, focused on the
read path Écluse must proxy: **package metadata** (the *packument*) and
**package details** (the per-version *manifest*), plus the version-resolution
and authentication behaviours that surround them.

The objective is a faithful JSON type model (see [Type model](#11-type-model)) that
lets Écluse act as both an npm **client** (fetching from upstreams) and an npm
**server** (answering a real `npm` CLI).

> **Provenance.** Live examples were captured on **2026-06-21** against
> `https://registry.npmjs.org` with `curl`/`jq` (see
> [Reproducing the probes](#13-reproducing-the-probes)). Normative claims are
> backed by the npm registry docs ([npm/registry](https://github.com/npm/registry)),
> quoted inline. The public registry is fronted by Cloudflare and has drifted
> from the published spec in places; where they differ, both are noted and the
> **observed** behaviour wins for implementation.

---

## Table of contents

1. [Mental model](#1-mental-model)
2. [Transport & conventions](#2-transport--conventions)
3. [Endpoint catalogue](#3-endpoint-catalogue)
4. [Package metadata — the packument (full)](#4-package-metadata--the-packument-full)
5. [Abbreviated packument](#5-abbreviated-packument)
6. [Package details — the version manifest](#6-package-details--the-version-manifest)
7. [The `dist` object](#7-the-dist-object)
8. [Version & availability resolution](#8-version--availability-resolution)
9. [Authentication (in theory)](#9-authentication-in-theory)
10. [Write path (for completeness)](#10-write-path-for-completeness)
11. [Type model](#11-type-model)
12. [Implementing the protocol](#12-implementing-the-protocol)
13. [Reproducing the probes](#13-reproducing-the-probes)
14. [References](#14-references)

---

## 1. Mental model

The npm registry is, historically, a **CouchDB** database exposed over HTTP.
That heritage leaks through the protocol everywhere (`_id`, `_rev`,
`org.couchdb.user:`, the publish document shape), so it pays to keep in mind:

- A **package** is one CouchDB document, addressed by name at `/{name}`. That
  document — the **packument** ("package document") — embeds *every* published
  version's manifest under a `versions` map, plus package-level metadata and a
  `time` map of publish timestamps.
- A **version manifest** (what we call *package details*) is the per-version
  object: essentially the package's `package.json` at publish time, plus a few
  registry-injected fields (`dist`, `_npmUser`, …).
- A **tarball** is the actual artifact, served from a separate, immutable URL.
- **Resolution is the client's job.** The registry stores discrete versions and
  named tags; it does **not** understand semver ranges. `npm install lodash`
  works because the *client* downloads the packument and picks a version. This
  is the single most important fact for a proxy — see §8.

Three request shapes cover ~all of install traffic:

| Intent | Request |
|--------|---------|
| "What versions of X exist, and which is `latest`?" | `GET /{pkg}` (abbreviated) |
| "Give me the manifest for X@1.2.3" | embedded in the packument, or `GET /{pkg}/1.2.3` |
| "Give me the bytes for X@1.2.3" | `GET /{pkg}/-/{file}.tgz` |

---

## 2. Transport & conventions

### Base URL & scheme

- Default public registry: `https://registry.npmjs.org`. Always HTTPS; the
  public endpoint negotiates HTTP/2.
- A registry is identified solely by a base URL. Clients map an **unscoped**
  package to the `registry` config and a **scoped** package to an optional
  `@scope:registry` override — both are just base URLs the same protocol is
  spoken against. This is what lets Écluse insert itself: point `registry` at
  the proxy.

### Package-name encoding

| Name | On the wire |
|------|-------------|
| `is-odd` (unscoped) | `/is-odd` |
| `@babel/code-frame` (scoped) | `/@babel%2Fcode-frame` — the `/` is percent-encoded |

The canonical npm client percent-encodes the scope separator (`%2F`). The
public registry tolerates the raw form (`/@babel/code-frame` → `200`) too, but a
server implementation **must** accept the `%2F` form and should accept both. The
leading `@` is **not** encoded.

> Implementation note: depending on the client and the server's routing, the
> scope separator may arrive **already percent-decoded**, so
> `["@babel/code-frame"]` and `["@babel", "code-frame"]` are both possible —
> normalise early.

### Content negotiation (the key lever)

Metadata comes in **two formats**, selected by the `Accept` header:

| `Accept` | Response `Content-Type` | Format |
|----------|------------------------|--------|
| _absent_ or `application/json` | `application/json` | **full** packument |
| `application/vnd.npm.install-v1+json` | `application/vnd.npm.install-v1+json` | **abbreviated** packument |

> "If you provide no Accept header, the full document is returned. To request an
> _abbreviated_ document with only the fields required to support installation,
> set the `Accept` header … to `application/vnd.npm.install-v1+json`."
> — [package-metadata.md](https://github.com/npm/registry/blob/main/docs/responses/package-metadata.md)

The real `npm` CLI sends a quality-weighted header:

```
Accept: application/vnd.npm.install-v1+json; q=1.0, application/json; q=0.8, */*
```

Responses carry `Vary: accept-encoding, accept`, so caches key on both. A proxy
that caches **must** preserve this `Vary` (the full and abbreviated bodies are
different documents at the same URL).

### Compression

`Accept-Encoding: gzip` is honoured (`Content-Encoding: gzip`). Packuments for
popular packages are large (megabytes); always request gzip on the upstream
fetch.

### Caching & conditional requests

| Header (response) | Observed value | Meaning |
|-------------------|----------------|---------|
| `ETag` | `"5b3d535e31ab…"` (weak/strong opaque) | entity tag for the body |
| `Last-Modified` | RFC-1123 date | last change |
| `Cache-Control` | `public, max-age=300` (metadata) | metadata is cacheable ~5 min |
| `Cache-Control` | `public, immutable, max-age=31557600` (tarballs) | tarballs never change |

Conditional revalidation works: replaying the `ETag` as
`If-None-Match: "…"` returns **`304 Not Modified`** with no body. This is the
cheap freshness check a proxy should use against upstreams, and should *offer*
to its own clients.

Tarballs are **immutable** (a published `name@version` artifact never changes —
that's the integrity guarantee), so they can be cached forever; metadata cannot.

### Errors

Errors are JSON. The documented [Error object](https://github.com/npm/registry/blob/main/docs/user/authentication.md#error)
is `{ message?, error?, ok: false }`, and "Clients should check for `message`,
then `error`." Observed shapes:

| Situation | Status | Body |
|-----------|--------|------|
| Unknown package | `404` | `{"error":"Not found"}` |
| Unknown **version** (`GET /is-odd/99.99.99`) | `404` | `"version not found: 99.99.99"` ← bare JSON **string** |
| Unknown **range** (`GET /is-odd/^3.0.0`) | `404` | `"version not found: ^3.0.0"` |
| Unauthenticated protected route | `401` | `{"error":"Unauthorized"}` / `{}` |
| Wrong method | `405` | `{"code":"MethodNotAllowedError","message":"GET is not allowed"}` |

Note the inconsistency: the per-version 404 is a bare JSON string, not an
object. A lenient decoder must tolerate "JSON value that isn't the success
shape" rather than assuming `{error}`.

---

## 3. Endpoint catalogue

`✓` = exercised live on 2026-06-21; `▢` = documented / theory only.

### Read path (the proxy's hot path)

| Method | Path | Purpose | Auth |
|--------|------|---------|------|
| `GET` ✓ | `/{pkg}` | Full or abbreviated packument (by `Accept`) | none (public) |
| `GET` ✓ | `/{pkg}/{version}` | One version manifest (exact version) | none |
| `GET` ✓ | `/{pkg}/{dist-tag}` | One version manifest (e.g. `/{pkg}/latest`) | none |
| `GET` ✓ | `/{pkg}/-/{file}.tgz` | Tarball bytes (`application/octet-stream`) | none |
| `GET` ✓ | `/-/package/{pkg}/dist-tags` | The `dist-tags` map alone | none |
| `GET` ✓ | `/-/v1/search?text=…` | Search | none |
| `POST` ✓ | `/-/npm/v1/security/advisories/bulk` | Bulk advisories (CVE subsystem) | none |

### Auth & account (theory — no token available)

| Method | Path | Purpose |
|--------|------|---------|
| `PUT` ▢ | `/-/user/org.couchdb.user:{user}` | Legacy login → bearer token |
| `POST` ▢ | `/-/v1/login` | Web-login handshake (modern default) |
| `GET` ✓ | `/-/whoami` | Current user (`401` unauthenticated) |
| `GET`/`POST`/`DELETE` ▢ | `/-/npm/v1/tokens[/token/{hash}]` | Token list / create / delete |
| `POST` ▢ | `/-/npm/v1/user` | User update (incl. 2FA settings) |

### Write path (theory)

| Method | Path | Purpose |
|--------|------|---------|
| `PUT` ▢ | `/{pkg}` | Publish (body = packument + `_attachments`) |
| `PUT`/`DELETE` ▢ | `/-/package/{pkg}/dist-tags/{tag}` | Manage dist-tags |
| `DELETE` ▢ | `/{pkg}/-rev/{rev}` | Unpublish |

---

## 4. Package metadata — the packument (full)

`GET /{pkg}` with `Accept: application/json` (or no `Accept`). One document
describing the package and **all** its versions.

### Top-level fields

| Field | Type | Notes |
|-------|------|-------|
| `_id` | string | CouchDB id; equals `name`. |
| `_rev` | string | CouchDB revision, e.g. `17-b1f96ea3…`. Needed only for writes. |
| `name` | string | Package name (may be `@scope/name`). |
| `dist-tags` | object`<tag,version>` | Named pointers; **always** includes `latest`. e.g. `{"latest":"3.0.1"}`. |
| `versions` | object`<version, `[manifest](#6-package-details--the-version-manifest)`>` | Every published version, keyed by exact semver. |
| `time` | object`<key, ISO-date>` | `created`, `modified`, and one timestamp per version. The source of truth for **publish age**. |
| `maintainers` | [Person](#person)[] | Current maintainers. |
| `description` | string | Hoisted from the latest version. |
| `readme` | string | Rendered README (can be large). |
| `readmeFilename` | string | e.g. `README.md`. |
| `homepage` | string | URL. |
| `repository` | [Repository](#repository) \| string | SCM location. |
| `bugs` | object `{url?, email?}` \| string | Issue tracker. |
| `license` | string \| object | SPDX string, or legacy `{type,url}`. |
| `keywords` | string[] | Search keywords. |
| `author` | [Person](#person) \| string | |
| `contributors` | [Person](#person)[] | |
| `users` | object`<user, bool>` | Stars. |
| `_attachments` | object | Tarball attachments — **only** populated on the publish document; the GET response shows `{}` or omits it. Do **not** rely on it for reads. |

Package-level fields like `description`/`author`/`license` are **hoisted from
the `latest` version** for convenience; the authoritative per-version copy lives
in each manifest. A parser should treat top-level copies as a hint and prefer
the manifest.

### Why a proxy mostly avoids the full form

The full packument carries `readme`, every historical manifest with `scripts`,
`gitHead`, `_npmOperationalInternal`, etc. For a popular package this is
megabytes. The **abbreviated** form (§5) carries everything install needs at a
fraction of the size — prefer it for the proxy's metadata fetch, and only fall to
full when a field you need is full-only (notably `time`, see §8).

### Real example (trimmed — `is-odd`)

```json
{
  "_id": "is-odd",
  "_rev": "17-b1f96ea3c62e53b66584e3743a1945a3",
  "name": "is-odd",
  "dist-tags": { "latest": "3.0.1" },
  "time": {
    "created":  "2015-02-24T05:53:13.392Z",
    "modified": "2026-04-14T14:26:11.557Z",
    "3.0.1":    "2018-05-31T20:04:53.306Z"
  },
  "versions": { "3.0.1": { "...": "see §6" } },
  "maintainers": [ { "name": "jonschlinkert", "email": "github@sellside.com" } ],
  "license": "MIT",
  "description": "Returns true if the given number is odd…"
}
```

---

## 5. Abbreviated packument

`GET /{pkg}` with `Accept: application/vnd.npm.install-v1+json`. The
install-optimised view, and the one the proxy should treat as primary.

### Top-level fields (only four)

| Field | Type | Notes |
|-------|------|-------|
| `name` | string | |
| `modified` | ISO-date | Equivalent to `time.modified`; the full `time` map is **dropped**. |
| `dist-tags` | object`<tag,version>` | As in full. |
| `versions` | object`<version, `[abbreviated manifest](#abbreviated-version-object)`>` | |

> Top-level abbreviated fields are exactly `name`, `modified`, `dist-tags`,
> `versions` — confirmed live and in the spec.

### Abbreviated version object

Per the spec, **required**: `name`, `version`, `dist`. **Optional, present only
when relevant**:

`deprecated`, `dependencies`, `optionalDependencies`, `devDependencies`,
`bundleDependencies`, `peerDependencies`, `peerDependenciesMeta`,
`acceptDependencies`, `bin`, `directories`, `engines`, `_hasShrinkwrap`,
**`hasInstallScript`**, `funding`, `cpu`, `os`.

Two of these are decisive for install-time policy and deserve emphasis:

- **`hasInstallScript: true`** — present in the **abbreviated** form when the
  version declares `preinstall`/`install`/`postinstall` scripts. This is the
  single cleanest signal for an install-time code-execution policy.
  ⚠️ **It does not exist in the full manifest** — there you must derive it
  yourself from the `scripts` object (see §6). *Captured live:* `core-js@3.49.0`
  abbreviated → `"hasInstallScript": true`; the same version's full manifest has
  **no** `hasInstallScript` key, only `scripts.postinstall`.
- **`deprecated: "<message>"`** — a string deprecation notice, absent when not
  deprecated. *Captured live:* `request@2.88.2` →
  `"deprecated": "request has been deprecated, see …"`.

### Real example (`is-odd@3.0.1`, abbreviated)

```json
{
  "dist-tags": { "latest": "3.0.1" },
  "modified": "2026-04-14T14:26:11.557Z",
  "name": "is-odd",
  "versions": {
    "3.0.1": {
      "name": "is-odd",
      "version": "3.0.1",
      "dependencies": { "is-number": "^6.0.0" },
      "devDependencies": { "mocha": "^3.5.3", "gulp-format-md": "^1.0.0" },
      "engines": { "node": ">=4" },
      "dist": {
        "shasum": "65101baf3727d728b66fa62f50cda7f2d3989601",
        "tarball": "https://registry.npmjs.org/is-odd/-/is-odd-3.0.1.tgz",
        "integrity": "sha512-CQpnWPrDwmP1+SMHXZhtLtJv90yiyVfluGsX5iNCVkrhQtU3TQHsUWPG9wkdk9Lgd5yNpAg9jQEo90CBaXgWMA==",
        "fileCount": 4,
        "unpackedSize": 6510,
        "signatures": [ { "sig": "MEUCIQ…", "keyid": "SHA256:jl3bwswu80…" } ]
      }
    }
  }
}
```

---

## 6. Package details — the version manifest

The per-version object is the **version manifest** (what §1 called *package
details*) — the snapshot a policy evaluates. It is available two ways:

1. **Embedded**: `packument.versions["1.2.3"]` (one round trip for everything).
2. **Standalone**: `GET /{pkg}/{version}` or `GET /{pkg}/{dist-tag}` →
   `Content-Type: application/json`. Returns the **full** manifest (not the
   abbreviated one), even though it is a single version.

A manifest is essentially the package's published `package.json`, plus
registry-injected fields. Fields divide into three groups:

### (a) Author-supplied (from `package.json`)

`name`, `version`, `description`, `keywords`, `homepage`, `bugs`, `license`,
`author`, `contributors`, `maintainers`, `repository`, `main`, `bin`, `files`,
`directories`, `scripts`, `engines`, `dependencies`, `devDependencies`,
`peerDependencies`, `peerDependenciesMeta`, `optionalDependencies`,
`bundleDependencies`, `funding`, `cpu`, `os`, `type`, `exports`, `gitHead`, …
(arbitrary extra keys appear too — e.g. `is-odd` ships a `verb` tool-config
block; a decoder must ignore unknown keys).

### (b) Registry-injected (recognisable by the `_` prefix)

| Field | Meaning |
|-------|---------|
| `_id` | `name@version`, e.g. `is-odd@3.0.1`. |
| `_npmUser` | The publisher: `{name, email}`. **Provenance** — who actually pushed this version. |
| `_npmVersion` / `_nodeVersion` | Tool versions used to publish. |
| `_hasShrinkwrap` | Whether an `npm-shrinkwrap.json` ships. |
| `_npmOperationalInternal` | Registry bookkeeping (`{host, tmp}`); ignore. |
| `_shasum` / `_from` | Legacy; mirror of `dist.shasum` / install source. |

### (c) `dist` — the artifact descriptor

Always present; the gateway to the bytes and the integrity guarantee. Its own
section follows (§7).

### Deriving install-script presence from the full form

Because the full manifest has **no** `hasInstallScript`, derive it:

```
hasInstallScript  ≡  scripts has any of {preinstall, install, postinstall}
```

*Captured live:* `core-js@3.49.0` full manifest →
`"scripts": { "postinstall": "node -e \"…\"" }`, no `hasInstallScript` key.
Prefer the abbreviated `hasInstallScript`; fall back to this derivation when
only the full form is available.

### Resolving by tag

`GET /{pkg}/latest` returns the manifest the `latest` dist-tag points at
(`is-odd/latest` → `3.0.1`). Any dist-tag name works in the version slot; a
semver **range** does not (§8).

---

## 7. The `dist` object

The security-critical sub-object. Every version manifest (full and abbreviated)
carries it.

| Field | Type | Since | Meaning |
|-------|------|-------|---------|
| `tarball` | string (URL) | always | Absolute URL of the `.tgz`. |
| `shasum` | string (hex) | always | **SHA-1** of the tarball (legacy integrity). |
| `integrity` | string | 2017-04 | [SRI](https://w3c.github.io/webappsec-subresource-integrity/) string `"<alg>-<base64>"`, e.g. `sha512-…`. The modern integrity check; **prefer it** over `shasum`. |
| `fileCount` | number | 2018-02 | Files in the tarball. |
| `unpackedSize` | number | 2018-02 | Bytes unpacked. |
| `signatures` | array | 2022 | Registry **ECDSA** signatures: `[{ sig, keyid }]`. Verifiable against npm's published public keys (`GET /-/npm/v1/keys`) — the basis of `npm audit signatures`. |
| `npm-signature` | string | 2018-04 (legacy) | Old PGP signature; present on historical versions, no longer issued. |

### Integrity & the tarball URL

- The `tarball` URL points back at the registry host. **A proxy that rewrites
  `registry` to its own URL should consider rewriting `dist.tarball`** so
  clients fetch artifacts through the proxy too — otherwise the client resolves
  metadata via Écluse but pulls bytes straight from `registry.npmjs.org`,
  bypassing the gate. (Whether to rewrite is a policy decision; note it.)
- `integrity`/`shasum` are what a downloaded tarball is verified against. The
  client **fails the install** if the bytes don't match, so any mirror/rewrite
  must preserve the exact artifact.
- Tarball path convention: `/{pkg}/-/{basename}-{version}.tgz`. For scoped
  packages the basename drops the scope:
  `@babel/code-frame` → `/@babel/code-frame/-/code-frame-7.0.0.tgz`.

---

## 8. Version & availability resolution

> This section answers the explicit requirement: *"proxy requests that try to
> fetch version and availability information — like when someone tries to
> install a new package without specifying its version."*

### The core fact: the registry does not resolve ranges

The registry resolves only **exact versions** and **dist-tag names**. Semver
ranges are a **client-side** computation. Captured live:

| `GET /is-odd/{spec}` | Status | Body |
|----------------------|--------|------|
| `3.0.1` (exact) | `200` | manifest |
| `latest` (tag) | `200` | manifest |
| `^3.0.0` (range) | `404` | `"version not found: ^3.0.0"` |
| `~3.0.0` (range) | `404` | `"version not found: ~3.0.0"` |
| `3.x` (range) | `404` | `"version not found: 3.x"` |

So there is **no endpoint** that takes `lodash@^4` and hands back a version.

### What `npm install <pkg>` actually does

When a user runs `npm install lodash` (no version) or `npm install lodash@^4`:

1. **Fetch the packument** — `GET /lodash` with the abbreviated `Accept`. One
   request returns *all* versions, the `dist-tags`, and each version's `dist`
   and dependency ranges.
2. **Resolve locally** — the client computes the target version from the
   `versions` keys and `dist-tags`:
   - bare `npm install lodash` → the `latest` **dist-tag**.
   - `lodash@^4.17.0` → `semver.maxSatisfying(Object.keys(versions), "^4.17.0")`.
   - `lodash@next` → the `next` dist-tag.
   - `lodash@4.17.21` → that key directly (404-equivalent if absent).
3. **Recurse** — read the resolved version's `dependencies` (ranges), and repeat
   1–2 for each, building the dependency graph. (This is what makes `npm
   install` fan out into many packument GETs.)
4. **Fetch tarballs** — `GET dist.tarball` for each resolved version, verify
   `integrity`, unpack.

**Availability** of a version is therefore "is this key present in `versions`
(and reachable via a tag)?" There is no separate availability API — presence in
the packument *is* availability.

### Consequences for a proxy (both directions)

- **As a client**, fetching version/availability info = fetch the (abbreviated)
  packument and read `versions` + `dist-tags`. Don't try to ask the upstream to
  resolve a range; resolve it yourself (or just forward the whole packument).
- **As a server**, to let a client resolve, the proxy must return a **coherent
  packument**: `versions` containing every offered version's manifest, a
  `dist-tags.latest` that points at a key actually present, and matching `time`
  entries (clients/tools read `time` for age and "last published"). An empty or
  inconsistent `dist-tags`/`versions` breaks resolution.
- **Policy shapes availability.** A per-version policy decides what to serve. To
  *hide* a denied version, drop its key from `versions` (and `time`, and never
  let it be `latest`); to *block at fetch*, keep it listed but deny the
  tarball/manifest request with a 403. Dropping from the packument is the cleaner
  client experience (the version simply "doesn't exist" to
  `semver.maxSatisfying`); blocking the tarball yields a hard install error
  mid-resolution. A deny-by-default policy makes the served packument, in
  effect, a **filtered projection** of the upstream one.
- **`time` is full-only.** Age-based policy (e.g. "allow only versions published
  before a date") needs the publish timestamp, which is in the full packument's
  `time` map (and the standalone manifest is timeless). The abbreviated form
  gives only top-level `modified`. So the metadata pipeline needs the full
  packument (or to retain `time` when projecting) to know *when* a version was
  published.

### Adjacent discovery endpoints

- **dist-tags only**: `GET /-/package/{pkg}/dist-tags` → `{"latest":"3.0.1"}`.
  A cheap "what's current" probe (`npm dist-tag ls`).
- **search**: `GET /-/v1/search?text={q}&size={n}&from={k}` → `{ objects[],
  total, time }`, each object `{ package{name,version,description,date,links,
  publisher,maintainers,…}, score{final,detail{quality,popularity,maintenance}},
  searchScore, flags, downloads, dependents }`. Discovery, not install; Écluse
  can pass it through largely untouched.

---

## 9. Authentication (in theory)

No token is available, so this section is grounded in the official
[authentication doc](https://github.com/npm/registry/blob/main/docs/user/authentication.md)
(quoted) plus the unauthenticated responses we *could* observe.

### How credentials travel

> "Authentication can be provided in `Basic` or `Bearer` form. … One time passes
> may be provided using the `npm-otp` header."

| Scheme | Header | Form |
|--------|--------|------|
| Bearer (preferred) | `Authorization: Bearer <token>` | `<token>` is opaque. Legacy tokens are UUIDs; modern npm tokens are prefixed **`npm_…`** (classic & granular access tokens). |
| Basic | `Authorization: Basic <base64(user:pass)>` | Legacy username/password. |
| 2FA one-time pass | `npm-otp: <code>` | Six-digit TOTP (30 s window) **or** a recovery code, sent *alongside* Basic/Bearer. |

In `.npmrc`, the bearer token is stored against a **"nerf dart"** — the
registry URL minus scheme — so credentials are scoped per registry:

```ini
//registry.npmjs.org/:_authToken=npm_xxxxxxxx
@myscope:registry=https://registry.npmjs.org
```

The CLI turns `//host/:_authToken=…` into `Authorization: Bearer …` on requests
to that host. (`_auth` = base64 user:pass for Basic; `username`/`_password`
also exist — see `npm-registry-fetch` options `token`, `_authToken`,
`username`, `password`/`_password`, `otp`, `forceAuth`, `alwaysAuth`.)

### 2FA modes

> Two modes: **`auth-only`** (only password-bearing requests need an OTP — login,
> token create, any Basic request) and **`auth-and-writes`** (all `PUT`/`POST`/
> `DELETE` need an OTP, *except* starring and non-`latest` dist-tag changes).

A 2FA-required request without a valid `npm-otp` is rejected with `401`; the CLI
then prompts and retries with the header (`npm-registry-fetch`'s `otpPrompt`).

### Login flows

**Legacy (CouchDB) — `PUT /-/user/org.couchdb.user:{user}`**, no prior auth:

- Request body (Login Request): `{ name, password, readonly?, cidr_whitelist? }`.
- `201` → **Login Response** `{ token, ok: true, id, rev }` (the `id`/`rev` are
  "vestigial … should be ignored"). `token` is the bearer token.
- `401` on bad credentials. If the user has 2FA, a valid `npm-otp` is required.

**Web (modern default, `auth-type=web`) — `POST /-/v1/login`**: the CLI posts,
and a web-capable registry replies with `{ loginUrl, doneUrl }`; the CLI opens
`loginUrl` in a browser and polls `doneUrl` until it returns the token. If the
registry doesn't support it, the CLI falls back to the legacy flow.
*Observed:* an anonymous `POST /-/v1/login` to the public registry is gated
(`401 {"error":"You must be logged in to publish packages."}`), so the handshake
can't be completed without a real browser session — documented behaviour, not
reproducible here.

### Tokens (lifecycle)

| Method | Route | Result |
|--------|-------|--------|
| `GET` | `/-/npm/v1/tokens` | `Page` of `Token` objects (UUIDs redacted; `key` = sha512 hash shown). |
| `POST` | `/-/npm/v1/tokens` | Create; body `{ password, readonly?, cidr_whitelist? }` → `Token`. |
| `DELETE` | `/-/npm/v1/tokens/token/{hash}` | Revoke (cache eviction lags **~1 hour**). |

`Token` object: `{ token, key, cidr_whitelist, created, updated, readonly }`. A
`readonly` token authenticates only non-destructive methods (`GET`/`HEAD`) —
exactly the shape a *read-through proxy* like Écluse wants when calling a
private upstream.

*Observed unauthenticated:* `GET /-/whoami` → `401 {"error":"Unauthorized"}`;
`GET /-/npm/v1/tokens` → `401`; a bogus `Authorization: Bearer …` → `401 {}`.

### Implications for a proxy

- **Client side.** A proxy holds upstream credentials and attaches
  `Authorization: Bearer …` on the private-upstream and mirror requests. For
  **CodeArtifact**, the bearer token is a short-lived AWS-issued token refreshed
  via the SDK — same wire shape, different issuer.
- **Server side.** A proxy may require its own client auth (presented as
  `Bearer`/`_authToken` and validated at the edge before proxying), but it does
  **not** need to implement the login/token-lifecycle endpoints to be a
  functional install server — those are publish-time concerns.
- Forward `npm-otp` and `Authorization` transparently if the proxy ever carries
  write traffic; a read-only resilience gateway can ignore 2FA entirely.

---

## 10. Write path (for completeness)

Not on the proxy's critical path (Écluse delegates storage; mirror writes are a
separate concern), but documented so "act as an npm server" is complete.

- **Publish** — `PUT /{pkg}` with a body that is itself a packument:
  `{ _id, name, "dist-tags", versions: { "<v>": <manifest> }, _attachments: {
  "<pkg>-<v>.tgz": { content_type: "application/octet-stream", data:
  "<base64 tarball>", length } } }`. Conflicts (re-publishing an existing
  version) return `409`. *Observed:* an unauthenticated `PUT` to a non-existent
  package returns `404` (the registry obscures rather than `401`s).
- **dist-tags** — `PUT`/`DELETE /-/package/{pkg}/dist-tags/{tag}` to move/remove
  named tags (`npm dist-tag add/rm`).
- **deprecate** — no dedicated endpoint; `npm deprecate` re-publishes the
  packument with `deprecated: "<msg>"` set on the targeted versions (which is
  why `deprecated` surfaces in the abbreviated manifest, §5).
- **unpublish** — CouchDB-style `DELETE` against the package/revision.

---

## 11. Type model

A JSON type model for the wire format. Lenient on input (ignore unknown keys,
tolerate nulls and the string-vs-object license/bugs/person variants), strict on
output.

### Shared scalars

```
Person          = { name: string, email?: string, url?: string }   -- author/maintainer/contributor; may arrive as a bare string "Name <email> (url)"
Repository      = { type?: string, url: string } | string
Bugs            = { url?: string, email?: string } | string
License         = string (SPDX) | { type: string, url?: string }    -- legacy object form
SemverVersion   = string  -- exact, e.g. "1.2.3"; opaque, never resolved server-side
SemverRange     = string  -- a *dependency* spec, e.g. "^4.17.0"; never resolved server-side
DistTag         = string  -- "latest", "next", …
ISODate         = string  -- ISO-8601 UTC
```

### `Dist`

```
Dist = {
  tarball:        string,
  shasum?:        string,            -- SHA-1, legacy
  integrity?:     string,            -- SRI "sha512-…"
  fileCount?:     number,
  unpackedSize?:  number,
  signatures?:    [{ sig: string, keyid: string }],  -- registry ECDSA
  "npm-signature"?: string        -- legacy PGP
}
```

### `VersionManifest`

The per-version snapshot. Combine the full-form fields with the abbreviated-only
`hasInstallScript`:

```
VersionManifest = {
  name:           string,            -- parse @scope/base → scope + base
  version:        SemverVersion,
  dist:           Dist,
  dependencies?:        { [name]: SemverRange },
  devDependencies?:     { [name]: SemverRange },
  peerDependencies?:    { [name]: SemverRange },
  optionalDependencies?:{ [name]: SemverRange },
  bundleDependencies?:  [string],
  deprecated?:    string,
  license?:       License,
  maintainers?:   [Person],
  scripts?:       { [name]: string }, -- source for install-script derivation
  hasInstallScript?: boolean,        -- abbreviated-only; else derive from scripts
  engines?:       { [name]: string },
  bin?:           string | { [name]: string },
  cpu?: [string], os?: [string], funding?: …,
  _npmUser?:      Person,         -- provenance (publisher)
  _npmVersion?:   string, _nodeVersion?: string, _hasShrinkwrap?: boolean
  -- … ignore unknown keys (gitHead, exports, type, tool-config blocks, _*)
}

-- the publish timestamp is NOT in the manifest; it comes from packument.time[version] (see below).
```

### `Packument` (full) and `AbbreviatedPackument`

```
Packument = {
  _id: string, _rev?: string,
  name: string,
  "dist-tags": { [DistTag]: SemverVersion },   -- always has "latest"
  versions: { [SemverVersion]: VersionManifest },
  time: { created: ISODate, modified: ISODate, [SemverVersion]: ISODate },
  maintainers?: [Person], author?: Person, description?: string,
  readme?: string, readmeFilename?: string,
  homepage?: string, repository?: Repository, bugs?: Bugs,
  license?: License, keywords?: [string], contributors?: [Person],
  users?: { [user]: boolean }
  -- _attachments intentionally ignored on reads
}

AbbreviatedPackument = {
  name: string,
  modified: ISODate,
  "dist-tags": { [DistTag]: SemverVersion },
  versions: { [SemverVersion]: VersionManifest }   -- abbreviated subset of fields
}
```

### Resolution & ancillary types

```
DistTags        = { [DistTag]: SemverVersion }      -- GET /-/package/{pkg}/dist-tags
SearchResponse  = { objects: [SearchResult], total: number, time: ISODate }
SearchResult    = { package: {...}, score: {...}, searchScore: number, … }
ErrorResponse   = { error?: string, message?: string, ok?: false } | string   -- tolerate bare-string 404s
-- auth (theory):
LoginResponse   = { token: string, ok: true, id?: string, rev?: string }
Token           = { token: string, key: string, cidr_whitelist: [string]|null,
                    created: ISODate, updated: ISODate, readonly: boolean }
```

### Encoding rules (server output)

- `dist-tags` MUST contain a `latest` that is a key of `versions`.
- `time` MUST have a timestamp for every key in `versions`, plus
  `created`/`modified`.
- Preserve `dist.integrity`/`dist.shasum` **byte-for-byte** from the source —
  clients verify against them.
- Honour `Accept`: emit `application/vnd.npm.install-v1+json` (abbreviated
  subset) when requested, `application/json` (full) otherwise; set `Vary:
  accept, accept-encoding`.

---

## 12. Implementing the protocol

A checklist that turns this protocol into proxy obligations.

### To be a believable npm **server** (answer `npm install`)

- [ ] `GET /{pkg}` honouring `Accept` → full **and** abbreviated packument,
      with correct `Content-Type` and `Vary`.
- [ ] Coherent `versions` / `dist-tags.latest` / `time` so the client can run
      `semver.maxSatisfying` and tag lookups locally (§8).
- [ ] `GET /{pkg}/{version}` and `GET /{pkg}/{tag}` → single manifest;
      `404 "version not found: …"` for unknown/range specs.
- [ ] `GET /{pkg}/-/{file}.tgz` → tarball bytes, `application/octet-stream`,
      immutable cache headers; decide tarball-URL rewriting (§7).
- [ ] Scoped names via `%2F` (and tolerate raw `/`).
- [ ] `404 {"error":"Not found"}` for unknown packages; policy denials as
      `403 {"error": "…reason…"}`.
- [ ] Optional `GET /-/package/{pkg}/dist-tags`, `GET /-/v1/search` passthrough.
- [ ] ETag/`If-None-Match` → `304` support to keep clients cheap.

### To be a correct npm **client** (fetch from upstreams)

- [ ] Request abbreviated metadata (`Accept: …install-v1+json`) + `Accept-Encoding: gzip`.
- [ ] Fetch the **full** packument when `time`/publish-age is needed for policy.
- [ ] Resolve ranges/tags **locally**; never expect the upstream to.
- [ ] Attach `Authorization: Bearer` (static, or CodeArtifact-refreshed) per §9.
- [ ] Verify downloaded tarballs against `dist.integrity` before mirroring.
- [ ] Project upstream packuments into a **filtered** packument reflecting policy
      decisions (drop denied versions, fix `latest`, retain `time`).

---

## 13. Reproducing the probes

All captures on 2026-06-21 against `https://registry.npmjs.org`. Representative
commands (full set in the session that produced this doc):

```bash
# Full packument + headers
curl -s -D - -o is-odd.full.json https://registry.npmjs.org/is-odd
jq 'keys' is-odd.full.json

# Abbreviated packument (note Content-Type and the trimmed version object)
curl -s -D - -H 'Accept: application/vnd.npm.install-v1+json' \
     -o is-odd.abbr.json https://registry.npmjs.org/is-odd

# Single version manifest (full) and by-tag resolution
curl -s https://registry.npmjs.org/is-odd/3.0.1 | jq 'keys'
curl -s https://registry.npmjs.org/is-odd/latest | jq '{name,version}'

# Registry does NOT resolve ranges (the §8 finding)
for s in '^3.0.0' '~3.0.0' '3.x' 'latest' '3.0.1'; do
  curl -s -o /dev/null -w "%{http_code} $s\n" \
       "https://registry.npmjs.org/is-odd/$(jq -rn --arg s "$s" '$s|@uri')"
done

# Install-script marker: abbreviated has it, full does not
curl -s -H 'Accept: application/vnd.npm.install-v1+json' \
     https://registry.npmjs.org/core-js \
  | jq '.versions[(."dist-tags".latest)].hasInstallScript'
curl -s https://registry.npmjs.org/core-js/latest | jq '{scripts, hasInstallScript}'

# Conditional request → 304
ET=$(curl -sD - -o /dev/null https://registry.npmjs.org/is-odd | awk 'tolower($1)=="etag:"{print $2}')
curl -s -o /dev/null -w "%{http_code}\n" -H "If-None-Match: $ET" https://registry.npmjs.org/is-odd

# Auth surfaces (unauthenticated)
curl -s -w " [%{http_code}]\n" https://registry.npmjs.org/-/whoami
curl -s -X POST -H 'content-type: application/json' \
     -d '{"lodash":["4.17.4"]}' \
     https://registry.npmjs.org/-/npm/v1/security/advisories/bulk | jq '.lodash[0]|keys'
```

---

## 14. References

- npm/registry — package metadata response:
  <https://github.com/npm/registry/blob/main/docs/responses/package-metadata.md>
- npm/registry — user authentication:
  <https://github.com/npm/registry/blob/main/docs/user/authentication.md>
- npm/registry — REGISTRY-API, COUCHDB (CouchDB heritage, replication):
  <https://github.com/npm/registry/tree/main/docs>
- `npm-registry-fetch` — auth options on the wire (`token`, `_authToken`,
  `otp`, `forceAuth`, basic): <https://github.com/npm/npm-registry-fetch>
- npm CLI registry config (`registry`, `@scope:registry`, nerf-dart auth):
  <https://docs.npmjs.com/cli/v10/using-npm/registry>
- SRI (the `integrity` format):
  <https://w3c.github.io/webappsec-subresource-integrity/>
- Internal: [`../../architecture.md`](../../architecture.md) — how Écluse
  consumes these protocols.
