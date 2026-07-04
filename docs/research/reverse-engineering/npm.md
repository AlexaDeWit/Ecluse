# The npm registry protocol

Reverse-engineering reference for the npm registry HTTP API, focused on the read
path Écluse proxies: the *packument* (package metadata), the per-version
*manifest* (package details), and the version-resolution and auth behaviours
around them. The goal is a JSON type model (see [Type model](#11-type-model)) that
lets Écluse act as both an npm client (fetching upstream) and an npm server
(answering a real `npm` CLI).

> **Provenance.** Live examples captured 2026-06-21 against
> `https://registry.npmjs.org` with `curl`/`jq` (see
> [Reproducing the probes](#13-reproducing-the-probes)); normative claims cite the
> [npm/registry](https://github.com/npm/registry) docs inline. The registry sits
> behind Cloudflare and has drifted from the spec; where they differ, observed
> behaviour wins.

---

## Table of contents

1. [Mental model](#1-mental-model)
2. [Transport & conventions](#2-transport--conventions)
3. [Endpoint catalogue](#3-endpoint-catalogue)
4. [Package metadata, the packument (full)](#4-package-metadata--the-packument-full)
5. [Abbreviated packument](#5-abbreviated-packument)
6. [Package details, the version manifest](#6-package-details--the-version-manifest)
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

The npm registry is historically a CouchDB database over HTTP; the heritage
leaks everywhere (`_id`, `_rev`, `org.couchdb.user:`, the publish document
shape).

- A **package** is one CouchDB document at `/{name}`: the **packument**, which
  embeds every version's manifest under a `versions` map plus a `time` map of
  publish timestamps.
- A **version manifest** (package details) is the per-version object: the
  package's `package.json` at publish time plus registry-injected fields
  (`dist`, `_npmUser`, …).
- A **tarball** is the artifact, served from a separate immutable URL.
- **Resolution is the client's job.** The registry stores discrete versions and
  named tags, not semver ranges. `npm install lodash` works because the client
  downloads the packument and picks. This is the key fact for a proxy (§8).

Three request shapes cover ~all of install traffic:

| Intent | Request |
|--------|---------|
| "What versions of X exist, and which is `latest`?" | `GET /{pkg}` (abbreviated) |
| "Give me the manifest for X@1.2.3" | embedded in the packument, or `GET /{pkg}/1.2.3` |
| "Give me the bytes for X@1.2.3" | `GET /{pkg}/-/{file}.tgz` |

---

## 2. Transport & conventions

### Base URL & scheme

- Default public registry: `https://registry.npmjs.org`. Always HTTPS, HTTP/2.
- A registry is just a base URL. Clients route an unscoped package via the
  `registry` config and a scoped package via an optional `@scope:registry`
  override, both the same protocol against different base URLs. Écluse inserts
  itself by pointing `registry` at the proxy.

### Package-name encoding

| Name | On the wire |
|------|-------------|
| `is-odd` (unscoped) | `/is-odd` |
| `@babel/code-frame` (scoped) | `/@babel%2Fcode-frame`, the `/` is percent-encoded |

The npm client percent-encodes the scope separator (`%2F`); the registry
tolerates the raw form (`/@babel/code-frame` → `200`) too. A server must accept
`%2F` and should accept both; the leading `@` is never encoded. Depending on
client and routing the separator may arrive already decoded, so both
`["@babel/code-frame"]` and `["@babel", "code-frame"]` are possible: normalise
early.

### Content negotiation

Metadata comes in **two formats**, selected by the `Accept` header:

| `Accept` | Response `Content-Type` | Format |
|----------|------------------------|--------|
| _absent_ or `application/json` | `application/json` | **full** packument |
| `application/vnd.npm.install-v1+json` | `application/vnd.npm.install-v1+json` | **abbreviated** packument |

The real `npm` CLI sends a weighted header:

```
Accept: application/vnd.npm.install-v1+json; q=1.0, application/json; q=0.8, */*
```

Responses carry `Vary: accept-encoding, accept`. A caching proxy **must**
preserve it: full and abbreviated are different documents at the same URL.

### Compression

`Accept-Encoding: gzip` is honoured. Packuments run to megabytes; always request
gzip upstream.

### Caching & conditional requests

| Header (response) | Observed value | Meaning |
|-------------------|----------------|---------|
| `ETag` | `"5b3d535e31ab…"` (weak/strong opaque) | entity tag for the body |
| `Last-Modified` | RFC-1123 date | last change |
| `Cache-Control` | `public, max-age=300` (metadata) | metadata is cacheable ~5 min |
| `Cache-Control` | `public, immutable, max-age=31557600` (tarballs) | tarballs never change |

Replaying the `ETag` as `If-None-Match` returns `304` with no body: the cheap
freshness check to use upstream and to offer downstream. Tarballs are immutable
(a published `name@version` never changes, the integrity guarantee) and cache
forever; metadata does not.

### Errors

Errors are JSON. The documented [Error object](https://github.com/npm/registry/blob/main/docs/user/authentication.md#error)
is `{ message?, error?, ok: false }`; check `message`, then `error`. Observed:

| Situation | Status | Body |
|-----------|--------|------|
| Unknown package | `404` | `{"error":"Not found"}` |
| Unknown **version** (`GET /is-odd/99.99.99`) | `404` | `"version not found: 99.99.99"` ← bare JSON **string** |
| Unknown **range** (`GET /is-odd/^3.0.0`) | `404` | `"version not found: ^3.0.0"` |
| Unauthenticated protected route | `401` | `{"error":"Unauthorized"}` / `{}` |
| Wrong method | `405` | `{"code":"MethodNotAllowedError","message":"GET is not allowed"}` |

Note the per-version 404 is a bare JSON string, not an object: a lenient decoder
must tolerate any non-success JSON value, not assume `{error}`.

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

### Auth & account (theory, no token available)

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

## 4. Package metadata, the packument (full)

`GET /{pkg}` with `Accept: application/json` (or none). One document covering
the package and all its versions.

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
| `_attachments` | object | Tarball attachments, **only** populated on the publish document; the GET response shows `{}` or omits it. Do **not** rely on it for reads. |

Package-level `description`/`author`/`license` are hoisted from the `latest`
version; the authoritative copy is per-manifest. Treat top-level copies as a
hint.

### Why a proxy mostly avoids the full form

The full form carries `readme` and every historical manifest (`scripts`,
`gitHead`, `_npmOperationalInternal`), megabytes for a popular package. The
abbreviated form (§5) has everything install needs at a fraction of the size:
prefer it, and fall to full only for a full-only field (notably `time`, §8).

### Real example (trimmed, `is-odd`)

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
install-optimised view and the proxy's primary fetch.

### Top-level fields (only four)

| Field | Type | Notes |
|-------|------|-------|
| `name` | string | |
| `modified` | ISO-date | Equivalent to `time.modified`; the full `time` map is **dropped**. |
| `dist-tags` | object`<tag,version>` | As in full. |
| `versions` | object`<version, `[abbreviated manifest](#abbreviated-version-object)`>` | |

### Abbreviated version object

Required: `name`, `version`, `dist`. Optional when relevant: `deprecated`,
`dependencies`, `optionalDependencies`, `devDependencies`, `bundleDependencies`,
`peerDependencies`, `peerDependenciesMeta`, `acceptDependencies`, `bin`,
`directories`, `engines`, `_hasShrinkwrap`, **`hasInstallScript`**, `funding`,
`cpu`, `os`.

Two are decisive for install-time policy:

- **`hasInstallScript: true`** when the version declares
  `preinstall`/`install`/`postinstall`, the cleanest code-execution signal.
  ⚠️ It is absent from the full manifest; derive it from `scripts` there (§6).
  Live: `core-js@3.49.0` abbreviated has `hasInstallScript: true`; its full
  manifest has only `scripts.postinstall`.
- **`deprecated: "<message>"`**, absent when not deprecated. Live:
  `request@2.88.2` → `"request has been deprecated, see …"`.

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

## 6. Package details, the version manifest

The version manifest is the snapshot a policy evaluates. Two ways to get it:

1. **Embedded**: `packument.versions["1.2.3"]`, one round trip.
2. **Standalone**: `GET /{pkg}/{version}` or `GET /{pkg}/{dist-tag}` → the
   **full** manifest (not abbreviated), even for a single version.

It is the published `package.json` plus registry-injected fields, in three
groups:

### (a) Author-supplied (from `package.json`)

`name`, `version`, `description`, `keywords`, `homepage`, `bugs`, `license`,
`author`, `contributors`, `maintainers`, `repository`, `main`, `bin`, `files`,
`directories`, `scripts`, `engines`, `dependencies`, `devDependencies`,
`peerDependencies`, `peerDependenciesMeta`, `optionalDependencies`,
`bundleDependencies`, `funding`, `cpu`, `os`, `type`, `exports`, `gitHead`, …
(arbitrary extra keys appear; ignore unknown ones).

### (b) Registry-injected (recognisable by the `_` prefix)

| Field | Meaning |
|-------|---------|
| `_id` | `name@version`, e.g. `is-odd@3.0.1`. |
| `_npmUser` | The publisher: `{name, email}`. **Provenance**, who actually pushed this version. |
| `_npmVersion` / `_nodeVersion` | Tool versions used to publish. |
| `_hasShrinkwrap` | Whether an `npm-shrinkwrap.json` ships. |
| `_npmOperationalInternal` | Registry bookkeeping (`{host, tmp}`); ignore. |
| `_shasum` / `_from` | Legacy; mirror of `dist.shasum` / install source. |

### (c) `dist`, the artifact descriptor

Always present: the bytes and the integrity guarantee (§7).

### Deriving install-script presence from the full form

The full manifest has no `hasInstallScript`; derive it:

```
hasInstallScript  ≡  scripts has any of {preinstall, install, postinstall}
```

Prefer the abbreviated field; fall back to this when only the full form is
available.

### Resolving by tag

`GET /{pkg}/latest` returns the manifest `latest` points at. Any dist-tag works
in the version slot; a semver range does not (§8).

---

## 7. The `dist` object

The security-critical sub-object, on every manifest (full and abbreviated).

| Field | Type | Since | Meaning |
|-------|------|-------|---------|
| `tarball` | string (URL) | always | Absolute URL of the `.tgz`. |
| `shasum` | string (hex) | always | **SHA-1** of the tarball (legacy integrity). |
| `integrity` | string | 2017-04 | [SRI](https://w3c.github.io/webappsec-subresource-integrity/) string `"<alg>-<base64>"`, e.g. `sha512-…`. The modern integrity check; **prefer it** over `shasum`. |
| `fileCount` | number | 2018-02 | Files in the tarball. |
| `unpackedSize` | number | 2018-02 | Bytes unpacked. |
| `signatures` | array | 2022 | Registry **ECDSA** signatures: `[{ sig, keyid }]`. Verifiable against npm's published public keys (`GET /-/npm/v1/keys`), the basis of `npm audit signatures`. |
| `npm-signature` | string | 2018-04 (legacy) | Old PGP signature; present on historical versions, no longer issued. |

### Integrity & the tarball URL

- The `tarball` URL points at the registry host. If a proxy rewrites `registry`
  to its own URL it should rewrite `dist.tarball` too, or clients resolve
  metadata via Écluse but pull bytes straight from `registry.npmjs.org`,
  bypassing the gate. Whether to rewrite is a policy call.
- The client fails the install if the bytes don't match `integrity`/`shasum`, so
  any mirror or rewrite must preserve the exact artifact.
- Path: `/{pkg}/-/{basename}-{version}.tgz`; scoped names drop the scope from the
  basename (`@babel/code-frame` → `/@babel/code-frame/-/code-frame-7.0.0.tgz`).

---

## 8. Version & availability resolution

### The core fact: the registry does not resolve ranges

The registry resolves only exact versions and dist-tag names; semver ranges are
client-side. Live:

| `GET /is-odd/{spec}` | Status | Body |
|----------------------|--------|------|
| `3.0.1` (exact) | `200` | manifest |
| `latest` (tag) | `200` | manifest |
| `^3.0.0` (range) | `404` | `"version not found: ^3.0.0"` |
| `~3.0.0` (range) | `404` | `"version not found: ~3.0.0"` |
| `3.x` (range) | `404` | `"version not found: 3.x"` |

No endpoint takes `lodash@^4` and returns a version.

### What `npm install <pkg>` actually does

`npm install lodash` or `npm install lodash@^4`:

1. **Fetch the packument**, `GET /lodash` (abbreviated): all versions,
   `dist-tags`, and each version's `dist` and dependency ranges in one request.
2. **Resolve locally** from `versions` keys and `dist-tags`:
   - bare `lodash` → the `latest` dist-tag.
   - `lodash@^4.17.0` → `semver.maxSatisfying(Object.keys(versions), "^4.17.0")`.
   - `lodash@next` → the `next` dist-tag.
   - `lodash@4.17.21` → that key directly.
3. **Recurse** into the resolved version's `dependencies` and repeat, which is
   why `npm install` fans out into many packument GETs.
4. **Fetch tarballs**, verify `integrity`, unpack.

Availability is therefore "is this key in `versions` and reachable via a tag?"
Presence in the packument is availability; there is no separate API.

### Consequences for a proxy (both directions)

- **As a client**, read `versions` + `dist-tags` from the abbreviated packument;
  resolve ranges yourself, never ask the upstream to.
- **As a server**, return a coherent packument: every offered version's manifest
  in `versions`, a `dist-tags.latest` pointing at a present key, and matching
  `time` entries. Inconsistent `dist-tags`/`versions` breaks resolution.
- **Policy shapes availability.** To hide a denied version, drop its key from
  `versions` and `time` (and never make it `latest`); the version simply
  "doesn't exist" to `semver.maxSatisfying`. To block at fetch, keep it listed
  and 403 the tarball, a hard mid-resolution error. Deny-by-default makes the
  served packument a **filtered projection** of the upstream.
- **`time` is full-only.** Age-based policy needs the publish timestamp from the
  full packument's `time` map; the abbreviated form gives only top-level
  `modified`. Fetch full, or retain `time` when projecting.

### Adjacent discovery endpoints

- **dist-tags only**: `GET /-/package/{pkg}/dist-tags` → `{"latest":"3.0.1"}`.
  A cheap "what's current" probe (`npm dist-tag ls`).
- **search**: `GET /-/v1/search?text={q}&size={n}&from={k}` → `{ objects[],
  total, time }`, each object `{ package{name,version,description,date,links,
  publisher,maintainers,…}, score{final,detail{quality,popularity,maintenance}},
  searchScore, flags, downloads, dependents }`. Discovery, not install; pass
  through untouched.

---

## 9. Authentication (in theory)

No token available, so this cites the official
[authentication doc](https://github.com/npm/registry/blob/main/docs/user/authentication.md)
plus the unauthenticated responses we could observe.

### How credentials travel

| Scheme | Header | Form |
|--------|--------|------|
| Bearer (preferred) | `Authorization: Bearer <token>` | `<token>` is opaque. Legacy tokens are UUIDs; modern npm tokens are prefixed **`npm_…`** (classic & granular access tokens). |
| Basic | `Authorization: Basic <base64(user:pass)>` | Legacy username/password. |
| 2FA one-time pass | `npm-otp: <code>` | Six-digit TOTP (30 s window) **or** a recovery code, sent *alongside* Basic/Bearer. |

In `.npmrc` the bearer token is keyed by "nerf dart", the registry URL minus
scheme, so credentials are per-registry:

```ini
//registry.npmjs.org/:_authToken=npm_xxxxxxxx
@myscope:registry=https://registry.npmjs.org
```

The CLI turns `//host/:_authToken=…` into `Authorization: Bearer …` for that
host. (`_auth` is base64 user:pass for Basic; see `npm-registry-fetch` options
`token`, `_authToken`, `username`, `password`, `otp`, `forceAuth`, `alwaysAuth`.)

### 2FA modes

Two modes: `auth-only` (only password-bearing requests need an OTP) and
`auth-and-writes` (all `PUT`/`POST`/`DELETE` need one, except starring and
non-`latest` dist-tag changes). Without a valid `npm-otp` a 2FA request gets
`401`; the CLI prompts and retries.

### Login flows

**Legacy (CouchDB), `PUT /-/user/org.couchdb.user:{user}`**, no prior auth:
body `{ name, password, readonly?, cidr_whitelist? }`; `201` →
`{ token, ok: true, id, rev }` (`id`/`rev` vestigial); `401` on bad credentials
or missing `npm-otp`.

**Web (modern default), `POST /-/v1/login`**: the registry replies
`{ loginUrl, doneUrl }`; the CLI opens `loginUrl` and polls `doneUrl` for the
token, falling back to legacy if unsupported. Anonymous `POST /-/v1/login`
against public is gated (`401 "You must be logged in to publish packages."`), so
the handshake needs a browser session.

### Tokens (lifecycle)

| Method | Route | Result |
|--------|-------|--------|
| `GET` | `/-/npm/v1/tokens` | `Page` of `Token` objects (UUIDs redacted; `key` = sha512 hash shown). |
| `POST` | `/-/npm/v1/tokens` | Create; body `{ password, readonly?, cidr_whitelist? }` → `Token`. |
| `DELETE` | `/-/npm/v1/tokens/token/{hash}` | Revoke (cache eviction lags **~1 hour**). |

`Token`: `{ token, key, cidr_whitelist, created, updated, readonly }`. A
`readonly` token authenticates only `GET`/`HEAD`, exactly what a read-through
proxy wants for a private upstream. Unauthenticated: `GET /-/whoami` → `401`,
`GET /-/npm/v1/tokens` → `401`, bogus bearer → `401 {}`.

### Implications for a proxy

- **Client side.** The proxy attaches `Authorization: Bearer …` on
  private-upstream and mirror requests. For CodeArtifact it's a short-lived
  AWS-issued token, same wire shape, different issuer.
- **Server side.** The proxy may gate its own clients (`Bearer`/`_authToken` at
  the edge) but need not implement login/token-lifecycle endpoints to serve
  installs; those are publish-time.
- Forward `npm-otp` and `Authorization` only if the proxy carries writes; a
  read-only proxy ignores 2FA.

---

## 10. Write path (for completeness)

Off the proxy's critical path (Écluse delegates storage), here for completeness.

- **Publish**, `PUT /{pkg}` with a packument body:
  `{ _id, name, "dist-tags", versions: { "<v>": <manifest> }, _attachments: {
  "<pkg>-<v>.tgz": { content_type, data: "<base64 tarball>", length } } }`.
  Re-publishing a version → `409`. Unauthenticated `PUT` to a missing package →
  `404` (obscured, not `401`).
- **dist-tags**, `PUT`/`DELETE /-/package/{pkg}/dist-tags/{tag}`.
- **deprecate**, no endpoint; `npm deprecate` re-publishes with `deprecated` set,
  which is why it surfaces in the abbreviated manifest (§5).
- **unpublish**, CouchDB-style `DELETE` on package/revision.

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
- Preserve `dist.integrity`/`dist.shasum` **byte-for-byte** from the source;
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

All captures 2026-06-21 against `https://registry.npmjs.org`.

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

- npm/registry, package metadata response:
  <https://github.com/npm/registry/blob/main/docs/responses/package-metadata.md>
- npm/registry, user authentication:
  <https://github.com/npm/registry/blob/main/docs/user/authentication.md>
- npm/registry, REGISTRY-API, COUCHDB (CouchDB heritage, replication):
  <https://github.com/npm/registry/tree/main/docs>
- `npm-registry-fetch`, auth options on the wire (`token`, `_authToken`,
  `otp`, `forceAuth`, basic): <https://github.com/npm/npm-registry-fetch>
- npm CLI registry config (`registry`, `@scope:registry`, nerf-dart auth):
  <https://docs.npmjs.com/cli/v10/using-npm/registry>
- SRI (the `integrity` format):
  <https://w3c.github.io/webappsec-subresource-integrity/>
- Internal: [`../../architecture.md`](../../architecture.md), how Écluse
  consumes these protocols.
