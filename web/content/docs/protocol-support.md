+++
title = "Protocol reference"
description = "Which registry protocols an Écluse server speaks, and what each endpoint answers, per ecosystem."
weight = 6
+++

Use this reference to check supported registry endpoints, retained metadata, and protocol limits.
The route tables render from the same OpenAPI document Écluse publishes as JSON.

## Supported registries

| Registry | Status |
| --- | --- |
| npm | Served, mirrored, and published |
| PyPI | Served, mirrored, and published |
| Cargo | Sparse HTTPS reads, mirroring, and first-party publication, Cargo 1.74+ |
| RubyGems | Named-gem reads, complete platform mirrors, and native first-party publication |

A **served** registry answers reads: the metadata a client resolves against and the artifact
bytes it installs, both gated by the same rules, integrity floors, and egress controls.

[Complete PyPI setup](@/docs/pypi-setup.md) explains how to configure these capabilities together.

A `pypi` mount accepts native multipart publication when `publicationTarget` is configured.
The upload route otherwise answers `405`. Caller Basic credentials retain both username and
password, including an empty username. The first-party guard relays the original bytes and
Content-Type only for an upload with these parts:

| Part | The relay requires |
| --- | --- |
| `:action` | Exactly one, with the value `file_upload`, which every uploader sends. Any other value, such as `remove_pkg` or `doc_upload`, answers `501`. |
| `name` | One or more, which all name the same project. |
| `version` | Exactly one, a PEP 440 version. |
| `content` | Exactly one file part, whose file name parses as a wheel or source distribution of that project at that version. |

An upload that breaks any other rule in the table answers `400`. The guard compares versions under
PEP 440 and wheel project names under PEP 503, so `Acme.Widget-1.0-py3-none-any.whl` matches
`acme-widget` at `1.0.0`. It reads the `content` part's file name, never the file data. An upload
also answers `400` when its file name:

- holds a character other than an ASCII letter or digit, `-`, `_`, `.`, `!` or `+`
- has a version that does not start with a digit, or that holds a `-`, which no build tool writes
- is not a wheel, and is not the PEP 625 source distribution name `{normalised_name}-{version}.tar.gz`

The normalised name is the PEP 503 name with `_` for each `-`. So `acme.co-widget` at `1.0`
uploads as `acme_co_widget-1.0.tar.gz`. Build tools that predate PEP 625 write other names, such as
`acme.co-widget-1.0.tar.gz` or a `.zip` archive. Their uploads answer `400` with the expected
form. The publisher must build with a tool that writes the PEP 625 name.

Some stores end a multipart part at a boundary delimiter after a bare CR or LF, or after trailing
whitespace on the delimiter line. So a body with such a delimiter inside a part answers `400`,
because the guard and the store would read different parts.

PyPI mirroring requires an explicit upload endpoint and a durable mirror-state store. It copies
all admitted supported wheels and source distributions for a version. A different recorded wheel
then serves from the hot target without another public admission. Completion requires current
per-file digest proof and all declared retained companions. Incremental physical uploads do not
make an incomplete version complete through Écluse. A registry that cannot provide usable file
identity cannot satisfy this proof.

The age rule measures a PyPI release from its newest file upload. Every offered file must
have a usable upload timestamp. If any timestamp is absent or malformed, the release age
stays unknown and the age rule cannot admit it. An explicit policy exception can still admit
the release, subject to the other rules and integrity floors.

PyPI indexes retain the supported installation fields: filename, URL, hashes, interpreter
constraint, size, upload time, yank status and provenance URL. The proxy also preserves the
project name, API version, serial, tracking declarations, alternate locations and project status.
It derives the served version list from the admitted releases.

The proxy retains canonical `core-metadata` declarations, including their exact digest names and values.
It omits unknown fields and the legacy `dist-info-metadata` and `data-dist-info-metadata` aliases.
A true declaration or a nonempty digest map enables `GET` and `HEAD` for the distribution's `.metadata` companion.
Absent, false and empty-map declarations enable no companion fetch.
Both wheel and source-distribution companions require the parent's origin-specific host and integrity checks.
The companion path checks edge authentication and private access, but does not evaluate package rules or enqueue a mirror.
The package client, such as pip, verifies advertised companion digests on ordinary upstream reads. For mirrored versions, the worker verifies and retains declared companions, and Écluse verifies retained bytes before serving them.
Distribution bytes stay unchanged. The proxy hashes the complete upstream index for response
validators, including fields omitted from the served response.

{{ generated(name="openapi") }}

The raw OpenAPI document is published at [/api/openapi.json](/api/openapi.json).

## npm metadata fields

Écluse retains metadata needed for installation, runtime resolution, policy evaluation and mirroring.
Unknown fields are skipped during extraction. Full `npm view` fidelity is not a supported contract.
The source tarball is unchanged. Served and mirrored metadata replace author lists with a short
pointer to the source registry's package document.

The version representation retains these supported fields:

| Use | Fields |
| --- | --- |
| Identity and policy | `name`, `version`, `dist`, `deprecated`, `hasInstallScript`, `scripts`, `license`, `_npmUser` |
| Dependency resolution | `dependencies`, `dependenciesMeta`, `acceptDependencies`, `devDependencies`, `optionalDependencies`, `peerDependencies`, `peerDependenciesMeta`, `bundleDependencies`, `bundledDependencies` |
| Runtime and type resolution | `main`, `module`, `browser`, `exports`, `imports`, `type`, `types`, `typings`, `typesVersions`, `sideEffects` |
| Installation and platforms | `engines`, `engineStrict`, `os`, `cpu`, `libc`, `bin`, `man`, `directories`, `gypfile`, `preferGlobal`, `_hasShrinkwrap` |
| Package configuration | `files`, `config`, `workspaces`, `packageManager`, `devEngines`, `publishConfig` |

The top-level document retains `name`, `versions`, `time` and `dist-tags`.
Dependency names, script names, engine names, export conditions, import mappings and type mappings
are protocol data, so their map entries remain available. Fixed schemas do not retain arbitrary keys:
publisher records retain `name`, `email` and `url`, and legacy licence objects retain `type` and `url`.
`dependenciesMeta` and `peerDependenciesMeta` retain each entry's `optional` flag and preserve its key.
Yarn uses [dependency optionality](https://yarnpkg.com/configuration/manifest#dependenciesMeta.optional)
from registry metadata throughout the dependency tree. Other dependency metadata flags are not retained.

`dist` retains `tarball`, `shasum`, `integrity`, `unpackedSize`, `fileCount`, `signatures` and
`attestations`. Signatures retain `keyid` and `sig`. Attestations retain `url` and
`provenance.predicateType`. Mirror publication still replaces artifact coordinates and verified
hashes, and removes signatures that belong to the source registry.

`directories` retains `lib`, `bin`, `man`, `doc`, `example` and `test`. `devEngines` retains
`cpu`, `os`, `libc`, `runtime` and `packageManager`, with `name`, `version` and `onFail` in each entry.
The object form of `workspaces` retains `packages` and `nohoist`.
`publishConfig` retains `registry`, `tag`, `access`, `provenance`, `ignore-scripts`, `directory`,
`linkDirectory`, `executableFiles`, `main`, `module`, `types`, `typings`, `exports`, `imports`,
`bin` and `browser`.

The installation fields follow [npm's package metadata contract](https://docs.npmjs.com/cli/v11/configuring-npm/package-json/).
Adding support for another field requires an explicit compatibility change.

The [registry metadata specification](https://github.com/npm/registry/blob/main/docs/responses/package-metadata.md#abbreviated-version-object)
identifies `acceptDependencies` and `_hasShrinkwrap` as installation inputs. Mirroring preserves the
shrinkwrap marker while removing other source-registry bookkeeping.

## Cargo sparse registries {#cargo}

Cargo uses two views under one mount. [Connect Cargo](@/docs/connect.md#cargo) gives the client configuration.

| View | Source and operations |
|---|---|
| `/cargo/` | crates.io source replacement. Reads crates.io and valid completed managed mirrors. Excludes private-only additions and declared first-party names. No publication API. |
| `/cargo/private/` | Alternate registry. Reads arbitrary caller-authorised private packages and accepts configured first-party publication. No public fallback. |

Both views serve `config.json`, native sparse index paths, and local crate archive routes.
Every emitted download location points through Écluse.
Git index serving is unsupported. Cargo can retain its canonical crates.io registry identity in lockfiles, including the historical GitHub URL.
That identity does not select Git transport when source replacement uses a sparse HTTPS index.

The private discovery response advertises `api` only when publication is configured.
Its API base is `https://packages.example.com/cargo/private`.
Cargo appends `/api/v1/crates/new`, producing `PUT /cargo/private/api/v1/crates/new`.
Do not append another `/api` to the configured client registry.
The proxy supports publication, not registry management operations such as yank or unyank.

Discovery runs against the selected origin after the operation obtains its caller or service credential.
Boot performs no registry discovery. Private discovery stays local to the request.
Local `auth-required` combines the edge requirement and the selected trusted origin's declaration.
Without a trusted origin, public discovery answers locally.
It never contacts crates.io merely to construct that local response.
An anonymous native `401` returns a Cargo challenge so the credential provider can retry.
A trusted origin's `403` remains a refusal.
Discovery failure stays explicit. A new client can fail to bootstrap while its configured trusted registry is unavailable.

The index reader retains admitted native records, their order, raw versions, checksums, and additive fields.
It preserves dependencies, renames, features, targets, source identities, schema, Rust-version constraints, yank, and publication time.
Omission and explicit null remain distinct.
It skips blank lines and processes a final record without LF.
It quarantines malformed rows and every occurrence of a duplicate native version identity.
Transport and resource failures fail the whole read.

Published spelling, lowercase lookup identity, and case-insensitive hyphen/underscore ownership matching have separate uses.
Hyphen and underscore aliases do not merge distinct index lookup paths.
Cargo versions use native SemVer identity and ordering. Build metadata does not create a second publishable version identity.

The age rule uses native `pubtime`. Missing or invalid time stays unknown.
OSV `crates.io` advisories use the existing freshness, malicious-package, and exact-fix rules.
The default age and advisory path makes no archive evidence request.
An explicit `DenyInstallTimeExecution` rule inspects bounded, checksum-verified archive and TOML evidence only when ordered evaluation reaches it.
Higher-ranked decisions and first-party treatment retain their precedence.
Required unavailable evidence produces structured inability. Écluse never runs the inspected code.

Cargo mirrors retain the original admitted index row, source identity, and checksum-verified archive using durable version state.
Native-library `links` declarations must agree. Cargo defines omitted index `links` as null.
Target publication can reset time or yank and omit fields, so an archive alone cannot prove faithful source reads.
Missing, corrupt, or unrecorded state cannot supply a managed original-source copy during public outage.
Ordinary compatible private reads remain available through the alternate view.
Retained fields reflect first admission. Écluse does not continuously synchronise later yanks or mutate the target's yank state.

Native publication checks length framing, archive root, manifest identity, version, and digest before discovery or writes.
Dependency inventories, aliases, kinds, targets, registry sources, version requirements, and dependency features must agree with the effective archived manifest.
Package feature graphs, native-library `links` declarations, and the optional minimum Rust version must also agree, allowing native normalisation.
Target comparison parses native cfg expressions under the configured nesting limit, including final commas in `all` and `any` lists.
Registry comparison uses native logical identities, preserving source kinds and sparse registry paths.
A null upload dependency registry identifies the selected private view. An absent manifest registry identifies crates.io.
Descriptive and additive metadata need not match the manifest. Accepted uploads retain their original bytes.
The sparse read root and approved upload API root remain distinct.
Discovery must advertise the exact approved API authority and base path before Écluse attaches the publication credential.
CodeArtifact supplies Cargo inventory and physical deletion through the existing maintenance interface.
A generic Cargo API yank is not physical deletion. Maintenance support depends on the store's capabilities.

## RubyGems and Bundler {#rubygems}

The client baseline is Bundler 2.6.9 and 4.0.23 for registry-based `bundle lock`, `bundle install`, and `bundle update`.
Historical full-index clients and `--full-index` are outside this contract.
Git and local-path dependencies do not acquire registry admission guarantees.

| Route below `/rubygems/` | Behaviour |
|---|---|
| `GET /versions` | Returns `404`, selecting the supported Bundler Dependencies API path. |
| `GET /api/v1/dependencies` | Answers the native capability probe. |
| `GET /api/v1/dependencies?gems=a,b` | Returns admitted native Marshal records for at most 100 named gems. |
| `GET /quick/Marshal.4.8/<coordinate>.gemspec.rz` | Returns the exact admitted coordinate's validated native compact specification. |
| `GET /gems/<coordinate>.gem` | Applies artifact admission and relays the selected archive. |
| `POST /api/v1/gems` | Validates and relays an unchanged first-party `.gem` archive. Returns `405` without configured publication. |

Read routes also accept `HEAD` under their response contracts.
A `.gem` GET buffers bounded transient bytes and verifies the advertised SHA-256 before returning artifact success.
This covers public-only mounts and caller-authorised private reads, without requiring mirroring or a persistent artifact cache.
A local strong ETag can produce `304` only after the GET fetches and verifies the complete body.
Origin `304`, partial responses, nonidentity coding, and corrupt bytes cannot bypass verification.
Private access refusal or a private digest mismatch does not substitute public bytes.
HEAD checks the selected origin's authorisation and availability before local conditional evaluation.
It fetches no archive body, computes no archive hash, and enqueues no mirroring.
Its success does not prove body verification.
`limits.maxArtifactBytes` also controls this verified-read cap, under the resolved transient capacity.
Verified downloads delay the first byte until verification ends and use memory proportional to the bounded archive.
There is no registry-wide catalogue, full specification index, unfiltered fallback, or client yank relay.
Public acquisition uses per-gem `/info/<gem>` metadata and selected native requests.
Compact specification and archive locations point through Écluse.
A batch shares cumulative limits and one deadline. A failed read stays a failure, rather than an empty successful result.

The named Dependencies API carries no source archive hashes. Installed archives can populate lockfile hashes.
Bundler 4.0.23 leaves foreign platform checksum entries empty after ordinary `bundle lock` and `bundle cache` commands.
Complete existing frozen lockfiles remain supported, including their platform hashes.
These client limits do not change the server's archive verification.

Names, raw versions, and platform labels retain their native identity.
Dependencies, version constraints, required Ruby and RubyGems versions, checksums, and supported source fields remain available.
Malformed records and conflicting coordinates cannot supply an admitted version.
Native builds remain allowed. RubyGems blocks active `DenyInstallTimeExecution`, `DenyIfDeprecated`, and `AllowScope` rules at boot.
Unknown installation metadata does not prove that an archive executes no code.

Private reads preserve the caller's complete Basic credential pair.
Publication uses the complete native API-key header value and an optional validated `OTP` header.
A declared CodeArtifact service role uses Basic with username `aws` for reads and Bearer for publication.
The backend declaration selects this presentation, not a hostname guess.
Static publication credentials retain the required edge-token guard.

Mirroring copies every supported platform artifact of an admitted raw version, with original compact specifications and durable source facts.
The worker verifies exact archives and native identities without executing Ruby, constructing YAML objects, or extracting files to disk.
Target publication timestamps do not replace the original public age.
Current inventory, verified digests, and retained companions prove completion. A duplicate upload response or version name does not.
Missing or corrupt durable state cannot prove an original-source mirror during a public outage.
Registry management is separate: CodeArtifact inventory and physical deletion use existing worker authority, consent, and deletion caps.
A native yank is not physical deletion.
