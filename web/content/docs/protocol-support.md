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
The [private access policy](@/docs/configuration.md#first-party-namespaces) gives Cargo's `401` challenge and `403` refusal.
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
Build scripts and procedural macros remain allowed. Cargo blocks active `DenyInstallTimeExecution`, `DenyIfDeprecated`, and `AllowScope` rules at boot.

Cargo mirrors retain the original admitted index row, source identity, and checksum-verified archive using durable version state.
Native-library `links` declarations must agree. Cargo defines omitted index `links` as null.
Target publication can reset time or yank and omit fields, so an archive alone cannot prove faithful source reads.
Missing, corrupt, or unrecorded state cannot supply a managed original-source copy during a public outage.
Ordinary compatible private reads remain available through the alternate view.
Retained fields reflect first admission. Écluse does not continuously synchronise later yanks or mutate the target's yank state.

Native publication checks length framing, archive root, manifest identity, and version before discovery or writes.
Dependency inventories, aliases, kinds, targets, registry sources, version requirements, and dependency features must agree with the effective archived manifest.
Package feature graphs, native-library `links` declarations, and the optional minimum Rust version must also agree, allowing native normalisation.
Upload dependencies must not carry the `artifact`, `bindep_target`, `lib`, or `public` fields of unstable Cargo features such as nightly `-Z bindeps`.
Écluse refuses such an upload with `400`, as mirroring refuses index rows that carry them.
Target comparison parses native cfg expressions under the configured nesting limit, including final commas in `all` and `any` lists.
Registry comparison uses native logical identities, preserving source kinds and sparse registry paths.
A null upload dependency registry identifies the selected private view. An absent manifest registry identifies crates.io.
Every dependency, development and build dependencies included, must come from crates.io or from Écluse's `/cargo/` or `/cargo/private/` index under `server.publicUrl`.
Écluse refuses a dependency from any other registry with `400` and names it.
Publish that dependency through Écluse first, then depend on Écluse's copy.
Écluse does not check crates that reach the private upstream by other means.
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
| `GET /versions` | Merges public and caller-authorised private discovery. Directory membership does not grant admission. |
| `GET /info/<gem>` | Returns admitted native rows, dependencies, runtime requirements and archive SHA-256 values. |
| `/api/v1/dependencies` | Returns `404`. The checksumless fallback is unsupported. |
| `GET /quick/Marshal.4.8/<coordinate>.gemspec.rz` | Returns the exact admitted coordinate's validated native compact specification. |
| `GET /gems/<coordinate>.gem` | Applies artifact admission and relays the selected archive. |
| `POST /api/v1/gems` | Validates and relays an unchanged first-party `.gem` archive. Returns `405` without configured publication. |

Read routes also accept `HEAD` under their response contracts.
Bundler verifies streamed archives against the SHA-256 values from admitted `/info` metadata, including the first installation without a lockfile.
Keep checksum validation enabled. The server verifies archive bytes during mirroring.
Private access refusal does not authorise public substitution.
HEAD authorises and checks availability without reading or hashing an archive body, and never enqueues mirroring.

The public `/versions` directory adds a global transfer and refresh cost before per-gem acquisition.
The public and private directories each accept at most 2,000,000 distinct gem names, and a directory past that ceiling fails its read.
The ceiling is fixed in Écluse and independent of `limits.maxVersionCount` and `limits.maxArtifactCount`, which bound per-gem `/info` reads.
Private discovery uses current caller credentials and remains outside public shared retention.
Private directory failure remains a failure rather than a public-only or stale conditional response.
When the public directory times out, fails, cools down or cannot be decoded, the reply serves the caller-authorised private rows alone.
With no private rows, the request fails with `502` or `503`.
A public directory past its byte bound or gem-name ceiling refuses the request with `502`, even beside private rows.
A malformed directory row is dropped and logged, and the rest of the directory serves.
Detailed metadata acquisition stays on demand. There is no full specification index, unfiltered fallback, or client yank relay.
Compact specification and archive locations point through Écluse.
A failed read stays a failure rather than an empty successful result.
The Dependencies API does not supply archive checksums to these Bundler clients.
Écluse refuses that fallback after either directory or per-gem metadata failure.

The [Compact Index format](https://guides.rubygems.org/rubygems-org-compact-index-api/) describes an MD5 hint for each info file.
Filtered or merged info bytes differ from upstream bytes, so forwarding an upstream MD5 can preserve stale client metadata.
Écluse emits an explicit blank hint when the transformed MD5 is unknown.
Bundler 2.6.9 and 4.0.23 then revalidate `/info` while retaining archive SHA-256 verification.
This is the tested client contract, not a claim of compatibility with every Compact Index client.
Responses use validators for the selected content coding.
`Repr-Digest` describes the decoded native file. Supported Bundler clients decompress gzip before checking that digest.
Modified directories and info files return complete `200` replacements when clients request byte ranges.
Matching validators return `304` after current caller authorisation, including ranged requests.
Client caches can retain removed names, metadata or installed packages. New archive requests still enforce current authorisation and admission.

[CodeArtifact](https://docs.aws.amazon.com/codeartifact/latest/ug/bundler-compatibility.html) limits `/versions` and `/names` to 5,000 gems.
It weights directory reads by the number of returned gems, so concurrent installs can reach read quotas at low HTTP request rates.
A directory failure does not remove unrelated backend capabilities, such as publication or maintenance.

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

RubyGems' own archive reader treats PAX and GNU extended tar headers as ordinary members.
So `POST /api/v1/gems` answers `400` when the outer gem tar carries a PAX (`x`, `g`) or GNU (`L`, `K`) extended header, or a member name split by a ustar prefix, because Écluse and the store could read different gem names.
Archives that `gem build` writes carry neither.

Mirroring copies every supported platform artifact of an admitted raw version, with original compact specifications and durable source facts.
The worker verifies exact archives and native identities without executing Ruby, constructing YAML objects, or extracting files to disk.
Target publication timestamps do not replace the original public age.
Current inventory, verified digests, and retained companions prove completion. A duplicate upload response or version name does not.
Missing or corrupt durable state cannot prove an original-source mirror during a public outage.
Registry management is separate: CodeArtifact inventory and physical deletion use existing worker authority, consent, and deletion caps.
A native yank is not physical deletion.
