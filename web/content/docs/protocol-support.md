+++
title = "Protocol support"
description = "Which registry protocols an Écluse server speaks, and what each endpoint answers, per ecosystem."
weight = 6
+++

The reference below is what an Écluse server speaks: every endpoint it answers, per
ecosystem, with the responses each one returns. Écluse renders the page from the OpenAPI
document it publishes as JSON, so the page and the document always agree.

## Supported registries

| Registry | Status |
| --- | --- |
| npm | Served, mirrored, and published |
| PyPI | Served, mirrored, and published |
| RubyGems | Planned |

A **served** registry answers reads: the metadata a client resolves against and the artifact
bytes it installs, both gated by the same rules, integrity floors, and egress controls.

[Complete PyPI setup](@/docs/pypi-setup.md) explains how to configure these capabilities together.

A `pypi` mount accepts native multipart publication when `publicationTarget` is configured.
The upload route otherwise answers `405`. The first-party guard validates every declared project
name before relaying the original bytes and Content-Type. Caller Basic credentials retain both
username and password, including an empty username.

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
The resolver verifies advertised companion digests on ordinary upstream reads. For mirrored versions, the worker verifies and retains declared companions, and Écluse verifies retained bytes before serving them.
Distribution bytes stay unchanged. The proxy hashes the complete upstream index for response
validators, including fields omitted from the served response.

A **planned** registry is already a valid `mounts` key, but no adapter answers its routes yet,
so activating one refuses the boot.

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
