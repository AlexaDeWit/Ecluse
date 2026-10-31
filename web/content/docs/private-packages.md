+++
title = "Use and publish private packages"
description = "Reserve the names your organisation owns, and keep their reads and publications on private repositories."

weight = 40

[extra]
nav_title = "Use private packages"
group = "Set up"
kind = "Guide"
reference = "/docs/configuration/#first-party-namespaces"
+++

## Declare first-party names {#names}

Configure a private upstream, then declare your package names. npm uses scopes.
PyPI, Cargo, and RubyGems use exact names or separator-bounded prefixes.

**Namespace declarations**

```yaml
mounts:
  npm:
    firstParty: "@acme"
  pypi:
    firstParty: "acme,acme-*"
  cargo:
    firstParty: "acme-tools,acme-*"
  rubygems:
    firstParty: "acme-tools,acme-*"
```

These are additions to mounts with a configured `privateUpstream`. A first-party name never falls back to a public registry.

Cargo declarations fold ASCII case and equate hyphen with underscore while preserving separator runs.
A prefix must end in `-*` or `_*`. `acme-*` covers `acme-tools` and `acme_tools`, but excludes `acmeco`.
Cargo's `/cargo/` registry merges `privateUpstream` with crates.io, so declare every private crate name or prefix.
A declared crate reads `privateUpstream` only, and its crates.io namesake never reaches clients.

RubyGems declarations preserve case, dots, hyphens, and underscores.
A prefix ends in `.*`, `-*`, or `_*`.

Use the same declarations in every role. Review existing public-derived copies before reserving a name: cleanup protects first-party names.

## Add a publication target {#publication}

Set `publicationTarget` to your first-party repository. For npm, clients publish to the npm mount. PyPI publishers use the `/pypi/legacy/` route.

For PyPI, set `url` to the target's registry root and `uploadUrl` to its native upload endpoint.

Cargo clients publish with a plain `cargo publish` to `/cargo/` after [setting Écluse as the default registry](@/docs/connect.md#cargo), and each first-party crate sets `publish = ["ecluse"]`.
Set `url` to the target's sparse read root and `uploadUrl` to its exact approved API base.
The discovery API base must match `uploadUrl`, including its path.

RubyGems clients publish `.gem` archives to `/rubygems/api/v1/gems` with their native API key and optional `OTP` header.

The proxy uses a configured static publication credential behind an authenticated edge.
Otherwise, it forwards the publisher's credential.
It refuses publications outside the declared first-party names.

## Check both directions {#verify}

Publish a disposable package under a declared name. Verify it appears in the first-party repository and that an authorised client can install it through Écluse.

Check that an unauthorised caller cannot read it. Also check that a missing first-party package does not resolve from the public source.

Some registry management commands are outside the proxy's contract. Manage tags, deprecations, and yanks at the publication repository as described in the [protocol reference](@/docs/protocol-support.md).
