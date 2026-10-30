+++
title = "Connect your package managers"
description = "Give clients the Écluse registry address. When you use a private registry, each caller also supplies their own credential."

weight = 27

[extra]
nav_title = "Connect package managers"
group = "Set up"
kind = "Guide"
reference = "/docs/deployment/#what-a-client-configures"
+++

## npm and compatible clients {#npm}

**.npmrc**

```ini
registry=https://packages.example.com/npm/
//packages.example.com/npm/:_authToken=${CODEARTIFACT_AUTH_TOKEN}
```

Omit the token line for a public-only proxy. The credential belongs to the private read repository when one is configured.

npm, pnpm, Yarn, and Bun use the npm protocol. Check each client's active registry settings, including project and scope overrides.

## Python clients {#python}

Use `https://packages.example.com/pypi/simple/` as the index URL in pip, uv, or Poetry.

For a CodeArtifact private repository, supply HTTP Basic authentication with username `aws` and the caller's token as the password.

Use your client's supported credential store or runtime secret injection. Keep credentials out of committed configuration.

**pip: public-only example**

```bash
python -m pip install \
  --index-url https://packages.example.com/pypi/simple/ \
  your-package
```

Do not configure a public `extra-index-url` as a fallback. It gives the client a route around the policy.

## Cargo clients {#cargo}

Use Cargo 1.74+ and sparse HTTPS. Écluse serves one registry at `/cargo/` that replaces crates.io, so
private crates and crates.io crates come from the same place. Name it in `.cargo/config.toml` and
replace crates.io with it:

```toml
[registries.ecluse]
index = "sparse+https://packages.example.com/cargo/"
credential-provider = "cargo:token"

[source.crates-io]
replace-with = "ecluse"
```

Developers and pipelines that publish also make it the default registry, so a plain `cargo publish`
goes to Écluse:

```toml
[registry]
default = "ecluse"
```

Every first-party crate restricts publication to Écluse in its `Cargo.toml`:

```toml
[package]
publish = ["ecluse"]
```

Without it, `cargo publish --registry crates-io`, which Cargo's own error message offers as a fix, packages
the crate through Écluse and uploads it to crates.io.

Declare private crates as plain dependencies, such as `acme-core = "1"`, never with
`registry = "ecluse"`. A dependency that names the registry is a second identity for the same crate,
which Cargo builds separately. Declare every private crate name or prefix as first-party
([first-party names](@/docs/private-packages.md#names)), so a crates.io crate of the same name never
merges into it.

Écluse's discovery document sets `auth-required` whenever the mount has a `privateUpstream` or
`server.authToken`. Cargo then needs a token from the credential provider for every request. Keep the
trailing slash on sparse index URLs, and keep tokens in your approved provider or Cargo's token store,
out of committed files. Cargo sends the token directly in `Authorization`, without a Bearer prefix.
When the private registry allows anonymous reads, give consumers a placeholder token, which Écluse
forwards and the registry ignores.

`Cargo.lock` records every crate, private crates included, under the crates.io source with the
checksum Écluse served. Builds resolve those entries through Écluse only. Verify actual archive
requests with fresh client state, because a lockfile identity alone does not establish the network
path.

## RubyGems clients {#rubygems}

Use Bundler 2.6.9 or 4.0.23 with the native registry source:

```ruby
source "https://packages.example.com/rubygems/"
```

Configure the caller's Basic credentials through Bundler's credential settings or your approved secret provider.
Keep credentials out of the Gemfile and committed Bundler configuration.
Use ordinary `bundle lock`, `bundle install`, and `bundle update`, without `--full-index`.
Bundler reads Compact Index `/versions` discovery and fetches admitted `/info` metadata for each required gem.
Keep Bundler checksum validation enabled.
Publication uses `gem push --host https://packages.example.com/rubygems` with a native API key and optional OTP.
Only declared first-party packages can publish to a configured publication target.

## Where credentials come from {#credentials}

Use the same registry credential the caller would use for the private repository. For CodeArtifact, the caller obtains a short-lived token through their AWS identity.

The proxy forwards this credential for private reads. It removes the credential before a public registry request. Worker credentials are separate and do not give callers private access.

A failed private authentication check is not a reason to try the public registry. See [Private reads and credentials](@/docs/private-reads.md).

## Check for routes around Écluse {#bypass}

- Review scope-specific registries, extra indexes, and environment overrides.
- Inspect lockfiles for direct artifact URLs.
- Use fresh client state when verifying, so a cached package does not stand in for a registry request.
- Restrict direct public package downloads from CI at the network layer.

Client configuration selects the proxy. The network policy enforces that choice.

{{ next(path="@/docs/verify.md", title="Verify your deployment") }}
