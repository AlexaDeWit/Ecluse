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

Use Cargo 1.74+ and sparse HTTPS. Replace crates.io with the public view in `.cargo/config.toml`:

```toml
[source.crates-io]
replace-with = "ecluse"

[source.ecluse]
registry = "sparse+https://packages.example.com/cargo/"
```

If the replacement requires authentication, also name that index for Cargo's credential provider:

```toml
[registries.ecluse]
index = "sparse+https://packages.example.com/cargo/"
credential-provider = "cargo:token"
```

For private dependencies and publication, configure the alternate registry separately:

```toml
[registries.ecluse-private]
index = "sparse+https://packages.example.com/cargo/private/"
credential-provider = "cargo:token"
```

Set `registry = "ecluse-private"` on private dependencies.
Publish declared first-party crates with `cargo publish --registry ecluse-private`.
Keep the trailing slash on sparse index URLs.
Configure a credential provider for each named authenticated registry, including a replacement source when it requires authentication.
Use your approved provider or Cargo's token store. Keep tokens out of committed files.
Cargo sends the token directly in `Authorization`, without a Bearer prefix.

The public view contains only crates.io packages and completed managed mirrors.
It cannot supply private-only packages or declared first-party names.
The private view never falls back to crates.io.
Cargo can keep the historical crates.io GitHub registry URL in its lockfile while fetching through sparse source replacement.
Verify actual archive requests with fresh client state. A lockfile identity alone does not establish the network path.

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

## RubyGems clients {#rubygems}

Use Bundler 2.6.9 or 4.0.23 with the native registry source:

```ruby
source "https://packages.example.com/rubygems/"
```

Configure the caller's Basic credentials through Bundler's credential settings or your approved secret provider.
Keep credentials out of the Gemfile and committed Bundler configuration.
Use ordinary `bundle lock`, `bundle install`, and `bundle update`, without `--full-index`.
The registry returns `404` for the Compact Index probe so Bundler requests named dependency records.
Publication uses `gem push --host https://packages.example.com/rubygems` with a native API key and optional OTP.
Only declared first-party packages can publish to a configured publication target.
