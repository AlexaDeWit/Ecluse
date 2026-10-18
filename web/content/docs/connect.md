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
