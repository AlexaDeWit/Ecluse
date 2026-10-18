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

Configure a private upstream, then declare your package names. npm uses scopes. PyPI uses exact names or separator-bounded prefixes.

**Namespace declarations**

```yaml
mounts:
  npm:
    firstParty: "@acme"
  pypi:
    firstParty: "acme,acme-*"
```

These are additions to mounts with a configured `privateUpstream`. A first-party name never falls back to a public registry.

Use the same declarations in every role. Review existing public-derived copies before reserving a name: cleanup protects first-party names.

## Add a publication target {#publication}

Set `publicationTarget` to your first-party repository. For npm, clients publish to the npm mount. PyPI publishers use the `/pypi/legacy/` route.

For PyPI, configure the target’s exact native upload endpoint. Do not assume it is the same as its Simple index address.

The proxy forwards the publisher’s credential. It refuses publications outside the declared first-party names.

## Check both directions {#verify}

Publish a disposable package under a declared name. Verify it appears in the first-party repository and that an authorised client can install it through Écluse.

Check that an unauthorised caller cannot read it. Also check that a missing first-party package does not resolve from the public source.

Some registry management commands are outside the proxy’s contract. Manage tags, deprecations, and yanks at the publication repository as described in the [protocol reference](@/docs/protocol-support.md).
