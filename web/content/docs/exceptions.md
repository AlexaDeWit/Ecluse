+++
title = "Allow a specific version"
description = "Add a named exception when you accept the reason a version is currently refused. Check which rules the exception will override."

weight = 30

[extra]
nav_title = "Allow a specific version"
group = "Operate and troubleshoot"
kind = "Guide"
reference = "/docs/configuration/#rule-policy"
+++

## Identify the rule first {#identify}

Read the denial and inspect the mount's resolved rule order. A quarantine exception and an override of a known-malicious denial have different effects.

Prefer an exact `package@version` identity. A bare package name matches every version, including future releases.

## Add the exception to the relevant mount {#change}

**Example: npm version exception**

```yaml
mounts:
  npm:
    rules:
      approved-version:
        type: AllowByIdentity
        identity: example-package@1.2.3
```

At its default precedence, this allow overrides the minimum age and scored CVSS/EPSS denies. It does not override the default MAL, npm install-script, npm external-dependency, or Cargo external-dependency denies.

An override above precedence `300` can bypass those denies. Use it only after reviewing that wider effect.

## Review and verify {#review}

- Record the reason and scope in your configuration review.
- Run `check-config` and inspect the resulting rule order.
- Roll the intended policy to the relevant roles, including cleanup before a relaxation can affect stored copies.
- Repeat the request with fresh client state and verify the deciding rule.

An exception does not waive artifact host or integrity checks.

## Removing the exception later {#remove}

A mirrored copy remains trusted. Deleting the allow is not itself a revocation.

Read [Admission, revocation, and retained copies](@/docs/revocation.md) before assuming the version becomes unavailable.
