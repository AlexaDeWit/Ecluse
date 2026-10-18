+++
title = "Verify your deployment"
description = "Check the paths you intend to use before moving your builds over. A running container is only the first check."

weight = 53

[extra]
nav_title = "Verify your deployment"
group = "Set up"
kind = "Guide"
reference = "/docs/operations/"
+++

## Check configuration and health {#config}

- Run `check-config` with the deployed environment. Resolve warnings for every role you run.
- Check `/livez` and `/readyz`. When advisories are configured, review the reported state of each ecosystem.
- If you use advisories, confirm each ecosystem loaded a database with a current publication time.
- Confirm the public URL in returned package metadata points back to Écluse.

Without an advisory store, readiness can return an empty `mounts` object. Verify the enabled registry routes with actual installs.

Readiness does not prove that every repository operation works. After the first advisory sync, it also does not prove the database remains fresh.

## Check an allowed and a denied request {#policy}

From a disposable project, install a known older package with fresh client state. Confirm the request appears in proxy logs.

In a trial deployment, add an explicit deny for that exact version:

**Trial config addition**

```yaml
rules:
  verify-denial:
    type: DenyByIdentity
    identity: is-number@7.0.0
    appliesTo: [admission]
```

Restart with the new policy and repeat the public request. It should fail, and the decision should identify `DenyByIdentity` and the denied package version. The response names the rule type, not the config entry `verify-denial`. Remove the trial rule afterwards.

Use a version absent from your private repositories for this check. Private copies do not pass through public admission again.

## Check private access {#private}

- An authorised caller can install a first-party package.
- An unauthorised caller cannot read that package.
- A missing first-party name returns a miss instead of resolving from the public registry.
- If publication is enabled, a test package reaches the publication repository and becomes readable through the private endpoint.

## Check mirroring {#mirror}

Request an admitted public version that is absent from the mirror. Confirm that the worker copies it and completes the job.

For PyPI, check the entire supported version inventory, including declared metadata companions. One uploaded wheel does not prove completion.

In a controlled test, make the public source unavailable and request a completed copy through Écluse. Use fresh client state and valid private credentials.

These are checks of the configured deployment. Do not disrupt a production upstream to run them.

## Check enforcement and monitoring {#enforcement}

- A representative CI job can install through Écluse but cannot fetch directly from public package hosts.
- Only intended clients can reach the proxy, including through internal container addresses.
- Advisory failures, stalled mirror work, and cleanup failures reach your monitoring system.
- Each operating role uses the intended configuration and its own workload identity.

Before enabling removal, preview it against both the mirror and private cache. Follow [Remove blocked packages](@/docs/remove.md).

{{ next(path="@/docs/operate.md", title="Operate and upgrade Écluse") }}
