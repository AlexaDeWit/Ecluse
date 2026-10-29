+++
title = "Remove blocked packages"
description = "Use the cleanup service to remove stored versions that a named revocation rule denies. Its command is ecluse dredger."

weight = 45

[extra]
nav_title = "Remove blocked packages"
group = "Operate and troubleshoot"
kind = "Guide"
reference = "/docs/dredger/"
+++

{% note(title="Deletion can remove the only surviving copy", warning=true) %}
An upstream registry may no longer hold the version. Review the exact package, version, and target repositories before enabling deletion.
{% end %}

## Prepare the policy and targets {#prepare}

Add the deny to the intended policy and make sure it applies at revocation. Update the proxy and mirror workers before removing stored copies.

The mirror repository and private cache each need independent maintenance credentials and consent. Keep the first-party publication repository separate.

For PyPI, Cargo, and RubyGems, cleanup requires CodeArtifact. Generic registries for those ecosystems can still support reads and writes, but do not provide this maintenance capability.

## Preview the result {#preview}

Run the configured image with:

**Container arguments**

```bash
dredger --dry-run --once
```

The preview observes both target inventories and reports what policy selects. It needs no deletion consent and does not delete or write a walk cursor.

Review target-specific results and incomplete scans. Exit `0` means the preview completed, not that deletion is authorised.

## Enable deletion deliberately {#consent}

For CodeArtifact, set `ecluse-dredger-consent=true` on each maintained repository. For Verdaccio, configure `permitDeletion: true` separately for each supported target.

The cleanup role must not be able to grant itself consent. Give it only the inventory, read, and deletion permissions needed for those targets.

Review the deletion cap, then run `dredger --once` for one cycle or `dredger` continuously. Run only one instance per store.

## Verify each serving copy {#verify}

Check the mirror and private-cache results independently. A successful mirror deletion does not prove the cached copy is gone.

After older writes settle, inspect inventories and repeat authorised reads with fresh client state. A permission failure is not proof of absence.

Client caches and already-installed packages remain outside this operation.

## If the deletion cap stops the service {#halt}

Reaching the cap halts cleanup for the process's lifetime. Liveness stays healthy and readiness reports the halt.

Investigate the policy and advisory generation before restarting or increasing the cap. An automatic restart would reset the deletion budget.

{{ task_link(path="@/docs/revocation.md", title="Read the revocation rules", description="Decisive denies, protected names, retained copies, and rollout order.") }}

{{ task_link(path="@/docs/dredger.md", title="Read the complete cleanup reference", description="Cycle budgets, full walks, consent, backend limits, and recovery.") }}
