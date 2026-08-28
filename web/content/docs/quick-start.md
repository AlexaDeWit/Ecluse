+++
title = "Quick start"
description = "Run Écluse as a serve-only gate on the public npm registry, then point a package manager at it."
weight = 2
+++

The fastest way to put the gate in front of real installs is a **serve-only** deployment. It needs
no private registry, no mirror, no queue, and no cloud account.

```bash
ECLUSE_MOUNTS__NPM__ENABLED=true \
ECLUSE_SERVER__PUBLIC_URL=http://127.0.0.1:8080 \
ecluse proxy
```

Then point your package manager at the npm mount. For an npm-protocol client:

```ini
# .npmrc
registry=http://127.0.0.1:8080/npm/
```

Every rule, advisory gate, integrity floor, and egress control applies exactly as on a mirrored
deployment. Only the mirror write is missing. The trade: the public leg is permanent, availability
stays coupled to the public registry, and no mirrored copy survives an upstream yank. Use it to
evaluate the gate, then graduate to the [recommended topology](@/docs/deployment.md#the-recommended-topology) by
declaring a `mirrorTarget`.
