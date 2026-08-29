+++
title = "Quick start"
description = "Run Écluse as a serve-only gate on the public npm registry, then point a package manager at it."
weight = 2
+++

The fastest way to put the gate in front of real installs is a **serve-only** deployment. It needs
no private registry, no mirror, no queue, and no cloud account.

Écluse ships as a container image and nowhere else: `ghcr.io/alexadewit/ecluse`, one immutable
tag per version and no `latest`. Pick the tag from the
[releases](https://github.com/AlexaDeWit/Ecluse/releases) page. The image entrypoint is the
`ecluse` binary, so the only argument is the role to run, here `proxy`:

```bash
docker run --rm -p 127.0.0.1:8080:8080 \
  -e ECLUSE_MOUNTS__NPM__ENABLED=true \
  -e ECLUSE_SERVER__PUBLIC_URL=http://127.0.0.1:8080 \
  ghcr.io/alexadewit/ecluse:<version> proxy
```

A binary you built yourself (`nix build`) runs the same way: set the two variables in the
environment and run `ecluse proxy`.

Then point your package manager at the npm mount. For an npm-protocol client, that is one
line in `.npmrc`:

```ini
# .npmrc
registry=http://127.0.0.1:8080/npm/
```

Every rule, advisory gate, integrity floor, and egress control behaves exactly as it does on a
mirrored deployment. The only thing you give up is the mirror write, and it costs you three
things: the public leg never goes away, your availability stays coupled to the public registry,
and no mirrored copy survives an upstream yank. Evaluate the gate this way, then graduate to the
[recommended topology](@/docs/deployment.md#the-recommended-topology) by declaring a
`mirrorTarget`.
