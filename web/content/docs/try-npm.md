+++
title = "Try an npm install"
description = "Run Écluse on your machine and install a package through it. You need Docker and an npm client."

weight = 50

[extra]
nav_title = "Try an npm install"
group = "Set up"
kind = "Guide"
reference = "/docs/quick-start/"
+++

npm | [Switch to Python](@/docs/try-pypi.md)

## 1. Start the proxy {#start}

**Terminal 1**

```bash
docker run --rm --name ecluse-trial \
  -p 127.0.0.1:8080:8080 \
  -e ECLUSE_MOUNTS__NPM__ENABLED=true \
  -e ECLUSE_SERVER__PUBLIC_URL=http://127.0.0.1:8080 \
  ghcr.io/alexadewit/ecluse:0.3.3 proxy
```

Leave this terminal open. The proxy listens only on your machine. It does not write to a package registry.

## 2. Install a package {#install}

In a second terminal, create an empty directory for the trial. Then run:

**Terminal 2**

```bash
npm install is-number@7.0.0 --registry=http://127.0.0.1:8080/npm/
```

The client should install `is-number@7.0.0`. In the first terminal, confirm that Écluse received the request.

If the client reports no matching version or a refusal, use [Investigate a failed install](@/docs/install-failed.md).

## 3. Check what is enabled {#policy}

This setup uses a seven-day minimum age. It has no advisory source, so known-malicious advisory checks and early security-fix admission are inactive.

The npm defaults also deny public versions that run install scripts, and versions whose dependencies name a git source, a remote tarball URL or an unsupported scheme such as `jsr:`. An older release can still be denied for these reasons.

For a repeatable policy check, add an exact package deny to the config, restart, and repeat the request with fresh client state. The [verification guide](@/docs/verify.md#policy) shows that check.

## 4. Finish the trial {#finish}

Press <kbd>Ctrl</kbd> + <kbd>C</kbd> in the first terminal to stop and remove the container. The installed files remain in your trial directory.

This trial changes no global client setting. A team deployment also needs restricted network access, a stable HTTPS address, and operational checks.

{{ next(path="@/docs/plan-deployment.md", title="Choose your deployment") }}
