+++
title = "Package your configuration"
description = "Keep shared settings in a config file. Supply deployment-specific values and secrets at runtime."

weight = 26

[extra]
nav_title = "Package your configuration"
group = "Set up"
kind = "Guide"
reference = "/docs/configuration/"
+++

## 1. Write the config file {#file}

Create `config.yaml`. Include only the settings you want to change from the defaults.

**config.yaml**

```yaml
server:
  publicUrl: https://packages.example.com
mounts:
  npm:
    enabled: true
  pypi:
    enabled: true
```

Set `publicUrl` to the address clients use. Écluse uses it when rewriting download URLs. Remove the mount for an ecosystem you do not need.

This example enables public reads. Add private stores, mirroring, or advisories using their setup guides.

## 2. Embed it in your image {#image}

A derived image lets you deploy the same configuration with every role. Copy the file to the path Écluse reads by default:

**Dockerfile**

```dockerfile
FROM ghcr.io/alexadewit/ecluse:0.4.0
ENV LANG=C.UTF-8
COPY --chown=65532:65532 config.yaml /etc/ecluse/config.yaml
```

Pin the base image by its verified release digest for production. Build and publish the derived image through your normal pipeline.

The UTF-8 locale lets configuration output include non-ASCII text. Without it, the configuration check can fail while printing a warning.

Keep the base image's entrypoint and non-root user. Set the role as the container arguments. The same image can run `proxy`, `mirror`, `pilot`, or `dredger`.

You can also mount the file at `/etc/ecluse/config.yaml`. To use another path, set `ECLUSE_CONFIG`.

## 3. Add runtime values {#environment}

Environment variables override the file. For example, use the same image in staging and production with a different public address:

**Container environment**

```bash
ECLUSE_SERVER__PUBLIC_URL=https://packages.example.com
ECLUSE_OBSERVABILITY__LOG_FORMAT=json
```

Double underscores separate nested keys. `server.publicUrl` becomes `ECLUSE_SERVER__PUBLIC_URL`.

Values resolve in this order: embedded defaults, config file, then environment variables.

## 4. Supply secrets separately {#secrets}

Do not bake tokens into the image or put them in the config file. Use your platform's secret injection, or the setting's `_FILE` form.

**Example: a Verdaccio mirror token**

```bash
ECLUSE_MOUNTS__NPM__MIRROR_TARGET__VERDACCIO__TOKEN_FILE=/run/secrets/mirror-token
```

Set either the value or its `_FILE` path, not both. The process must be able to read the file.

A CodeArtifact mirror uses workload identity instead of a static write token. Do not put credentials in registry URLs.

## 5. Validate the resolved configuration {#validate}

Run the same image with the same environment and mounted secrets, using `check-config` as its argument.

**Example with a derived image**

```bash
docker run --rm \
  -e ECLUSE_SERVER__PUBLIC_URL=https://packages.example.com \
  your-registry/ecluse-configured:0.4.0 check-config
```

Review enabled mounts, rule order, advisory requirements, and warnings for the role you intend to run.

An exit status of `0` confirms shared configuration validity. Role-specific refusals can appear as warnings. This check does not test cloud permissions or reachability.

{{ next(path="@/docs/connect.md", title="Connect your package managers") }}
