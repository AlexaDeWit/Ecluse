+++
title = "Set up Écluse"
description = "For the platform, DevOps, or IT team deploying Écluse across an organisation. Evaluate it locally, then connect your developer teams and CI."

weight = 48

[extra]
nav_title = "Start here"
group = "Set up"
kind = "Guide"
reference = "/docs/deployment/"
+++

{{ task_link(path="@/docs/try-npm.md", title="Evaluate it locally", description="Try a package install before planning your organisation's deployment. No cloud account required.") }}

{{ task_link(path="@/docs/plan-deployment.md", title="Plan a team deployment", description="Choose your registries, services, and network access.") }}

## The setup path {#path}

{{ task_link(path="@/docs/plan-deployment.md", title="1. Choose your deployment", description="Decide which capabilities you need and what runs alongside the proxy.") }}

{{ task_link(path="@/docs/configure.md", title="2. Package your configuration", description="Embed a config file in your image or mount it at runtime. Add environment overrides and secrets.") }}

{{ task_link(path="@/docs/connect.md", title="3. Connect your package managers", description="Registry URLs, caller credentials, and client settings that bypass the proxy.") }}

{{ task_link(path="@/docs/verify.md", title="4. Verify the deployment", description="Check installs, policy decisions, private access, and optional background work.") }}

## Add a capability {#optional}

{{ task_link(path="@/docs/advisory-setup.md", title="Check security advisories", description="Configure advisory storage, publish the first database, and keep it updated.") }}

{{ task_link(path="@/docs/mirror-setup.md", title="Keep copies of public packages", description="Configure the mirror, its read path, and the background worker.") }}

{{ task_link(path="@/docs/private-packages.md", title="Use and publish private packages", description="Reserve your package names and connect the publication target.") }}

## Use your existing platform {#platform}

The deployment guide assumes you know how to run containers, manage credentials, and restrict network access on your platform.

It explains what Écluse needs. Kubernetes and AWS examples show where those requirements fit, without teaching cluster or account administration.
