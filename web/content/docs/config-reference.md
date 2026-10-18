+++
title = "Configuration reference"
description = "The same schema describes the config file and environment variables. Unknown keys stop startup."

weight = 24

[extra]
nav_title = "Configuration reference"
group = "Detailed behaviour"
kind = "Reference"
reference = "/docs/configuration/"
+++

## Resolution order {#resolution}

Each value resolves from embedded defaults, then the optional config document, then environment variables. The last supplied value wins.

The default file path is `/etc/ecluse/config.yaml`. `ECLUSE_CONFIG` selects another path. An explicitly selected missing file is an error.

A double underscore descends into an object. An underscore splits a camelCase name. For example, `mounts.npm.mirrorTarget.codeArtifact.url` becomes `ECLUSE_MOUNTS__NPM__MIRROR_TARGET__CODE_ARTIFACT__URL`.

## Find a setting {#groups}

{{ task_link(path="@/docs/configuration.md#the-configuration-reference", title="All settings and defaults", description="The complete reference, rendered from the embedded configuration.") }}

{{ task_link(path="@/docs/policy.md", title="Rules and their order", description="Defaults, precedence, and public admission.") }}

{{ task_link(path="@/docs/configuration.md#endpoint-collisions", title="Endpoint collisions", description="Which repository combinations warn or prevent a role from starting.") }}

{{ task_link(path="@/docs/configuration.md#store-tags", title="Registry backend tags", description="Generic registries, Verdaccio, and CodeArtifact capabilities.") }}

{{ task_link(path="@/docs/configuration.md#secrets", title="Secret settings", description="Environment variables, file indirection, and refused URL credentials.") }}

## What validation establishes {#validation}

`check-config` prints the resolved settings and their sources, redacting typed secrets. It checks each role's configuration without contacting cloud services.

A shared configuration error returns exit `2`. A refusal specific to one role can appear as a warning while the overall command returns `0`.

Review warnings for the roles you deploy. Startup and operational checks still need to prove connectivity, permissions, and repository relationships.
