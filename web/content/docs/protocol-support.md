+++
title = "Protocol support"
description = "Which registry protocols an Écluse server speaks, and what each endpoint answers, per ecosystem."
weight = 6
+++

The reference below is what an Écluse server speaks: every endpoint it answers, per
ecosystem, with the responses each one returns. Écluse renders the page from the OpenAPI
document it publishes as JSON, so the page and the document always agree.

## Supported registries

| Registry | Status |
| --- | --- |
| npm | Served |
| PyPI | Planned |
| RubyGems | Planned |

A planned registry is already a valid `mounts` key, but no adapter answers its routes yet,
so declaring one serves nothing.

{{ openapi_reference() }}

The raw OpenAPI document is published at [/api/openapi.json](/api/openapi.json).
