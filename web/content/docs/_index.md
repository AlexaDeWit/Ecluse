+++
title = "Documentation"
description = "Understand Écluse, deploy it for your organisation, or investigate its detailed behaviour."
sort_by = "weight"
template = "docs-section.html"
page_template = "docs-page.html"
+++

{{ task_link(path="@/docs/understand.md", title="Understand Écluse", description="What it controls, where it fits, and what your platform team provides.") }}

{{ task_link(path="@/docs/setup.md", title="Set up Écluse", description="Choose a deployment, package its configuration, connect clients, and verify the result.") }}

{{ task_link(path="@/docs/behaviour.md", title="Look up detailed behaviour", description="Policy decisions, advisory outages, private reads, mirror completion, and revocation.") }}

## Operate an existing deployment

{{ task_link(path="@/docs/install-failed.md", title="Investigate a failed install", description="Trace a report from a developer or CI job to the request and its result.") }}

{{ task_link(path="@/docs/operate.md", title="Operate and upgrade", description="Monitor background work and prepare changes to images, policies, and data formats.") }}

## Security and contributor references

The [rendered threat model](@/docs/threat-model.md) holds the system diagrams, controls, assumptions, and accepted risks.
The [architecture documents](https://github.com/AlexaDeWit/Ecluse/blob/main/docs/architecture.md) explain design decisions.
The [API reference](@/api/_index.md) documents contributor interfaces.
