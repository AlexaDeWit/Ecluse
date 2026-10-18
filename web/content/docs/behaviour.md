+++
title = "Understand a specific behaviour"
description = "Follow the part of the request or policy you are investigating. Each topic links to its complete reference."

weight = 23

[extra]
nav_title = "Find a topic"
group = "Detailed behaviour"
kind = "Reference index"
reference = "/docs/"
+++

{{ task_link(path="@/docs/policy.md", title="How a public version is admitted", description="Defaults, rule order, exceptions, and ecosystem differences.") }}

{{ task_link(path="@/docs/private-reads.md", title="Why private reads behave differently", description="Caller credentials, trusted copies, and digest conflicts.") }}

{{ task_link(path="@/docs/advisory-behaviour.md", title="What happens when advisory data is missing", description="Freshness, skipped checks, partial fixes, and readiness.") }}

{{ task_link(path="@/docs/mirror-behaviour.md", title="When a mirrored version becomes readable", description="Whole-version completion, partial uploads, and public outages.") }}

{{ task_link(path="@/docs/revocation.md", title="Why a denied version is still available", description="Admission versus revocation, retained copies, and policy rollout.") }}

{{ task_link(path="@/docs/config-reference.md", title="How configuration resolves", description="Defaults, file values, environment overrides, and role validation.") }}

{{ task_link(path="@/docs/permissions.md", title="Which identity needs which permission", description="Cloud roles, caller credentials, and deletion authority.") }}

{{ task_link(path="@/docs/protocol-support.md", title="Protocol details", description="Supported endpoints, metadata fields, and ecosystem limits.") }}

## Security and design evidence {#security}

The [rendered threat model](@/docs/threat-model.md) remains the security reference. It includes operation-specific diagrams, threats, controls, and accepted assumptions.

The [architecture documents](https://github.com/AlexaDeWit/Ecluse/blob/main/docs/architecture.md) explain design choices. The [generated API reference](https://ecluse-proxy.com/api/) covers contributor interfaces.
