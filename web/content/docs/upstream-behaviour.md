+++
title = "Public upstream throttling"
description = "How Écluse responds when a public registry asks it to slow down."
weight = 42

[extra]
nav_title = "Public upstream throttling"
group = "Detailed behaviour"
kind = "Reference"
reference = "/docs/how-it-works/#a-request-step-by-step"
+++

## Cooldown applies to the destination authority

A public `429` response starts a cooldown for that host and port.
A public `503` starts one only when it carries a valid, positive `Retry-After`.
Écluse accepts numeric seconds and HTTP dates, with a maximum of 600 seconds.

A `429` without a usable hint starts with a 60-second fallback.
Failed recovery probes double the fallback ceiling up to 600 seconds.
Jitter selects a delay between 60 seconds and that ceiling.
Missing, malformed, zero, and expired hints use this fallback.
These defaults are fixed and have no configuration key.

Every mount and the embedded mirror worker share the process's authority state.
PyPI metadata and files use separate authorities and therefore separate cooldowns.
Private reads and publication or mirror-target writes do not use this public cooldown.

## What clients and workers receive

During cooldown, Écluse sends no public request to that authority.
A request that needs that read receives `503` with the remaining `Retry-After`, rounded up to whole seconds.
A listing can still serve resolved private versions.
Metadata retained by the selected-version cache needs no public metadata request.
An artifact or companion fetch still checks its own destination authority.

The first throttled artifact or companion response retains the upstream status and headers.
Later local refusals use `503` and the remaining delay.
Mirror jobs that need a cooling authority remain unacknowledged for queue redelivery.

After the deadline, one request probes the authority.
Other callers receive a one-second retry hint while that probe runs.
A non-throttling response restores ordinary traffic.
A cancelled probe or a connection failure releases the probe slot for another request.

## Scope and observation

The state belongs to one process. It does not coordinate pods or impose a cluster-wide request budget.
The proposed Valkey control remains separate work. Threat 120 remains Open.

Cooldown entry and recovery logs carry `upstream_host` and `upstream_port`.
The [public request counter](@/docs/operations.md) reports actual public attempts by host, kind, and outcome.
