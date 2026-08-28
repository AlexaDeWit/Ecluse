+++
title = "Operating Écluse"
description = "Health probes, graceful shutdown, exit codes, logs, telemetry, the memory plan, and the sizing arithmetic behind a pod."
weight = 5
+++

This page covers the probes, the shutdown behaviour, the logs, and the arithmetic behind a
pod's memory limit.

## Health probes

`GET /livez` reports process liveness: `200` while the process is healthy and `503` when it is
not. On a mirroring deployment a stalled mirror worker fails it. On a serve-only deployment
liveness is the listener alone.

`GET /readyz` reports config loaded and the listener serving. It is deliberately lenient about
public-upstream reachability, so a transient blip does not pull a healthy pod from rotation. It
answers `503` in exactly two cases: the instance is draining, or it is still starting up. With an
advisory bucket configured, that startup gate also waits for each ecosystem's first advisory sync,
a one-way flip that never flaps back. Give a cold pod room for that first database download: a
Kubernetes `startupProbe`, or a readiness `failureThreshold` sized for it. Mounting an ecosystem
whose artifact Pilot never publishes leaves the pod never ready.

The npm liveness probe `GET /npm/-/ping` answers locally with `200 {}`. `GET /npm/-/v1/search`
returns `501` by design: search is a discovery convenience, not an install path. Pilot and Dredger
export the same `/livez` and `/readyz` on `ECLUSE_SERVER__PORT`.

## Graceful shutdown and pod drain

On `SIGTERM`/`SIGINT` Écluse drains in-flight work rather than dropping it. `GET /readyz` flips to
`503`, the signal a load balancer or mesh watches to stop routing new traffic here, while
`GET /livez` stays `200`. An orchestrator therefore does not kill a still-draining instance early.
Every response then carries `Connection: close`, so a keep-alive pool reconnects to a ready
instance. The process finishes in-flight requests and in-progress artifact streams before exiting,
so a half-delivered tarball runs to completion.

`ECLUSE_SERVER__SHUTDOWN_DRAIN_TIMEOUT` bounds the drain at 30 seconds by default. **Set the
platform's termination grace period above it**, so the orchestrator does not `SIGKILL` mid-drain.
On Kubernetes that is `terminationGracePeriodSeconds`. On an interactive terminal a second `Ctrl+C`
(or `Ctrl+D`) forces an immediate halt that bypasses the drain. That halt needs standard input to
be a TTY, so production has no such bypass.

## Exit codes

The exit status states how a run ended, so an orchestrator can branch without parsing logs:

| Code | Meaning |
|---|---|
| `0` | Graceful shutdown: the drain completed and the services returned. |
| `1` | A service exited abnormally. The last `ecluse: service exited:` line on standard error carries the detail. |
| `2` | The boot aborted: Écluse rejected the configuration or wiring and reported every problem. A restart without changes fails identically. |
| `3` | Something outside cancelled the run: a kill that bypassed the graceful path. |
| `130` | The local-development halt (Ctrl-D on an interactive terminal). |

## Logs

One JSON object per line by default (`ECLUSE_OBSERVABILITY__LOG_FORMAT=json`), or `console` for
local development. Each JSON line carries `timestamp` (RFC 3339 UTC), `status` (`debug`, `info`,
`warn`, `error`), `message`, and the `service`/`env`/`version` identity. While a span is in scope
the line also carries a `dd` object with `trace_id` and `span_id`. The emitting call's own fields
sit under `data`, and the `katip` emitter fields under `katip`. Those include the emitting
process's hostname (`katip.host`), so a collector's own host attribution governs the line's
`host`. `timestamp`, `status`, `message`, and `service` are Datadog's reserved log attributes, and
its JSON preprocessing reads them unmodified. `env` and `version` are ordinary attributes any
backend indexes. `ECLUSE_OBSERVABILITY__LOG_LEVEL` sets the floor (`info` by default).

Bearer tokens render as a redacted placeholder, and on every running path Écluse reduces a URL to
its host and port. Neither token material nor a signed query string reaches a log field. The
boot-time configuration echo prints each configured endpoint as you gave it. That is safe, because
the boot refuses a URL that carries a credential (see [Secrets](@/docs/configuration.md#secrets)).

## Telemetry (opt-in)

Set `ECLUSE_OBSERVABILITY__TELEMETRY=on`, then `DD_*` (`DD_SERVICE`, `DD_ENV`, `DD_VERSION`,
`DD_AGENT_HOST`) for Datadog or the standard `OTEL_*` for any other backend. `DD_*` wins where both
are set, and the resolved identity stamps both traces and every log line. With no `DD_VERSION` or
`service.version` set, exported traces and log lines carry the running binary's own build version.
The version tag is never blank. `DD_API_KEY`/`DD_SITE` have no effect, because Écluse exports only
to a node-local collector or Agent, at `http://localhost:4318` by default or wherever
`DD_AGENT_HOST`/`OTEL_EXPORTER_OTLP_ENDPOINT` points. Authenticate a remote collector out of band
with `OTEL_EXPORTER_OTLP_HEADERS`.

## Memory plan and runtime sizing

Every byte-valued bound is a named tenant of the effective heap ceiling, not an independent
multiplier. The tenants are the cache, response cap, publish aggregate, and in-memory queue. Each
one boot-logs as a `memory plan:` line. A pod too small for the tenants' floors **degrades
gracefully instead of refusing**. It sheds the mirror-artifact cap first, then the cache, each to
zero if needed, then serves uncached. Each step is a loud warning, and it always boots. Only an
explicit override that breaks the plan refuses (exit `2`). The model is in
[Runtime sizing](https://github.com/AlexaDeWit/Ecluse/blob/main/docs/architecture/configuration.md#runtime-sizing-cores-and-heap-ceiling).

Cores and the heap ceiling resolve at boot from config, else the cgroup, else a capped fallback.
The boot log records each decision with its provenance. The whole-cores guidance, what to set on a
pod with no CPU limit, and the per-pod memory arithmetic are in the
[appendix](@/docs/operations.md#appendix-runtime-sizing-arithmetic).

A cold install against an empty cache hits the proxy with dozens of heavy requests at once, which
causes latency spikes or `503` backpressure. Run one install after starting Écluse and before
production traffic. Once warm, request coalescing absorbs spikes.

## Revoking a mirrored version (internal yank)

The mirror store deliberately resists upstream yanks, so a benign yank does not break your
installs. A version later found malicious therefore stays, because Écluse never re-gates trusted
content. Usually this resolves itself: once the public registry yanks the bad version, re-mirroring
cannot reproduce its bytes and you purge the stale copy at leisure. When your own scanning is ahead
of the public yank, revoke in order. First **deny the identity** with a `DenyByIdentity` rule, so
the serve path stops admitting the version and the worker stops re-mirroring it. Then **purge that
version** from the mirror. That **order matters:** purge alone is a treadmill, since the next
install re-admits and re-mirrors a version still live upstream.

## Poison mirror jobs

Some mirror jobs can never succeed. Examples: an artifact past `ECLUSE_LIMITS__MAX_ARTIFACT_BYTES`,
a payload that no longer decodes, or a publish target that refuses it every time. On SQS, **attach
a redrive policy with a dead-letter queue** to the mirror queue. The worker leaves such a message
undeleted. Your policy then moves it to the dead-letter queue, where you can read it and work out
what happened. At boot, Écluse reads the queue's redrive configuration. If the queue has no policy,
start-up logs a loud `WARNING` that poison messages have no terminus. If the probe itself fails,
that warning names the missing `sqs:GetQueueAttributes` permission. Either way the process boots.

Without a dead-letter queue, nothing captures that message. SQS redelivers it, and the worker
re-fetches the artifact each time, until the retention window (up to 14 days) drops it unseen. So
Écluse retires the job itself after `ECLUSE_QUEUE__MAX_RECEIVE_COUNT` deliveries. It writes an
error log naming the job and the reason. The `ecluse.mirror.jobs.processed` counter records it at
`result="discarded"`. **Alert on that series.** Every discard is a job nothing else caught. With a
redrive policy attached, Écluse runs one delivery above its `maxReceiveCount`. Your dead-letter
queue always captures first, and the discard path stays dormant. Either way you lose nothing.
Mirroring is demand-driven, so the next client request for that artifact re-enqueues the job. It
fails the same way until you fix the cause.

## Appendix: runtime-sizing arithmetic

**Give Écluse whole cores.** A fractional CPU limit, say 3.5, has no good option. Claiming 4
capabilities overruns the CFS quota during stop-the-world GC and freezes the process mid-pause.
Flooring to 3 strands the fraction. So pair an integer limit with `requests = limits` (and
exclusive cores where offered) to remove throttling structurally, since Écluse floors the derived
count.

**A pod with no CPU limit is the case to configure.** A CPU **limit** is a cgroup quota Écluse
reads, and it does not shrink the processor count the runtime sees. A CPU **request** is not a
quota. It reaches the container only as a scheduler weight, and the same weight has meant requests
up to 3.4x apart across runc versions, so Écluse will not guess a core count from it. With no limit
set, Écluse falls back to the count the memory limit can feed, and with no memory limit either it
caps at `ECLUSE_RUNTIME__CORES_CEILING` (8). Neither number is your request, and the boot log warns
and says so. On a 32-core node a 2-CPU-request pod with no memory limit therefore claims 8
capabilities, not 2. Tell it the number with the Downward API:

```yaml
env:
  - name: ECLUSE_RUNTIME__CORES
    valueFrom:
      resourceFieldRef:
        resource: requests.cpu
        divisor: "1"
```

Read `requests.cpu`, never `limits.cpu`: with no limit set, the kubelet substitutes the node's
allocatable CPU, which is the whole-node claim you are trying to avoid. `divisor: "1"` rounds up
to whole cores, so a `500m` request becomes 1.

**Bare metal and dev hosts** have no cgroup limits either, so they take the same ceiling of 8, or
the processor count when that is lower. Raise `ECLUSE_RUNTIME__CORES_CEILING`, or set
`ECLUSE_RUNTIME__CORES`, to use a bigger box fully.

**Memory arithmetic (proxy pod).** The binary ships `-A64m -n4m`, a 64 MiB per-core allocation area
in 4 MiB chunks. That trades bounded extra memory for far fewer GCs under load. Budget roughly
`cores x 64 MiB` of nursery, plus the live heap, which the metadata cache dominates. Add up to one
live-heap of copying headroom during a major GC. Worked shapes:

- a 2-CPU / 512 MiB pod runs as-is
- a 2-CPU / 256 MiB pod also needs `GHCRTS="-A16m"`
- a 4-CPU pod wants ~750 MiB on defaults, or 512 MiB with `-A32m`

Taller pods amortise the cache and coalescing better, so prefer 4-CPU-ish shapes. Tune the
allocation area with `GHCRTS`. The boot log prints the effective value. Pilot and Dredger run
different workloads, so tune their allocation area separately.
