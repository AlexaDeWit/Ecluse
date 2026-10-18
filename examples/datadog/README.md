# Datadog operator example

Copy and adapt this example to add an Écluse operations dashboard to your Datadog account.
The local module is a starting point for operators. It is not a published Terraform Registry module.

The example creates one dashboard. Collection, log indexes, retention, metrics configuration, monitors,
and trace sampling remain under your deployment's control.

## Scope

The default example shows serving, capacity, dependencies, and logs.
Optional sections cover advisory consumers, Pilot, mirroring, and Dredger.

- Deployment tags constrain every generated data query. A dashboard is not an access-control boundary.
- Environment and service selectors narrow that deployment.
- The ecosystem selector affects advisory metrics only. Most serving metrics have no ecosystem label.
- Optional platform and usage panels require existing metrics with matching deployment, environment, and service tags.
- Existing health monitors appear as links with fixed scopes. Dashboard selectors do not change those scopes.
- Missing data stays unknown. Sparse event counters can be absent before their first event.

## Connect the signals

Enable Écluse telemetry and direct OTLP to your local Agent or Collector.
Follow the [operations reference](https://ecluse-proxy.com/docs/operations/#telemetry-opt-in).
Use the JSON log format and configure service and environment identity for every role.

The collector must add matching deployment, env, and service tags to both metrics and logs.
The JSON env attribute alone is not a substitute for the env tag used by the shared selector.
The example uses `ecluse_deployment`. Écluse does not automatically copy arbitrary resource attributes into JSON logs.
Confirm the received tags in Datadog before applying this dashboard.

Keep separate cumulative producers identifiable through the collector.
Use `instance_tag` only when that tag uniquely identifies an emitting process.
A host tag is insufficient when several replicas share one host.
Without an instance tag, memory ratios show fleet totals and can hide a saturated replica.

The queries target OTLP metric names and Datadog distributions.
Prometheus/OpenMetrics ingestion needs explicit name mappings and compatible metric types.
Renaming a bucket counter does not turn it into a distribution.
Check the received HTTP status tag through `http_tag_keys` if your pipeline changes it.
Use `upstream_tag_keys` for the public upstream host, request kind, and status tags.

## Run locally

From the repository root:

```bash
nix develop .#datadog
task datadog:init
task datadog:check
```

The dedicated shell pins OpenTofu through the repository's Nix lock.
The module uses Terraform-compatible HCL. The local checks use OpenTofu and a mocked Datadog provider.
Mocked tests create no Datadog resources and do not prove that a live account has the required metrics.

For an operator-owned deployment, copy this directory and prepare its variables:

```bash
cp terraform.tfvars.example terraform.tfvars
# Edit terraform.tfvars for your deployment.
terraform init
terraform plan -out=review.tfplan
```
Set the Datadog provider's credentials through its documented environment variables.
Select the API endpoint for your site through `datadog_api_url`.
Review the plan before running `terraform apply review.tfplan` yourself.

The minimal root accepts `deployment` and `environment`.
Edit its module block for separate roles or other optional inputs:

```hcl
module "dashboard" {
  source          = "./modules/dashboard"
  environment     = var.environment
  scope_tags      = { ecluse_deployment = var.deployment }
  datadog_app_url = "https://app.datadoghq.eu"
  instance_tag    = "pod_name"

  services = {
    proxy   = ["ecluse-proxy"]
    mirror  = ["ecluse-mirror"]
    dredger = ["ecluse-dredger"]
    pilot   = ["ecluse-pilot"]
  }
  sections = {
    advisories = true
    mirror     = true
    dredger    = true
    pilot      = true
  }
  thresholds = { advisory_age = 3600 }
}
```

The example threshold is illustrative. Set it from your deployment's advisory policy.
Embedded roles can share a service name.
The module returns `dashboard_id`, `dashboard_url`, `dashboard_json`, and `metric_queries`.

## Inputs

[variables.tf](modules/dashboard/variables.tf) defines types, validation, descriptions, and defaults.

| Input | Purpose |
| --- | --- |
| `scope_tags`, `environment` | Required deployment scope and initial environment selection |
| `services`, `instance_tag` | Role identity and optional per-producer grouping |
| `sections`, `advisory_ecosystems` | Available capabilities and advisory ecosystem choices |
| `metric_names`, `http_tag_keys`, `upstream_tag_keys` | Explicit ingestion name and HTTP or upstream tag mappings |
| `latency_statistics` | Statistics per histogram, mean by default |
| `thresholds` | Reference lines by enabled timeseries panel ID |
| `log_indexes`, `log_attribute_paths` | Existing indexes and application JSON fields |
| `datadog_app_url`, `runbook_urls`, `health_monitor_ids` | Site and investigation links |
| `external_panels` | Existing platform metrics or deployment-attributed usage metrics |
| `title`, `description`, `dashboard_tags` | Dashboard presentation and ownership |

The `panels.json` entries own panel IDs, units, formulas, and metric references.
The module rejects unknown metric overrides and thresholds on disabled panels.
Percentile selections query existing Datadog support.

For example, after checking the metric's percentile configuration:

```hcl
latency_statistics = {
  "http.server.request.duration" = ["avg", "p95"]
}
```

## Interpretation limits

HTTP charts include proxy health probes and unmounted requests.
Serve decisions cover a different population. Recorded version denials are not a rejected-request rate.
Successful metadata filtering does not report every removed version through the denial counter.

Public upstream panels show request rates and `429` response rates per host.
The table sums attempts per host over the past 30 days, independent of the dashboard time selector.
It includes public reads by the proxy and enabled mirror workers, with shared embedded services counted once.
The environment, service selector, and fixed deployment scope still apply.
Selecting one service narrows the total to that service.

The counter includes failed connections and cancellations after attempt notification.
A response counts under its exact status when headers arrive, even if its body later fails.
Cache hits, followers, local cooldown refusals, private reads, and target writes contribute nothing.
The counter measures attempted exchanges, not confirmed receipt by the registry.
It does not measure the rest of an organisation's registry traffic.

A 30-day total only covers received telemetry within that window.
New deployments, disabled telemetry, export gaps, and insufficient retention make the total incomplete.
Missing data remains unknown.
The mocked checks validate generated queries, not account ingestion or live query results.

The older upstream latency and error charts describe metadata fetching.
A genuine upstream 404 contributes to the metadata error counter.
Dredger records an examined count and a disposition for the same version.
Keep those separate, and keep dry-run `would_delete` separate from `deleted`.

Advisory publication age governs freshness. Installation age is a process diagnostic.
No age can mean no loaded database or no publication timestamp.
Readiness must come from the deployment's actual probes or health monitors.

Cache and breaker gauges retain their last recorded value. Disabling interpolation cannot make them liveness signals.
Idle credential expiry alone is not a provider failure.
Logs show indexed evidence, which can omit events under the operator's retention or sampling policy.
Serve-time denial details require debug logging. Mirror publication audits remain at info.
The mirror section can show those publication records when its log section is enabled.

## Signal inventory and validation

[signals.json](modules/dashboard/signals.json) lists the 49 instrument names, types, SDK units, and label vocabulary.
A vocabulary can include an enum value that no configured role emits.
Datadog lowercases metric tags, including values such as `codeArtifact` and `mirrorTarget`.
Logs preserve their attribute case.

The inventory covers the application catalogue and the pinned WAI instrumentation.
The Haskell source remains authoritative when instrumentation changes.
Plan tests check scope preservation, optional roles, per-instance formulas, ingestion overrides, and rejected inputs.
They do not replace account-level query verification.
