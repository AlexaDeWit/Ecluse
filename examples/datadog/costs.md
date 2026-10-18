# Telemetry cost assessment

The source supports a useful dashboard. It does not prove a bounded Datadog bill.
No production Datadog account, billing export, collector configuration, or representative production traffic sample was provided.
No authenticated Datadog query validation ran.

### Logs

The default is JSON at `info`, with telemetry off.
`serveAdmittedPackument` emits one info line per request reaching that metadata path, even with warm caches.
A no-survivors metadata response can emit one warning per rejected version.
Artifact denials also emit audit lines. Retries, upstream status failures, and background work add more logs.
Raising the floor to `warn` retains the denial amplification and removes useful lifecycle evidence.

A saved PyPI E2E sample contains 89 application log files and 8,234 parsed JSON lines.
The sample contains 182 metadata request lines, averaging 291.6 bytes, and 32 denial lines, averaging 410.4 bytes.
It is a test workload dominated by repeated startup. It does not establish production event rates or trace-enabled sizes.
The calculation below therefore uses an explicit 1,000-byte planning size, including room for ingestion enrichment.

Model a 30-day month with 50% metadata requests and five emitted denial lines per metadata request on average.
The latter is a stress assumption, not a measured rejection rate. It includes zero-log successful responses in its average.
Assume all these logs enter Datadog and receive seven-day indexing.

`events = requests_per_second * 2,592,000 * 0.5 * (1 + emitted_denials_per_metadata_request)`

`log_cost_USD = events * bytes_per_event / 1,000,000,000 * 0.10 + events / 1,000,000 * 1.27`

| Request rate | Modelled log events/month | Modelled GB/month | Log cost/month, USD |
| --- | --- | --- | --- |
| 10/s | 77,760,000 | 77.76 | $106.53 |
| 100/s | 777,600,000 | 777.60 | $1,065.31 |
| 1,000/s | 7,776,000,000 | 7,776.00 | $10,653.12 |

These figures cover only the modelled request and denial lines. Background logs, commitments, taxes, archives, and other products are excluded.
At 100 requests/s with no emitted denial lines, the same model costs $177.55/month.
At 100 requests/s with 50 emitted denial lines per metadata request, it costs $9,055.15/month.
The denial frequency matters more than small changes to JSON line size.

Before changing retention, distinguish runtime audit evidence from routine request breadcrumbs.
Recommend a measured index policy that preserves errors, security audits, deletion records, and lifecycle records.
Any sampling or archive-only treatment of audit logs needs a separate owner decision.
Dropping a log before vendor intake reduces ingestion and indexing. Excluding it only from an index reduces indexing only.

### Metrics

The domain labels are small enums, built-in rule names, or the fixed BlockedByDefault label. Package, version, and URL are not domain metric labels.
The WAI library records the raw HTTP method, so method values are not bounded by the application enum contract.
Its HTTP metrics also include status, protocol version, scheme, and optional route.
The current application does not set `http.route`, so the raw URL path does not become a route label through this code.
This is a concrete residual cardinality risk, not a reason to assume every request creates a unique series.

Resource and infrastructure tags multiply these combinations. Keep `resource_attributes_as_tags` disabled unless a measured need justifies it.
Do not remove process identity blindly: cumulative streams from separate producers must remain distinguishable before conversion or aggregation.
Review short-lived pod IDs, image/version churn, process IDs, and arbitrary resource attributes in the actual received tag set.

The pinned SDK uses cumulative metrics and a 60-second export interval by default.
The current application creates its meter with those defaults.
Do not prescribe an environment variable for delta temporality without checking that this SDK actually reads it.
Datadog documents cumulative histogram conversion to distributions and first-point handling for cumulative counters.
Short-lived processes therefore need special validation of startup and shutdown observations.

For cardinality billing, a distribution costs five series per tag combination, or ten with percentiles enabled.
For illustration only, assume 100 scalar combinations and 30 histogram combinations per stable producer.
Enabling percentiles for eight HTTP combinations yields `100 + 5*30 + 5*8 = 290` billable combinations per producer.

| Stable producers | Illustrative combinations | Gross custom-metric cost/month, USD |
| --- | --- | --- |
| 2 | 580 | $29 |
| 10 | 2,900 | $145 |
| 50 | 14,500 | $725 |

This applies the public $5 per 100 custom metrics rate before account allowances.
It is not a measured count, an upper bound, or a host count. Role composition changes the combinations.
Account-wide allowances must not be deducted twice when other services use them.
For configured Metrics without Limits metrics, separately account for ingested custom metrics and indexed custom metrics.
Reducing export frequency does not reduce cardinality-based billing.

Datadog also documents opt-in Metric Name pricing, which bills names and datapoints instead of cardinality.
Under that contract, collection frequency matters. Contract rates are needed before pricing it.
For the illustrative 290 weighted combinations, 60-second exports produce 12,528,000 weighted points per producer per 30 days.
At 15 seconds, that rises to 50,112,000. These are usage calculations, not a quote under Metric Name pricing.
Do not add the mutually exclusive billing models together.

### Traces and sensitive fields

Telemetry enables always-on head sampling by default. One request can create several spans, including version evaluations.
The dashboard does not need trace-derived metrics or full trace retention.
Keep trace links optional and choose the trace sampling policy separately from log retention.
Head sampling can drop rare denials. Tail sampling still receives traces before making its decision.
Neither a sampled trace link nor a missing trace proves that an operation did not occur.

Application diagnostics reduce several URLs to authority, and secret-typed values redact credentials.
Generic HTTP instrumentation still records paths/queries and client metadata on spans.
Review those attributes before export. Do not turn package names, trace IDs, advisory IDs, or full URLs into metric tags.
Log attributes can contain package/version/advisory identities that need controlled retention and access.


## Operator worksheet

Each operator sets their own monthly budget. The worksheet leaves it unset by default.
Edit `cost-model.json`, then run `task datadog:cost` in the dedicated Nix shell.
The output lists every assumption, priced components, unpriced components, and the budget comparison.
A projection below the budget is not a verified production cost.

The worksheet includes an illustrative trace-volume model with ten spans per request, 1,000 bytes per span,
full head sampling, and 1% indexing. These are assumptions, not measured properties of Écluse.
APM host count stays unknown until the operator supplies it. Background spans also need measurement.
The default worksheet queries mean latency only, so its percentile-combination input is zero.
The optional-percentile example above uses eight combinations and therefore has a different metric total.
The worksheet assumes the same ingested and indexed metric combinations. It gives no credit for tag-indexing reductions.
Remaining account allowances must represent capacity not already used by other services.
Do not infer them from replica counts.

For Metric Name pricing, the worksheet reports weighted points but leaves metric charges unpriced.
Apply the account contract separately. The worksheet never adds the two metric billing models together.
Log fractions model observed or approved collection policies. They do not change collection or approve dropping audit records.

## Verify a deployment

1. Confirm the collector version, metric types, metric names, and received tags in Datadog.
2. Measure log bytes and events by message family over representative traffic and failure periods.
3. Measure active custom metric combinations, including rollouts and autoscaling churn.
4. Measure span volume and size before and after sampling, plus indexed span volume.
5. Replace worksheet assumptions with those observations and the actual contract rates.
6. Include the unpriced components and compare the result with the operator budget.
7. Retain this evidence before describing the deployment cost as acceptable.

## Sources

- [Datadog price list](https://www.datadoghq.com/pricing/list/)
- [Custom metrics billing](https://docs.datadoghq.com/account_management/billing/custom_metrics/)
- [Metric Name pricing](https://docs.datadoghq.com/account_management/billing/metric_name_pricing/)
- [OTLP metric mapping](https://docs.datadoghq.com/opentelemetry/reference/otlp_metric_types/)
- [Resource mapping](https://docs.datadoghq.com/opentelemetry/mapping/semantic_mapping/)
- [Index exclusions](https://docs.datadoghq.com/logs/log_configuration/indexes/)
- [Application telemetry architecture](../../docs/architecture/observability.md)
- [Metadata request and denial paths](../../core/src/Ecluse/Core/Server/Pipeline/Packument.hs)

Pricing checked on 2026-10-18. Public rates are USD, using the annual column where commitments differ.
