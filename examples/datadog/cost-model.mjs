import { readFileSync } from "node:fs"
import { pathToFileURL } from "node:url"

/** Estimate selected telemetry charges from explicit assumptions, never a live bill. */
export function estimate(input, scenario) {
  const nonnegative = (value, name) => {
    if (typeof value !== "number" || !Number.isFinite(value) || value < 0) throw new Error(name + " must be a finite nonnegative number")
  }
  for (const key of ["seconds_per_month", "export_interval_seconds"]) nonnegative(input[key], key)
  const required = {
    log: ["metadata_fraction", "info_events_per_metadata_request", "denial_events_per_metadata_request", "background_events_per_second", "bytes_per_event", "ingested_fraction", "indexed_fraction_of_ingested", "ingest_usd_per_gb", "index_usd_per_million", "retention_days"],
    metrics: ["scalar_combinations_per_producer", "histogram_combinations_per_producer", "percentile_combinations_per_producer", "indexed_usd_per_100", "remaining_indexed_allowance", "remaining_ingested_allowance", "ingested_usd_per_100"],
    traces: ["spans_per_request", "head_sample_fraction", "bytes_per_span", "indexed_fraction", "remaining_ingest_allowance_gb", "remaining_indexed_allowance", "ingest_usd_per_gb", "index_usd_per_million", "usd_per_apm_host"]
  }
  for (const [group, keys] of Object.entries(required)) for (const key of keys) nonnegative(input[group]?.[key], group + "." + key)
  for (const key of ["requests_per_second", "metric_producers"]) nonnegative(scenario[key], key)
  if (input.monthly_budget_usd !== null) nonnegative(input.monthly_budget_usd, "monthly_budget_usd")
  if (input.traces.apm_hosts !== null) nonnegative(input.traces.apm_hosts, "apm_hosts")
  if (typeof input.metrics.metrics_without_limits !== "boolean") throw new Error("metrics_without_limits must be a boolean")
  if (input.currency !== "USD") throw new Error("This worksheet uses USD prices")
  if (input.export_interval_seconds === 0 || input.seconds_per_month === 0) throw new Error("Time intervals must be positive")
  for (const [key, fraction] of Object.entries({
    metadata: input.log.metadata_fraction, log_ingestion: input.log.ingested_fraction,
    log_indexing: input.log.indexed_fraction_of_ingested, trace_sampling: input.traces.head_sample_fraction,
    trace_indexing: input.traces.indexed_fraction
  })) if (fraction > 1) throw new Error(key + " fraction must be at most one")
  if (input.metrics.percentile_combinations_per_producer > input.metrics.histogram_combinations_per_producer) throw new Error("Percentile combinations must be a subset of histogram combinations")
  if (!["cardinality", "metric_names"].includes(input.metric_billing_model)) throw new Error("Select cardinality or metric_names billing")

  const requests = scenario.requests_per_second * input.seconds_per_month
  const emittedLogs = requests * input.log.metadata_fraction * (input.log.info_events_per_metadata_request + input.log.denial_events_per_metadata_request) + input.log.background_events_per_second * input.seconds_per_month
  const ingestedLogs = emittedLogs * input.log.ingested_fraction
  const indexedLogs = ingestedLogs * input.log.indexed_fraction_of_ingested
  const logGb = ingestedLogs * input.log.bytes_per_event / 1e9
  const logsUsd = logGb * input.log.ingest_usd_per_gb + indexedLogs / 1e6 * input.log.index_usd_per_million
  const combinations = scenario.metric_producers * (input.metrics.scalar_combinations_per_producer + 5 * input.metrics.histogram_combinations_per_producer + 5 * input.metrics.percentile_combinations_per_producer)
  const metricUsd = input.metric_billing_model === "cardinality"
    ? Math.max(0, combinations - input.metrics.remaining_indexed_allowance) / 100 * input.metrics.indexed_usd_per_100
      + (input.metrics.metrics_without_limits ? Math.max(0, combinations - input.metrics.remaining_ingested_allowance) / 100 * input.metrics.ingested_usd_per_100 : 0)
    : null
  const points = combinations * input.seconds_per_month / input.export_interval_seconds
  const spans = requests * input.traces.spans_per_request * input.traces.head_sample_fraction
  const traceGb = spans * input.traces.bytes_per_span / 1e9
  const indexedSpans = spans * input.traces.indexed_fraction
  const traceUsd = Math.max(0, traceGb - input.traces.remaining_ingest_allowance_gb) * input.traces.ingest_usd_per_gb
    + Math.max(0, indexedSpans - input.traces.remaining_indexed_allowance) / 1e6 * input.traces.index_usd_per_million
  const hostUsd = input.traces.apm_hosts === null ? null : input.traces.apm_hosts * input.traces.usd_per_apm_host
  const known = logsUsd + (metricUsd ?? 0) + traceUsd + (hostUsd ?? 0)
  const unpriced = ["infrastructure hosts", "container monitoring", "archives and rehydration", "cloud egress", "collector compute", "taxes and contract commitments"]
  if (metricUsd === null) unpriced.push("Metric Name contract pricing")
  if (hostUsd === null) unpriced.push("APM hosts")
  return {
    scenario: scenario.name, requests, emitted_logs: emittedLogs, ingested_log_gb: logGb, indexed_logs: indexedLogs,
    weighted_metric_combinations: combinations, weighted_metric_points: points,
    ingested_trace_gb: traceGb, indexed_spans: indexedSpans,
    log_usd: logsUsd, custom_metric_usd: metricUsd, trace_volume_usd: traceUsd, apm_host_usd: hostUsd,
    priced_components_usd: known, unpriced_components: unpriced,
    budget_status: input.monthly_budget_usd === null ? "operator_budget_not_set"
      : known > input.monthly_budget_usd ? "modelled_components_exceed_budget" : "modelled_components_below_budget_real_cost_unverified"
  }
}

if (process.argv[1] && import.meta.url === pathToFileURL(process.argv[1]).href) {
  const input = JSON.parse(readFileSync(process.argv[2] ?? new URL("./cost-model.json", import.meta.url), "utf8"))
  process.stdout.write(JSON.stringify({
    disclaimer: "Illustrative projections, not observed production usage or a quote. See unpriced components.",
    assumptions: input,
    estimates: input.scenarios.map(scenario => estimate(input, scenario))
  }, null, 2) + "\n")
}
