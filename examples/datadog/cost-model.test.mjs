import { readFileSync } from "node:fs"
import { test } from "node:test"
import assert from "node:assert/strict"
import { estimate } from "./cost-model.mjs"

const model = JSON.parse(readFileSync(new URL("./cost-model.json", import.meta.url), "utf8"))
const scenario = model.scenarios[1]
test("seven-day indexing and GB ingestion stay separate", () => {
  const result = estimate(model, scenario)
  assert.equal(result.indexed_logs, 777600000)
  assert.equal(result.ingested_log_gb, 777.6)
  assert.ok(Math.abs(result.log_usd - 1065.312) < 1e-8)
  const noIndex = structuredClone(model)
  noIndex.log.indexed_fraction_of_ingested = 0
  assert.ok(Math.abs(estimate(noIndex, scenario).log_usd - 77.76) < 1e-8)
})
test("export frequency changes points but not cardinality charges", () => {
  const fast = structuredClone(model)
  fast.export_interval_seconds = 15
  assert.equal(estimate(fast, scenario).custom_metric_usd, estimate(model, scenario).custom_metric_usd)
  assert.equal(estimate(fast, scenario).weighted_metric_points, 4 * estimate(model, scenario).weighted_metric_points)
})
test("unknown contract prices and unset budgets are not reported as acceptable", () => {
  const alternative = structuredClone(model)
  alternative.metric_billing_model = "metric_names"
  const result = estimate(alternative, scenario)
  assert.equal(result.custom_metric_usd, null)
  assert.equal(result.budget_status, "operator_budget_not_set")
  assert.ok(result.unpriced_components.includes("Metric Name contract pricing"))
})
test("an explicit exceeded budget produces a warning status", () => {
  const limited = structuredClone(model)
  limited.monthly_budget_usd = 1
  assert.equal(estimate(limited, scenario).budget_status, "modelled_components_exceed_budget")
})
test("invalid fractions and negative volumes fail", () => {
  const bad = structuredClone(model)
  bad.log.ingested_fraction = 2
  assert.throws(() => estimate(bad, scenario), /fraction/)
  assert.throws(() => estimate(model, { ...scenario, requests_per_second: -1 }), /nonnegative/)
})

test("missing and string-valued cost inputs fail", () => {
  const bad = structuredClone(model)
  delete bad.log.bytes_per_event
  assert.throws(() => estimate(bad, scenario), /bytes_per_event/)
  bad.log.bytes_per_event = "1000"
  assert.throws(() => estimate(bad, scenario), /bytes_per_event/)
})
