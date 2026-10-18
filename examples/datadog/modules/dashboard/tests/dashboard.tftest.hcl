mock_provider "datadog" {}

variables {
  environment = "production"
  scope_tags = {
    ecluse_deployment = "platform-east"
    region            = "ca-central-1"
  }
}

run "default_scope_and_optional_sections" {
  command = plan
  assert {
    condition = alltrue(flatten([for queries in output.metric_queries : [for query in queries :
      strcontains(query.query, "ecluse_deployment:platform-east") &&
      strcontains(query.query, "region:ca-central-1") &&
      strcontains(query.query, "$env") && strcontains(query.query, "$service")
    ]]))
    error_message = "Every query must retain deployment scope and dashboard selectors."
  }
  assert {
    condition = alltrue([for id in keys(output.metric_queries) :
      !startswith(id, "advisory_") && !startswith(id, "pilot_") && !startswith(id, "mirror_") && id != "dredger"
    ])
    error_message = "Optional role panels must be absent by default."
  }
  assert {
    condition = alltrue(flatten([for queries in output.metric_queries : [for query in queries :
      !strcontains(query.query, "$advisory_ecosystem") && !strcontains(query.query, "p95:") && !strcontains(query.query, "p99:")
    ]]))
    error_message = "Defaults must not require advisory labels or percentile enablement."
  }
  assert {
    condition = alltrue([for widget in jsondecode(output.dashboard_json).widgets :
      widget.definition.type != "log_stream" ? true :
      strcontains(widget.definition.query, "ecluse_deployment:platform-east") &&
      strcontains(widget.definition.query, "$env") &&
      strcontains(widget.definition.query, "$service")
    ])
    error_message = "Logs must follow deployment, environment, and service selection."
  }
}

run "separate_roles_and_optional_statistics" {
  command = plan
  variables {
    sections     = { advisories = true, pilot = true, mirror = true, dredger = true, logs = false }
    instance_tag = "pod_name"
    services = {
      proxy   = ["ecluse-proxy-a", "ecluse-proxy-b"]
      mirror  = ["ecluse-mirror"]
      dredger = ["ecluse-dredger"]
      pilot   = ["ecluse-pilot"]
    }
    latency_statistics = { "http.server.request.duration" = ["avg", "p95"] }
    metric_names       = { "http.server.request.count" = "otel.http.requests" }
    http_tag_keys      = { status = "http.status_code" }
    thresholds         = { advisory_age = 3600 }
    external_panels = [{
      title  = "Existing attributed log ingestion"
      metric = "example.logs.ingested_bytes"
      role   = "proxy"
      rate   = true
      unit   = "B/s"
    }]
  }
  assert {
    condition = (
      strcontains(output.metric_queries.mirror_outcomes[0].query, "service:ecluse-mirror") &&
      !strcontains(output.metric_queries.mirror_outcomes[0].query, "service:ecluse-proxy") &&
      strcontains(output.metric_queries.mirror_enqueue[0].query, "service:ecluse-proxy-a OR service:ecluse-proxy-b") &&
      strcontains(output.metric_queries.pilot_runs[0].query, "service:ecluse-pilot")
    )
    error_message = "Select the service that emits each role's signal."
  }
  assert {
    condition = (
      strcontains(output.metric_queries.advisory_age[0].query, "$advisory_ecosystem") &&
      !strcontains(output.metric_queries.traffic[0].query, "$advisory_ecosystem") &&
      strcontains(output.metric_queries.traffic[0].query, "otel.http.requests") &&
      strcontains(output.metric_queries.traffic[0].query, "by {http.status_code}")
    )
    error_message = "Scope advisory queries separately and apply ingestion overrides."
  }
  assert {
    condition = (
      alltrue([for query in output.metric_queries.memory_ratio : strcontains(query.query, "by {pod_name}")]) &&
      anytrue([for query in output.metric_queries.http_latency : startswith(query.query, "p95:")]) &&
      alltrue([for query in output.metric_queries.upstream_latency : startswith(query.query, "avg:")])
    )
    error_message = "Match instance groups and keep percentile opt-in per metric."
  }
  assert {
    condition = (
      strcontains(output.metric_queries.external_0[0].query, "ecluse_deployment:platform-east") &&
      alltrue([for widget in jsondecode(output.dashboard_json).widgets : widget.definition.type != "log_stream"])
    )
    error_message = "Scope external queries and remove disabled log widgets."
  }
}

run "reject_unscoped_deployment" {
  command = plan
  variables { scope_tags = {} }
  expect_failures = [var.scope_tags]
}
run "reject_scope_query_injection" {
  command = plan
  variables { scope_tags = { deployment = "production OR env:*" } }
  expect_failures = [var.scope_tags]
}
run "reject_unknown_metric" {
  command = plan
  variables { metric_names = { "ecluse.does.not.exist" = "custom.unknown" } }
  expect_failures = [datadog_dashboard_json.this]
}
run "reject_disabled_threshold" {
  command = plan
  variables { thresholds = { advisory_age = 3600 } }
  expect_failures = [datadog_dashboard_json.this]
}
run "reject_scalar_percentiles" {
  command = plan
  variables { latency_statistics = { "ecluse.serve.decision" = ["p95"] } }
  expect_failures = [datadog_dashboard_json.this]
}
