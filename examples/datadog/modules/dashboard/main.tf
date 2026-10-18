locals {
  signals       = jsondecode(file("${path.module}/signals.json"))
  catalogue     = { for signal in local.signals : signal.name => signal }
  panel_specs   = jsondecode(file("${path.module}/panels.json"))
  metric_names  = { for name in keys(local.catalogue) : name => lookup(var.metric_names, name, name) }
  latency_names = [for signal in local.signals : signal.name if signal.instrument == "histogram"]
  active_services = sort(distinct(concat(
    tolist(var.services.proxy),
    var.sections.mirror ? tolist(var.services.mirror) : [],
    var.sections.dredger ? tolist(var.services.dredger) : [],
    var.sections.pilot ? tolist(var.services.pilot) : []
  )))
  role_services = merge(var.services, {
    consumer = sort(distinct(concat(
      tolist(var.services.proxy),
      var.sections.mirror ? tolist(var.services.mirror) : [],
      var.sections.dredger ? tolist(var.services.dredger) : []
    )))
    all = local.active_services
  })
  deployment_tags = [for key, value in var.scope_tags : "${key}:${value}"]
  metric_scopes = {
    for role, services in local.role_services : role => join(" AND ", concat(
      local.deployment_tags,
      ["$env", "$service", "(${join(" OR ", [for service in services : "service:${service}"])})"]
    ))
  }
  log_scope = join(" ", concat(local.deployment_tags, [
    "$env", "$service",
    "(${join(" OR ", [for service in local.active_services : "service:${service}"])})"
  ]))
  tag_keys = {
    "@status@"   = var.http_tag_keys.status
    "@instance@" = var.instance_tag == null ? "" : var.instance_tag
  }
  specs = concat(
    [for panel in local.panel_specs : panel if panel.section == "always" ? true : var.sections[panel.section]],
    [for index, panel in var.external_panels : {
      id      = "external_${index}"
      group   = "external"
      title   = panel.title
      unit    = panel.unit
      role    = panel.role
      type    = "timeseries"
      queries = [{ metric = panel.metric, alias = panel.title, aggregation = panel.aggregation, modifier = panel.rate ? "rate" : "gauge", group_by = panel.group_by }]
    }]
  )
  series = {
    for panel in local.specs : panel.id => flatten([
      for query in panel.queries : [
        for stat in(try(query.statistics, false) ? lookup(var.latency_statistics, query.metric, toset(["avg"])) : toset([query.aggregation])) : {
          metric      = lookup(local.metric_names, query.metric, query.metric)
          aggregation = stat
          alias       = try(query.statistics, false) ? "${query.alias} (${stat})" : query.alias
          modifier    = query.modifier
          group_by    = compact([for key in try(query.group_by, []) : lookup(local.tag_keys, key, key)])
          filters     = [for filter in try(query.filters, []) : replace(filter, "@status@", var.http_tag_keys.status)]
          ecosystem   = try(query.ecosystem, false)
        }
      ]
    ])
  }
  queries = {
    for panel in local.specs : panel.id => [
      for index, series in local.series[panel.id] : merge({
        data_source = "metrics"
        name        = "q${index}"
        query = format("%s:%s{%s}%s%s",
          series.aggregation,
          series.metric,
          join(" AND ", concat([local.metric_scopes[panel.role]], series.filters, series.ecosystem ? ["$advisory_ecosystem"] : [])),
          length(series.group_by) > 0 ? " by {${join(",", series.group_by)}}" : "",
          lookup({ rate = ".as_rate()", count = ".as_count()", gauge = ".fill(null)", histogram = "" }, series.modifier, "")
        )
      }, panel.type == "query_value" ? { aggregator = try(panel.time_aggregation, "avg") } : {})
    ]
  }
  context_links = [{
    label = "Investigate logs"
    link  = "${var.datadog_app_url}/logs?query=${join(" ", local.deployment_tags)} {{$env}} {{$service}}&from_ts={{timestamp_start}}&to_ts={{timestamp_end}}"
    }, {
    label = "Recorded denials"
    link  = "${var.datadog_app_url}/logs?query=${join(" ", local.deployment_tags)} {{$env}} {{$service}} message:denied&from_ts={{timestamp_start}}&to_ts={{timestamp_end}}"
    }, {
    label = "Telemetry export warnings"
    link  = "${var.datadog_app_url}/logs?query=${join(" ", local.deployment_tags)} {{$env}} {{$service}} @${var.log_attribute_paths.module}:Ecluse.Runtime.Telemetry.Resolve&from_ts={{timestamp_start}}&to_ts={{timestamp_end}}"
  }]
  metric_widgets = {
    for panel in local.specs : panel.id => merge({
      title = panel.id == "memory_ratio" ? "${panel.title} (${var.instance_tag == null ? "fleet totals" : "per instance"})" : panel.title
      type  = panel.type
      requests = [merge({
        response_format = panel.type == "query_value" ? "scalar" : "timeseries"
        queries         = local.queries[panel.id]
        formulas = try(panel.formulas, [for index, series in local.series[panel.id] : {
          formula = series.modifier == "histogram" && panel.unit == "ms" ? "1000 * q${index}" : "q${index}"
          alias   = series.alias
        }])
      }, panel.type == "timeseries" ? { display_type = "line" } : {})]
      custom_links = local.context_links
      }, {
      query_value = {
        autoscale   = true
        precision   = 2
        custom_unit = panel.unit
      }
      timeseries = {
        show_legend = true
        yaxis       = { label = panel.unit, include_zero = true, scale = "linear" }
        markers = contains(keys(var.thresholds), panel.id) ? [{
          value        = "y = ${var.thresholds[panel.id]}"
          display_type = "warning dashed"
          label        = "Operator reference"
        }] : []
      }
    }[panel.type])
  }
  groups = [
    { id = "overview", title = "Overview" },
    { id = "serving", title = "01 Serving and policy" },
    { id = "capacity", title = "02 Capacity and caches" },
    { id = "dependencies", title = "03 Dependencies and credentials" },
    { id = "advisories", title = "04 Advisory consumers and Pilot" },
    { id = "background", title = "05 Mirror and Dredger" },
    { id = "external", title = "06 Existing platform and usage signals" }
  ]
  operator_notes = join("\n\n", concat([
    "Deployment scope: ${join(", ", local.deployment_tags)}. HTTP traffic includes proxy probes. Advisory ecosystem selection applies only to advisory metrics.",
    "Missing data is unknown. Sparse counters can be absent before the first event. Cache and breaker gauges retain their last recorded value. The dashboard does not prove readiness.",
    "Recorded version denials are not denied requests or a complete count of versions removed from successful metadata responses. Dredger examined and outcome counts must not be added together.",
    var.instance_tag == null ? "Memory ratios use fleet totals because no producer identity tag is configured. These ratios can hide a saturated instance." : "Memory ratios group by ${var.instance_tag}. This tag must uniquely identify a telemetry producer.",
    "Idle credential expiry alone is not a provider fault. Advisory publication age, not installation age, governs freshness."
    ], [for name, url in var.runbook_urls : "[${name}](${url})"],
    [for name, id in var.health_monitor_ids : "[Health: ${name}](${var.datadog_app_url}/monitors/${id}) (fixed monitor scope, independent of selectors)"]
  ))
  dashboard = {
    title       = var.title
    description = "Deployment-configurable Écluse operator example. ${var.description}"
    layout_type = "ordered"
    reflow_type = "fixed"
    tags        = sort(tolist(var.dashboard_tags))
    template_variables = concat([
      { name = "env", prefix = "env", default = var.environment },
      { name = "service", prefix = "service", default = "*", available_values = local.active_services }
      ], var.sections.advisories || var.sections.pilot ? [
      { name = "advisory_ecosystem", prefix = "ecosystem", default = "*", available_values = sort(tolist(var.advisory_ecosystems)) }
    ] : [])
    widgets = concat(
      [{ definition = { type = "note", content = local.operator_notes, background_color = "white", font_size = "14", text_align = "left", show_tick = false }, layout = { x = 0, y = 0, width = 12, height = 3 } }],
      [for group in local.groups : {
        definition = {
          type        = "group"
          title       = group.title
          layout_type = "ordered"
          show_title  = true
          widgets = [for index, panel in [for spec in local.specs : spec if spec.group == group.id] : {
            definition = local.metric_widgets[panel.id]
            layout     = group.id == "overview" ? { x = (index % 4) * 3, y = floor(index / 4) * 2, width = 3, height = 2 } : { x = (index % 3) * 4, y = floor(index / 3) * 3, width = 4, height = 3 }
          }]
        }
      } if anytrue([for panel in local.specs : panel.group == group.id])],
      var.sections.logs ? [{ definition = {
        type                = "log_stream"
        title               = "Warnings and errors: indexed investigation evidence"
        query               = "${local.log_scope} status:(warn OR error)"
        indexes             = var.log_indexes
        columns             = ["timestamp", "status", "service", "message", "@${var.log_attribute_paths.package}", "@${var.log_attribute_paths.rule}"]
        show_date_column    = true
        show_message_column = true
        message_display     = "expanded-md"
        sort                = { column = "timestamp", order = "desc" }
      } }] : []
    )
  }
}

resource "datadog_dashboard_json" "this" {
  dashboard = jsonencode(local.dashboard)

  lifecycle {
    precondition {
      condition     = length(setsubtract(toset(keys(var.metric_names)), toset(keys(local.catalogue)))) == 0
      error_message = "Metric overrides must name an instrument in signals.json."
    }
    precondition {
      condition     = length(setsubtract(toset(keys(var.latency_statistics)), toset(local.latency_names))) == 0
      error_message = "Latency statistics must name a histogram in signals.json."
    }
    precondition {
      condition     = length(setsubtract(toset(keys(var.thresholds)), toset([for panel in local.specs : panel.id if panel.type == "timeseries"]))) == 0
      error_message = "Thresholds must reference enabled timeseries panel IDs."
    }
  }
}
