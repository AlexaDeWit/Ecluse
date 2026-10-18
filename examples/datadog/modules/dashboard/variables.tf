variable "title" {
  description = "Dashboard title."
  type        = string
  default     = "Écluse operations"
}

variable "description" {
  description = "Operator context appended to the dashboard description."
  type        = string
  default     = ""
}

variable "dashboard_tags" {
  description = "Dashboard ownership tags. These do not tag telemetry."
  type        = set(string)
  default     = []
}

variable "scope_tags" {
  description = "Exact deployment tags present on metrics and logs. No wildcard scopes."
  type        = map(string)
  validation {
    condition = length(var.scope_tags) > 0 && alltrue([
      for key, value in var.scope_tags :
      can(regex("^[a-z][a-z0-9_.-]*$", key)) &&
      can(regex("^[a-z0-9][a-z0-9_./:-]*$", value)) &&
      !contains(["env", "service", "ecosystem"], key)
    ])
    error_message = "Supply exact lowercase deployment tags. env, service, and ecosystem have separate selectors."
  }
}

variable "environment" {
  description = "Default environment in the env template variable. The deployment tags always apply."
  type        = string
  validation {
    condition     = can(regex("^[a-z0-9][a-z0-9_.-]*$", var.environment))
    error_message = "Use a nonempty lowercase environment name without query operators."
  }
}

variable "services" {
  description = "Services by role. Embedded roles can share the same service."
  type = object({
    proxy   = optional(set(string), ["ecluse"])
    mirror  = optional(set(string), ["ecluse"])
    dredger = optional(set(string), ["ecluse"])
    pilot   = optional(set(string), ["ecluse"])
  })
  default = {}
  validation {
    condition = alltrue([
      for names in values(var.services) : length(names) > 0 && alltrue([
        for name in names : can(regex("^[a-z0-9][a-z0-9_.-]*$", name))
      ])
    ])
    error_message = "Each role needs at least one exact lowercase service name."
  }
}

variable "instance_tag" {
  description = "Tag that uniquely identifies a producer, such as pod_name. Null shows fleet totals only."
  type        = string
  default     = null
  validation {
    condition     = var.instance_tag == null ? true : can(regex("^[a-z][a-z0-9_.-]*$", var.instance_tag))
    error_message = "Use a metric tag key, or null when producer identity is unavailable."
  }
}

variable "sections" {
  description = "Optional capabilities. Disabled sections produce no widgets."
  type = object({
    advisories = optional(bool, false)
    pilot      = optional(bool, false)
    mirror     = optional(bool, false)
    dredger    = optional(bool, false)
    logs       = optional(bool, true)
  })
  default = {}
}

variable "advisory_ecosystems" {
  description = "Ecosystems offered by the advisory-only selector."
  type        = set(string)
  default     = ["npm", "pypi"]
  validation {
    condition     = length(var.advisory_ecosystems) > 0 && alltrue([for value in var.advisory_ecosystems : can(regex("^[a-z][a-z0-9_-]*$", value))])
    error_message = "Provide at least one exact ecosystem name."
  }
}

variable "metric_names" {
  description = "Overrides from original OTel metric names to received Datadog metric names."
  type        = map(string)
  default     = {}
  validation {
    condition     = alltrue([for value in values(var.metric_names) : can(regex("^[a-zA-Z][a-zA-Z0-9_.]*$", value))])
    error_message = "Metric overrides must be names, not queries."
  }
}

variable "http_tag_keys" {
  description = "Received Datadog tag for the OTel HTTP status attribute."
  type = object({
    status = optional(string, "http.response.status_code")
  })
  default = {}
  validation {
    condition     = alltrue([for key in values(var.http_tag_keys) : can(regex("^[a-z][a-z0-9_.-]*$", key))])
    error_message = "HTTP tag mappings must be tag keys."
  }
}

variable "latency_statistics" {
  description = "Statistics by latency metric name. Percentiles require existing Datadog percentile support."
  type        = map(set(string))
  default     = {}
  validation {
    condition     = alltrue([for stats in values(var.latency_statistics) : length(stats) > 0 && alltrue([for stat in stats : contains(["avg", "p95", "p99"], stat)])])
    error_message = "Each latency metric needs one or more of avg, p95, p99."
  }
}

variable "thresholds" {
  description = "Optional horizontal reference lines by panel ID, in the panel's displayed unit. No monitors are created."
  type        = map(number)
  default     = {}
  validation {
    condition     = alltrue([for value in values(var.thresholds) : value >= 0])
    error_message = "Reference thresholds cannot be negative."
  }
}

variable "log_indexes" {
  description = "Existing log indexes to search."
  type        = list(string)
  default     = ["*"]
  validation {
    condition     = length(var.log_indexes) > 0 && alltrue([for name in var.log_indexes : name == "*" || can(regex("^[a-z][a-z0-9-]*$", name))])
    error_message = "Select existing index names or *."
  }
}

variable "log_attribute_paths" {
  description = "Structured application JSON fields. Deployment, env, and service tags must also exist."
  type = object({
    package = optional(string, "data.package")
    rule    = optional(string, "data.rule")
    module  = optional(string, "data.module")
    event   = optional(string, "data.event")
  })
  default = {}
  validation {
    condition     = alltrue([for path in values(var.log_attribute_paths) : can(regex("^[a-zA-Z][a-zA-Z0-9_.]*$", path))])
    error_message = "Log paths must be attribute paths without @ or query syntax."
  }
}

variable "datadog_app_url" {
  description = "Datadog application origin, for example https://app.datadoghq.eu."
  type        = string
  default     = "https://app.datadoghq.com"
  validation {
    condition     = can(regex("^https://[a-z0-9.-]+$", var.datadog_app_url))
    error_message = "Use an HTTPS origin without a trailing slash or path."
  }
}

variable "runbook_urls" {
  description = "Links shown in the dashboard's operator notes."
  type        = map(string)
  default = {
    Operations = "https://ecluse-proxy.com/docs/operations/"
  }
  validation {
    condition     = alltrue([for url in values(var.runbook_urls) : can(regex("^https://[^ ]+$", url))])
    error_message = "Runbook links must use HTTPS."
  }
}

variable "health_monitor_ids" {
  description = "Links to existing deployment-scoped monitors. Their scope is fixed and independent of dashboard selectors."
  type        = map(string)
  default     = {}
  validation {
    condition     = alltrue([for id in values(var.health_monitor_ids) : can(regex("^[0-9]+$", id))])
    error_message = "Existing monitor IDs must be numeric."
  }
}

variable "external_panels" {
  description = "Existing platform or attributed usage metrics. These need the same deployment, env, and service tags as application metrics."
  type = list(object({
    title       = string
    metric      = string
    role        = optional(string, "proxy")
    aggregation = optional(string, "max")
    rate        = optional(bool, false)
    unit        = optional(string, "")
    group_by    = optional(list(string), [])
  }))
  default = []
  validation {
    condition = alltrue([for panel in var.external_panels :
      can(regex("^[a-zA-Z][a-zA-Z0-9_.]*$", panel.metric)) &&
      contains(["proxy", "mirror", "dredger", "pilot"], panel.role) &&
      contains(["sum", "avg", "min", "max"], panel.aggregation) &&
      alltrue([for tag in panel.group_by : can(regex("^[a-z][a-z0-9_.-]*$", tag))])
    ])
    error_message = "External panels need a metric name, a supported role and aggregation, and valid group-by keys."
  }
}
