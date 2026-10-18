output "dashboard_id" {
  description = "Created dashboard ID."
  value       = datadog_dashboard_json.this.id
}

output "dashboard_url" {
  description = "Created dashboard URL."
  value       = datadog_dashboard_json.this.url
}

output "dashboard_json" {
  description = "Rendered dashboard definition, available at plan time."
  value       = jsonencode(local.dashboard)
}

output "metric_queries" {
  description = "Queries by panel ID for validation and review."
  value       = local.queries
}
