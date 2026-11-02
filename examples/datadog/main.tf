terraform {
  required_version = ">= 1.7, < 2.0"
  required_providers {
    datadog = {
      source  = "registry.terraform.io/DataDog/datadog"
      version = "~> 4.27.0"
    }
  }
}

provider "datadog" {
  api_url = var.datadog_api_url
}

variable "datadog_api_url" {
  description = "Datadog API endpoint for your site. Supply credentials through the provider environment variables."
  type        = string
  default     = "https://api.datadoghq.com/"
}

variable "environment" {
  description = "Deployment environment."
  type        = string
}

variable "deployment" {
  description = "Value of the ecluse_deployment tag added by your collector to all signals."
  type        = string
}

module "dashboard" {
  source      = "./modules/dashboard"
  environment = var.environment
  scope_tags  = { ecluse_deployment = var.deployment }
}

output "dashboard_json" {
  description = "Dashboard definition for review before applying."
  value       = module.dashboard.dashboard_json
}

output "dashboard_url" {
  description = "URL after the operator applies the example."
  value       = module.dashboard.dashboard_url
}
