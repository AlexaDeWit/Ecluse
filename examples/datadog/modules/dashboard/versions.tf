terraform {
  required_version = ">= 1.7, < 2.0"
  required_providers {
    datadog = {
      source  = "registry.terraform.io/DataDog/datadog"
      version = "~> 4.27.0"
    }
  }
}
