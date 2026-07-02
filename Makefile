# Makefile for Écluse
#
# Delegates all tasks to Taskfile.yml, wrapping them in `nix develop` when
# running outside the Nix shell to ensure the toolchain is always available.
# This is the primary entry point for human contributors and automation.

TASK := $(if $(IN_NIX_SHELL),task,nix develop --command task)

.PHONY: default
default:
	@$(TASK) --list

.PHONY: check
check:
	@$(TASK) check

.PHONY: test
test:
	@$(TASK) test

.PHONY: test-integration
test-integration:
	@$(TASK) test-integration

.PHONY: test-e2e
test-e2e:
	@$(TASK) test-e2e

.PHONY: docs
docs:
	@$(TASK) docs

.PHONY: docs-site
docs-site:
	@$(TASK) docs-site

.PHONY: site
site:
	@$(TASK) site

.PHONY: format
format:
	@$(TASK) format

.PHONY: lint
lint:
	@$(TASK) lint

.PHONY: ci
ci:
	@$(TASK) ci

.PHONY: gate
gate:
	@$(TASK) gate

.PHONY: bench
bench:
	@$(TASK) bench

.PHONY: clean
clean:
	@$(TASK) clean

# Catch-all: pass any other target through to task
%:
	@$(TASK) $@
