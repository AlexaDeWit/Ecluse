# Écluse — one entry point for every task, locally and in CI.
#
# Tools come from the Nix dev shell (pinned by flake.lock). Recipes run the tools
# directly when you are inside the shell (`nix develop` / direnv sets
# IN_NIX_SHELL), and otherwise wrap themselves in `nix develop --command` — so
# every target works whether or not you have entered the shell. CI runs
# `nix develop --command make <target>` (entering the shell once).

# Empty inside the dev shell; the wrapper otherwise.
NIX := $(if $(IN_NIX_SHELL),,nix develop --command)

# Tracked Haskell sources, for the formatter and linter.
HS := $(shell git ls-files '*.hs')

# Published image repository. Override for forks/mirrors: `make docker-push IMAGE=…`.
IMAGE ?= docker.io/alexadewit/ecluse

.DEFAULT_GOAL := help
.PHONY: help update build test test-integration test-smoke test-all \
        format format-check lint sast check run nix-build nix-check \
        docker-build docker-push docker-sign clean

help: ## Show this help
	@grep -hE '^[a-zA-Z_-]+:.*?## ' $(MAKEFILE_LIST) \
	  | awk 'BEGIN{FS=":.*?## "}{printf "  %-16s %s\n", $$1, $$2}'

update: ## Refresh the cabal package index
	$(NIX) cabal update

build: ## Build library, executable, and all test suites
	$(NIX) cabal build all --enable-tests

test: ## Run the fast, gating unit suite
	$(NIX) cabal test ecluse-unit --test-show-details=direct

test-integration: ## Run the integration suite (requires a Docker daemon)
	$(NIX) cabal test ecluse-integration --test-show-details=direct

test-smoke: ## Run the smoke suite (live registries; non-gating)
	$(NIX) cabal test ecluse-smoke --test-show-details=direct

test-all: test test-integration test-smoke ## Run every test suite

format: ## Reformat Haskell sources in place
	$(NIX) fourmolu --mode inplace $(HS)

format-check: ## Check formatting without writing
	$(NIX) fourmolu --mode check $(HS)

lint: ## Run hlint
	$(NIX) hlint $(HS)

sast: ## Static analysis (Semgrep, registry rules)
	$(NIX) semgrep scan --config auto --severity ERROR --severity WARNING --error .

check: build test format-check lint sast ## Run everything the CI gate requires

run: ## Run the proxy
	$(NIX) cabal run ecluse

nix-build: ## Build the release artifact via Nix (hermetic)
	nix build

nix-check: ## Run the hermetic flake checks
	nix flake check

docker-build: ## Build the lean OCI image via Nix → ./result (a docker-archive)
	nix build .#dockerImage

# Push/sign read DOCKERHUB_USERNAME / DOCKERHUB_TOKEN from the environment and
# require TAG (immutable tags: no `latest`). The token is piped via stdin, never
# placed on the command line, and the login line is not echoed.
docker-push: ## Push ./result to $(IMAGE):$(TAG) (needs TAG + DOCKERHUB_USERNAME/TOKEN)
	@test -n "$(TAG)" || { echo "set TAG, e.g. make docker-push TAG=0.1.0"; exit 1; }
	@printf '%s' "$$DOCKERHUB_TOKEN" | $(NIX) skopeo login docker.io -u "$$DOCKERHUB_USERNAME" --password-stdin
	$(NIX) skopeo copy docker-archive:./result "docker://$(IMAGE):$(TAG)"

docker-sign: ## Sign $(IMAGE):$(TAG) with cosign (keyless; needs OIDC + creds)
	@test -n "$(TAG)" || { echo "set TAG, e.g. make docker-sign TAG=0.1.0"; exit 1; }
	@printf '%s' "$$DOCKERHUB_TOKEN" | $(NIX) cosign login docker.io -u "$$DOCKERHUB_USERNAME" --password-stdin
	$(NIX) cosign sign --yes "$(IMAGE):$(TAG)"

clean: ## Remove build artifacts
	rm -rf dist-newstyle result
