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

# Haddock flags shared by the local `docs` target and the Pages `docs-site`
# target: hyperlinked source (clickable identifiers) + the quick-jump overlay.
HADDOCK_FLAGS := --haddock-hyperlink-source --haddock-quickjump

.DEFAULT_GOAL := help
.PHONY: help update build test test-integration test-smoke test-all coverage \
        gen-version-fixtures format format-check lint sast check run docs \
        docs-site nix-build nix-check docker-build docker-push docker-sign \
        sbom attest clean

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

# Suite to measure for `coverage`; override to cover another gating tier, e.g.
# `make coverage SUITE=ecluse-integration` (needs Docker, like the suite itself).
SUITE ?= ecluse-unit

coverage: ## Generate $(SUITE) coverage as Codecov JSON under coverage/
	$(NIX) bash scripts/coverage.sh $(SUITE)

gen-version-fixtures: ## Regenerate version-ordering fixtures from the reference tools (node-semver / packaging / Gem::Version)
	$(NIX) bash scripts/gen-version-fixtures.sh

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

# Hyperlinked source (clickable identifiers jump straight to the code) plus the
# quick-jump fuzzy-search overlay make the API docs pleasant to read locally.
# Output lands under dist-newstyle (git-ignored). We open it in the browser via
# xdg-open when present, and always print the path so the target stays usable in
# headless/CI contexts.
docs: ## Build hyperlinked, searchable Haddock HTML for the library and open it
	$(NIX) cabal haddock lib:ecluse $(HADDOCK_FLAGS)
	@html=$$(find dist-newstyle -path '*/doc/html/ecluse/index.html' | head -n1); \
	  echo "Haddock: $$html"; \
	  command -v xdg-open >/dev/null 2>&1 && xdg-open "$$html" >/dev/null 2>&1 &

# Same docs, staged into ./_site for the GitHub Pages workflow to upload. The
# Haddock output path embeds the arch + GHC version, so we locate it rather than
# hard-code it. .nojekyll keeps Pages from touching the static assets.
docs-site: ## Build Haddock and stage it into ./_site for GitHub Pages
	$(NIX) cabal haddock lib:ecluse $(HADDOCK_FLAGS)
	@src=$$(find dist-newstyle -path '*/doc/html/ecluse' -type d | head -n1); \
	  rm -rf _site && mkdir -p _site && cp -R "$$src"/. _site/ && touch _site/.nojekyll; \
	  echo "Staged Haddock into ./_site"

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

# SBOM of exactly the binary the image ships — the runtime closure of
# `.#ecluse-bin` (stripped, static), so it lists what is really in the image
# (incl. the curl/openssl/krb5 chunk) without a scanner's dynamic-build noise.
# The Haskell deps are statically inside the `ecluse` component, pinned by
# flake.lock. SPDX + CycloneDX. See CONTRIBUTING.md → "Supply-chain attestations".
sbom: ## Generate the image SBOM (SPDX + CycloneDX) under sbom/
	@mkdir -p sbom
	$(NIX) sbomnix --spdx sbom/ecluse.spdx.json --cdx sbom/ecluse.cdx.json --csv sbom/ecluse.csv .#ecluse-bin

# Attest the SBOM and SLSA provenance to $(IMAGE):$(TAG), keyless. cosign
# resolves the tag to its digest, so both attestations bind to the digest (and
# land under cosign's `.att`/`.sbom` tags — no registry referrers API needed).
attest: ## Attest SBOM + SLSA provenance to $(IMAGE):$(TAG) (cosign keyless; needs OIDC + creds)
	@test -n "$(TAG)" || { echo "set TAG, e.g. make attest TAG=0.1.0"; exit 1; }
	@test -f sbom/ecluse.spdx.json || { echo "run 'make sbom' first"; exit 1; }
	TAG="$(TAG)" $(NIX) bash scripts/gen-provenance.sh provenance.predicate.json
	@printf '%s' "$$DOCKERHUB_TOKEN" | $(NIX) cosign login docker.io -u "$$DOCKERHUB_USERNAME" --password-stdin
	$(NIX) cosign attest --yes --type spdxjson --predicate sbom/ecluse.spdx.json "$(IMAGE):$(TAG)"
	$(NIX) cosign attest --yes --type slsaprovenance1 --predicate provenance.predicate.json "$(IMAGE):$(TAG)"

clean: ## Remove build artifacts
	rm -rf dist-newstyle result sbom provenance.predicate.json
