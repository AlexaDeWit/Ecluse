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
.PHONY: help update build test test-integration test-smoke test-all doctest \
        coverage freeze gen-version-fixtures new-worktree format format-check lint sast \
        cabal-check lint-workflows weeder check run docs \
        docs-check docs-site site nix-build nix-check docker-build docker-push sbom scan \
        scan-vulnix clean

help: ## Show this help
	@grep -hE '^[a-zA-Z_-]+:.*?## ' $(MAKEFILE_LIST) \
	  | awk 'BEGIN{FS=":.*?## "}{printf "  %-16s %s\n", $$1, $$2}'

update: ## Refresh the cabal package index
	$(NIX) cabal update

# Regenerate the cabal lockfile. Advances index-state in cabal.project to the
# latest fetched index, then re-solves and rewrites cabal.project.freeze with
# exact versions. This is a deliberate, reviewable dependency bump for the cabal
# path; commit both files together. (The Nix path is bumped separately via
# `nix flake update`.) See CONTRIBUTING → "Dependency locking".
freeze: ## Regenerate cabal.project.freeze at the latest index-state
	$(NIX) cabal update
	$(NIX) bash -c 'ts=$$(cabal update 2>&1 | sed -n "s/.*index-state is set to \(.*\)\.$$/\1/p" | tail -1); \
	  [ -n "$$ts" ] && sed -i "s/^index-state:.*/index-state: $$ts/" cabal.project; \
	  cabal freeze'

build: ## Build library, executable, and all test suites
	$(NIX) cabal build all --enable-tests

test: ## Run the fast, gating unit suite
	$(NIX) cabal test ecluse-unit --test-show-details=direct

test-integration: ## Run the integration suite (requires a Docker daemon)
	$(NIX) cabal test ecluse-integration --test-show-details=direct

test-smoke: ## Run the smoke suite (live registries; non-gating)
	$(NIX) cabal test ecluse-smoke --test-show-details=direct

test-all: test test-integration test-smoke ## Run every test suite

# doctest runs the >>> examples embedded in Haddock comments as tests, so the
# documentation cannot drift from the code. It runs via
# `cabal repl --with-ghc=doctest`, which inherits the package's exact build
# configuration — most importantly the relude prelude (a cabal mixin) — instead
# of re-deriving GHC flags by hand. The repl-options relax two of the package's
# warnings for the doctest session only: -Wwarn undoes the package -Werror
# (enforcing warnings is `make build`'s job; doctest only checks each example's
# result), and -Wno-missing-export-lists silences the synthetic Ghci wrapper
# modules doctest generates, which have no export list (the same exemption the
# test suites already take in the cabal file).
doctest: ## Run the Haddock >>> examples as tests (doctest)
	$(NIX) cabal repl --with-ghc=doctest --repl-options="-Wwarn -Wno-missing-export-lists" lib:ecluse

# Suite to measure for `coverage`; override to cover another gating tier, e.g.
# `make coverage SUITE=ecluse-integration` (needs Docker, like the suite itself).
SUITE ?= ecluse-unit

coverage: ## Generate $(SUITE) coverage as Codecov JSON under coverage/
	$(NIX) bash scripts/coverage.sh $(SUITE)

gen-version-fixtures: ## Regenerate version-ordering fixtures from the reference tools (node-semver / packaging / Gem::Version)
	$(NIX) bash scripts/gen-version-fixtures.sh

# Create an isolated agent worktree on BRANCH and warm its HLS index in the
# background (a `make build` populating dist-newstyle, which HLS reuses) so the
# agent's first agent-lsp navigation call lands hot rather than a cold typecheck
# mid-session. One worktree per agent; cap concurrency at 2-3 and stagger
# creations (HLS is memory-hungry). NOT $(NIX)-wrapped: git/bash are ambient and
# the script enters the flake itself, only for the background build. Override
# BASE (default origin/main) and DIR (default .claude/worktrees/<branch-slug>).
# See AGENTS.md -> "Build & Tooling" and planning/orchestration-strategy.md.
new-worktree: ## Create an agent worktree on BRANCH and warm its HLS index (BASE=…, DIR=…)
	@test -n "$(BRANCH)" || { echo "set BRANCH, e.g. make new-worktree BRANCH=slice/foo"; exit 1; }
	bash scripts/new-worktree.sh "$(BRANCH)" "$(BASE)" "$(DIR)"

format: ## Reformat Haskell sources in place
	$(NIX) fourmolu --mode inplace $(HS)

format-check: ## Check formatting without writing
	$(NIX) fourmolu --mode check $(HS)

lint: ## Run hlint
	$(NIX) hlint $(HS)

# --sarif-output writes a SARIF report *alongside* the normal text output (it
# doesn't replace it), so the human-readable findings still print to the log and
# CI can upload the SARIF to GitHub code scanning (Security tab). --error keeps
# this gating: a non-zero exit on any ERROR/WARNING finding.
sast: ## Static analysis (Semgrep, registry rules; also writes semgrep.sarif)
	$(NIX) semgrep scan --config auto --severity ERROR --severity WARNING --sarif-output=semgrep.sarif --error .

# cabal check validates the package description: well-formed .cabal, no fields
# that would block distribution, sane structure. It exits 0 even on *warnings*,
# so we fail on any "Warning:"/"Error:" line to keep the metadata clean going
# forward (the package is currently warning-free).
cabal-check: ## Validate the .cabal package metadata (cabal check, strict)
	$(NIX) bash -c 'out=$$(cabal check 2>&1); printf "%s\n" "$$out"; case "$$out" in *Warning:*|*Error:*) exit 1 ;; esac'

# Lint the GitHub Actions workflows: actionlint (correctness) + zizmor (security).
# zizmor's online audits use $GH_TOKEN when present (CI passes the job token);
# without one it runs its offline audit subset and prints a notice. Runs from the
# lean `.#workflow-lint` shell in CI.
lint-workflows: ## Lint GitHub Actions workflows (actionlint + zizmor)
	$(NIX) actionlint
	$(NIX) zizmor .github/workflows/

# weeder reports library code not reachable from the application entry point —
# i.e. built (and usually tested) but not yet wired into the running proxy. It
# reads .hie files, so we build ONLY the library + executable with -fwrite-ide-info
# into a dedicated builddir (no test suites, so "reachable" means "used by the app",
# not "covered by a test"); roots + rationale live in weeder.toml. INFORMATIONAL:
# weeder exits non-zero when it finds something — useful locally — but the CI job
# never fails on it (the findings are deliberate feature scaffolding); it is not in
# `make check` and not a `gate` dependency.
weeder: ## Report app-unreachable library code (weeder; informational, non-gating)
	$(NIX) cabal build exe:ecluse --builddir=dist-weeder --ghc-options=-fwrite-ide-info
	$(NIX) weeder --hie-directory dist-weeder

check: build test doctest format-check lint sast cabal-check lint-workflows ## Run everything the CI gate requires

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

# Faster Haddock build for the CI gate, scoped to our own library. Two changes
# vs docs-site: it drops --haddock-hyperlink-source (the per-module source render),
# and adds --disable-documentation so cabal does NOT (re)build the ~130-package
# dependency haddock closure — it documents only lib:ecluse, which is all the gate
# needs to validate (our doc comments compile; no broken modules). The dropped
# dependency cross-links matter only for the published site, which the Pages
# publish (docs-site, full flags) still builds on main. A gate failure here still
# means our docs are broken. See CONTRIBUTING.md → "Continuous Integration".
docs-check: ## Build Haddock for the CI gate (our library only; no dep docs, no source links)
	$(NIX) cabal haddock lib:ecluse --haddock-quickjump --disable-documentation

# Build the library Haddock and stage it under ./_site/api for the GitHub Pages
# workflow to upload — the site root is left free for the home page (see `site`).
# The Haddock output path embeds the arch + GHC version, so we locate it rather
# than hard-code it. .nojekyll (at the _site root) keeps Pages from touching the
# static assets.
docs-site: ## Build Haddock and stage it under ./_site/api for GitHub Pages
	$(NIX) cabal haddock lib:ecluse $(HADDOCK_FLAGS)
	@src=$$(find dist-newstyle -path '*/doc/html/ecluse' -type d | head -n1); \
	  rm -rf _site && mkdir -p _site/api && cp -R "$$src"/. _site/api/ && touch _site/.nojekyll; \
	  echo "Staged Haddock into ./_site/api"

# Assemble the full Pages site that the workflow uploads:
#   /       the landing page + the user docs rendered from Markdown (pandoc)
#   /api    the library Haddock (via docs-site)
# Kept separate from docs-site so the gate-repro / local Haddock build stays about
# the docs alone. Build inputs (template, Lua filters) live in web/; only web/static
# is published. Mermaid renders client-side from a hash-pinned bundle vendored into
# _site/vendor (the flake's `mermaidJs`), so the published site has no external
# runtime dependency.
PANDOC_FLAGS := --standalone --from gfm --template web/template.html --lua-filter web/mermaid.lua --lua-filter web/links.lua
site: docs-site ## Assemble the Pages site (landing + rendered docs at /, Haddock under /api)
	@cp -R web/static/. _site/
	@cp docs/branding/logo.svg docs/branding/favicon-32.png docs/social-preview.png _site/
	@$(NIX) sh -c 'mkdir -p _site/vendor && cp "$$MERMAID_JS" _site/vendor/mermaid.min.js'
	$(NIX) pandoc MOTIVATION.md   -o _site/motivation.html   $(PANDOC_FLAGS) -M title="Why Écluse?"
	$(NIX) pandoc ALTERNATIVES.md -o _site/alternatives.html $(PANDOC_FLAGS) -M title="Alternatives"
	$(NIX) pandoc USAGE.md        -o _site/usage.html        $(PANDOC_FLAGS) -M title="Operator Manual"
	@echo "Assembled ./_site (landing + rendered docs at /, Haddock under /api)"

nix-build: ## Build the release artifact via Nix (hermetic)
	nix build

nix-check: ## Run the hermetic flake checks
	nix flake check

docker-build: ## Build the lean OCI image via Nix → ./result (a docker-archive)
	nix build .#dockerImage

# docker-push reads DOCKERHUB_USERNAME / DOCKERHUB_TOKEN from the environment and
# require TAG (immutable tags: no `latest`). The token is piped via stdin, never
# placed on the command line, and the login line is not echoed.
docker-push: ## Push ./result to $(IMAGE):$(TAG) (needs TAG + DOCKERHUB_USERNAME/TOKEN)
	@test -n "$(TAG)" || { echo "set TAG, e.g. make docker-push TAG=0.1.0"; exit 1; }
	@printf '%s' "$$DOCKERHUB_TOKEN" | $(NIX) skopeo login docker.io -u "$$DOCKERHUB_USERNAME" --password-stdin
	$(NIX) skopeo copy docker-archive:./result "docker://$(IMAGE):$(TAG)"

# SBOM of exactly the binary the image ships — the runtime closure of
# `.#ecluse-bin` (stripped, static), so it lists what is really in the image
# (incl. the curl/openssl/krb5 chunk) without a scanner's dynamic-build noise.
# The Haskell deps are statically inside the `ecluse` component, pinned by
# flake.lock. SPDX + CycloneDX. See CONTRIBUTING.md → "Supply-chain attestations".
sbom: ## Generate the image SBOM (SPDX + CycloneDX) under sbom/
	@mkdir -p sbom
	$(NIX) sbomnix --spdx sbom/ecluse.spdx.json --cdx sbom/ecluse.cdx.json --csv sbom/ecluse.csv .#ecluse-bin

# Scan the image's dependency closure for known CVEs. `scan` is the authority:
# sbomnix builds the SBOM of the shipped binary, grype scans it against its
# maintained DB → severity-rated table + grype.json. `scan-vulnix` is the
# secondary Nix-native cross-check (more comprehensive and patch-aware, but no
# severity grades). Both are report-only. See CONTRIBUTING.md → "Vulnerability
# scanning".
scan: ## Scan the image closure for CVEs (grype over the SBOM → grype.json)
	@mkdir -p sbom
	$(NIX) sbomnix --cdx sbom/ecluse.cdx.json .#ecluse-bin
	$(NIX) grype sbom:sbom/ecluse.cdx.json -o table -o json=grype.json

scan-vulnix: ## Secondary Nix-native cross-check (vulnix; comprehensive, no severity)
	-$(NIX) bash -c 'vulnix -C "$$(nix build .#ecluse-bin --no-link --print-out-paths)"'

clean: ## Remove build artifacts
	rm -rf dist-newstyle result sbom grype.json
