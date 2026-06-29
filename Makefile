# Écluse — one entry point for every task, locally and in CI.
#
# Tools come from the Nix dev shell (pinned by flake.lock). Recipes run the tools
# directly when you are inside the shell (`nix develop` / direnv sets
# IN_NIX_SHELL), and otherwise wrap themselves in `nix develop --command` — so
# every target works whether or not you have entered the shell. CI runs
# `nix develop --command make <target>` (entering the shell once).

# Empty inside the dev shell; the wrapper otherwise.
NIX := $(if $(IN_NIX_SHELL),,nix develop --command)

# The benchmark targets run from the lean `.#bench` shell (the CI toolchain plus the
# flame-graph tooling), not the default shell. Empty inside a dev shell (CI enters
# `.#bench` itself); otherwise it enters `.#bench`. See flake.nix `devShells.bench`.
NIX_BENCH := $(if $(IN_NIX_SHELL),,nix develop .#bench --command)

# Tracked Haskell sources, for the formatter and linter.
HS := $(shell git ls-files '*.hs')

# Tracked shell scripts, for shellcheck (`make lint-scripts`).
SH := $(shell git ls-files '*.sh')

# Published image repository. Override for forks/mirrors: `make docker-push IMAGE=…`.
IMAGE ?= docker.io/alexadewit/ecluse

# Haddock flags shared by the local `docs` target and the Pages `docs-site`
# target: hyperlinked source (clickable identifiers) + the quick-jump overlay.
HADDOCK_FLAGS := --haddock-hyperlink-source --haddock-quickjump

# The build directory `docs-site` builds Haddock + the OpenAPI manifest into.
# Defaults to the regular dist-newstyle, so a local `make docs-site` / `make site`
# is unchanged. The Pages publish overrides it (`make site DOCS_BUILDDIR=dist-docs`)
# so its build products — including the documentation-variant dependency closure,
# which differs from the regular build variant `cabal build` produces — land in a
# tree cached under their own key, never colliding with the gate's dist-newstyle.
DOCS_BUILDDIR ?= dist-newstyle

.DEFAULT_GOAL := help
.PHONY: help update build test test-integration test-smoke test-e2e test-all doctest \
        coverage coverage-unit freeze gen-version-fixtures new-worktree format format-check lint sast \
        cabal-check lint-workflows lint-scripts test-scripts weeder check gate run docs \
        docs-check docs-site site nix-build nix-check docker-build docker-push sbom scan \
        scan-vulnix clean version tag stan stan-all bench bench-profile bench-load \
        perf-acceptance

help: ## Show this help
	@grep -hE '^[a-zA-Z_-]+:.*?## ' $(MAKEFILE_LIST) \
	  | awk 'BEGIN{FS=":.*?## "}{printf "  %-16s %s\n", $$1, $$2}'

# The single source of truth for the release version is ecluse.cabal's `version:`
# field (semver, e.g. 0.1.0). Everything downstream derives from it: `make tag` cuts
# the git tag from it, and release.yml asserts the pushed tag matches it (the release
# fails on drift). Read through `cabal info` — cabal's own parser of the package
# description, not a hand-grep of the file — then strip the `<name>-` off its
# `* ecluse-<version>` line. (cabal info needs the toolchain, so unlike a raw grep
# this enters the Nix shell; CI's guard job pays that, but releases are infrequent.)
version: ## Print the release version (ecluse.cabal `version:`, via `cabal info` — the source of truth)
	@v="$$($(NIX) cabal info ./ecluse.cabal 2>/dev/null | sed -n -E 's/^\* ecluse-([0-9.]+).*/\1/p' | head -n1)"; \
	  [ -n "$$v" ] && echo "$$v" || { echo "make version: could not read the version from 'cabal info'" >&2; exit 1; }

# Cut a GPG-signed, annotated release tag vX.Y.Z FROM the cabal version, so the tag
# can never be mistyped. Does NOT push — releasing is deliberate: it prints the push
# command, and pushing the tag is what triggers release.yml.
tag: ## Create signed tag v<version> from the cabal version (then push it to release)
	@v="v$$($(MAKE) -s version)"; \
	  git tag -s -m "Release $$v" "$$v" \
	  && echo "created $$v — push to release:  git push origin $$v"

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

test: ## Run the fast, gating unit suites (core + app)
	$(NIX) cabal test ecluse-core-unit ecluse-unit --test-show-details=direct

test-integration: ## Run the integration suite (requires a Docker daemon)
	$(NIX) cabal test ecluse-integration --test-show-details=direct

test-smoke: ## Run the smoke suite (live registries; non-gating)
	$(NIX) cabal test ecluse-smoke --test-show-details=direct

# End-to-end: builds the real OCI image, loads it, and runs the whole system as
# containers driven by the real npm CLI. The CI `e2e` job GATES on this, but it is
# kept out of the local `check`/`gate` targets — too heavy for routine pre-push (an
# image build + containers + npm), and it skips vacuously without a Docker daemon +
# the npm CLI. Run it explicitly. See scripts/e2e.sh and
# planning/slices/S53-e2e-ecosystem.md.
test-e2e: ## Run the end-to-end suite (builds + runs the real image; CI-gating)
	$(NIX) bash scripts/e2e.sh

test-all: test test-integration test-smoke ## Run every test suite

# The work-per-request (Layer A) benchmarks over the pure ecluse-core hot paths
# (tasty-bench). Reports time AND allocations — allocations from GC stats via the
# component's baked-in `+RTS -T`, the machine-independent signal the baseline tracks;
# time is informational. INFORM-ONLY: it never computes a perf-regression fail, so the
# only red state is a literal benchmark failure (a build error, a crashed harness, or
# a tripped complexity assertion). Pass tasty-bench options through BENCH_OPTS, e.g.
#   make bench BENCH_OPTS='-p serve --csv bench.csv'
# (`--baseline before.csv` compares against a prior run for the human reading the log;
# it still never gates). See docs/architecture/performance.md.
BENCH_OPTS ?=
bench: ## Run the work-per-request benchmarks (time + allocations; inform-only, never gates on perf)
	$(NIX_BENCH) cabal bench ecluse-bench --benchmark-options='$(BENCH_OPTS)'

# Profiling build -> run under the cost-centre profiler -> render a flame graph, so a
# regression localises to a cost centre. Uses GHC's late cost-centre profiling
# (`--profiling-detail=late`: centres inserted after optimisation, so the flame graph
# reflects the optimised code with low skew), then ghc-prof-flamegraph renders the
# .prof to an SVG. Built into its own dist-bench-prof so it never disturbs the normal
# build. Override the profiled selection with BENCH_PROFILE_OPTS (a single bench keeps
# the .prof focused), e.g. make bench-profile BENCH_PROFILE_OPTS='-p "serve"'.
BENCH_PROFILE_DIR := dist-bench-prof
BENCH_PROFILE_OPTS ?= -p express
bench-profile: ## Profiling build -> run -> cost-centre flame graph (ecluse-bench.svg)
	$(NIX_BENCH) cabal build ecluse-bench --enable-profiling --profiling-detail=late --builddir=$(BENCH_PROFILE_DIR)
	$(NIX_BENCH) bash -c 'set -euo pipefail; \
	  bin=$$(cabal list-bin ecluse-bench --enable-profiling --builddir=$(BENCH_PROFILE_DIR)); \
	  "$$bin" $(BENCH_PROFILE_OPTS) +RTS -p -RTS; \
	  ghc-prof-flamegraph ecluse-bench.prof -o ecluse-bench.svg; \
	  echo "flame graph: ecluse-bench.svg  (cost-centre profile: ecluse-bench.prof)"'

# The throughput-under-load (Layer B) harness: boots the real composed proxy on warp
# over in-process stub upstreams and drives it with `oha` (so it needs the `.#bench`
# shell, which carries oha). Reports throughput, latency percentiles, peak residency, GC
# stats, and allocations/request per scenario. INFORM-ONLY: it never computes a
# perf-regression fail; the only red state is a literal failure (the harness cannot boot,
# oha cannot run, or a scenario served nothing). Override the load knobs through the
# environment, e.g. BENCH_LOAD_DURATION_SECONDS=10 make bench-load. See
# docs/architecture/performance.md.
bench-load: ## Run the throughput & latency load tests (inform-only, never gates on perf)
	$(NIX_BENCH) cabal run -v0 bench-load

# Context B: fetch LIVE packuments and check Ecluse's work-per-request overhead against
# the version-controlled acceptance budget (acceptance/criteria.json). Live + non-
# deterministic: exits non-zero ONLY on a real budget breach (a flaky registry is
# reported, not a breach). Standalone and NON-REQUIRED — a breach informs, never blocks.
# See docs/architecture/performance.md.
perf-acceptance: ## Run the live performance-acceptance harness (Context B; live, reds on a budget breach, non-blocking)
	$(NIX) cabal run -v0 perf-acceptance

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

# Coverage has two shapes (see CONTRIBUTING.md → "Coverage"):
#   make coverage                    COMBINED unit ∪ integration — the picture
#                                    Codecov shows (its merged project total).
#                                    Needs Docker (the integration tier). This is
#                                    the canonical local command: a local read of
#                                    it AGREES with the dashboard.
#   make coverage SUITE=ecluse-unit  ONE tier's report (the per-flag JSON CI
#                                    uploads). A PARTIAL view the run flags loudly;
#                                    `ecluse-unit` is the fast, Docker-free loop.
#                                    `make coverage-unit` is the same thing, named.
# CI calls the explicit SUITE= form for each tier so each flag keeps its own JSON.
#
# Bare `make coverage` runs the combined report; passing SUITE= on the command line
# selects the single-tier path. `origin` distinguishes a command-line SUITE= from
# this default, so the bare target is combined while `SUITE=…` stays per-tier.
SUITE ?= ecluse-unit
COVERAGE_FLAGS ?= -fdev-http-egress

coverage: ## Combined unit ∪ integration coverage matching Codecov (Docker); SUITE=<tier> for one tier
ifeq ($(origin SUITE),command line)
	$(NIX) bash scripts/coverage.sh $(SUITE) $(COVERAGE_FLAGS)
else
	$(NIX) bash scripts/coverage-combined.sh $(COVERAGE_FLAGS)
endif

coverage-unit: ## Fast, Docker-free unit-only coverage (a PARTIAL view; Codecov merges it with integration)
	$(NIX) bash scripts/coverage.sh ecluse-unit $(COVERAGE_FLAGS)

gen-version-fixtures: ## Regenerate version-ordering fixtures from the reference tools (node-semver / packaging / Gem::Version)
	$(NIX) bash scripts/gen-version-fixtures.sh

gen-bench-corpus: ## Re-capture the real-world packument benchmark corpus from the pins in bench/corpus/package.json
	$(NIX) bash scripts/gen-bench-corpus.sh

# Create an isolated agent worktree on BRANCH and warm its HLS index in the
# background (a `make build` populating dist-newstyle, which HLS reuses) so the
# agent's first agent-lsp navigation call lands hot rather than a cold typecheck
# mid-session. One worktree per agent; cap concurrency at 2-3 and stagger
# creations (HLS is memory-hungry). NOT $(NIX)-wrapped: git/bash are ambient and
# the script enters the flake itself, only for the background build. Override
# BASE (default origin/main) and DIR (default .agents/worktrees/<branch-slug>).
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

# Lint the committed shell scripts. --severity=warning gates on correctness/robustness
# (quoting, unset vars, …) but not shellcheck's opinionated "style" tier (e.g. SC2001,
# prefer-parameter-expansion-over-sed, which does not always apply). actionlint already
# shellchecks the workflow `run:` blocks; this covers scripts/*.sh. Runs from the lean
# `.#workflow-lint` shell in CI. See CONTRIBUTING.md → "Automation scripting".
lint-scripts: ## Lint shell scripts (shellcheck)
	$(NIX) shellcheck --severity=warning $(SH)

# Deterministic unit tests for the helper scripts that carry their own logic —
# currently the Haddock dependency-link rewriter, whose Hackage URL mapping is
# pinned here against the link shapes `cabal haddock` emits (a full docs build is
# far too heavy to gate every PR). No toolchain, so it runs in the lean
# `.#workflow-lint` shell in CI alongside the script lint.
test-scripts: ## Run the shell-script unit tests (deterministic, no toolchain)
	$(NIX) bash scripts/rewrite-haddock-dep-links.test.sh

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

# Stan: Haskell static analysis over GHC HIE files — partial functions and potential
# bugs that Semgrep (no Haskell) and hlint (no type info) miss. Scoped by .stan.toml
# to a Warning floor (Style/Performance space-leak noise excluded). FAILS on any
# finding (a visible signal); the CI `stan` job runs this but is NOT a `gate`
# dependency, so it never blocks a merge. `make stan-all` shows the full set. Like
# weeder it reads .hie, built once here into dist-stan.
stan: ## Haskell static analysis (stan; FAILS on findings, never gates; perf detail: stan-all)
	$(NIX) cabal build exe:ecluse --builddir=dist-stan --ghc-options=-fwrite-ide-info
	$(NIX) stan --hiedir dist-stan
	@n="$$($(NIX) bash -c 'stan --hiedir dist-stan --json-output | jq ".observations | length"')"; \
	  if [ "$$n" -gt 0 ]; then echo "stan: $$n finding(s) above the configured floor (non-gating job fails)"; exit 1; fi; \
	  echo "stan: clean at the configured floor"

# The full Stan report: every severity, including the Performance/space-leak findings
# the .stan.toml floor hides (e.g. STAN-0206 non-strict fields). Informational only.
stan-all: ## Full stan report incl Performance/space-leak (informational; ignores .stan.toml)
	$(NIX) cabal build exe:ecluse --builddir=dist-stan --ghc-options=-fwrite-ide-info
	$(NIX) stan --hiedir dist-stan --no-default

check: build test doctest format-check lint sast cabal-check lint-workflows lint-scripts test-scripts ## Fast pre-push checks: the gate minus its Docker integration + Haddock tiers (see gate)

# The faithful local mirror of the CI gate: everything `check` runs, plus the two
# tiers it omits — the integration suite (needs a Docker daemon, exactly like the
# gate's build-test job) and the Haddock build (docs-check). Two CI-gating inputs are
# deliberately NOT reproduced here: the Codecov coverage upload/status (computed
# server-side) and the e2e suite (an OCI image build + containers + npm — too heavy
# for routine pre-push; run `make test-e2e` before a risky composition-root change).
gate: check test-integration docs-check ## Reproduce the full CI gate locally (sans the heavy e2e tier; integration needs Docker)

run: ## Run the proxy
	$(NIX) cabal run ecluse

# Hyperlinked source (clickable identifiers jump straight to the code) plus the
# quick-jump fuzzy-search overlay make the API docs pleasant to read locally.
# Output lands under dist-newstyle (git-ignored). We open it in the browser via
# xdg-open when present, and always print the path so the target stays usable in
# headless/CI contexts.
docs: ## Build hyperlinked, searchable Haddock HTML for both libraries and open it
	$(NIX) cabal haddock lib:ecluse lib:ecluse-core $(HADDOCK_FLAGS)
	@html=$$(find dist-newstyle -path '*/doc/html/ecluse/index.html' | head -n1); \
	  echo "Haddock: $$html"; \
	  command -v xdg-open >/dev/null 2>&1 && xdg-open "$$html" >/dev/null 2>&1 &

# Faster Haddock build for the CI gate, scoped to our own library. Two changes
# vs docs-site: it drops --haddock-hyperlink-source (the per-module source render),
# and adds --disable-documentation so cabal does NOT (re)build the ~130-package
# dependency haddock closure — it documents only our own libraries, which is all the gate
# needs to validate (our doc comments compile; no broken modules). The dropped
# dependency cross-links matter only for the published site, which the Pages
# publish (docs-site, full flags) still builds on main. A gate failure here still
# means our docs are broken. See CONTRIBUTING.md → "Continuous Integration".
docs-check: ## Build Haddock for the CI gate (both libraries; no dep docs, no source links)
	$(NIX) cabal haddock lib:ecluse lib:ecluse-core --haddock-quickjump --disable-documentation

# Assemble the API tree under ./_site/api for the GitHub Pages workflow to upload —
# the site root is left free for the home page (see `site`). Two surfaces live here:
# both libraries' Haddock, and the OpenAPI capability manifest. The Haddock output
# path embeds the arch + GHC version, so we locate it rather than hard-code it.
# `cabal haddock --haddock-hyperlink-source` resolves cross-package identifiers
# against the local build tree, so the staged HTML is post-processed
# (scripts/rewrite-haddock-dep-links.sh) to repoint dependency links at canonical
# Hackage URLs: the raw links are dead off the build host, and one shape ships the
# runner's home path into public HTML. A guard then fails the build if any
# unresolved/leaky shape survived (a new link form Haddock started emitting). The
# manifest is derived build data (git-ignored, never committed), so it is generated
# here at publish time: `openapi-gen` writes openapi.json from the fixed canonical
# source, and the Redoc wrapper (web/redoc.html) renders it client-side from the
# vendored, hash-pinned bundle `site` copies into _site/vendor (like Mermaid) — no
# Node, no external runtime dependency. .nojekyll (at the _site root) keeps Pages
# from touching the static assets.
docs-site: ## Build both libraries' Haddock + the OpenAPI manifest and stage them under ./_site/api
	$(NIX) cabal haddock lib:ecluse lib:ecluse-core --builddir=$(DOCS_BUILDDIR) $(HADDOCK_FLAGS)
	@rm -rf _site && mkdir -p _site/api && touch _site/.nojekyll
	@appidx=$$(find $(DOCS_BUILDDIR) -path '*/doc/html/ecluse/index.html' | grep -v '/l/' | head -n1); \
	  coreidx=$$(find $(DOCS_BUILDDIR) -path '*/l/ecluse-core/doc/html/*/index.html' | head -n1); \
	  for pair in "ecluse:$$appidx" "ecluse-core:$$coreidx"; do \
	    name=$${pair%%:*}; idx=$${pair#*:}; \
	    if [ -z "$$idx" ] || [ ! -f "$$idx" ]; then \
	      echo "docs-site: no Haddock index for '$$name' (multi-component output path changed?)" >&2; \
	      exit 1; \
	    fi; \
	    mkdir -p "_site/api/$$name" && cp -R "$$(dirname "$$idx")"/. "_site/api/$$name/"; \
	    echo "Staged $$name -> _site/api/$$name"; \
	  done
	@echo "Rewriting dependency cross-links to canonical Hackage URLs"; \
	  $(NIX) bash scripts/rewrite-haddock-dep-links.sh _site/api
	@if grep -rlE 'file:///|\$$\{pkgroot\}|/store/ghc-' _site/api >/dev/null 2>&1; then \
	  echo "docs-site: unresolved/leaky dependency links survived the rewrite (new Haddock link shape?):" >&2; \
	  grep -rlE 'file:///|\$$\{pkgroot\}|/store/ghc-' _site/api >&2; \
	  exit 1; \
	fi
	@cp web/api-index.html _site/api/index.html
	$(NIX) cabal run -v0 --builddir=$(DOCS_BUILDDIR) openapi-gen -- _site/api/openapi.json
	@mkdir -p _site/api/openapi && cp web/redoc.html _site/api/openapi/index.html
	@echo "Staged both libraries' Haddock + the OpenAPI manifest under ./_site/api (Haddock subpages, openapi.json, Redoc page at /api/openapi/)"

# Assemble the full Pages site that the workflow uploads:
#   /                  the landing page + the user docs rendered from Markdown (pandoc)
#   /threat-model.html the threat register, generated from the Threat Dragon model
#   /api               the library Haddock (via docs-site)
# Kept separate from docs-site so the gate-repro / local Haddock build stays about
# the docs alone. Build inputs (template, Lua filters) live in web/; only web/static
# is published. Mermaid (the rendered docs) and Redoc (the /api/openapi manifest page)
# both render client-side from hash-pinned bundles vendored into _site/vendor (the
# flake's `mermaidJs` / `redocJs`), so the published site has no external runtime
# dependency. The threat-model page expands a `threat-register` fence from
# threat-modelling/ecluse.json at build time (web/threat-register.lua) — the model
# is the single source of truth; the register is never committed back to the repo.
PANDOC_FLAGS := --standalone --from gfm --template web/template.html --lua-filter web/mermaid.lua --lua-filter web/links.lua
site: docs-site ## Assemble the Pages site (landing + rendered docs at /, Haddock + manifest under /api)
	@cp -R web/static/. _site/
	@cp docs/branding/logo.svg docs/branding/favicon-32.png docs/branding/lock-illustration.svg docs/social-preview.png _site/
	@$(NIX) sh -c 'mkdir -p _site/vendor && cp "$$MERMAID_JS" _site/vendor/mermaid.min.js && cp "$$REDOC_JS" _site/vendor/redoc.standalone.js'
	$(NIX) pandoc MOTIVATION.md   -o _site/motivation.html   $(PANDOC_FLAGS) -M title="Why Écluse?"
	$(NIX) pandoc ALTERNATIVES.md -o _site/alternatives.html $(PANDOC_FLAGS) -M title="Alternatives"
	$(NIX) pandoc USAGE.md        -o _site/usage.html        $(PANDOC_FLAGS) -M title="Operator Manual"
	$(NIX) pandoc AI-DISCLOSURE.md -o _site/ai-disclosure.html $(PANDOC_FLAGS) -M title="Built with AI"
	$(NIX) pandoc web/threat-model.md -o _site/threat-model.html $(PANDOC_FLAGS) --lua-filter web/threat-register.lua -M title="Threat Model"
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
# maintained DB → severity-rated table + grype.json + grype.sarif (the SARIF the
# security workflow uploads to GitHub code scanning). `scan-vulnix` is the
# secondary Nix-native cross-check (more comprehensive and patch-aware, but no
# severity grades). Both are report-only. See CONTRIBUTING.md → "Vulnerability
# scanning".
scan: ## Scan the image closure for CVEs (grype over the SBOM → grype.json + grype.sarif)
	@mkdir -p sbom
	$(NIX) sbomnix --cdx sbom/ecluse.cdx.json .#ecluse-bin
	$(NIX) grype sbom:sbom/ecluse.cdx.json -o table -o json=grype.json -o sarif=grype.sarif
	$(NIX) bash scripts/grype-sarif-locations.sh grype.sarif

scan-vulnix: ## Secondary Nix-native cross-check (vulnix; comprehensive, no severity)
	-$(NIX) bash -c 'vulnix -C "$$(nix build .#ecluse-bin --no-link --print-out-paths)"'

clean: ## Remove build artifacts
	rm -rf dist-newstyle dist-bench-prof result sbom grype.json grype.sarif \
	  ecluse-bench.prof ecluse-bench.svg
