{
  description = "ecluse: supply-chain resilience proxy for package registries";

  inputs = {
    # Single pinned nixpkgs. The 26.05 base is current enough that every tool —
    # including vulnix and zizmor, which historically forced a second
    # newer-nixpkgs input on the 24.11 base (broken vulnix 1.10.1, ancient
    # zizmor 0.2.1) — comes from this one set. See CONTRIBUTING.md →
    # "Vulnerability scanning".
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-26.05";
    flake-utils.url = "github:numtide/flake-utils";
  };

  # `self` is unused (nothing here needs the flake's own source tree), but Nix
  # always passes it, so absorb it with `...` rather than binding it.
  outputs = { nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        hlib = pkgs.haskell.lib;

        # OpenTelemetry 1.0 overlay. The pinned nixpkgs (26.05) ships the older
        # 0.x line (sdk 0.1.0.1, api 0.3.1.0, no api-types package at all), but
        # the architect chose OTel 1.0 — the May-2026 release that finally ships
        # metrics and logs alongside tracing, with OTLP export. So the Nix path
        # (callCabal2nix, which resolves from this set rather than Hackage) gets
        # the 1.0 stack injected here, matching the cabal/Hackage path's 1.0 pin
        # in cabal.project + cabal.project.freeze. Sources are pinned by exact
        # version + Hackage tarball sha256 (callHackageDirect: no flake input, no
        # runtime fetch beyond the fixed-output derivation). The whole stack is
        # overridden together so each 1.0 package resolves its OTel deps against
        # the other 1.0 packages here, never the 0.x ones still in the base set.
        # See docs/architecture/observability.md → "OpenTelemetry as the substrate".
        otelOverlay = hself: _hsuper: {
          # cvss 0.3 adds CVSS v4 parsing (and fixes v2 scoring); nixpkgs pins
          # 0.2.0.1, which rejects the v4 vectors roughly a third of the scored npm
          # OSV advisories carry. dontCheck skips its tasty test suite (not ours to
          # gate) so it does not drag test-only version bounds into the closure.
          cvss = pkgs.haskell.lib.dontCheck (hself.callHackageDirect {
            pkg = "cvss";
            ver = "0.3.0.0";
            sha256 = "sha256-fRdZv2yVIAgBz9R36+V69GHc5h91/4lPXU4RU5Q2Q4Q=";
          } { });
          hs-opentelemetry-api-types =
            hself.callHackageDirect {
              pkg = "hs-opentelemetry-api-types";
              ver = "1.0.0.0";
              sha256 = "sha256-9ByP41wlV45TMCqbyyVpwejQDi5fsG0+j8bMk8ORLw8=";
            } { };
          hs-opentelemetry-api =
            hself.callHackageDirect {
              pkg = "hs-opentelemetry-api";
              ver = "1.0.0.0";
              sha256 = "sha256-COhj9Ms1eu1Gt9wTC21oQ37k6vJ9mxlJvYpHtvXff6A=";
            } { };
          hs-opentelemetry-otlp =
            hself.callHackageDirect {
              pkg = "hs-opentelemetry-otlp";
              ver = "1.0.0.0";
              sha256 = "sha256-kVuKKi6qRx+oBQclTpUnx20Eqw+CRQk8pT4tkcxt1xo=";
            } { };
          hs-opentelemetry-semantic-conventions =
            hself.callHackageDirect {
              pkg = "hs-opentelemetry-semantic-conventions";
              ver = "1.40.0.0";
              sha256 = "sha256-7cIC9dTrd5bJjAsiEyyupi1xSZyc17FpjbACnm0p5ik=";
            } { };
          # The SDK 1.0 re-exports the standard propagators (W3C TraceContext is
          # the default; B3/Jaeger/X-Ray/Datadog are alternates a deployment
          # selects), so they travel with it on the 1.0 line. The Datadog one is
          # the optional, vendor-specific propagator from the observability design.
          hs-opentelemetry-propagator-b3 =
            hself.callHackageDirect {
              pkg = "hs-opentelemetry-propagator-b3";
              ver = "1.0.0.0";
              sha256 = "sha256-gsNe818CprXM9l61mLUsdnePxIQChfml9kegmCDoAmw=";
            } { };
          hs-opentelemetry-propagator-datadog =
            hself.callHackageDirect {
              pkg = "hs-opentelemetry-propagator-datadog";
              ver = "1.0.0.0";
              sha256 = "sha256-nTXEtira3bktvycZkjDmPZewyMJ1IEEDygLT9OiIFYo=";
            } { };
          hs-opentelemetry-propagator-jaeger =
            hself.callHackageDirect {
              pkg = "hs-opentelemetry-propagator-jaeger";
              ver = "1.0.0.0";
              sha256 = "sha256-VL+3YwKbqe0elfZQ0EN7icNS0+pxmtlxlKauPHRqhb8=";
            } { };
          hs-opentelemetry-propagator-w3c =
            hself.callHackageDirect {
              pkg = "hs-opentelemetry-propagator-w3c";
              ver = "1.0.0.0";
              sha256 = "sha256-p8d2Tx8bCVRk6hps8k0qAg/L2gdBVoYuLYJbTzTbI3s=";
            } { };
          hs-opentelemetry-propagator-xray =
            hself.callHackageDirect {
              pkg = "hs-opentelemetry-propagator-xray";
              ver = "1.0.0.0";
              sha256 = "sha256-Tg7TrCMb8GA+jm+ohMAqMW7othRm/HLEyr9SifGa6qI=";
            } { };
          hs-opentelemetry-exporter-handle =
            hself.callHackageDirect {
              pkg = "hs-opentelemetry-exporter-handle";
              ver = "1.0.0.0";
              sha256 = "sha256-DCoVG0Y2aaMjinOP2GWmew0WmjN96j3/UUzEWxN7Ajs=";
            } { };
          # The in-memory exporter is part of the SDK's own dependency closure
          # in this set; the base set's 0.x build does not compile against the
          # 1.0 api (a SpanProcessor field changed type), so it moves to 1.0 too.
          hs-opentelemetry-exporter-in-memory =
            hself.callHackageDirect {
              pkg = "hs-opentelemetry-exporter-in-memory";
              ver = "1.0.0.0";
              sha256 = "sha256-bJjUHBNMRKhmkqRRnUrAQIDLWpUrox7F418r2QbVQ6o=";
            } { };
          hs-opentelemetry-sdk =
            hself.callHackageDirect {
              pkg = "hs-opentelemetry-sdk";
              ver = "1.0.0.0";
              sha256 = "sha256-kG8gmP8Lr9mPCnJjukCduFI/tADgKCfuelxcQZcXyA8=";
            } { };
          # HTTP/protobuf is the default; the gRPC path (which would pull in
          # grapesy) stays behind the package's cabal flag, off — matching the
          # `-grpc` flag the cabal freeze resolves. We need no gRPC.
          hs-opentelemetry-exporter-otlp =
            hself.callHackageDirect {
              pkg = "hs-opentelemetry-exporter-otlp";
              ver = "1.0.0.0";
              sha256 = "sha256-rHgsisH2d45CI9woEDb/j0WnTzllxaE2Mkx5/OmWn0c=";
            } { };
          # Request-lifecycle instrumentation (S25): the WAI server span and the
          # http-client data-plane child spans. The http-client instrumentation pulls
          # the conduit instrumentation as a 1.0 dependency, so it travels on the line
          # too; their only OTel deps (api, semantic-conventions) are above.
          hs-opentelemetry-instrumentation-wai =
            hself.callHackageDirect {
              pkg = "hs-opentelemetry-instrumentation-wai";
              ver = "1.0.0.0";
              sha256 = "sha256-gPU9k2H1MpMEGh0F1Oi5ri8gdsZMCvQBRTnXgDhVAa0=";
            } { };
          hs-opentelemetry-instrumentation-conduit =
            hself.callHackageDirect {
              pkg = "hs-opentelemetry-instrumentation-conduit";
              ver = "1.0.0.0";
              sha256 = "sha256-J4iv0uTsnmntoXOb6tf8CBnKa0KsspomwLN/mJ2ypTA=";
            } { };
          hs-opentelemetry-instrumentation-http-client =
            hself.callHackageDirect {
              pkg = "hs-opentelemetry-instrumentation-http-client";
              ver = "1.0.0.0";
              sha256 = "sha256-/+XwCJzMYtmBoHBuDGkmHR8ETKkxpMMtWNNWpbAGPYQ=";
            } { };
        };

        hpkgs = pkgs.haskell.packages.ghc910.override {
          overrides = otelOverlay;
        };

        # The cabal package, built by Nix. callCabal2nix reads ecluse.cabal and
        # resolves dependencies from the nixpkgs GHC 9.10 set (pinned by
        # flake.lock), not Hackage. This is the build/CI artifact; `cabal` in the
        # dev shell stays the incremental inner loop (Nix rebuilds the whole
        # package on any change, so it is poor for edit-compile cycles).
        ecluseRaw = hpkgs.callCabal2nix "ecluse" ./. { };

        # Release artifact: library + executable only. dontCheck keeps the build
        # from running the test suites — the pure tier is a flake check below, and
        # the impure suites (integration → Docker, smoke → live network) never
        # belong in a hermetic build.
        ecluse = hlib.dontCheck ecluseRaw;

        # The executable alone, stripped and with its reference to the full
        # Haskell library closure removed (justStaticExecutables). A plain dynamic
        # build drags that whole closure in and bloats the image to ~500 MB; this
        # keeps only the binary + its system C deps for the container image.
        ecluseBinUnpruned =
          hlib.justStaticExecutables (hlib.dontCheck ecluseRaw);

        # GHC's x86_64 Linux RTS links libdw/libelf for DWARF stack unwinding.
        # nixpkgs puts those libraries and the unused libdebuginfod in one
        # elfutils output; libdebuginfod alone retains curl, libssh2, OpenSSL,
        # Kerberos, and the HTTP/2 and HTTP/3 stacks in the image closure. Build
        # the same elfutils ABI without debuginfod, preserving the RTS feature
        # while removing that unreachable network-client surface. elfutils still
        # needs pkg-config at configure time, although nixpkgs makes that native
        # input conditional on debuginfod being enabled.
        elfutilsWithoutDebuginfod =
          (pkgs.elfutils.override { enableDebuginfod = false; }).overrideAttrs
            (old: {
              nativeBuildInputs =
                (old.nativeBuildInputs or [ ]) ++ [ pkgs.pkg-config ];
            });

        # The arm64 GHC build does not enable DWARF support, and Darwin does not
        # ship this Linux image closure, so only the affected release platform
        # needs the ABI-compatible substitution.
        ecluseBin =
          if pkgs.stdenv.hostPlatform.isLinux
            && pkgs.stdenv.hostPlatform.isx86_64
          then
            pkgs.replaceDependency {
              drv = ecluseBinUnpruned;
              oldDependency = pkgs.lib.getLib pkgs.elfutils;
              newDependency = pkgs.lib.getLib elfutilsWithoutDebuginfod;
            }
          else
            ecluseBinUnpruned;

        # The npm version-ordering oracle (`node-semver`) for the differential
        # smoke suite and `task gen-version-fixtures`. nixpkgs 26.05 removed the
        # node2nix-generated `nodePackages` set; the blessed replacement is to
        # build node_modules from a committed lockfile. `importNpmLock` reads the
        # integrity hashes already in test/oracles/package-lock.json (no separate
        # Nix hash to maintain), and Renovate's npm manager bumps that lockfile on
        # the same cadence as every other ecosystem. Exposed on NODE_PATH below
        # so `require("semver")` resolves.
        oracleNodeModules = pkgs.importNpmLock.buildNodeModules {
          npmRoot = ./test/oracles;
          inherit (pkgs) nodejs;
        };

        # ---- Dev-shell composition -------------------------------------------
        # Env shared by every shell: a UTF-8 locale (so hspec's '✔' and other
        # Unicode output encode regardless of host locale) and the NODE_PATH that
        # makes `require("semver")` resolve for the npm version oracle (fixture
        # generator + smoke suite).
        shellEnv = {
          LANG = "C.UTF-8";
          LC_ALL = "C.UTF-8";
          NODE_PATH = "${oracleNodeModules}/node_modules";
          AWS_EC2_METADATA_DISABLED = "true";
        };

        # ---- Tool groups -----------------------------------------------------
        # The lists below group tools by the concern each one serves, so a tool's
        # reason for being in the closure is documented beside it. They are
        # documentation, not a partition: every group here is in BOTH shells, via
        # `ciShellInputs`. `ideInputs` is the sole exception, and the only thing
        # that distinguishes `default` from `ci`.
        #
        # The base group: the toolchain every CI job needs. `go-task` lives here
        # and here only, because it is how every job is driven, so every shell
        # already gets it through this list.
        ciInputs = [
          pkgs.bashInteractive
          hpkgs.ghc
          hpkgs.cabal-install
          hpkgs.fourmolu
          hpkgs.hlint
          # Run the >>> examples in Haddock comments as tests (`task doctest`),
          # via `cabal repl --with-ghc=doctest`. Must come from the same GHC 9.10
          # set as the compiler it stands in for. See docs/haddock.md → "Examples that
          # run".
          hpkgs.doctest
          # Convert HPC coverage output (.tix/.mix) to Codecov JSON for the
          # `coverage` target (see CONTRIBUTING.md → "Coverage").
          hpkgs.hpc-codecov
          pkgs.semgrep
          pkgs.zlib
          pkgs.pkg-config
          # Reference version-ordering oracles for the differential smoke suite
          # and `task gen-version-fixtures`: node-semver (npm, built via
          # oracleNodeModules and put on NODE_PATH above), Python packaging
          # (PyPI), and Ruby Gem::Version (built into ruby).
          pkgs.nodejs
          (pkgs.python3.withPackages (ps: [ ps.packaging ]))
          pkgs.ruby
          pkgs.go-task
        ];

        # Site rendering: pandoc turns the repo's Markdown into the published site
        # pages (`task site`). Both the Pages publish and the PR-tier `site-stub`
        # gate run it, so it is part of the CI closure.
        docsInputs = [ pkgs.pandoc ];

        # Vendored Mermaid bundle for the site: one self-contained UMD build, pinned
        # by hash and copied into the published site (see `task site`) so diagrams
        # render with no external CDN dependency. Bump the version and hash together.
        mermaidJs = pkgs.fetchurl {
          url = "https://cdn.jsdelivr.net/npm/mermaid@11.15.0/dist/mermaid.min.js";
          hash = "sha256-cBN+d7snO7LvlyuG6LBADMqL5TyyW/xFkRoYbcmGZd4=";
        };

        # Vendored Redoc bundle for the capability-manifest page: the self-contained
        # standalone UMD build, pinned by hash and copied into the published site (see
        # `task site`) so the OpenAPI manifest renders client-side, with no external
        # CDN dependency and no Node needed to render it. Mirrors `mermaidJs`; bump
        # the version and hash together. See docs/architecture/api-surface.md →
        # "How it's built and published".
        redocJs = pkgs.fetchurl {
          url = "https://cdn.jsdelivr.net/npm/redoc@2.5.3/bundles/redoc.standalone.js";
          hash = "sha256-EyD0QhUcV8RH07cMf/xsT4bQhGQCD+NMjMXTFk6ZRPA=";
        };

        # Interactive-only tooling: in the default (human) shell, never needed by
        # CI. HLS/ghcid for live feedback; hoogle/cabal-plan for API & build-plan
        # search (see AGENTS.md).
        ideInputs = [
          hpkgs.haskell-language-server
          hpkgs.ghcid
          hpkgs.hoogle
          hpkgs.cabal-plan
          # The Spec.hs entry points carry `-pgmF hspec-discover`, a source
          # preprocessor GHC shells out to. cabal supplies it during a build via
          # build-tool-depends, but HLS runs the same preprocessor when it loads
          # those modules and does not reproduce cabal's build-tool PATH — so it
          # reports "could not execute: hspec-discover". Put it on the shell PATH
          # for HLS; from the same GHC 9.10 set, matching the build-tool-depends
          # version. CI never needs it here (cabal provides it), so it stays out
          # of ciInputs.
          hpkgs.hspec-discover
        ];

        # Release tooling: skopeo pushes the Nix-built image to a registry (no
        # Docker daemon needed) via `task docker-push`, and sbomnix generates the
        # Nix-native SBOM (`task sbom`), which is more accurate than scanning a
        # distroless image, whose static Haskell deps a scanner cannot see. The
        # provenance and SBOM attestations themselves are produced in CI by the
        # GitHub attest-actions (immutable OCI referrers); see CONTRIBUTING.md →
        # "Supply-chain attestations".
        releaseInputs = [
          pkgs.skopeo
          pkgs.sbomnix
        ];

        # Vulnerability scanning. grype is the authority (`task scan`): it scans
        # the sbomnix SBOM of the image's C closure (openssl/curl/glibc/…) against
        # its maintained DB and gives severity-rated, low-noise findings. vulnix
        # is a secondary, Nix-native cross-check (`task scan-vulnix`): more
        # comprehensive and patch-aware but un-graded, so not the authority. On
        # the 26.05 base it comes straight from the pinned set (the older base's
        # vulnix was broken against NVD's feeds, forcing a second input — no
        # longer). Haskell-advisory coverage (cabal-audit / HSEC) is a deferred
        # follow-up — the static Haskell deps are a lower-risk surface. See
        # CONTRIBUTING.md → "Vulnerability scanning".
        scanInputs = [
          pkgs.grype
          pkgs.vulnix
          # jq: scripts/grype-sarif-locations.sh post-processes grype.sarif for
          # GitHub code scanning; pinned here rather than relying on the runner's.
          pkgs.jq
          # reuse: per-file SPDX licence headers (`task lint-spdx` stamps and
          # gates via `reuse lint-file` / `reuse annotate`). A Python tool,
          # justified over a hand-rolled shell gate because it is the SPDX/REUSE
          # reference implementation; it stays scoped to tracked .hs files (no
          # repo-wide REUSE regime) and lives in the dev shell only, never on
          # the product path. See docs/style.md, "Licence headers".
          pkgs.reuse
        ];

        # GitHub Actions linting (`task lint-workflows`): actionlint for
        # correctness (shellcheck over `run:` blocks, expression/context checks)
        # and zizmor for security (template injection, credential persistence,
        # excessive permissions, dangerous triggers). Mechanizes the
        # injection-free workflow rule in AGENTS.md → "CI & Security". zizmor
        # comes from the pinned set on the 26.05 base (the older base shipped only
        # an ancient 0.2.1, which forced the second input alongside vulnix — no
        # longer). See CONTRIBUTING.md → "Continuous Integration".
        workflowLintInputs = [
          pkgs.actionlint
          pkgs.zizmor
          # shellcheck for `task lint-scripts` (scripts/*.sh). actionlint already runs
          # shellcheck on workflow `run:` blocks; this lints the committed scripts too.
          pkgs.shellcheck
        ];

        # Dead-code (weeder) and HIE-based static analysis (stan). Both gate CI
        # through their own jobs, and both are in `task check`.
        analysisInputs = [ hpkgs.weeder hpkgs.stan ];

        # Benchmarking and profiling: `task bench`, `task bench-load`, and the
        # cost-centre flame graph from `task bench-profile`.
        benchInputs = [
          pkgs.haskellPackages.ghc-prof-flamegraph
          pkgs.flamegraph
          pkgs.oha
        ];

        # agent-lsp: the LSP<->MCP bridge that lets an MCP client (e.g. agent
        # harnesses) drive haskell-language-server's semantic navigation — go-to-
        # definition, find-references, hover/type-at-point, diagnostics, rename —
        # over this project instead of lexical grep. It is built on a *complete*
        # LSP client. (The earlier bridge, mcp-language-server v0.1.1, was an
        # incomplete client that left HLS deadlocked at ~0 % CPU on every semantic
        # request — verified against this project; the same HLS is flawless under
        # VS Code's vscode-languageclient. See AGENTS.md → "Build & Tooling".)
        # agent-lsp is not in nixpkgs, so it is built from tagged source via
        # buildGoModule; go.mod needs Go 1.26 (the 26.05 set ships go_1_26) and it
        # is pure-Go (modernc sqlite — no cgo). HLS still needs the GHC 9.10
        # toolchain + hspec-discover on PATH to load the Spec.hs modules (same
        # reason as ideInputs), so they travel with it here.
        agent-lsp = (pkgs.buildGoModule.override { go = pkgs.go_1_26; }) rec {
          pname = "agent-lsp";
          version = "0.15.0";
          src = pkgs.fetchFromGitHub {
            owner = "blackwell-systems";
            repo = "agent-lsp";
            rev = "v${version}"; # commit ab89838db139125bcf0e3c4e0c10addf57ed52c6
            hash = "sha256-l04uuMP4giVUykDpR4mWK2P+Tkj/E16EqDuMOEYNa8U=";
          };
          vendorHash = "sha256-/y+v/aCzqigLut3kljCwa5iMD5yMLK1L5ul9ue8YFqU=";
          subPackages = [ "cmd/agent-lsp" ]; # the server binary only — skip ./scripts, ./test, experiments
          doCheck = false; # its test suite spins up real language servers
          ldflags = [ "-s" "-w" "-X main.Version=${version}" ];
        };

        # Opt-in only: entirely separate from default/ci so it imposes nothing on
        # the normal dev or gate flow; opt in via .mcp.json (see AGENTS.md).
        mcpInputs = [
          pkgs.bashInteractive
          hpkgs.ghc
          hpkgs.cabal-install
          hpkgs.hspec-discover
          hpkgs.haskell-language-server
          agent-lsp
        ];

        # Every tool any CI job drives through `task`, in ONE closure. Bundling
        # them means build-test realizes the whole thing and writes a single,
        # comprehensive GitHub Actions cache entry, so every downstream job gets a
        # 100% cache hit rather than substituting its own tools from
        # cache.nixos.org on each run. Every gate job therefore enters `.#ci`; do
        # not add a job-specific shell without a closure that genuinely differs,
        # because a second closure means a second cache entry to warm and evict.
        ciShellInputs =
          ciInputs ++ docsInputs ++ releaseInputs ++ scanInputs
          ++ workflowLintInputs ++ analysisInputs ++ benchInputs;
      in {
        packages = {
          default = ecluse;
          ecluse = ecluse;
          agent-lsp = agent-lsp; # LSP<->MCP bridge (see mcpInputs); `nix build .#agent-lsp`

          # The exact stripped, static binary that ships inside the image
          # (`justStaticExecutables`, no Haskell-library closure). Exposed so the
          # SBOM and any verifier can target precisely what the image contains —
          # `nix build .#ecluse-bin` — rather than the noisier dynamic package.
          ecluse-bin = ecluseBin;

          # Lean, reproducible OCI image, built straight from the binary's Nix
          # closure — no Dockerfile, no base distro. It contains only the runtime
          # closure plus CA certificates: no shell, no package manager, runs
          # non-root. `buildLayeredImage` splits the closure into cache-friendly
          # layers, and the build is bit-for-bit reproducible (a fitting property
          # for a supply-chain tool). `tag = null` derives a unique content-hash
          # tag for local use; releases retag at push time because the target
          # repo enforces immutable tags (see CONTRIBUTING.md "Releases").
          # Push via `task docker-push`; provenance + SBOM attestations are
          # attached in CI by release.yml (the GitHub attest-actions).
          dockerImage = pkgs.dockerTools.buildLayeredImage {
            name = "ecluse";
            tag = null;
            contents = [ ecluseBin pkgs.cacert ];
            config = {
              Entrypoint = [ "${ecluseBin}/bin/ecluse" ];
              ExposedPorts = { "8080/tcp" = { }; }; # default ECLUSE_SERVER__PORT
              User = "65532:65532"; # nonroot, distroless convention
              Env = [
                # A distroless image has no system trust store, so tls/x509-system
                # finds no CAs and every outbound HTTPS fetch fails. cacert is in
                # the closure via `contents` above; point GHC's TLS stack at it.
                "SSL_CERT_FILE=${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt"
              ];
              Labels = {
                "org.opencontainers.image.title" = "ecluse";
                "org.opencontainers.image.description" =
                  "Supply-chain resilience proxy for package registries";
                "org.opencontainers.image.source" =
                  "https://github.com/AlexaDeWit/Ecluse";
                "org.opencontainers.image.licenses" = "MIT";
              };
            };
          };
        };

        # The one flake check, and the only one CI builds
        # (`nix build .#checks.x86_64-linux.docs`, the `docs` job).
        #
        # It is here rather than in the Taskfile because Nix can do something cabal
        # cannot: the dependency closure comes prebuilt from the pinned Haskell set
        # WITH its .haddock interfaces, so only ecluse compiles and haddocks. The
        # cabal path (`task docs-check`) instead rebuilds the whole ~188-package
        # closure on every CI run, because `cabal haddock` wants a documentation
        # variant of the deps that build-test's `cabal build` store does not have.
        # That asymmetry is the bar a check must clear to earn a Nix implementation:
        # a check that merely re-runs a tool the dev shell already pins belongs in
        # the Taskfile, where it runs incrementally and is what actually gates.
        # doHaddock forces the Haddock pass, so a broken doc comment fails the
        # build; dontCheck skips the test suites.
        checks.docs = hlib.doHaddock (hlib.dontCheck ecluseRaw);

      devShells = {
        # The shell every CI job enters. See `ciShellInputs` for why it is one
        # closure rather than a per-job set.
        ci = pkgs.mkShell (shellEnv // {
          name = "ecluse-ci";
          buildInputs = ciShellInputs;
          # Paths to the pinned vendored bundles; `task site` copies them into
          # _site/vendor (Mermaid for the rendered docs, Redoc for the manifest page).
          MERMAID_JS = "${mermaidJs}";
          REDOC_JS = "${redocJs}";
        });

        # The shell for humans: everything CI has, plus the interactive tooling CI
        # never needs. That difference is the ONLY one, and it is deliberately a
        # strict superset: a human can reproduce any gate job locally, but the IDE
        # closure (HLS alone is ~8 GB, the heaviest and flakiest part to
        # substitute) stays out of what CI realizes and caches.
        default = pkgs.mkShell (shellEnv // {
          name = "ecluse";
          buildInputs = ciShellInputs ++ ideInputs;
          MERMAID_JS = "${mermaidJs}";
          REDOC_JS = "${redocJs}";
        });

        # LSP<->MCP bridge shell (HLS + agent-lsp). Opt-in only: not
        # built by CI (the gate runs no `nix flake check`) and not part of the
        # default dev shell. Enter with `nix develop .#mcp`, or let `.mcp.json`
        # launch it. See AGENTS.md → "Build & Tooling".
        mcp = pkgs.mkShell (shellEnv // {
          name = "ecluse-mcp";
          buildInputs = mcpInputs;
        });
      };
    });
}
