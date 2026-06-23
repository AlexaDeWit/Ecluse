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

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        hlib = pkgs.haskell.lib;
        hpkgs = pkgs.haskell.packages.ghc910;

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
        ecluseBin = hlib.justStaticExecutables (hlib.dontCheck ecluseRaw);

        # Sources for the format/lint checks: the .hs files plus the cabal files,
        # so fourmolu can read default-language / default-extensions (GHC2021 →
        # ImportQualifiedPost) and parse postpositive `qualified` imports. Without
        # the cabal in scope, fourmolu's parser rejects them.
        hsSrc = pkgs.lib.sourceFilesBySuffices ./. [ ".hs" ".cabal" "cabal.project" ];

        # The npm version-ordering oracle (`node-semver`) for the differential
        # smoke suite and `make gen-version-fixtures`. nixpkgs 26.05 removed the
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
        };

        # Everything CI drives through `make`, across every gate job — this is the
        # `.#ci` shell. Kept deliberately lean: it omits the IDE tooling (HLS,
        # ghcid, hoogle, cabal-plan) and release tooling, which is the heaviest,
        # flakiest part of the closure to substitute. That shrinks the Nix store
        # each CI job realizes and caches. See CONTRIBUTING.md → "Continuous
        # Integration".
        ciInputs = [
          pkgs.bashInteractive
          hpkgs.ghc
          hpkgs.cabal-install
          hpkgs.fourmolu
          hpkgs.hlint
          # Run the >>> examples in Haddock comments as tests (`make doctest`),
          # via `cabal repl --with-ghc=doctest`. Must come from the same GHC 9.10
          # set as the compiler it stands in for. See HADDOCK.md → "Examples that
          # run".
          hpkgs.doctest
          # Convert HPC coverage output (.tix/.mix) to Codecov JSON for the
          # `coverage` target (see CONTRIBUTING.md → "Coverage").
          hpkgs.hpc-codecov
          pkgs.semgrep
          pkgs.zlib
          pkgs.pkg-config
          # Reference version-ordering oracles for the differential smoke suite
          # and `make gen-version-fixtures`: node-semver (npm, built via
          # oracleNodeModules and put on NODE_PATH above), Python packaging
          # (PyPI), and Ruby Gem::Version (built into ruby).
          pkgs.nodejs
          (pkgs.python3.withPackages (ps: [ ps.packaging ]))
          pkgs.ruby
        ];

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
        # Docker daemon needed) via `make docker-push`, and sbomnix generates the
        # Nix-native SBOM (`make sbom`) — more accurate than scanning a distroless
        # image, whose static Haskell deps a scanner can't see. The provenance and
        # SBOM attestations themselves are produced in CI by the GitHub
        # attest-actions (immutable OCI referrers); see CONTRIBUTING.md →
        # "Supply-chain attestations".
        releaseInputs = [
          pkgs.skopeo
          pkgs.sbomnix
        ];

        # Vulnerability scanning. grype is the authority (`make scan`): it scans
        # the sbomnix SBOM of the image's C closure (openssl/curl/glibc/…) against
        # its maintained DB and gives severity-rated, low-noise findings. vulnix
        # is a secondary, Nix-native cross-check (`make scan-vulnix`): more
        # comprehensive and patch-aware but un-graded, so not the authority. On
        # the 26.05 base it comes straight from the pinned set (the older base's
        # vulnix was broken against NVD's feeds, forcing a second input — no
        # longer). Haskell-advisory coverage (cabal-audit / HSEC) is a deferred
        # follow-up — the static Haskell deps are a lower-risk surface. See
        # CONTRIBUTING.md → "Vulnerability scanning".
        scanInputs = [
          pkgs.grype
          pkgs.vulnix
        ];

        # GitHub Actions linting (`make lint-workflows`): actionlint for
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
        ];

        # Proof-of-concept: an LSP<->MCP bridge so an MCP client (e.g. Claude
        # Code) can drive haskell-language-server's semantic navigation —
        # go-to-definition, find-references, hover/type-at-point, diagnostics,
        # rename — over this project, instead of relying on lexical grep. The
        # bridge (mcp-language-server, from the pinned set — no npx/runtime fetch)
        # runs as an MCP stdio server and is internally an LSP client to HLS. HLS
        # needs the GHC 9.10 toolchain and hspec-discover on PATH to load the
        # Spec.hs modules (same reason as ideInputs), so they travel with it here.
        # Entirely separate from default/ci so it imposes nothing on the normal
        # dev or gate flow; opt in via .mcp.json (see AGENTS.md → "Build & Tooling").
        mcpInputs = [
          pkgs.bashInteractive
          hpkgs.ghc
          hpkgs.cabal-install
          hpkgs.hspec-discover
          hpkgs.haskell-language-server
          pkgs.mcp-language-server
        ];
      in {
        packages = {
          default = ecluse;
          ecluse = ecluse;

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
          # Push via `make docker-push`; provenance + SBOM attestations are
          # attached in CI by release.yml (the GitHub attest-actions).
          dockerImage = pkgs.dockerTools.buildLayeredImage {
            name = "ecluse";
            tag = null;
            contents = [ ecluseBin pkgs.cacert ];
            config = {
              Entrypoint = [ "${ecluseBin}/bin/ecluse" ];
              ExposedPorts = { "4873/tcp" = { }; }; # default PROXY_PORT
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

        checks = {
          # Pure, gating test tier. Builds the package and runs ONLY the unit
          # suite (testTarget), so this hermetic check never executes the Docker-
          # or network-dependent suites even once their real cases land.
          unit = hlib.overrideCabal ecluseRaw (_: {
            doCheck = true;
            testTarget = "ecluse-unit";
          });

          format = pkgs.runCommand "fourmolu-check"
            { nativeBuildInputs = [ hpkgs.fourmolu ]; } ''
            fourmolu --mode check $(find ${hsSrc} -name '*.hs')
            touch $out
          '';

          lint = pkgs.runCommand "hlint-check"
            { nativeBuildInputs = [ hpkgs.hlint ]; } ''
            hlint $(find ${hsSrc} -name '*.hs')
            touch $out
          '';
        };

        # Full shell for humans: lean CI set + IDE + release + scan + workflow-lint + weeder.
        devShells.default = pkgs.mkShell (shellEnv // {
          name = "ecluse";
          buildInputs =
            ciInputs ++ ideInputs ++ releaseInputs ++ scanInputs ++ workflowLintInputs
            ++ [ hpkgs.weeder ];
        });

        # Lean shell for CI: only what the gate jobs invoke through `make`. CI
        # enters it explicitly with `nix develop .#ci`; humans get the full shell
        # above. Keeping CI's closure small makes the Nix-store cache fast and
        # shrinks the surface exposed to cache.nixos.org flakiness.
        devShells.ci = pkgs.mkShell (shellEnv // {
          name = "ecluse-ci";
          buildInputs = ciInputs;
        });

        # Lean shell for the security workflow: the vuln scanners only. CI enters
        # it with `nix develop .#scan`.
        devShells.scan = pkgs.mkShell (shellEnv // {
          name = "ecluse-scan";
          buildInputs = [ pkgs.bashInteractive pkgs.sbomnix ] ++ scanInputs;
        });

        # Lean shell for the workflow-lint gate job: the two Actions linters only.
        # CI enters it with `nix develop .#workflow-lint`.
        devShells.workflow-lint = pkgs.mkShell (shellEnv // {
          name = "ecluse-workflow-lint";
          buildInputs = [ pkgs.bashInteractive ] ++ workflowLintInputs;
        });

        # Shell for the informational weeder job: the CI toolchain (to build the
        # .hie files weeder reads) plus weeder itself, which must come from the
        # same GHC 9.10 set as the compiler that produced those files. CI enters it
        # with `nix develop .#weeder`.
        devShells.weeder = pkgs.mkShell (shellEnv // {
          name = "ecluse-weeder";
          buildInputs = ciInputs ++ [ hpkgs.weeder ];
        });

        # Lean shell for the release workflow. release.yml enters it with
        # `nix develop .#release`. It carries the release tooling (skopeo, sbomnix)
        # plus the multi-arch assembly tools. The assembly is deliberately
        # DAEMONLESS: skopeo writes each per-arch Nix archive into an on-disk OCI
        # image layout (plain files — no container engine, no user namespace, which
        # ubuntu-24.04's AppArmor blocks for /nix/store binaries), and `regctl`
        # (regclient) builds the index from those layouts and copies it to the
        # registry as ONE canonical `:X.Y.Z` tag (an OCI index over both platform
        # images — no lingering per-arch tags, the way `docker buildx imagetools
        # create` would leave them). `jq` parses the pushed index for the
        # per-platform digests the attest-actions bind to. (regctl replaced podman
        # here: podman needs rootless containers-storage, whose user namespace the
        # runner's AppArmor denies.) See scripts/push-multiarch.sh and
        # docs/architecture/release-supply-chain.md → "Multi-architecture image".
        devShells.release = pkgs.mkShell (shellEnv // {
          name = "ecluse-release";
          buildInputs = [ pkgs.bashInteractive pkgs.regclient pkgs.jq ] ++ releaseInputs;
        });

        # PoC LSP<->MCP bridge shell (HLS + mcp-language-server). Opt-in only: not
        # built by CI (the gate runs no `nix flake check`) and not part of the
        # default dev shell. Enter with `nix develop .#mcp`, or let `.mcp.json`
        # launch it. See AGENTS.md → "Build & Tooling".
        devShells.mcp = pkgs.mkShell (shellEnv // {
          name = "ecluse-mcp";
          buildInputs = mcpInputs;
        });
      });
}
