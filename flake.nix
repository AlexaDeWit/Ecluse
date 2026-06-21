{
  description = "ecluse: supply-chain resilience proxy for package registries";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.11";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        hlib = pkgs.haskell.lib;
        hpkgs = pkgs.haskell.packages.ghc96;

        # The cabal package, built by Nix. callCabal2nix reads ecluse.cabal and
        # resolves dependencies from the nixpkgs GHC 9.6 set (pinned by
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
      in {
        packages = {
          default = ecluse;
          ecluse = ecluse;

          # Lean, reproducible OCI image, built straight from the binary's Nix
          # closure — no Dockerfile, no base distro. It contains only the runtime
          # closure plus CA certificates: no shell, no package manager, runs
          # non-root. `buildLayeredImage` splits the closure into cache-friendly
          # layers, and the build is bit-for-bit reproducible (a fitting property
          # for a supply-chain tool). `tag = null` derives a unique content-hash
          # tag for local use; releases retag at push time because the target
          # repo enforces immutable tags (see CONTRIBUTING.md "Releases").
          # Push/sign via `make docker-push` / `make docker-sign`.
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

        devShells.default = pkgs.mkShell {
          name = "ecluse";

          # Force a UTF-8 locale so test runners (hspec prints '✔') and any
          # other Unicode output encode correctly regardless of the host locale.
          LANG = "C.UTF-8";
          LC_ALL = "C.UTF-8";

          # Make `require("semver")` resolvable for the npm version-ordering
          # oracle (used by the version-fixture generator and the smoke suite).
          NODE_PATH = "${pkgs.nodePackages.semver}/lib/node_modules";

          buildInputs = [
            pkgs.bashInteractive
            hpkgs.ghc
            hpkgs.cabal-install
            hpkgs.haskell-language-server
            hpkgs.ghcid
            # Haskell API search & build-plan inspection (see AGENTS.md).
            hpkgs.hoogle
            hpkgs.cabal-plan
            hpkgs.fourmolu
            hpkgs.hlint
            pkgs.semgrep
            pkgs.zlib
            pkgs.pkg-config
            # Reference version-ordering oracles for the differential smoke
            # suite and `make gen-version-fixtures`: node-semver (npm), Python
            # packaging (PyPI), and Ruby Gem::Version (built into ruby).
            pkgs.nodejs
            pkgs.nodePackages.semver
            (pkgs.python3.withPackages (ps: [ ps.packaging ]))
            pkgs.ruby
            # Container image publishing: skopeo pushes the Nix-built image to a
            # registry (no Docker daemon needed), cosign signs it keylessly.
            pkgs.skopeo
            pkgs.cosign
          ];
        };
      });
}
