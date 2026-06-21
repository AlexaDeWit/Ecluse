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

        # Sources for the format/lint checks: the .hs files plus the cabal files,
        # so fourmolu can read default-language / default-extensions (GHC2021 →
        # ImportQualifiedPost) and parse postpositive `qualified` imports. Without
        # the cabal in scope, fourmolu's parser rejects them.
        hsSrc = pkgs.lib.sourceFilesBySuffices ./. [ ".hs" ".cabal" "cabal.project" ];
      in {
        packages = {
          default = ecluse;
          ecluse = ecluse;
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

          buildInputs = [
            pkgs.bashInteractive
            hpkgs.ghc
            hpkgs.cabal-install
            hpkgs.haskell-language-server
            hpkgs.ghcid
            hpkgs.fourmolu
            hpkgs.hlint
            pkgs.semgrep
            pkgs.zlib
            pkgs.pkg-config
          ];
        };
      });
}
