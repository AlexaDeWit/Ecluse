{
  description = "npm-secure-proxy: defense-in-depth npm registry proxy";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.11";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        hpkgs = pkgs.haskell.packages.ghc96;
      in {
        devShells.default = pkgs.mkShell {
          name = "npm-secure-proxy";

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
