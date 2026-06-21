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
          buildInputs = [
            hpkgs.ghc
            hpkgs.cabal-install
            hpkgs.haskell-language-server
            hpkgs.ghcid
            pkgs.zlib
            pkgs.pkg-config
          ];
        };
      });
}
