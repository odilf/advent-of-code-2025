{
  description = "Description for the project";

  inputs = {
    flake-parts.url = "github:hercules-ci/flake-parts";
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.05";
  };

  outputs =
    inputs@{ flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [
        "x86_64-linux"
        "aarch64-linux"
        "aarch64-darwin"
        "x86_64-darwin"
      ];
      perSystem =
        { pkgs, ... }:
        {
          # devShells.default = pkgs.haskellPackages.developPackage {
          #   root = ./.;
          # };
          devShells.default = pkgs.mkShell {
            packages = [
              # pkgs.cabal-install
              pkgs.stack
              pkgs.ghc
              pkgs.haskell-language-server
              pkgs.zlib
              # (pkgs.haskell-language-server.override { supportedGhcVersions = [ "910" ]; })
              # pkgs.gcc
            ];
          };
        };
      flake = {
      };
    };
}
