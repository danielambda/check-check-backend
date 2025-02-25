{
  description = "Check-Check backend dev shell";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.11";
    systems.url = "github:nix-systems/default";
    flake-utils = {
      url = "github:numtide/flake-utils";
      inputs.systems.follows = "systems";
    };
  };

  outputs = { nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        hPkgs = pkgs.haskell.packages.ghc966;

        packages = [
          pkgs.nixd

          hPkgs.ghc
          hPkgs.ghcid
          hPkgs.haskell-language-server
          pkgs.stack

          pkgs.zlib
        ];
      in {
        devShells.default = pkgs.mkShell {
          inherit packages;

          shellHook = ''
            set -a
            source ./.env
            set +a
          '';

          LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath packages;
        };
      }
    );
}
