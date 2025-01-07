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

        postgresql = pkgs.postgresql_17;

        stack-wrapped = pkgs.symlinkJoin {
          name = "stack";
          paths = [pkgs.stack];
          buildInputs = [pkgs.makeWrapper];
          postBuild = ''
            wrapProgram $out/bin/stack \
              --add-flags "--system-ghc --no-install-ghc"
          '';

          PG_CONFIG="${postgresql}/bin/pg_config";
        };

        packages = [
          stack-wrapped

          hPkgs.ghc
          hPkgs.ghcid
          hPkgs.haskell-language-server

          pkgs.zlib
        ];
      in {
        devShells.default = pkgs.mkShell {
          inherit packages;

          LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath packages;
        };
      }
    );
}
