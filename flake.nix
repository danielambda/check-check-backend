{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";

    smart-primitives.url              = "github:danielambda/smart-primitives";
    check-check-backend-contracts.url = "github:danielambda/check-check-backend-contracts";
  };

  outputs = inputs@{ nixpkgs, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [inputs.haskell-flake.flakeModule];
      perSystem = { self', config, pkgs, ... }:
        let
          web-api = self'.packages.web-api;
          infrastructure = self'.packages.infrastructure;
          web-api-image = pkgs.dockerTools.buildLayeredImage {
            name = "check-check-backend";
            tag = "latest";
            contents = [web-api pkgs.cacert];
            config = {
              Expose = [8080];
              Entrypoint = ["${web-api}/bin/web-api"];
            };
          };

          init-db-image = pkgs.dockerTools.buildLayeredImage {
            name = "check-check-backend-init-db";
            tag = "latest";
            contents = [infrastructure];
            config.Entrypoint = ["${infrastructure}/bin/init-db"];
          };
        in {
        haskellProjects.default = {
          autoWire = ["packages" "apps"];
          packages = {
            smart-primitives.source = inputs.smart-primitives;
            check-check-backend-contracts.source = inputs.check-check-backend-contracts;
          };
        };

        devShells.default = pkgs.mkShell {
          inputsFrom = [config.haskellProjects.default.outputs.devShell];
          packages = [pkgs.nixd];

          shellHook = ''
            set -a
            source ./.env
            set +a
          '';
        };

        packages = {
          default = web-api;
          inherit web-api-image init-db-image;
        };
      };
    };
}
