{
  description = "Check-Check backend flake";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
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
        hPkgs = pkgs.haskell.packages.ghc984;

        packages = [
          pkgs.nixd

          hPkgs.ghc
          hPkgs.ghcid
          hPkgs.haskell-language-server
          pkgs.stack

          pkgs.zlib
        ];

        smart-primitives = hPkgs.callCabal2nix "smart-primitives" ./smart-primitives {};
        check-check-backend-contracts = hPkgs.callCabal2nix "contracts" ./contracts {
          inherit smart-primitives;
        };
        core = hPkgs.callCabal2nix "core" ./core {
          inherit smart-primitives;
        };
        infrastructure = hPkgs.callCabal2nix "infrastructure" ./infrastructure {
          inherit smart-primitives core;
        };
        web-api = hPkgs.callCabal2nix "web-api" ./web-api {
          inherit smart-primitives core check-check-backend-contracts infrastructure;
        };
        telegram-bot = hPkgs.callCabal2nix "telegram-bot" ./telegram-bot {
          inherit smart-primitives check-check-backend-contracts;
        };

        web-api-image = pkgs.dockerTools.buildLayeredImage {
          name = "check-check-backend";
          tag = "latest";

          contents = [web-api pkgs.cacert];

          config.Expose = [8080];
          config.Entrypoint = ["${web-api}/bin/web-api"];
        };

        init-db-image = pkgs.dockerTools.buildLayeredImage {
          name = "check-check-backend-init-db";
          tag = "latest";

          contents = [infrastructure];

          config.Entrypoint = ["${infrastructure}/bin/init-db"];
        };

        telegram-bot-image = pkgs.dockerTools.buildLayeredImage {
          name = "check-check-telegram-bot";
          tag = "latest";

          contents = [telegram-bot pkgs.cacert];

          config.Entrypoint = ["${telegram-bot}/bin/telegram-bot"];
        };
      in {
        devShell = pkgs.mkShell {
          inherit packages;

          LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath packages;
        };

        packages.web-api-image = web-api-image;
        packages.init-db-image = init-db-image;
        packages.telegram-bot-image = telegram-bot-image;
      }
    );
}
