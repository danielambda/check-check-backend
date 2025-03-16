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
        contracts = hPkgs.callCabal2nix "contracts" ./contracts {
          inherit smart-primitives;
        };
        core = hPkgs.callCabal2nix "core" ./core {
          inherit smart-primitives;
        };
        infrastructure = hPkgs.callCabal2nix "infrastructure" ./infrastructure {
          inherit smart-primitives core;
        };
        web-api = hPkgs.callCabal2nix "web-api" ./web-api {
          inherit smart-primitives core contracts infrastructure;
        };
        telegram-bot = hPkgs.callCabal2nix "telegram-bot" ./telegram-bot {
          inherit smart-primitives contracts;
        };

        web-api-image = pkgs.dockerTools.buildLayeredImage {
          name = "check-check-backend";
          tag = "latest";

          contents = [infrastructure web-api pkgs.cacert];

          config.Cmd = [
            "${pkgs.runtimeShell}" "-c"
            "${infrastructure}/bin/init-db && ${web-api}/bin/web-api"
          ];
        };

        telegram-bot-image = pkgs.dockerTools.buildLayeredImage {
          name = "check-check-telegram-bot";
          tag = "latest";

          contents = [telegram-bot];

          config.Cmd = ["${telegram-bot}/bin/telegram-bot"];
        };
      in {
        devShell = pkgs.mkShell {
          inherit packages;

          LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath packages;
        };

        packages.telegram-bot = telegram-bot;
        packages.telegram-bot-image = telegram-bot-image;

        packages.web-api = web-api;
        packages.web-api-image = web-api-image;
      }
    );
}
