{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        config = { };

        overlay = pkgsNew: pkgsOld: {
          tribot = pkgsNew.haskell.lib.justStaticExecutables
            pkgsNew.haskellPackages.tribot;

          haskellPackages = pkgsOld.haskellPackages.override (old: {
            overrides =
              pkgsNew.haskell.lib.packageSourceOverrides { fswatcher = ./.; };
          });
        };

        pkgs = import nixpkgs {
          inherit config system;
          overlays = [ overlay ];
        };

      in rec {
        packages.default = pkgs.haskellPackages.fswatcher;

        apps.default = {
          type = "app";
          program = "${pkgs.fswatcher}/bin/fswatcher";
        };

        devShells = {
          default = pkgs.mkShell {
            buildInputs = with pkgs;
              [
                haskellPackages.haskell-language-server
                haskellPackages.hlint
                haskellPackages.cabal-fmt
                haskellPackages.ormolu
                cabal-install
                zlib
                sqlite
              ] ++ (if pkgs.stdenv.isDarwin then [
                pkgs.stdenv
                pkgs.darwin.apple_sdk.frameworks.Cocoa
                pkgs.darwin.apple_sdk.frameworks.CoreFoundation
              ] else
                [ ]);
          };
        };
      });
}
