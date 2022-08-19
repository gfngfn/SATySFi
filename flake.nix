{
  description = "Nix Flake for SATySFi";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    opam-nix.url = "github:tweag/opam-nix";
    satysfi-external-repo = {
      url = "github:gfngfn/satysfi-external-repo";
      flake = false;
    };

    # develop
    devshell.url = "github:numtide/devshell";
    flake-utils.url = "github:numtide/flake-utils";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };
  outputs = {
    self,
    nixpkgs,
    opam-nix,
    satysfi-external-repo,
    devshell,
    flake-utils,
    ...
  }:
    flake-utils.lib.eachSystem [
      "x86_64-linux"
    ] (
      system: let
        inherit (pkgs) lib;
        pkgs = import nixpkgs {
          inherit system;
          overlays = [
            devshell.overlay
          ];
        };
      in {
        packages.bin-satysfi =
          (
            (
              with opam-nix.lib.${system}; (
                buildDuneProject {
                  repos = [
                    opam-nix.inputs.opam-repository
                    (opam-nix.lib.${system}.makeOpamRepoRec satysfi-external-repo)
                  ];
                }
                "satysfi"
                ./.
                {
                  satysfi = null;
                  ocaml-base-compiler = null;
                }
              )
            )
            .overrideScope' (self: super: {
              satysfi = super.satysfi.overrideAttrs (oa: {
                doNixSupport = false;
                removeOcamlReferences = true;
              });
              camlpdf = super.camlpdf.overrideAttrs (oa: {
                nativeBuildInputs =
                  oa.nativeBuildInputs
                  ++ (with pkgs; [which])
                  ++ (with pkgs.ocamlPackages; [findlib]);
              });
              otfm = super.otfm.overrideAttrs (oa: {
                buildInputs =
                  oa.nativeBuildInputs
                  ++ (with pkgs; [ocamlPackages.topkg]);
                nativeBuildInputs =
                  oa.nativeBuildInputs
                  ++ (with pkgs.ocamlPackages; [findlib ocamlbuild topkg]);
              });
              yojson-with-position = super.yojson-with-position.overrideAttrs (oa: {
                nativeBuildInputs =
                  oa.nativeBuildInputs
                  ++ (with pkgs.ocamlPackages; [cppo]);
              });
            })
          )
          .satysfi;

        packages.lib-satysfi = pkgs.stdenv.mkDerivation {
          name = "lib-satysfi";
          src = ./.;
          dontBuild = true;
          installPhase = with pkgs; ''
            # https://github.com/NixOS/nixpkgs/blob/762b003329510ea855b4097a37511eb19c7077f0/pkgs/tools/typesetting/satysfi/default.nix
            cp -r ${lmodern}/share/fonts/opentype/public/lm/* lib-satysfi/dist/fonts/
            cp -r ${lmmath}/share/fonts/opentype/latinmodern-math.otf lib-satysfi/dist/fonts/
            cp -r ${ipaexfont}/share/fonts/opentype/* lib-satysfi/dist/fonts/
            cp -r ${junicode}/share/fonts/junicode-ttf/* lib-satysfi/dist/fonts/
            mkdir -p $out/share/satysfi
            cp -r lib-satysfi/dist/ $out/share/satysfi/
          '';
        };

        packages.satysfi = pkgs.stdenv.mkDerivation {
          name = "satysfi";
          unpackPhase = "true";
          installPhase = with self.packages.${system}; ''
            mkdir -p $out/{bin,share}
            cp -r ${bin-satysfi}/bin $out/bin
            cp -r ${lib-satysfi}/share $out/share
          '';
        };

        packages.default = self.packages.${system}.satysfi;

        apps.satysfi = flake-utils.lib.mkApp {drv = self.packages.${system}.satysfi;};
        apps.default = self.apps.${system}.satysfi;

        devShells.default = pkgs.devshell.mkShell {
          packages = with pkgs; [
            # develop
            treefmt # wrapper cli to run all formatters
            alejandra # Nix formatter
          ];
        };

        checks = {
          inherit
            (self.packages.${system})
            bin-satysfi
            lib-satysfi
            satysfi
            ;
        };
      }
    );
}
