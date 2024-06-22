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
    nix-filter.url = "github:numtide/nix-filter/3e1fff9ec0112fe5ec61ea7cc6d37c1720d865f8";
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
    nix-filter,
    opam-nix,
    satysfi-external-repo,
    devshell,
    flake-utils,
    ...
  }:
    flake-utils.lib.eachSystem [
      "x86_64-linux"
      "x86_64-darwin"
    ] (
      system: let
        inherit (pkgs) lib stdenv;
        inherit (opam-nix.lib.${system}) buildDuneProject makeOpamRepoRec;
        pkgs = import nixpkgs {
          inherit system;
          overlays = [
            devshell.overlay
          ];
        };

        duneProject =
          buildDuneProject {
            repos = [
              opam-nix.inputs.opam-repository
              (makeOpamRepoRec satysfi-external-repo)
            ];
          } "satysfi" (with nix-filter.lib;
            filter {
              root = ./.;
              include = [
                "dune-project"
                "satysfi.opam"
                "Makefile"
                (inDirectory "bin")
                (inDirectory "obsolete")
                (inDirectory "src")
                (inDirectory "tools")
                (inDirectory "lib-satysfi")
              ];
              exclude = [".merlin"];
            }) {satysfi = null;};
        duneProjectOverlay = self: super: {
          satysfi = super.satysfi.overrideAttrs (oa: {
            doNixSupport = false;
            removeOcamlReferences = true;
            preConfigure = ''
              substituteInPlace src/frontend/main.ml \
              --replace \
                '/usr/local/share/satysfi"; "/usr/share/satysfi' \
                $out/share/satysfi
            '';
            installPhase = with pkgs; ''
              cp -r ${lmodern}/share/fonts/opentype/public/lm/* lib-satysfi/dist/fonts/
              cp -r ${lmmath}/share/fonts/opentype/latinmodern-math.otf lib-satysfi/dist/fonts/
              cp -r ${ipaexfont}/share/fonts/opentype/* lib-satysfi/dist/fonts/
              cp -r ${junicode}/share/fonts/junicode-ttf/* lib-satysfi/dist/fonts/
              make install PREFIX=$out LIBDIR=$out/share/satysfi
              mkdir -p $out/share/satysfi/
              cp -r lib-satysfi/dist/ $out/share/satysfi/
            '';
          });
        };
        ocamlPkgs = duneProject.overrideScope' duneProjectOverlay;

        mkDoc = {
          name,
          root,
          entrypoint,
          output,
        }:
          stdenv.mkDerivation {
            name = "doc-${name}";
            buildInputs = with self.packages.${system}; [satysfi];
            src = with (nix-filter.lib);
              filter {
                root = root;
                exclude = [
                  (matchExt "pdf")
                  (matchExt "satysfi-aux")
                ];
              };
            buildPhase = ''
              satysfi ${entrypoint} --output ${output}
            '';
            installPhase = ''
              mkdir -p $out
              cp ${output} $out
            '';
          };
      in {
        packages.default = self.packages.${system}.satysfi;
        packages.satysfi = ocamlPkgs.satysfi;

        packages.doc-demo = mkDoc {
          name = "demo";
          root = ./demo;
          entrypoint = "demo.saty";
          output = "demo.pdf";
        };
        packages.doc-lang = mkDoc {
          name = "lang";
          root = ./doc;
          entrypoint = "doc-lang.saty";
          output = "doc-lang.pdf";
        };
        packages.doc-primitives = mkDoc {
          name = "lang";
          root = ./doc;
          entrypoint = "doc-primitives.saty";
          output = "doc-primitives.pdf";
        };
        packages.tests-images = mkDoc {
          name = "tests-image";
          root = ./tests/images;
          entrypoint = "test.saty";
          output = "test.pdf";
        };

        apps.satysfi = flake-utils.lib.mkApp {
          drv = self.packages.${system}.satysfi;
        };

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
            satysfi
            doc-demo
            doc-lang
            doc-primitives
            ;
        };
      }
    );
}
