{ pkgs, lib, stdenv, ocamlPackages, opam2nix }:
let
  inherit (lib) strings;

  source =
    let ignores = pkgs.lib.strings.fileContents ../.gitignore;
    in pkgs.nix-gitignore.gitignoreSourcePure ignores ../.;

  localPackages =
    let contents = builtins.attrNames (builtins.readDir ../.);
    in builtins.filter (strings.hasSuffix ".opam") contents;

  src = pkgs.lib.listToAttrs (builtins.map (name:
    { name = strings.removeSuffix ".opam" name; value = source; }) localPackages);

  args = {
    inherit (ocamlPackages) ocaml;
    inherit src;
    selection = ./opam-selection.nix;
  };

  opam-selection = opam2nix.build (args // {
    override = { pkgs, ... }: {
      camlpdf = super: super.overrideAttrs (attrs: {
        buildInputs = (attrs.buildInputs or []) ++ [ pkgs.which ];
      });
      easy-format = super: super.overrideAttrs (_: {
        buildPhase = ''
          dune build -p easy-format @install
        '';
      });
    };
  });

  resolve = opam2nix.resolve (args // {
    repo = {
      key = "satysfi-external";
      url = "gfngfn/satysfi-external-repo";
    };
  }) localPackages;


in (builtins.listToAttrs (builtins.map (fname:
  let packageName = strings.removeSuffix ".opam" fname;
  in {
    name = packageName;
    value = builtins.getAttr packageName opam-selection;
  }) localPackages)) // {
    inherit resolve opam-selection;
  }
