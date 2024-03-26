{ pkgs ? import <nixpkgs> {}
, ocamlPackages ? import ./nix/ocamlPackages.nix { inherit pkgs; } }:
let
  local = pkgs.callPackage ./. { inherit pkgs ocamlPackages; };
in
pkgs.mkShell {
  inputsFrom = with local; [ satysfi ];
  buildInputs = [ ocamlPackages.ocaml-lsp ocamlPackages.ocp-indent ];
}
