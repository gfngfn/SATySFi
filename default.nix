{ pkgs ? import <nixpkgs> {}
, ocamlPackages ? import ./nix/ocamlPackages.nix { inherit pkgs; }
, opam2nix ?
    pkgs.callPackage ./nix/opam2nix.nix {
      inherit pkgs;
      ocamlPackagesOverride = ocamlPackages;

} }:

pkgs.callPackage ./nix { inherit ocamlPackages opam2nix; }
