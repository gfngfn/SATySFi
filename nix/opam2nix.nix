{ source ? builtins.fetchTarball "https://github.com/timbertson/opam2nix/archive/v1.tar.gz"
, pkgs ? import <nixpkgs> {}
, ocamlPackagesOverride ? import ./ocamlPackages.nix { inherit pkgs; } }:
import source { inherit pkgs ocamlPackagesOverride; }
