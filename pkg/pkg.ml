#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let () =
  Pkg.describe "otfm" @@ fun c ->
  Ok [ Pkg.mllib "src/otfm.mllib";
       Pkg.bin "test/otftrip";
       Pkg.doc "test/examples.ml";
       Pkg.test "test/examples"; ]
