(* signature for out.ml *)
  open Types
  exception IllegalOut of string

  val main : environment -> abstract_tree -> string
