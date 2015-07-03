open Types
open Typeenv

val initialize : unit -> unit
val main : type_environment -> untyped_abstract_tree -> (string * type_environment * abstract_tree)
