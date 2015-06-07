open Types

exception TypeCheckError of string

val initialize : unit -> unit
val main : type_environment -> abstract_tree -> (string * type_environment)
