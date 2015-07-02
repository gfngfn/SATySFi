open Types

exception TypeCheckError of string

val initialize : unit -> unit
val main : Typeenv.t -> untyped_abstract_tree -> (string * type_environment)
