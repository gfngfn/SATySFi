open Types

exception TypeCheckError of string

val main : abstract_tree -> string
