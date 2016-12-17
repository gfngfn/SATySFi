open Types
open Typeenv

exception Error of string

val main : Variantenv.t -> Kindenv.t -> Typeenv.t -> untyped_abstract_tree -> (type_struct * Variantenv.t * Kindenv.t * Typeenv.t * abstract_tree)
