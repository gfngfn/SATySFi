
open MyUtil
open EnvelopeSystemBase
open Types

type error =
  | InvalidHeaderComment
  | InvalidExtraExpression

type t

val decode : abs_path -> string -> (t, error) result

val get_class_module_name : t -> module_name

val convert : markdown_conversion -> t -> untyped_abstract_tree
