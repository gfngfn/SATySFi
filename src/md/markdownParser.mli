
open EnvelopeSystemBase
open Types

type error =
  | InvalidHeaderComment
  | InvalidExtraExpression

type t

val decode : string -> (t, error) result

val convert : markdown_conversion -> t -> untyped_abstract_tree
