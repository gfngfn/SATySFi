module Types = Types_
open Types

exception EvalError of string

val interpret : environment -> abstract_tree -> syntactic_value

val select_pattern : Range.t -> environment -> syntactic_value -> pattern_branch list -> syntactic_value


val interpret_intermediate_input_horz : environment -> syntactic_value -> intermediate_input_horz_element list -> syntactic_value
