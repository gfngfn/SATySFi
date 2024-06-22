open Types

exception EvalError of string

val interpret_0 : environment -> abstract_tree -> syntactic_value

val interpret_1 : environment -> abstract_tree -> code_value

val interpret_bindings_0 : run_tests:bool -> environment -> binding list -> environment * code_rec_or_nonrec list

val select_pattern : Range.t -> environment -> syntactic_value -> pattern_branch list -> syntactic_value
