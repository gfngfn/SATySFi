open Types

exception EvalError of string

val interpret_0 : environment -> abstract_tree -> syntactic_value

val interpret_1 : environment -> abstract_tree -> code_value

val interpret_bindings_0 : environment -> binding list -> environment * code_rec_or_nonrec list

val select_pattern : Range.t -> environment -> syntactic_value -> pattern_branch list -> syntactic_value

val interpret_pdf_mode_intermediate_input_horz : environment -> syntactic_value -> intermediate_input_horz_element list -> syntactic_value
