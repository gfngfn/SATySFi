open Types

exception EvalError of string

val interpret : environment -> abstract_tree -> syntactic_value
