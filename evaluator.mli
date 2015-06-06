open Types

exception EvalError of string

val main : abstract_tree -> abstract_tree
