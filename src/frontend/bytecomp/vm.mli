
open Types

exception ExecError of string

val exec_code : environment -> instruction list -> syntactic_value
