
open Types
open CompiledTypes

exception ExecError of string

val exec_code : compiled_environment -> instruction list -> compiled_value * compiled_environment option
