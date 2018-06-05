
open Types_

val exec : syntactic_value list -> vmenv -> instruction list -> (vmenv * instruction list) list -> syntactic_value
