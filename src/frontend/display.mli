open Types
open StaticEnv

val show_mono_type : mono_type -> string

val show_mono_type_double : mono_type -> mono_type -> (string * string)

val show_poly_type : poly_type -> string

val show_macro_type : macro_type -> string
