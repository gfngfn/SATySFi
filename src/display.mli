open Types

val string_of_utast : untyped_abstract_tree -> string

val string_of_ast : abstract_tree -> string

val string_of_mono_type_basic : mono_type -> string

val string_of_poly_type_basic : poly_type -> string

val string_of_kind_basic : kind -> string

val string_of_mono_type : Kindenv.t -> mono_type -> string

val string_of_mono_type_double : Kindenv.t -> mono_type -> mono_type -> (string * string)

val string_of_kind_environment : Kindenv.t -> string
