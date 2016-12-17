open Types

val string_of_utast : untyped_abstract_tree -> string

val string_of_ast : abstract_tree -> string

val string_of_type_struct_basic : type_struct -> string

val string_of_kind_struct_basic : kind_struct -> string

val string_of_type_struct : Kindenv.t -> type_struct -> string

val string_of_type_struct_double : Kindenv.t -> type_struct -> type_struct -> (string * string)

val string_of_kind_environment : Kindenv.t -> string
