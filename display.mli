open Types

val string_of_utast : untyped_abstract_tree -> string

val string_of_ast : abstract_tree -> string

val describe_position : code_range -> string

val error_reporting : code_range -> string -> string

val bug_reporting : code_range -> string -> string

val string_of_type_struct_basic : type_struct -> string

val string_of_type_struct : type_struct -> string

val string_of_type_struct_double : type_struct -> type_struct -> (string * string)

val string_of_type_environment : type_environment -> string -> string

val string_of_control_sequence_type : type_environment -> string
