open Types

val empty : type_environment

val add : type_environment -> var_name -> type_struct -> type_environment

val find : type_environment -> var_name -> type_struct

val get_range_from_type : type_struct -> code_range

val overwrite_range_of_type : type_struct -> code_range -> type_struct

val erase_range_of_type : type_struct -> type_struct

val find_in_type_struct : type_variable_id -> type_struct -> bool

val find_in_type_environment : type_variable_id -> type_environment -> bool



val make_forall_type : type_struct -> type_environment -> type_struct