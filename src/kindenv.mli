open Types

type t

val empty : t

val to_kind_list : t -> kind list

val add : t -> Tyvarid.t -> kind -> t

val find : t -> Tyvarid.t -> kind

val replace_type_variable_in_kindenv : t -> Tyvarid.t -> mono_type -> t

val replace_type_variable_in_kind : kind -> Tyvarid.t -> mono_type -> kind

val to_string : (kind -> string) -> t -> string
