open Types

type t

val empty : t

val to_list : t -> (var_name * poly_type) list

val from_list : (var_name * poly_type) list -> t

val map : ((var_name * poly_type) -> (var_name * poly_type)) -> t -> t

val add : t -> var_name -> poly_type -> t

val find : t -> var_name -> poly_type

val find_in_mono_type : Tyvarid.t -> mono_type -> bool

val find_in_type_environment : Tyvarid.t -> t -> bool

val make_forall_type : mono_type -> t -> Kindenv.t -> poly_type

val string_of_type_environment : t -> string -> string

val string_of_control_sequence_type : t -> string

val replace_id : (Tyvarid.t * mono_type) list -> mono_type -> mono_type

val replace_id_poly : (Tyvarid.t * mono_type) list -> poly_type -> poly_type

val instantiate : Tyvarid.quantifiability -> Kindenv.t -> poly_type -> (mono_type * (mono_type list) * Kindenv.t)
