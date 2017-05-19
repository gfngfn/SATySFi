open Types

type t
(*
val empty : t
*)
val from_list : (var_name * poly_type) list -> t

val add : t -> var_name -> poly_type -> t

val find : t -> var_name -> poly_type
(*
val string_of_type_environment : t -> string -> string

val string_of_control_sequence_type : t -> string
*)

