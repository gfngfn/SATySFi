open Types
open Typeenv

exception ContradictionError of string
exception InclusionError     of string

type t

val empty : t

val add : t -> Tyvarid.t -> type_struct -> t

val find : t -> Tyvarid.t -> type_struct

val apply_to_type_struct : t -> type_struct -> type_struct

val apply_to_type_environment : t -> Typeenv.t -> Typeenv.t

val compose : t -> t -> t

val compose_list : t list -> t

val unify : type_struct -> type_struct -> t

val string_of_subst : t -> string
