open Types
open Typeenv

type t

val empty : t

val add : t -> Tyvarid.t -> type_struct -> t

val find : t -> Tyvarid.t -> type_struct

val eliminate : t -> Tyvarid.t -> t

val apply_to_type_struct : t -> type_struct -> type_struct

val apply_to_type_environment : t -> Typeenv.t -> Typeenv.t

val compose : t -> t -> t

val unify : type_struct -> type_struct -> t

val string_of_subst : t -> string
