open Types
open Display

type t

val empty : t

val add : t -> constructor_name -> type_struct -> type_name -> t

val add_mutual_cons : t -> untyped_mutual_variant_cons -> t

val add_mutual_cons_hidden : module_name -> t -> untyped_mutual_variant_cons -> t

val find : t -> constructor_name -> (type_name * type_struct)

val apply_to_type_synonym : type_struct list -> type_struct -> type_struct

val fix_manual_type : t -> untyped_type_argument_cons -> type_struct -> type_struct