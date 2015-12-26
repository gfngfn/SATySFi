open Types
open Display

type t

val empty : t

val add : t -> constructor_name -> type_struct -> type_name -> t

val add_cons : module_name -> t -> type_name -> untyped_variant_cons -> t

val add_mutual_cons : t -> untyped_mutual_variant_cons -> t

val add_mutual_cons_hidden : module_name -> t -> untyped_mutual_variant_cons -> t

val add_type_synonym : t -> type_name -> type_struct -> t

val find : t -> constructor_name -> (type_name * type_struct)
