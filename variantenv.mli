open Types
open Display

type t

val empty : t

val add_cons : t -> variant_type_name -> untyped_variant_cons -> t

val add_mutual_cons : t -> untyped_mutual_variant_cons -> t

val find : t -> constructor_name -> (variant_type_name * type_struct)
