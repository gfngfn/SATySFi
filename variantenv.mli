open Types
open Display

type t

val empty : t

val add_cons : t -> variant_type_name -> untyped_abstract_tree -> t

val find : t -> constructor_name -> (variant_type_name * type_struct)
