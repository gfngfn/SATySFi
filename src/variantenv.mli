open Types

exception IllegalNumberOfTypeArguments of Range.t * type_name * int * int
exception UndefinedTypeName            of Range.t * type_name
exception UndefinedTypeArgument        of Range.t * var_name
exception CyclicTypeDefinition         of type_name list
exception MultipleTypeDefinition       of type_name

type t

val empty : t
(*
val add_constructor : t -> constructor_name -> (type_variable_info ref) list -> poly_type -> type_name -> t
*)
val add_mutual_cons : t -> untyped_mutual_variant_cons -> t

val find_constructor : quantifiability -> t -> constructor_name -> (mono_type list * Typeid.t * mono_type)

val fix_manual_type_for_inner : quantifiability -> t -> manual_type -> mono_type

val find_type_id : t -> type_name -> Typeid.t

val find_type_name : t -> Typeid.t -> type_name
(*
val instantiate_type_scheme : mono_type list -> (type_variable_info ref) list -> poly_type -> mono_type
*)
