open Types
open Display

exception IllegalNumberOfTypeArguments of Range.t * type_name * int * int
exception UndefinedTypeName            of Range.t * type_name
exception UndefinedTypeArgument        of Range.t * var_name

type t

val empty : t

val add : t -> constructor_name -> poly_type -> type_name -> t

val add_list : t -> (constructor_name * poly_type * type_name) list -> t

val register_variant : t -> int -> type_name -> t

val register_variant_list : t -> (int * type_name) list -> t

val add_mutual_cons : t -> untyped_mutual_variant_cons -> t

val add_mutual_cons_hidden : module_name -> t -> untyped_mutual_variant_cons -> t

val find : t -> constructor_name -> (Typeid.t * poly_type)

val apply_to_type_synonym : mono_type list -> poly_type -> mono_type

val fix_manual_type_for_inner_and_outer : Tyvarid.quantifiability -> t -> manual_type -> (mono_type * mono_type)

val append_module_name : module_name -> var_name -> var_name

val find_type_id : t -> type_name -> Typeid.t

val find_type_name : t -> Typeid.t -> type_name
