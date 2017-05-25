open Types

type t

exception IllegalNumberOfTypeArguments    of Range.t * type_name * int * int
exception UndefinedTypeName               of Range.t * type_name
exception UndefinedTypeArgument           of Range.t * var_name
exception CyclicTypeDefinition            of (Range.t * type_name) list
exception MultipleTypeDefinition          of Range.t * Range.t * type_name
exception NotProvidingValueImplementation of Range.t * var_name
exception NotProvidingTypeImplementation  of Range.t * type_name
exception NotMatchingInterface            of Range.t * var_name * t * poly_type * t * poly_type

val from_list : (var_name * poly_type) list -> t

val add : t -> var_name -> poly_type -> t

val find : t -> (module_name list) -> var_name -> poly_type

val enter_new_module : t -> module_name -> t

val leave_module : t -> t

val add_mutual_cons : t -> Tyvarid.level -> untyped_mutual_variant_cons -> t

val find_constructor : quantifiability -> t -> Tyvarid.level -> constructor_name -> (mono_type list * Typeid.t * mono_type)

val fix_manual_type_free : quantifiability -> t -> Tyvarid.level -> manual_type -> mono_type

val find_type_id : t -> type_name -> Typeid.t

val find_type_name : t -> Typeid.t -> type_name

val sigcheck : Range.t -> quantifiability -> Tyvarid.level -> t -> t -> manual_signature option -> t
