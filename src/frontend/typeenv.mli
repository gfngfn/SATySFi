module Types = Types_
open Types

type t

exception IllegalNumberOfTypeArguments    of Range.t * type_name * int * int
exception UndefinedTypeName               of Range.t * module_name list * type_name * type_name list
exception UndefinedTypeArgument           of Range.t * var_name * var_name list
exception CyclicTypeDefinition            of (Range.t * type_name) list
exception MultipleTypeDefinition          of Range.t * Range.t * type_name
exception NotProvidingValueImplementation of Range.t * var_name
exception NotProvidingTypeImplementation  of Range.t * type_name
exception NotMatchingInterface            of Range.t * var_name * t * poly_type * t * poly_type
exception UndefinedModuleName             of Range.t * module_name * module_name list
(*
exception UndefinedModuleNameList         of module_name list
*)

val initialize_id : unit -> unit

val empty : t

val add : t -> var_name -> (poly_type * EvalVarID.t) -> t

val find : t -> (module_name list) -> var_name -> Range.t -> (poly_type * EvalVarID.t) option

val find_candidates : t -> (module_name list) -> var_name -> Range.t -> var_name list

val open_module : t -> Range.t -> module_name -> t

val enter_new_module : t -> module_name -> t

val leave_module : t -> t

val add_mutual_cons : t -> FreeID.level -> untyped_mutual_variant_cons -> t

val find_constructor : quantifiability -> t -> FreeID.level -> constructor_name -> (mono_type list * type_id * mono_type) option

val find_constructor_candidates : quantifiability -> t -> FreeID.level -> constructor_name -> constructor_name list

val enumerate_constructors : quantifiability -> t -> FreeID.level -> type_id -> (constructor_name * (mono_type list -> mono_type)) list

val fix_manual_type_free : quantifiability -> t -> FreeID.level -> manual_type -> constraints -> mono_type

val find_type_id : t -> module_name list -> type_name -> Range.t -> type_id option

val find_type_name : t -> type_id -> type_name

val sigcheck : Range.t -> quantifiability -> FreeID.level -> t -> t -> manual_signature option -> t

module Raw : sig
  val fresh_type_id : string -> type_definition -> type_id
  val add_constructor : constructor_name -> type_scheme -> type_id -> t -> t
  val register_type : type_name -> type_id -> t -> t
end
