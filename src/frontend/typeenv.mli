module Types = Types_
open Types

type t

type type_scheme = BoundID.t list * poly_type

type type_definition =
  | Data  of int
  | Alias of type_scheme

exception IllegalNumberOfTypeArguments    of Range.t * type_name * int * int
exception UndefinedTypeName               of Range.t * module_name list * type_name
exception UndefinedTypeArgument           of Range.t * var_name
exception CyclicTypeDefinition            of (Range.t * type_name) list
exception MultipleTypeDefinition          of Range.t * Range.t * type_name
exception NotProvidingValueImplementation of Range.t * var_name
exception NotProvidingTypeImplementation  of Range.t * type_name
exception NotMatchingInterface            of Range.t * var_name * t * poly_type * t * poly_type
exception UndefinedModuleName             of Range.t * module_name
(*
exception UndefinedModuleNameList         of module_name list
*)

val initialize_id : unit -> unit

val empty : t

val add : t -> var_name -> (poly_type * EvalVarID.t) -> t

val find : t -> (module_name list) -> var_name -> Range.t -> (poly_type * EvalVarID.t) option

val open_module : t -> Range.t -> module_name -> t

val enter_new_module : t -> module_name -> t

val leave_module : t -> t

val add_mutual_cons : t -> FreeID.level -> untyped_mutual_variant_cons -> t

val find_constructor : quantifiability -> t -> FreeID.level -> constructor_name -> (mono_type list * TypeID.t * mono_type) option

val fix_manual_type_free : quantifiability -> t -> FreeID.level -> manual_type -> constraints -> mono_type

val find_type_id : t -> module_name list -> type_name -> Range.t -> TypeID.t option

val find_type_name : t -> TypeID.t -> type_name

val sigcheck : Range.t -> quantifiability -> FreeID.level -> t -> t -> manual_signature option -> t

module Raw : sig
  val fresh_type_id : string -> TypeID.t
  val add_constructor : constructor_name -> type_scheme -> TypeID.t -> t -> t
  val register_type : type_name -> TypeID.t -> type_definition -> t -> t
end
