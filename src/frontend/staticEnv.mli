open Types

type type_scheme = BoundID.t list * poly_type
(*
type type_definition =
  | Data  of int
  | Alias of type_scheme
*)
type struct_signature

type signature =
  | ConcStructure of struct_signature
  | ConcFunctor   of functor_signature

and functor_signature = {
  opaques  : OpaqueIDSet.t;
  domain   : signature;
  codomain : OpaqueIDSet.t * signature;
}

type value_entry = {
  val_name  : EvalVarID.t;
  val_type  : poly_type;
  val_stage : stage;
}

type type_entry = {
  type_id    : TypeID.t;
  type_arity : int;
}

type constructor_entry = {
  ctor_belongs_to : TypeID.Variant.t;
  ctor_parameter  : type_scheme;
}

type macro_entry = {
  macro_type : macro_type;
  macro_name : EvalVarID.t;
}

type module_entry = {
  mod_name      : ModuleID.t;
  mod_signature : signature;
}

(*
exception UndefinedTypeName               of Range.t * module_name list * type_name * type_name list
exception UndefinedTypeArgument           of Range.t * var_name * var_name list
exception CyclicTypeDefinition            of (Range.t * type_name) list
exception MultipleTypeDefinition          of Range.t * Range.t * type_name
exception NotProvidingValueImplementation of Range.t * var_name
exception NotProvidingTypeImplementation  of Range.t * type_name
exception NotMatchingInterface            of Range.t * var_name * t * poly_type * t * poly_type
exception UndefinedModuleName             of Range.t * module_name * module_name list
*)

module Typeenv : sig

  type t

  val empty : t

  val add_macro : ctrlseq_name -> macro_entry -> t -> t

  val find_macro : ctrlseq_name -> t -> macro_entry option

  val add_value : var_name -> value_entry -> t -> t

  val find_value : var_name -> t -> value_entry option

  val add_type : type_name -> type_entry -> t -> t

  val find_type : type_name -> t -> type_entry option

  val add_constructor : constructor_name -> constructor_entry -> t -> t

  val find_constructor : constructor_name -> t -> constructor_entry option

  val add_module : module_name -> module_entry -> t -> t

  val find_module : module_name -> t -> module_entry option

  val add_signature : signature_name -> signature abstracted -> t -> t

  val find_signature : signature_name -> t -> (signature abstracted) option

end

module StructSig : sig

  type t = struct_signature

  val empty : t

  val add_value : var_name -> value_entry -> t -> t

  val find_value : var_name -> t -> value_entry option

  val add_types : (type_name * type_entry) list -> t -> t

  val find_type : type_name -> t -> type_entry option

  val add_module : module_name -> module_entry -> t -> t

  val find_module : module_name -> t -> module_entry option

  val add_signature : signature_name -> signature abstracted -> t -> t

  val find_signature : signature_name -> t -> (signature abstracted) option

  val fold :
    v:(var_name -> value_entry -> 'a -> 'a) ->
    t:((type_name * type_entry) list -> 'a -> 'a) ->
    m:(module_name -> module_entry -> 'a -> 'a) ->
    s:(signature_name -> signature abstracted -> 'a -> 'a) ->
    'a -> t -> 'a

  val union : t -> t -> (t, string) result

end

(*
val add_mutual_cons : t -> level -> untyped_type_binding list -> t

val open_module : Range.t -> module_name -> t -> t

val find_candidates : t -> (module_name list) -> var_name -> Range.t -> var_name list

val find_constructor_candidates : pre -> t -> constructor_name -> constructor_name list

val enumerate_constructors : pre -> t -> TypeID.t -> (constructor_name * (mono_type list -> mono_type)) list
*)
