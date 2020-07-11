open Types

type type_scheme = BoundID.t list * poly_type

type type_definition =
  | Data  of int
  | Alias of type_scheme

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

  val add_macro : ctrlseq_name -> (macro_type * EvalVarID.t) -> t -> t

  val find_macro : ctrlseq_name -> t -> (macro_type * EvalVarID.t) option

  val add_value : var_name -> (poly_type * EvalVarID.t * stage) -> t -> t

  val find_value : var_name -> t -> (poly_type * EvalVarID.t * stage) option

  val add_type : type_name -> TypeID.t * int -> t -> t

  val find_type : type_name -> t -> (TypeID.t * int) option

  val add_constructor : constructor_name -> TypeID.Variant.t * type_scheme -> t -> t

  val find_constructor : constructor_name -> t -> (TypeID.Variant.t * type_scheme) option

end

module StructSig : sig

  type t

  val empty : t

  val add_value : var_name -> (poly_type * EvalVarID.t * stage) -> t -> t

  val find_value : var_name -> t -> (poly_type * EvalVarID.t * stage) option

  val fold :
    v:(var_name -> (poly_type * EvalVarID.t * stage) -> 'a -> 'a) ->
    'a -> t -> 'a

  val union : t -> t -> t

end

(*
val add_mutual_cons : t -> level -> untyped_type_binding list -> t

val open_module : Range.t -> module_name -> t -> t

val find_candidates : t -> (module_name list) -> var_name -> Range.t -> var_name list

val find_constructor_candidates : pre -> t -> constructor_name -> constructor_name list

val enumerate_constructors : pre -> t -> TypeID.t -> (constructor_name * (mono_type list -> mono_type)) list
*)
