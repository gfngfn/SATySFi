open Types

type 'a abstracted =
  OpaqueIDSet.t * 'a

type type_scheme =
  BoundID.t list * poly_type

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
  val_name  : EvalVarID.t option;
  val_type  : poly_type;
  val_stage : stage;
}

type type_entry = {
  type_scheme : type_scheme;
  type_kind   : kind;
}

type constructor_entry = {
  ctor_belongs_to : TypeID.t;
  ctor_parameter  : type_scheme;
}

type macro_entry = {
  macro_type : macro_type;
  macro_name : EvalVarID.t;
}

type module_entry = {
  mod_name      : ModuleID.t option;
  mod_signature : signature;
}


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

  val add_type : type_name -> type_entry -> t -> t

  val find_type : type_name -> t -> type_entry option

  val add_module : module_name -> module_entry -> t -> t

  val find_module : module_name -> t -> module_entry option

  val add_signature : signature_name -> signature abstracted -> t -> t

  val find_signature : signature_name -> t -> (signature abstracted) option

  val fold :
    v:(var_name -> value_entry -> 'a -> 'a) ->
    t:(type_name -> type_entry -> 'a -> 'a) ->
    m:(module_name -> module_entry -> 'a -> 'a) ->
    s:(signature_name -> signature abstracted -> 'a -> 'a) ->
    'a -> t -> 'a

  val map_and_fold :
    v:(var_name -> value_entry -> 'a -> value_entry * 'a) ->
    t:(type_name -> type_entry -> 'a -> type_entry * 'a) ->
    m:(module_name -> module_entry -> 'a -> module_entry * 'a) ->
    s:(signature_name -> signature abstracted -> 'a -> signature abstracted * 'a) ->
    'a -> t -> t * 'a

  val map :
    v:(var_name -> value_entry -> value_entry) ->
    t:(type_name -> type_entry -> type_entry) ->
    m:(module_name -> module_entry -> module_entry) ->
    s:(signature_name -> signature abstracted -> signature abstracted) ->
    t -> t

  val union : t -> t -> (t, string) result

end
