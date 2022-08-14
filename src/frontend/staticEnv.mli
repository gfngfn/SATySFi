open Types

type quantifier =
  kind OpaqueIDMap.t

type 'a abstracted =
  quantifier * 'a

type type_scheme =
  BoundID.t list * poly_type

type 'v struct_signature

type 'v signature =
  | ConcStructure of 'v struct_signature
  | ConcFunctor   of 'v functor_signature

and 'v functor_signature = {
  opaques  : quantifier;
  domain   : 'v signature;
  codomain : ('v signature) abstracted;
  closure  : (module_name ranged * untyped_module * 'v type_environment) option;
}

and 'v type_environment
[@@deriving show]

type 'v value_entry = {
  val_name  : 'v;
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

type 'v module_entry = {
  mod_signature : 'v signature;
}

type virtual_signature = unit signature

type virtual_type_environment = unit type_environment

type target_value_entry = EvalVarID.t value_entry

type target_module_entry = EvalVarID.t module_entry

type target_signature = EvalVarID.t signature

type target_type_environment = EvalVarID.t type_environment

type target_struct_signature = EvalVarID.t struct_signature

module Typeenv : sig

  type 'v t = 'v type_environment

  val empty : 'v t

  val add_macro : ctrlseq_name -> macro_entry -> 'v t -> 'v t

  val find_macro : ctrlseq_name -> 'v t -> macro_entry option

  val add_value : var_name -> 'v value_entry -> 'v t -> 'v t

  val find_value : var_name -> 'v t -> ('v value_entry) option

  val add_type : type_name -> type_entry -> 'v t -> 'v t

  val find_type : type_name -> 'v t -> type_entry option

  val add_constructor : constructor_name -> constructor_entry -> 'v t -> 'v t

  val find_constructor : constructor_name -> 'v t -> constructor_entry option

  val enumerate_constructors : TypeID.t -> 'v t -> (constructor_name * type_scheme) list

  val add_module : module_name -> 'v module_entry -> 'v t -> 'v t

  val find_module : module_name -> 'v t -> ('v module_entry) option

  val add_signature : signature_name -> virtual_signature abstracted -> 'v t -> 'v t

  val find_signature : signature_name -> 'v t -> (virtual_signature abstracted) option

end

module StructSig : sig

  type 'v t = 'v struct_signature

  val empty : 'v t

  val add_value : var_name -> 'v value_entry -> 'v t -> 'v t

  val find_value : var_name -> 'v t -> ('v value_entry) option

  val add_constructor : constructor_name -> constructor_entry -> 'v t -> 'v t

  val find_constructor : constructor_name -> 'v t -> constructor_entry option

  val add_dummy_fold : type_name -> poly_type -> 'v t -> 'v t

  val find_dummy_fold : type_name -> 'v t -> poly_type option

  val add_type : type_name -> type_entry -> 'v t -> 'v t

  val find_type : type_name -> 'v t -> type_entry option

  val add_module : module_name -> 'v module_entry -> 'v t -> 'v t

  val find_module : module_name -> 'v t -> ('v module_entry) option

  val add_signature : signature_name -> virtual_signature abstracted -> 'v t -> 'v t

  val find_signature : signature_name -> 'v t -> (virtual_signature abstracted) option

  val fold :
    v:(var_name -> 'v value_entry -> 'a -> 'a) ->
    c:(constructor_name -> constructor_entry -> 'a -> 'a) ->
    f:(type_name -> poly_type -> 'a -> 'a) ->
    t:(type_name -> type_entry -> 'a -> 'a) ->
    m:(module_name -> 'v module_entry -> 'a -> 'a) ->
    s:(signature_name -> virtual_signature abstracted -> 'a -> 'a) ->
    'a -> 'v t -> 'a

  val map_and_fold :
    v:(var_name -> 'v value_entry -> 'a -> 'v value_entry * 'a) ->
    c:(constructor_name -> constructor_entry -> 'a -> constructor_entry * 'a) ->
    f:(type_name -> poly_type -> 'a -> poly_type * 'a) ->
    t:(type_name -> type_entry -> 'a -> type_entry * 'a) ->
    m:(module_name -> 'v module_entry -> 'a -> 'v module_entry * 'a) ->
    s:(signature_name -> virtual_signature abstracted -> 'a -> virtual_signature abstracted * 'a) ->
    'a -> 'v t -> 'v t * 'a

  val map :
    v:(var_name -> 'v value_entry -> 'v value_entry) ->
    c:(constructor_name -> constructor_entry -> constructor_entry) ->
    f:(type_name -> poly_type -> poly_type) ->
    t:(type_name -> type_entry -> type_entry) ->
    m:(module_name -> 'v module_entry -> 'v module_entry) ->
    s:(signature_name -> virtual_signature abstracted -> virtual_signature abstracted) ->
    'v t -> 'v t

  val union : 'v t -> 'v t -> ('v t, string) result

end
