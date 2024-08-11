open Types

type quantifier =
  kind OpaqueIDMap.t

type 'a abstracted =
  quantifier * 'a

type type_scheme =
  BoundID.t list * poly_type

type struct_signature

type type_environment

type signature =
  | ConcStructure of struct_signature
  | ConcFunctor   of functor_signature

and functor_signature = {
  opaques  : quantifier;
  domain   : signature;
  codomain : signature abstracted;
  closure  : (module_name ranged * untyped_module * type_environment) option;
}
[@@deriving show]

type value_entry = {
  val_name  : EvalVarID.t option;
  val_type  : poly_type;
  val_stage : stage;
}

type type_entry = {
  type_scheme : type_scheme;
  type_kind   : kind;
}
[@@deriving show]

type constructor_entry = {
  ctor_belongs_to : TypeID.t;
  ctor_parameter  : type_scheme;
}

type macro_entry = {
  macro_type : poly_macro_type;
  macro_name : EvalVarID.t option;
}

type module_entry = {
  mod_signature : signature;
}


module Typeenv : sig

  type t = type_environment

  val empty : t

  val add_macro : command_name -> macro_entry -> t -> t

  val find_macro : command_name -> t -> macro_entry option

  val add_value : var_name -> value_entry -> t -> t

  val find_value : var_name -> t -> value_entry option

  val add_type : type_name -> type_entry -> t -> t

  val find_type : type_name -> t -> type_entry option

  val add_constructor : constructor_name -> constructor_entry -> t -> t

  val find_constructor : constructor_name -> t -> constructor_entry option

  val enumerate_constructors : TypeID.t -> t -> (constructor_name * type_scheme) list

  val add_module : module_name -> module_entry -> t -> t

  val find_module : module_name -> t -> module_entry option

  val add_signature : signature_name -> signature abstracted -> t -> t

  val find_signature : signature_name -> t -> (signature abstracted) option

  val fold_value : (var_name -> value_entry -> 'a -> 'a) -> t -> 'a -> 'a

end

module StructSig : sig

  type t = struct_signature

  val empty : t

  val add_macro : macro_name -> macro_entry -> t -> t

  val find_macro : macro_name -> t -> macro_entry option

  val add_value : var_name -> value_entry -> t -> t

  val find_value : var_name -> t -> value_entry option

  val add_constructor : constructor_name -> constructor_entry -> t -> t

  val find_constructor : constructor_name -> t -> constructor_entry option

  val add_dummy_fold : type_name -> poly_type -> t -> t

  val find_dummy_fold : type_name -> t -> poly_type option

  val add_type : type_name -> type_entry -> t -> t

  val find_type : type_name -> t -> type_entry option

  val add_module : module_name -> module_entry -> t -> t

  val find_module : module_name -> t -> module_entry option

  val add_signature : signature_name -> signature abstracted -> t -> t

  val find_signature : signature_name -> t -> (signature abstracted) option

  val fold :
    v:(var_name -> value_entry -> 'a -> 'a) ->
    a:(macro_name -> macro_entry -> 'a -> 'a) ->
    c:(constructor_name -> constructor_entry -> 'a -> 'a) ->
    f:(type_name -> poly_type -> 'a -> 'a) ->
    t:(type_name -> type_entry -> 'a -> 'a) ->
    m:(module_name -> module_entry -> 'a -> 'a) ->
    s:(signature_name -> signature abstracted -> 'a -> 'a) ->
    'a -> t -> 'a

  val map_and_fold :
    v:(var_name -> value_entry -> 'a -> value_entry * 'a) ->
    a:(macro_name -> macro_entry -> 'a -> macro_entry * 'a) ->
    c:(constructor_name -> constructor_entry -> 'a -> constructor_entry * 'a) ->
    f:(type_name -> poly_type -> 'a -> poly_type * 'a) ->
    t:(type_name -> type_entry -> 'a -> type_entry * 'a) ->
    m:(module_name -> module_entry -> 'a -> module_entry * 'a) ->
    s:(signature_name -> signature abstracted -> 'a -> signature abstracted * 'a) ->
    'a -> t -> t * 'a

  val map :
    v:(var_name -> value_entry -> value_entry) ->
    a:(macro_name -> macro_entry -> macro_entry) ->
    c:(constructor_name -> constructor_entry -> constructor_entry) ->
    f:(type_name -> poly_type -> poly_type) ->
    t:(type_name -> type_entry -> type_entry) ->
    m:(module_name -> module_entry -> module_entry) ->
    s:(signature_name -> signature abstracted -> signature abstracted) ->
    t -> t

  val union : t -> t -> (t, string) result

end

val find_candidates_in_type_environment : Typeenv.t -> var_name -> var_name list

val find_candidates_in_struct_sig : StructSig.t -> var_name -> var_name list

type global_type_environment = StructSig.t GlobalTypeenv.t
