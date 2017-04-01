
exception ParseErrorDetail of string

type ctrlseq_name     = string
type var_name         = string
type id_name          = string
type class_name       = string
type type_name        = string
type constructor_name = string
type module_name      = string
type field_name       = string

module Typeid : sig
  type t
  val initialize : unit -> unit
  val fresh : unit -> t
  val to_string : t -> string
  val eq : t -> t -> bool
end = struct
  type t = int
  let current_id = ref 0
  let initialize () = ( current_id := 0 )
  let fresh () = ( incr current_id ; !current_id )
  let to_string = string_of_int
  let eq = (=)
end
(*
type scope = GlobalScope | LocalScope of module_name
*)
type manual_type = Range.t * manual_type_main
and manual_type_main =
  | MTypeName    of (manual_type list) * type_name
  | MTypeParam   of var_name
  | MFuncType    of manual_type * manual_type
  | MProductType of manual_type list
  | MRecordType  of (field_name, manual_type) Assoc.t

type mono_type = Range.t * mono_type_main
and mono_type_main =
  | UnitType
  | IntType
  | StringType
  | BoolType
  | FuncType     of mono_type * mono_type
  | ListType     of mono_type
  | RefType      of mono_type
  | ProductType  of mono_type list
  | TypeVariable of Tyvarid.t
  | TypeSynonym  of (mono_type list) * Typeid.t * poly_type
  | VariantType  of (mono_type list) * Typeid.t
  | TypeArgument of var_name
  | RecordType   of (field_name, mono_type) Assoc.t

and poly_type =
  | Mono   of mono_type
  | Forall of Tyvarid.t * kind * poly_type

and kind =
  | UniversalKind
  | RecordKind of (field_name, mono_type) Assoc.t

type id_name_arg =
  | IDName       of id_name
  | NoIDName

type class_name_arg =
  | ClassName    of class_name
  | NoClassName

(* ---- untyped ---- *)
type untyped_argument_variable_cons =
  | UTArgumentVariableCons of untyped_pattern_tree * untyped_argument_variable_cons
  | UTEndOfArgumentVariable
and untyped_argument_cons =
  | UTArgumentCons         of untyped_abstract_tree * untyped_argument_cons
  | UTEndOfArgument
and untyped_mutual_let_cons =
  | UTMutualLetCons        of manual_type option * var_name * untyped_abstract_tree * untyped_mutual_let_cons
  | UTEndOfMutualLet
and untyped_abstract_tree = Range.t * untyped_abstract_tree_main
and untyped_abstract_tree_main =
(* -- basic value -- *)
  | UTStringEmpty
  | UTNumericConstant      of int
  | UTBooleanConstant      of bool
  | UTStringConstant       of string
  | UTUnitConstant
  | UTConcat               of untyped_abstract_tree * untyped_abstract_tree
  | UTBreakAndIndent
(*  | UTNoContent *)
(* -- list value -- *)
  | UTListCons             of untyped_abstract_tree * untyped_abstract_tree
  | UTEndOfList
(* -- tuple value -- *)
  | UTTupleCons            of untyped_abstract_tree * untyped_abstract_tree
  | UTEndOfTuple
(* -- record value -- *)
  | UTRecord               of (field_name * untyped_abstract_tree) list
  | UTAccessField          of untyped_abstract_tree * field_name
(* -- fundamental -- *)
  | UTContentOf            of var_name
  | UTApply                of untyped_abstract_tree * untyped_abstract_tree
  | UTLetIn                of untyped_mutual_let_cons * untyped_abstract_tree
  | UTIfThenElse           of untyped_abstract_tree * untyped_abstract_tree * untyped_abstract_tree
  | UTLambdaAbstract       of Range.t * var_name * untyped_abstract_tree
  | UTFinishHeaderFile
(* -- pattern match -- *)
  | UTPatternMatch         of untyped_abstract_tree * untyped_pattern_match_cons
  | UTConstructor          of constructor_name * untyped_abstract_tree
(* -- declaration of type and module -- *)
  | UTDeclareVariantIn     of untyped_mutual_variant_cons * untyped_abstract_tree
  | UTModule               of module_name * untyped_module_tree * untyped_abstract_tree
(* -- implerative -- *)
  | UTLetMutableIn         of Range.t * var_name * untyped_abstract_tree * untyped_abstract_tree
  | UTSequential           of untyped_abstract_tree * untyped_abstract_tree
  | UTWhileDo              of untyped_abstract_tree * untyped_abstract_tree
  | UTDeclareGlobalHash    of untyped_abstract_tree * untyped_abstract_tree
  | UTOverwriteGlobalHash  of untyped_abstract_tree * untyped_abstract_tree
  | UTOverwrite            of Range.t * var_name * untyped_abstract_tree
  | UTReferenceFinal       of untyped_abstract_tree
  | UTLazyContent          of untyped_abstract_tree
(* -- lightweight itemize -- *)
  | UTItemize              of untyped_itemize
(* -- class and id option -- *)
  | UTApplyClassAndID      of untyped_abstract_tree * untyped_abstract_tree * untyped_abstract_tree
  | UTClassAndIDRegion     of untyped_abstract_tree

and untyped_itemize =
  | UTItem                 of untyped_abstract_tree * (untyped_itemize list)

and untyped_variant_cons = Range.t * untyped_variant_cons_main
and untyped_variant_cons_main =
  | UTVariantCons          of constructor_name * manual_type * untyped_variant_cons
  | UTEndOfVariant

and untyped_mutual_variant_cons =
  | UTMutualVariantCons    of untyped_type_argument_cons * type_name * untyped_variant_cons * untyped_mutual_variant_cons
  | UTMutualSynonymCons    of untyped_type_argument_cons * type_name * manual_type * untyped_mutual_variant_cons
  | UTEndOfMutualVariant

and untyped_pattern_tree = Range.t * untyped_pattern_tree_main
and untyped_pattern_tree_main =
  | UTPNumericConstant     of int
  | UTPBooleanConstant     of bool
  | UTPStringConstant      of untyped_abstract_tree
  | UTPUnitConstant
  | UTPListCons            of untyped_pattern_tree * untyped_pattern_tree
  | UTPEndOfList
  | UTPTupleCons           of untyped_pattern_tree * untyped_pattern_tree
  | UTPEndOfTuple
  | UTPWildCard
  | UTPVariable            of var_name
  | UTPAsVariable          of var_name * untyped_pattern_tree
  | UTPConstructor         of constructor_name * untyped_pattern_tree

and untyped_pattern_match_cons =
  | UTPatternMatchCons     of untyped_pattern_tree * untyped_abstract_tree * untyped_pattern_match_cons
  | UTPatternMatchConsWhen of untyped_pattern_tree * untyped_abstract_tree * untyped_abstract_tree * untyped_pattern_match_cons
  | UTEndOfPatternMatch

and untyped_let_pattern_cons =
  | UTLetPatternCons of untyped_argument_variable_cons * untyped_abstract_tree * untyped_let_pattern_cons
  | UTEndOfLetPattern

and untyped_module_tree = Range.t * untyped_module_tree_main
and untyped_module_tree_main =
  | UTMFinishModule
  | UTMPublicLetIn                 of untyped_mutual_let_cons * untyped_module_tree
  | UTMPublicLetMutableIn          of Range.t * var_name * untyped_abstract_tree * untyped_module_tree
  | UTMPublicDeclareVariantIn      of untyped_mutual_variant_cons * untyped_module_tree
  | UTMPrivateLetIn                of untyped_mutual_let_cons * untyped_module_tree
  | UTMPrivateLetMutableIn         of Range.t * var_name * untyped_abstract_tree * untyped_module_tree
  | UTMPrivateDeclareVariantIn     of untyped_mutual_variant_cons * untyped_module_tree
  | UTMDirectLetIn                 of untyped_mutual_let_cons * untyped_module_tree

and untyped_type_argument_cons =
  | UTTypeArgumentCons  of Range.t * var_name * untyped_type_argument_cons
  | UTEndOfTypeArgument


(* ---- typed ---- *)
type argument_variable_cons =
  | ArgumentVariableCons  of var_name * argument_variable_cons
  | EndOfArgumentVariable

type argument_cons =
  | ArgumentCons          of abstract_tree * argument_cons
  | EndOfArgument

and mutual_let_cons =
  | MutualLetCons         of var_name * abstract_tree * mutual_let_cons
  | EndOfMutualLet

and environment = (var_name, location) Hashtbl.t

and location = abstract_tree ref

and abstract_tree =
(* -- basic value -- *)
  | StringEmpty
  | NumericConstant       of int
  | BooleanConstant       of bool
  | StringConstant        of string
  | UnitConstant
  | DeeperIndent          of abstract_tree
  | BreakAndIndent
  | SoftBreakAndIndent
  | Concat                of abstract_tree * abstract_tree
(*  | NoContent (* for class and id *) *)
  | FuncWithEnvironment   of var_name * abstract_tree * environment
  | EvaluatedEnvironment  of environment
(* -- list value -- *)
  | ListCons              of abstract_tree * abstract_tree
  | EndOfList
(* -- tuple value -- *)
  | TupleCons             of abstract_tree * abstract_tree
  | EndOfTuple
(* -- record value -- *)
  | Record                of (field_name, abstract_tree) Assoc.t
  | AccessField           of abstract_tree * field_name
(* -- fundamental -- *)
  | LetIn                 of mutual_let_cons * abstract_tree
  | ContentOf             of var_name
  | IfThenElse            of abstract_tree * abstract_tree * abstract_tree
  | LambdaAbstract        of var_name * abstract_tree
  | Apply                 of abstract_tree * abstract_tree
  | FinishHeaderFile
(* -- pattern match -- *)
  | PatternMatch          of abstract_tree * pattern_match_cons
  | Constructor           of constructor_name * abstract_tree
(* -- imperative -- *)
  | LetMutableIn          of var_name * abstract_tree * abstract_tree
  | Sequential            of abstract_tree * abstract_tree
  | WhileDo               of abstract_tree * abstract_tree
  | Overwrite             of var_name * abstract_tree
  | Location              of abstract_tree ref
  | Reference             of abstract_tree
  | DeclareGlobalHash     of abstract_tree * abstract_tree
  | OverwriteGlobalHash   of abstract_tree * abstract_tree
  | ReferenceFinal        of abstract_tree
  | LazyContent           of abstract_tree
  | LazyContentWithEnvironmentRef of abstract_tree * (environment ref)
(* -- class and id option -- *)
  | ApplyClassAndID       of abstract_tree * abstract_tree * abstract_tree
(* (* -- lightweight itemize -- *)
  | Itemize               of itemize *)
(* -- primitive operation -- *)
  | Times                 of abstract_tree * abstract_tree
  | Divides               of abstract_tree * abstract_tree
  | Mod                   of abstract_tree * abstract_tree
  | Plus                  of abstract_tree * abstract_tree
  | Minus                 of abstract_tree * abstract_tree
  | GreaterThan           of abstract_tree * abstract_tree
  | LessThan              of abstract_tree * abstract_tree
  | EqualTo               of abstract_tree * abstract_tree
  | LogicalAnd            of abstract_tree * abstract_tree
  | LogicalOr             of abstract_tree * abstract_tree
  | LogicalNot            of abstract_tree
  | PrimitiveSame         of abstract_tree * abstract_tree
  | PrimitiveStringSub    of abstract_tree * abstract_tree * abstract_tree
  | PrimitiveStringLength of abstract_tree
(*  | PrimitiveInclude      of abstract_tree *)
  | PrimitiveArabic       of abstract_tree
  | Module                of module_name * module_tree * abstract_tree
(* and itemize =
  | Item                  of abstract_tree * (itemize list) *)
and pattern_match_cons =
  | PatternMatchCons      of pattern_tree * abstract_tree * pattern_match_cons
  | PatternMatchConsWhen  of pattern_tree * abstract_tree * abstract_tree * pattern_match_cons
  | EndOfPatternMatch
and pattern_tree =
  | PNumericConstant      of int
  | PBooleanConstant      of bool
  | PStringConstant       of abstract_tree
  | PUnitConstant
  | PListCons             of pattern_tree * pattern_tree
  | PEndOfList
  | PTupleCons            of pattern_tree * pattern_tree
  | PEndOfTuple
  | PWildCard
  | PVariable             of var_name
  | PAsVariable           of var_name * pattern_tree
  | PConstructor          of constructor_name * pattern_tree
and module_tree =
  | MFinishModule
  | MPublicLetIn                 of mutual_let_cons * module_tree
  | MPublicLetMutableIn          of var_name * abstract_tree * module_tree
(*  | MPublicDeclareVariantIn      of mutual_variant_cons * module_tree *)
  | MPrivateLetIn                of mutual_let_cons * module_tree
  | MPrivateLetMutableIn         of var_name * abstract_tree * module_tree
(*  | MPrivateDeclareVariantIn     of mutual_variant_cons * module_tree *)
  | MDirectLetIn                 of mutual_let_cons * module_tree

type output_unit =
  | OString             of string
  | OBreakAndIndent
  | OSoftBreakAndIndent
  | ODeepen
  | OShallow


let poly_extend_general
    (fmono : mono_type -> 'a) (fpoly : (poly_type -> 'a) -> Tyvarid.t -> kind -> poly_type -> 'a) : (poly_type -> 'a) =
  let rec iter pty =
    match pty with
    | Mono(ty)                 -> fmono ty
    | Forall(tvid, kd, ptysub) -> fpoly iter tvid kd ptysub
  in
    iter


let poly_extend (fmono : mono_type -> mono_type) : (poly_type -> poly_type) =
  let rec iter pty =
    match pty with
    | Mono(ty)                 -> Mono(fmono ty)
    | Forall(tvid, kd, ptysub) -> Forall(tvid, kd, iter ptysub)
  in
    iter


let rec replace_type_variable ((rng, tymain) : mono_type) (key : Tyvarid.t) (value : mono_type) =
  let iter = (fun ty -> replace_type_variable ty key value) in
    match tymain with
    | TypeVariable(k)                       -> if Tyvarid.same k key then value else (rng, TypeVariable(k))
    | FuncType(tydom, tycod)                -> (rng, FuncType(iter tydom, iter tycod))
    | ProductType(tylst)                    -> (rng, ProductType(List.map iter tylst))
    | ListType(tycont)                      -> (rng, ListType(iter tycont))
    | RefType(tycont)                       -> (rng, RefType(iter tycont))
    | VariantType(tyarglist, varntnm)       -> (rng, VariantType(List.map iter tyarglist, varntnm))
    | TypeSynonym(tyarglist, tysynnm, pty)  -> (rng, TypeSynonym(List.map iter tyarglist, tysynnm,
                                                                 poly_extend_general
                                                                   (fun ty -> Mono(iter ty))
                                                                   (fun it tvid kd ptysub ->
                                                                     if Tyvarid.same tvid key then Forall(tvid, kd, ptysub)
                                                                                              else Forall(tvid, kd, it ptysub)) pty))
    | RecordType(asc)                       -> (rng, RecordType(Assoc.map_value iter asc))
    | _                                     -> (rng, tymain)


let get_range (rng, _) = rng


let overwrite_range_of_type ((_, tymain) : mono_type) (rng : Range.t) = (rng, tymain)


let rec erase_range_of_type ((_, tymain) : mono_type) =
  let iter = erase_range_of_type in
  let newtymain =
    match tymain with
    | FuncType(tydom, tycod)            -> FuncType(iter tydom, iter tycod)
    | ProductType(tylist)               -> ProductType(List.map iter tylist)
    | VariantType(tylist, tynm)         -> VariantType(List.map iter tylist, tynm)
    | ListType(tycont)                  -> ListType(iter tycont)
    | RefType(tycont)                   -> RefType(iter tycont)
    | TypeSynonym(tylist, tynm, pty)    -> TypeSynonym(List.map iter tylist, tynm, poly_extend erase_range_of_type pty)
    | _                                 -> tymain
  in
    (Range.dummy "erased", newtymain)


and erase_range_of_kind (kd : kind) =
  match kd with
  | UniversalKind   -> UniversalKind
  | RecordKind(asc) -> RecordKind(Assoc.map_value erase_range_of_type asc)


(* !!!! ---- global variable ---- !!!! *)

let global_hash_env : environment = Hashtbl.create 32

(*
let print_for_debug msg =
(* enable below to see the process of type inference *)
(*
  print_string msg ;
*)
  ()
*)
