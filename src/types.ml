
exception ParseErrorDetail of string

type ctrlseq_name       = string
type var_name           = string
type id_name            = string
type class_name         = string
type type_name          = string
type constructor_name   = string
type module_name        = string
type sig_var_name       = string
type field_name         = string
type type_argument_name = string


module TypeID : sig
  type t
  val initialize : unit -> unit
  val fresh : type_name -> t
  val extract_name : t -> type_name
  val equal : t -> t -> bool
  val show_direct : t -> string
end = struct
  type t = int * type_name
  let current_id = ref 0
  let initialize () = ( current_id := 0 )
  let fresh tynm = begin incr current_id ; (!current_id, tynm) end
  let extract_name (_, tynm) = tynm
  let equal (n1, _) (n2, _) = (n1 = n2)
  let show_direct (n, tynm) = (string_of_int n) ^ "(" ^ tynm ^ ")"
end


module EvalVarID
: sig
    type t
    val initialize : unit -> unit
    val fresh : var_name -> t
    val equal : t -> t -> bool
    val for_class_name : t
    val for_id_name : t
    val show_direct : t -> string
  end
= struct
    type t = int * string
    let current_id = ref 0
    let initialize () = ( current_id := 0 )
    let fresh varnm = begin incr current_id ; (!current_id, varnm) end
    let equal (i1, _) (i2, _) = (i1 = i2)
    let for_class_name = (-1, "*class-name*")
    let for_id_name = (-2, "*id-name*")
    let show_direct (i, varnm) = "<" ^ (string_of_int i) ^ "|" ^ varnm ^ ">"
  end


type quantifiability = Quantifiable | Unquantifiable

module FreeID_
: sig
    type level
    type 'a t_
    val bottom_level : level
    val succ_level : level -> level
    val less_than : level -> level -> bool
    val get_level : 'a t_ -> level
    val set_level : 'a t_ -> level -> 'a t_
    val initialize : unit -> unit
    val fresh : 'a -> quantifiability -> level -> unit -> 'a t_
    val equal : 'a t_ -> 'a t_ -> bool
    val is_quantifiable : 'a t_ -> bool
    val set_quantifiability : quantifiability -> 'a t_ -> 'a t_
    val get_kind : 'a t_ -> 'a
    val set_kind : 'a t_ -> 'a -> 'a t_
    val show_direct : ('a -> string) -> 'a t_ -> string
    val show_direct_level : level -> string
  end
= struct
    type level = int
    type 'a t_ = int * 'a * quantifiability * level

    let bottom_level = 0

    let succ_level lev = lev + 1

    let less_than = (<)

    let get_level (_, _, _, lev) = lev

    let set_level (idmain, kd, qtfbl, _) lev = (idmain, kd, qtfbl, lev)

    let current_id = ref 0

    let initialize () = ( current_id := 0 )

    let fresh kd qtfbl lev () =
      begin
        incr current_id ;
        (!current_id, kd, qtfbl, lev)
      end

    let equal (i1, _, _, _) (i2, _, _, _) = (i1 = i2)

    let is_quantifiable (_, _, qtfbl, _) =
        match qtfbl with
        | Quantifiable   -> true
        | Unquantifiable -> false

    let set_quantifiability qtfbl (idmain, kd, _, lev) = (idmain, kd, qtfbl, lev)

    let get_kind (_, kd, _, _) = kd

    let set_kind (idmain, _, qtfbl, lev) kd = (idmain, kd, qtfbl, lev)

    let show_direct f (idmain, kd, qtfbl, lev) =
      match qtfbl with
      | Quantifiable   -> (string_of_int idmain) ^ "[Q" ^ (string_of_int lev) ^ "::" ^ (f kd) ^ "]"
      | Unquantifiable -> (string_of_int idmain) ^ "[U" ^ (string_of_int lev) ^ "::" ^ (f kd) ^ "]"

    let show_direct_level = string_of_int

  end


module BoundID_
: sig
    type 'a t_
    val initialize : unit -> unit
    val fresh : 'a -> unit -> 'a t_
    val eq : 'a t_ -> 'a t_ -> bool
    val get_kind : 'a t_ -> 'a
    val show_direct : ('a -> string) -> 'a t_ -> string
  end
= struct
    type 'a t_ = int * 'a

    let current_id = ref 0

    let initialize () = ( current_id := 0 )

    let fresh kd () =
      begin
        incr current_id ;
        (!current_id, kd)
      end

    let eq (i1, _) (i2, _) = (i1 = i2)

    let get_kind (_, kd) = kd

    let show_direct f (i, kd) = "[" ^ (string_of_int i) ^ "::" ^ (f kd) ^ "]"

  end



type manual_type = Range.t * manual_type_main
and manual_type_main =
  | MTypeName    of (manual_type list) * type_name
  | MTypeParam   of var_name
  | MFuncType    of manual_type * manual_type
  | MProductType of manual_type list
  | MRecordType  of (field_name, manual_type) Assoc.t

type manual_kind =
  | MUniversalKind
  | MRecordKind    of (field_name, manual_type) Assoc.t

type base_type =
  | UnitType
  | IntType
  | StringType
  | BoolType
  | TextRowType
  | TextColType
  | BoxRowType
  | BoxColType
  | FontType
  | ContextType

type mono_type = Range.t * mono_type_main
and mono_type_main =
  | BaseType        of base_type
  | FuncType        of mono_type * mono_type
  | ListType        of mono_type
  | RefType         of mono_type
  | ProductType     of mono_type list
  | TypeVariable    of type_variable_info ref
  | SynonymType     of (mono_type list) * TypeID.t * mono_type
  | VariantType     of (mono_type list) * TypeID.t
  | RecordType      of (field_name, mono_type) Assoc.t
  | HorzCommandType of mono_type list
  | VertCommandType of mono_type list

and poly_type =
  | Poly of mono_type

and kind =
  | UniversalKind
  | RecordKind of (field_name, mono_type) Assoc.t

and type_variable_info =
  | Free  of kind FreeID_.t_
  | Bound of kind BoundID_.t_
  | Link  of mono_type


module FreeID =
  struct
    include FreeID_
    type t = kind FreeID_.t_
  end


module BoundID =
  struct
    include BoundID_
    type t = kind BoundID_.t_
  end


type id_name_arg =
  | IDName   of id_name
  | NoIDName

type class_name_arg =
  | ClassName   of class_name
  | NoClassName

(* ---- untyped ---- *)
type untyped_argument_variable_cons = untyped_pattern_tree list

and untyped_argument_cons = untyped_abstract_tree list

and untyped_mutual_let_cons = (manual_type option * var_name * untyped_abstract_tree) list

and untyped_input_horz_element = Range.t * untyped_input_horz_element_main
and untyped_input_horz_element_main =
  | UTInputHorzText     of string
  | UTInputHorzEmbedded of untyped_abstract_tree * untyped_abstract_tree list

and untyped_input_vert_element = Range.t * untyped_input_vert_element_main
and untyped_input_vert_element_main =
  | UTInputVertEmbedded of untyped_abstract_tree * untyped_abstract_tree list

and untyped_abstract_tree = Range.t * untyped_abstract_tree_main
and untyped_abstract_tree_main =
(* -- basic value -- *)
  | UTStringEmpty
  | UTNumericConstant      of int
  | UTBooleanConstant      of bool
  | UTStringConstant       of string
  | UTUnitConstant
  | UTBreakAndIndent
(* -- inputs -- *)
  | UTInputHorz            of untyped_input_horz_element list
  | UTInputVert            of untyped_input_vert_element list
  | UTConcat               of untyped_abstract_tree * untyped_abstract_tree
  | UTLambdaHorz           of Range.t * var_name * untyped_abstract_tree
  | UTLambdaVert           of Range.t * var_name * untyped_abstract_tree
(* -- horizontal box list -- *)
  | UTHorz                 of HorzBox.horz_box list
  | UTHorzConcat           of untyped_abstract_tree * untyped_abstract_tree
(* -- vertical box list -- *)
  | UTVert                 of HorzBox.intermediate_vert_box list
  | UTVertConcat           of untyped_abstract_tree * untyped_abstract_tree
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
  | UTContentOf            of (module_name list) * var_name
  | UTApply                of untyped_abstract_tree * untyped_abstract_tree
  | UTLetIn                of untyped_mutual_let_cons * untyped_abstract_tree
  | UTIfThenElse           of untyped_abstract_tree * untyped_abstract_tree * untyped_abstract_tree
  | UTLambdaAbstract       of Range.t * var_name * untyped_abstract_tree
  | UTFinishHeaderFile
  | UTFinishStruct
(* -- pattern match -- *)
  | UTPatternMatch         of untyped_abstract_tree * untyped_pattern_match_cons
  | UTConstructor          of constructor_name * untyped_abstract_tree
(* -- declaration of type and module -- *)
  | UTDeclareVariantIn     of untyped_mutual_variant_cons * untyped_abstract_tree
  | UTModule               of Range.t * module_name * manual_signature option * untyped_abstract_tree * untyped_abstract_tree
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

and constraint_cons = (var_name * manual_kind) list

and manual_signature_content =
  | SigType   of untyped_type_argument_cons * type_name
  | SigValue  of var_name * manual_type * constraint_cons
  | SigDirect of var_name * manual_type * constraint_cons
(*
  | SigModule of module_name * manual_signature
*)

and manual_signature = manual_signature_content list

and untyped_itemize =
  | UTItem                 of untyped_abstract_tree * (untyped_itemize list)

and untyped_variant_cons = Range.t * untyped_variant_cons_main
and untyped_variant_cons_main =
  | UTVariantCons          of constructor_name * manual_type * untyped_variant_cons
  | UTEndOfVariant

and untyped_mutual_variant_cons =
  | UTMutualVariantCons    of untyped_type_argument_cons * Range.t * type_name * untyped_variant_cons * untyped_mutual_variant_cons
  | UTMutualSynonymCons    of untyped_type_argument_cons * Range.t * type_name * manual_type * untyped_mutual_variant_cons
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

and untyped_unkinded_type_argument_cons = (Range.t * var_name) list

and untyped_type_argument_cons = (Range.t * var_name * manual_kind) list

(* ---- typed ---- *)
type argument_variable_cons =
  | ArgumentVariableCons  of var_name * argument_variable_cons
  | EndOfArgumentVariable

type argument_cons =
  | ArgumentCons          of abstract_tree * argument_cons
  | EndOfArgument

and mutual_let_cons =
  | MutualLetCons         of EvalVarID.t * abstract_tree * mutual_let_cons
  | EndOfMutualLet

and environment = (EvalVarID.t, location) Hashtbl.t

and location = abstract_tree ref

and input_horz_element =
  | InputHorzText     of string
  | InputHorzEmbedded of abstract_tree * abstract_tree list

and input_vert_element =
  | InputVertEmbedded of abstract_tree * abstract_tree list

and input_context = {
  font_info     : HorzBox.font_info;
  space_natural : HorzBox.Length.t;
  space_shrink  : HorzBox.Length.t;
  space_stretch : HorzBox.Length.t;
}
(* temporary *)

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
  | FuncWithEnvironment   of EvalVarID.t * abstract_tree * environment
  | EvaluatedEnvironment  of environment
(* -- input texts -- *)
  | InputHorz             of input_horz_element list
  | InputVert             of input_vert_element list
  | InputHorzWithEnvironment of input_horz_element list * environment
  | InputVertWithEnvironment of input_vert_element list * environment
(* -- horizontal box list -- *)
  | Horz                  of HorzBox.horz_box list
  | HorzConcat            of abstract_tree * abstract_tree
(* -- vertical box list -- *)
  | Vert                  of HorzBox.intermediate_vert_box list
  | VertConcat            of abstract_tree * abstract_tree
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
  | ContentOf             of EvalVarID.t
  | IfThenElse            of abstract_tree * abstract_tree * abstract_tree
  | LambdaAbstract        of EvalVarID.t * abstract_tree
  | Apply                 of abstract_tree * abstract_tree
  | FinishHeaderFile
  | FinishStruct
(* -- pattern match -- *)
  | PatternMatch          of abstract_tree * pattern_match_cons
  | Constructor           of constructor_name * abstract_tree
(* -- imperative -- *)
  | LetMutableIn          of EvalVarID.t * abstract_tree * abstract_tree
  | Sequential            of abstract_tree * abstract_tree
  | WhileDo               of abstract_tree * abstract_tree
  | Overwrite             of EvalVarID.t * abstract_tree
  | Location              of abstract_tree ref
  | Reference             of abstract_tree
  | DeclareGlobalHash     of abstract_tree * abstract_tree
  | OverwriteGlobalHash   of abstract_tree * abstract_tree
  | ReferenceFinal        of abstract_tree
  | LazyContent           of abstract_tree
  | LazyContentWithEnvironmentRef of abstract_tree * (environment ref)
(* -- class and id option -- *)
  | ApplyClassAndID       of EvalVarID.t * EvalVarID.t * abstract_tree * abstract_tree * abstract_tree
(* -- module system -- *)
  | Module                of abstract_tree * abstract_tree
(* -- basic primitive operations -- *)
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
  | PrimitiveArabic       of abstract_tree
(* -- backend primitives -- *)
  | LambdaHorz                of EvalVarID.t * abstract_tree
  | LambdaHorzWithEnvironment of EvalVarID.t * abstract_tree * environment
  | LambdaVert                of EvalVarID.t * abstract_tree
  | LambdaVertWithEnvironment of EvalVarID.t * abstract_tree * environment
  | FontDesignation       of HorzBox.font_info
  | Context               of input_context
  | HorzLex               of abstract_tree * abstract_tree
  | VertLex               of abstract_tree * abstract_tree
  | BackendFont           of abstract_tree * abstract_tree
  | BackendSetFont        of abstract_tree * abstract_tree
  | BackendLineBreaking   of abstract_tree
  | BackendFixedString    of abstract_tree * abstract_tree
  | BackendFixedEmpty     of abstract_tree
  | BackendOuterEmpty     of abstract_tree * abstract_tree * abstract_tree

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
  | PVariable             of EvalVarID.t
  | PAsVariable           of EvalVarID.t * pattern_tree
  | PConstructor          of constructor_name * pattern_tree

type output_unit =
  | OString             of string
  | OBreakAndIndent
  | OSoftBreakAndIndent
  | ODeepen
  | OShallow


let poly_extend (fmono : mono_type -> mono_type) : (poly_type -> poly_type) =
  (fun (Poly(ty)) -> Poly(fmono ty))


let get_range (rng, _) = rng


let overwrite_range_of_type ((_, tymain) : mono_type) (rng : Range.t) = (rng, tymain)


let rec erase_range_of_type ((_, tymain) : mono_type) =
  let iter = erase_range_of_type in
  let newtymain =
    match tymain with
    | FuncType(tydom, tycod)            -> FuncType(iter tydom, iter tycod)
    | ProductType(tylist)               -> ProductType(List.map iter tylist)
    | SynonymType(tylist, tyid, tyreal) -> SynonymType(List.map iter tylist, tyid, iter tyreal)
    | VariantType(tylist, tyid)         -> VariantType(List.map iter tylist, tyid)
    | ListType(tycont)                  -> ListType(iter tycont)
    | RefType(tycont)                   -> RefType(iter tycont)
    | _                                 -> tymain
  in
    (Range.dummy "erased", newtymain)


and erase_range_of_kind (kd : kind) =
  match kd with
  | UniversalKind   -> UniversalKind
  | RecordKind(asc) -> RecordKind(Assoc.map_value erase_range_of_type asc)


module BoundIDHashtbl = Hashtbl.Make(
  struct
    type t = BoundID.t
    let equal = BoundID.eq
    let hash = Hashtbl.hash
  end)


let instantiate (lev : FreeID.level) (qtfbl : quantifiability) ((Poly(ty)) : poly_type) =
  let current_ht : (type_variable_info ref) BoundIDHashtbl.t = BoundIDHashtbl.create 32 in
  let rec aux ((rng, tymain) as ty) =
    match tymain with
    | TypeVariable(tvref) ->
        begin
          match !tvref with
          | Link(tyl)  -> aux tyl
          | Free(tvid) -> ty
          | Bound(bid) ->
              begin
                try
                  let tvrefnew = BoundIDHashtbl.find current_ht bid in
                    (rng, TypeVariable(tvrefnew))
                with
                | Not_found ->
                    let kd = BoundID.get_kind bid in
                    let kdfree = instantiate_kind kd in
                    let tvid = FreeID.fresh kdfree qtfbl lev () in
                    let tvrefnew = ref (Free(tvid)) in
                    begin
                      BoundIDHashtbl.add current_ht bid tvrefnew ;
                      (rng, TypeVariable(tvrefnew))
                    end
              end
        end
    | FuncType(tydom, tycod)            -> (rng, FuncType(aux tydom, aux tycod))
    | ProductType(tylist)               -> (rng, ProductType(List.map aux tylist))
    | RecordType(tyasc)                 -> (rng, RecordType(Assoc.map_value aux tyasc))
    | SynonymType(tylist, tyid, tyreal) -> (rng, SynonymType(List.map aux tylist, tyid, tyreal))
    | VariantType(tylist, tyid)         -> (rng, VariantType(List.map aux tylist, tyid))
    | ListType(tysub)                   -> (rng, ListType(aux tysub))
    | RefType(tysub)                    -> (rng, RefType(aux tysub))
    | BaseType(_)                       -> ty
    | HorzCommandType(tylist)           -> (rng, HorzCommandType(List.map aux tylist))
    | VertCommandType(tylist)           -> (rng, VertCommandType(List.map aux tylist))

  and instantiate_kind kd =
    match kd with
    | UniversalKind     -> UniversalKind
    | RecordKind(tyasc) -> RecordKind(Assoc.map_value aux tyasc)
  in
    aux ty


let generalize (lev : FreeID.level) (ty : mono_type) =
  let rec iter ((rng, tymain) as ty) =
    match tymain with
    | TypeVariable(tvref) ->
        begin
          match !tvref with
          | Link(tyl)  -> iter tyl
          | Bound(_)   -> ty
          | Free(tvid) ->
              if not (FreeID.is_quantifiable tvid) then
                ty
              else
                if not (FreeID.less_than lev (FreeID.get_level tvid)) then
                  ty
                else
                  let kd = FreeID.get_kind tvid in
                  let kdgen = generalize_kind kd in
                  let bid = BoundID.fresh kdgen () in
                  begin
                    tvref := Bound(bid);
                    ty
                  end
        end
    | FuncType(tydom, tycod)            -> (rng, FuncType(iter tydom, iter tycod))
    | ProductType(tylist)               -> (rng, ProductType(List.map iter tylist))
    | RecordType(tyasc)                 -> (rng, RecordType(Assoc.map_value iter tyasc))
    | SynonymType(tylist, tyid, tyreal) -> (rng, SynonymType(List.map iter tylist, tyid, iter tyreal))
    | VariantType(tylist, tyid)         -> (rng, VariantType(List.map iter tylist, tyid))
    | ListType(tysub)                   -> (rng, ListType(iter tysub))
    | RefType(tysub)                    -> (rng, RefType(iter tysub))
    | BaseType(_)                       -> ty
    | HorzCommandType(tylist)           -> (rng, HorzCommandType(List.map iter tylist))
    | VertCommandType(tylist)           -> (rng, VertCommandType(List.map iter tylist))

  and generalize_kind kd =
    match kd with
    | UniversalKind     -> UniversalKind
    | RecordKind(tyasc) -> RecordKind(Assoc.map_value iter tyasc)
  in
    Poly(iter ty)


(* !!!! ---- global variable ---- !!!! *)

let global_hash_env : (string, location) Hashtbl.t = Hashtbl.create 32


(* -- following are all for debugging -- *)

let string_of_record_type (f : mono_type -> string) (asc : (field_name, mono_type) Assoc.t) =
  let rec aux lst =
    match lst with
    | []                     -> " -- "
    | (fldnm, tystr) :: []   -> fldnm ^ " : " ^ (f tystr)
    | (fldnm, tystr) :: tail -> fldnm ^ " : " ^ (f tystr) ^ "; " ^ (aux tail)
  in
    "(|" ^ (aux (Assoc.to_list asc)) ^ "|)"


let string_of_kind (f : mono_type -> string) (kdstr : kind) =
  let rec aux lst =
    match lst with
    | []                     -> " -- "
    | (fldnm, tystr) :: []   -> fldnm ^ " : " ^ (f tystr)
    | (fldnm, tystr) :: tail -> fldnm ^ " : " ^ (f tystr) ^ "; " ^ (aux tail)
  in
    match kdstr with
    | UniversalKind   -> "U"
    | RecordKind(asc) -> "(|" ^ (aux (Assoc.to_list asc)) ^ "|)"


let rec string_of_mono_type_basic tystr =
  let (rng, tymain) = tystr in
  let qstn = if Range.is_dummy rng then "?" else "" in
    match tymain with
    | BaseType(UnitType)    -> "unit" ^ qstn
    | BaseType(BoolType)    -> "bool" ^ qstn
    | BaseType(IntType)     -> "int" ^ qstn
    | BaseType(StringType)  -> "string" ^ qstn
    | BaseType(TextRowType) -> "text-row" ^ qstn
    | BaseType(TextColType) -> "text-col" ^ qstn
    | BaseType(BoxRowType)  -> "box-row" ^ qstn
    | BaseType(BoxColType)  -> "box-col" ^ qstn
    | BaseType(FontType)    -> "font" ^ qstn
    | BaseType(ContextType) -> "context" ^ qstn

    | VariantType(tyarglist, tyid) ->
        (string_of_type_argument_list_basic tyarglist) ^ (TypeID.show_direct tyid) (* temporary *) ^ "@" ^ qstn

    | SynonymType(tyarglist, tyid, tyreal) ->
        (string_of_type_argument_list_basic tyarglist) ^ (TypeID.show_direct tyid) ^ "@ (= " ^ (string_of_mono_type_basic tyreal) ^ ")"

    | FuncType(tydom, tycod)    ->
        let strdom = string_of_mono_type_basic tydom in
        let strcod = string_of_mono_type_basic tycod in
          begin match tydom with
          | (_, FuncType(_, _)) -> "(" ^ strdom ^ ")"
          | _                   -> strdom
          end ^ " ->" ^ qstn ^ " " ^ strcod

    | ListType(tycont)          ->
        let strcont = string_of_mono_type_basic tycont in
        let (_, tycontmain) = tycont in
          begin match tycontmain with
          | ( FuncType(_, _)
            | ProductType(_)
            | VariantType(_ :: _, _)
(*            | TypeSynonym(_ :: _, _, _) *) ) -> "(" ^ strcont ^ ")"
          | _                             -> strcont
          end ^ " list" ^ qstn

    | RefType(tycont)           ->
        let strcont = string_of_mono_type_basic tycont in
        let (_, tycontmain) = tycont in
          begin match tycontmain with
          | ( FuncType(_, _)
            | ProductType(_)
            | VariantType(_ :: _, _)
(*            | TypeSynonym(_ :: _, _, _) *) ) -> "(" ^ strcont ^ ")"
          | _                                -> strcont
          end ^ " ref" ^ qstn

    | ProductType(tylist)       -> string_of_mono_type_list_basic tylist
    | TypeVariable(tvref)       ->
        begin
          match !tvref with
          | Link(tyl)  -> "$(" ^ (string_of_mono_type_basic tyl) ^ ")"
          | Free(tvid) -> "'" ^ (FreeID.show_direct (string_of_kind string_of_mono_type_basic) tvid) ^ qstn
          | Bound(bid) -> "'#" ^ (BoundID.show_direct (string_of_kind string_of_mono_type_basic) bid) ^ qstn
        end

    | RecordType(asc)           -> string_of_record_type string_of_mono_type_basic asc
    | HorzCommandType(tylist)   ->
        let slist = List.map string_of_mono_type_basic tylist in
        "(" ^ (String.concat ", " slist) ^ ") horz-command"
    | VertCommandType(tylist)   ->
        let slist = List.map string_of_mono_type_basic tylist in
        "(" ^ (String.concat ", " slist) ^ ") vert-command"


and string_of_type_argument_list_basic tyarglist =
  match tyarglist with
  | []           -> ""
  | head :: tail ->
      let strhd = string_of_mono_type_basic head in
      let strtl = string_of_type_argument_list_basic tail in
      let (_, headmain) = head in
        begin
          match headmain with
          | ( FuncType(_, _) | ProductType(_) (* | TypeSynonym(_ :: _, _, _) *) (* temporary *)
            | ListType(_) | RefType(_) | VariantType(_ :: _, _) )          -> "(" ^ strhd ^ ")"
          | _                                                              -> strhd
        end ^ " " ^ strtl


and string_of_mono_type_list_basic tylist =
  match tylist with
  | []           -> ""
  | head :: []   ->
      let strhd = string_of_mono_type_basic head in
      let (_, headmain) = head in
        begin
          match headmain with
          | ( ProductType(_) | FuncType(_, _) ) -> "(" ^ strhd ^ ")"
          | _                                   -> strhd
        end
  | head :: tail ->
      let strhd = string_of_mono_type_basic head in
      let strtl = string_of_mono_type_list_basic tail in
      let (_, headmain) = head in
        begin
          match headmain with
          | ( ProductType(_) | FuncType(_, _) ) -> "(" ^ strhd ^ ")"
          | _                                   -> strhd
        end ^ " * " ^ strtl


and string_of_poly_type_basic (Poly(ty)) =
  string_of_mono_type_basic ty (* temporary *)


and string_of_kind_basic kd = string_of_kind string_of_mono_type_basic kd


let rec string_of_manual_type (_, mtymain) =
  let iter = string_of_manual_type in
  match mtymain with
  | MTypeName(mtylst, tynm)   -> (String.concat " " (List.map iter mtylst)) ^ " " ^ tynm
  | MTypeParam(tpnm)          -> "'" ^ tpnm
  | MFuncType(mtydom, mtycod) -> (iter mtydom) ^ " -> " ^ (iter mtycod)
  | MProductType(mtylst)      -> (String.concat " * " (List.map iter mtylst))
  | MRecordType(mtyasc)       -> "(|" ^ (String.concat "; " (List.map (fun (fldnm, mty) -> fldnm ^ " : " ^ (iter mty)) (Assoc.to_list mtyasc))) ^ "|)"
