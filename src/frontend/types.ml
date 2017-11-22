
exception ParseErrorDetail of string

type ctrlseq_name       = string  [@@deriving show]
type var_name           = string  [@@deriving show]
type id_name            = string  [@@deriving show]
type class_name         = string  [@@deriving show]
type type_name          = string  [@@deriving show]
type constructor_name   = string  [@@deriving show]
type module_name        = string  [@@deriving show]
type sig_var_name       = string  [@@deriving show]
type field_name         = string  [@@deriving show]
type type_argument_name = string  [@@deriving show]
type length_unit_name   = string  [@@deriving show]


module TypeID : sig
  type t
    [@@deriving show]
  val initialize : unit -> unit
  val fresh : type_name -> t
  val extract_name : t -> type_name
  val equal : t -> t -> bool
  val show_direct : t -> string
end = struct
  type t = int * type_name
    [@@deriving show]
  let current_id = ref 0
  let initialize () = begin current_id := 0; end
  let fresh tynm = begin incr current_id; (!current_id, tynm) end
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
    val show_direct : t -> string
  end
= struct
    type t = int * string
    let current_id = ref 0
    let initialize () = ( current_id := 0 )
    let fresh varnm = begin incr current_id ; (!current_id, varnm) end
    let equal (i1, _) (i2, _) = (i1 = i2)
    let show_direct (i, varnm) = "<" ^ (string_of_int i) ^ "|" ^ varnm ^ ">"
  end


type quantifiability = Quantifiable | Unquantifiable
[@@deriving show]

module FreeID_
: sig
    type level  [@@deriving show]
    type 'a t_  [@@deriving show]
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
    [@@deriving show]

    type 'a t_ = int * 'a * quantifiability * level
    [@@deriving show]

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
    type 'a t_  [@@deriving show]
    val initialize : unit -> unit
    val fresh : 'a -> unit -> 'a t_
    val eq : 'a t_ -> 'a t_ -> bool
    val get_kind : 'a t_ -> 'a
    val show_direct : ('a -> string) -> 'a t_ -> string
  end
= struct
    type 'a t_ = int * 'a
    [@@deriving show]

    let current_id = ref 0

    let initialize () = begin current_id := 0; end

    let fresh kd () =
      begin
        incr current_id;
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
[@@deriving show]

type manual_kind =
  | MUniversalKind
  | MRecordKind    of (field_name, manual_type) Assoc.t
[@@deriving show]

type base_type =
  | UnitType
  | BoolType
  | IntType
  | FloatType
  | LengthType
  | StringType
  | TextRowType
  | TextColType
  | BoxRowType
  | BoxColType
  | FontType
  | ContextType
  | PrePathType
  | PathType
  | GraphicsType
  | DocumentType
  | MathType
[@@deriving show]


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
  | VertDetailedCommandType of mono_type list

and poly_type =
  | Poly of mono_type

and kind =
  | UniversalKind
  | RecordKind of (field_name, mono_type) Assoc.t

and type_variable_info =
  | Free  of kind FreeID_.t_
  | Bound of kind BoundID_.t_
  | Link  of mono_type

[@@deriving show]


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

module FontSchemeMap = Map.Make
  (struct
    type t = CharBasis.script
    let compare = Pervasives.compare
  end)

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

and 'a untyped_path_component =
  | UTPathLineTo        of 'a
  | UTPathCubicBezierTo of untyped_abstract_tree * untyped_abstract_tree * 'a

and untyped_abstract_tree =
  Range.t * untyped_abstract_tree_main
    [@printer (fun fmt (_, utastmain) -> Format.fprintf fmt "%a" pp_untyped_abstract_tree_main utastmain)]

and untyped_abstract_tree_main =
(* -- basic value -- *)
  | UTUnitConstant
      [@printer (fun fmt () -> Format.fprintf fmt "()")]
  | UTBooleanConstant      of bool
  | UTIntegerConstant      of int
  | UTFloatConstant        of float
  | UTLengthDescription    of float * length_unit_name
      [@printer (fun fmt (fl, lun) -> Format.fprintf fmt "%f%s" fl lun)]
  | UTStringEmpty
  | UTStringConstant       of string
      [@printer (fun fmt s -> Format.fprintf fmt "\"%s\"" s)]
(* -- inputs -- *)
  | UTInputHorz            of untyped_input_horz_element list
  | UTInputVert            of untyped_input_vert_element list
  | UTConcat               of untyped_abstract_tree * untyped_abstract_tree
  | UTLambdaHorz           of Range.t * var_name * untyped_abstract_tree
  | UTLambdaVert           of Range.t * var_name * untyped_abstract_tree
  | UTLambdaVertDetailed   of Range.t * var_name * untyped_abstract_tree
(* -- graphics -- *)
  | UTPath                 of untyped_abstract_tree * (untyped_abstract_tree untyped_path_component) list * (unit untyped_path_component) option
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
      [@printer (fun fmt (_, vn) -> Format.fprintf fmt "%s" vn)]
  | UTApply                of untyped_abstract_tree * untyped_abstract_tree
      [@printer (fun fmt (u1, u2) -> Format.fprintf fmt "(%a %a)" pp_untyped_abstract_tree u1 pp_untyped_abstract_tree u2)]
  | UTLetIn                of untyped_mutual_let_cons * untyped_abstract_tree
  | UTIfThenElse           of untyped_abstract_tree * untyped_abstract_tree * untyped_abstract_tree
  | UTLambdaAbstract       of Range.t * var_name * untyped_abstract_tree
  | UTFinishHeaderFile
  | UTFinishStruct
(* -- pattern match -- *)
  | UTPatternMatch         of untyped_abstract_tree * untyped_pattern_match_cons
  | UTConstructor          of constructor_name * untyped_abstract_tree
      [@printer (fun fmt (cn, u) -> Format.fprintf fmt "%s(%a)" cn pp_untyped_abstract_tree u)]
(* -- declaration of type and module -- *)
  | UTDeclareVariantIn     of untyped_mutual_variant_cons * untyped_abstract_tree
  | UTModule               of Range.t * module_name * manual_signature option * untyped_abstract_tree * untyped_abstract_tree
(* -- implerative -- *)
  | UTLetMutableIn         of Range.t * var_name * untyped_abstract_tree * untyped_abstract_tree
  | UTSequential           of untyped_abstract_tree * untyped_abstract_tree
  | UTWhileDo              of untyped_abstract_tree * untyped_abstract_tree
(*
  | UTDeclareGlobalHash    of untyped_abstract_tree * untyped_abstract_tree
  | UTOverwriteGlobalHash  of untyped_abstract_tree * untyped_abstract_tree
  | UTReferenceFinal       of untyped_abstract_tree
*)
  | UTOverwrite            of Range.t * var_name * untyped_abstract_tree
(* -- lightweight itemize -- *)
  | UTItemize              of untyped_itemize

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

and untyped_variant_cons = (Range.t * constructor_name * manual_type) list

and untyped_mutual_variant_cons =
  | UTMutualVariantCons    of untyped_type_argument_cons * Range.t * type_name * untyped_variant_cons * untyped_mutual_variant_cons
  | UTMutualSynonymCons    of untyped_type_argument_cons * Range.t * type_name * manual_type * untyped_mutual_variant_cons
  | UTEndOfMutualVariant

and untyped_pattern_tree = Range.t * untyped_pattern_tree_main
and untyped_pattern_tree_main =
  | UTPIntegerConstant     of int
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
[@@deriving show { with_path = false }]

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
  font_size        : HorzBox.length;
  font_scheme      : HorzBox.font_with_ratio FontSchemeMap.t;
  dominant_script  : CharBasis.script;
  space_natural    : float;
  space_shrink     : float;
  space_stretch    : float;
  adjacent_stretch : float;
  paragraph_width  : HorzBox.length;
  paragraph_top    : HorzBox.length;
  paragraph_bottom : HorzBox.length;
  leading          : HorzBox.length;
  text_color       : HorzBox.color;
  manual_rising    : HorzBox.length;
  page_scheme      : HorzBox.page_scheme;
}
(* temporary *)

and 'a path_component =
  | PathLineTo        of 'a
  | PathCubicBezierTo of abstract_tree * abstract_tree * 'a

and abstract_tree =
(* -- basic value -- *)
  | UnitConstant
  | BooleanConstant       of bool
  | IntegerConstant       of int
  | FloatConstant         of float
  | LengthDescription     of float * length_unit_name
  | LengthConstant        of HorzBox.length
  | StringEmpty
  | StringConstant        of string
(*
  | DeeperIndent          of abstract_tree
  | BreakAndIndent
  | SoftBreakAndIndent
*)
  | Concat                of abstract_tree * abstract_tree
  | FuncWithEnvironment   of EvalVarID.t * abstract_tree * environment
  | EvaluatedEnvironment  of environment
(* -- input texts -- *)
  | InputHorz             of input_horz_element list
  | InputVert             of input_vert_element list
  | InputHorzWithEnvironment of input_horz_element list * environment
  | InputVertWithEnvironment of input_vert_element list * environment
(* -- graphics -- *)
  | Path                        of abstract_tree * (abstract_tree path_component) list * (unit path_component) option
  | PathValue                   of HorzBox.path list
  | PathUnite                   of abstract_tree * abstract_tree
  | GraphicsValue               of Pdfops.t list
  | PrePathValue                of PrePath.t
  | PrePathBeginning            of abstract_tree
  | PrePathLineTo               of abstract_tree * abstract_tree
  | PrePathCubicBezierTo        of abstract_tree * abstract_tree * abstract_tree * abstract_tree
  | PrePathTerminate            of abstract_tree
  | PrePathCloseWithLine        of abstract_tree
  | PrePathCloseWithCubicBezier of abstract_tree * abstract_tree * abstract_tree
  | PrimitiveDrawStroke         of abstract_tree * abstract_tree * abstract_tree
  | PrimitiveDrawDashedStroke   of abstract_tree * abstract_tree * abstract_tree * abstract_tree
  | PrimitiveDrawFill           of abstract_tree * abstract_tree
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
(*
  | DeclareGlobalHash     of abstract_tree * abstract_tree
  | OverwriteGlobalHash   of abstract_tree * abstract_tree
  | ReferenceFinal        of abstract_tree
*)
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
  | PrimitiveFloat        of abstract_tree
  | FloatPlus             of abstract_tree * abstract_tree
  | FloatMinus            of abstract_tree * abstract_tree
  | LengthPlus            of abstract_tree * abstract_tree
  | LengthMinus           of abstract_tree * abstract_tree
  | LengthTimes           of abstract_tree * abstract_tree
(* -- backend primitives -- *)
  | MathValue                   of HorzBox.math list
  | BackendMathGlyph            of abstract_tree
  | BackendMathConcat           of abstract_tree * abstract_tree
  | BackendMathSuperscript      of abstract_tree * abstract_tree
  | BackendEmbeddedMath         of abstract_tree
  | LambdaHorz                  of EvalVarID.t * abstract_tree
  | LambdaHorzWithEnvironment   of EvalVarID.t * abstract_tree * environment
  | LambdaVert                  of EvalVarID.t * abstract_tree
  | LambdaVertWithEnvironment   of EvalVarID.t * abstract_tree * environment
  | LambdaVertDetailed          of EvalVarID.t * abstract_tree
  | LambdaVertDetailedWithEnv   of EvalVarID.t * abstract_tree * environment
  | FontDesignation             of HorzBox.font_with_ratio
  | Context                     of input_context
  | UninitializedContext
  | HorzLex                     of abstract_tree * abstract_tree
  | VertLex                     of abstract_tree * abstract_tree
  | PrimitiveGetInitialContext  of abstract_tree * abstract_tree * abstract_tree * abstract_tree
  | PrimitiveSetSpaceRatio      of abstract_tree * abstract_tree
  | PrimitiveSetFontSize        of abstract_tree * abstract_tree
  | PrimitiveGetFontSize        of abstract_tree
  | PrimitiveSetFont            of abstract_tree * abstract_tree * abstract_tree
  | PrimitiveGetFont            of abstract_tree * abstract_tree
  | PrimitiveSetDominantScript  of abstract_tree * abstract_tree
(*
  | PrimitiveSetTitle           of abstract_tree * abstract_tree
  | PrimitiveGetTitle           of abstract_tree
*)
  | PrimitiveSetTextColor       of abstract_tree * abstract_tree
  | PrimitiveSetLeading         of abstract_tree * abstract_tree
  | PrimitiveGetTextWidth       of abstract_tree
  | PrimitiveSetManualRising    of abstract_tree * abstract_tree
  | PrimitiveEmbed              of abstract_tree
  | PrimitiveGetNaturalWidth    of abstract_tree
  | PrimitiveDrawText           of abstract_tree * abstract_tree
  | BackendFont                 of abstract_tree * abstract_tree * abstract_tree
  | BackendLineBreaking         of abstract_tree * abstract_tree
  | BackendPageBreaking         of abstract_tree * abstract_tree
  | DocumentValue               of input_context * HorzBox.intermediate_vert_box list
(*
  | BackendFixedString         of abstract_tree * abstract_tree
*)
  | BackendFixedEmpty           of abstract_tree
  | BackendOuterEmpty           of abstract_tree * abstract_tree * abstract_tree
  | BackendOuterFrame           of abstract_tree * abstract_tree * abstract_tree
  | BackendOuterFrameBreakable  of abstract_tree * abstract_tree * abstract_tree
  | BackendVertFrame            of abstract_tree * abstract_tree * abstract_tree * abstract_tree
  | BackendEmbeddedVert         of abstract_tree * abstract_tree * abstract_tree
  | BackendInlineGraphics       of abstract_tree * abstract_tree * abstract_tree * abstract_tree

and pattern_match_cons =
  | PatternMatchCons      of pattern_tree * abstract_tree * pattern_match_cons
  | PatternMatchConsWhen  of pattern_tree * abstract_tree * abstract_tree * pattern_match_cons
  | EndOfPatternMatch

and pattern_tree =
  | PUnitConstant
  | PBooleanConstant      of bool
  | PIntegerConstant      of int
  | PStringConstant       of abstract_tree
  | PListCons             of pattern_tree * pattern_tree
  | PEndOfList
  | PTupleCons            of pattern_tree * pattern_tree
  | PEndOfTuple
  | PWildCard
  | PVariable             of EvalVarID.t
  | PAsVariable           of EvalVarID.t * pattern_tree
  | PConstructor          of constructor_name * pattern_tree

(*
type output_unit =
  | OString             of string
  | OBreakAndIndent
  | OSoftBreakAndIndent
  | ODeepen
  | OShallow
*)


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
    | VertDetailedCommandType(tylist)   -> (rng, VertDetailedCommandType(List.map aux tylist))

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
    | VertDetailedCommandType(tylist)   -> (rng, VertDetailedCommandType(List.map iter tylist))

  and generalize_kind kd =
    match kd with
    | UniversalKind     -> UniversalKind
    | RecordKind(tyasc) -> RecordKind(Assoc.map_value iter tyasc)
  in
    Poly(iter ty)


let default_font_with_ratio =
  ("Arno", 1., 0.)


let get_font_with_ratio ctx script_raw =
  let script =
    match script_raw with
    | (CharBasis.Common | CharBasis.Unknown | CharBasis.Inherited ) -> ctx.dominant_script
    | _                                                             -> script_raw
  in
    try ctx.font_scheme |> FontSchemeMap.find script with
    | Not_found -> default_font_with_ratio


let get_string_info ctx script_raw =
  let (font_abbrev, ratio, rising_ratio) = get_font_with_ratio ctx script_raw in
  HorzBox.({
    font_abbrev = font_abbrev;
    font_size   = ctx.font_size *% ratio;
    text_color  = ctx.text_color;
    rising      = ctx.manual_rising +% ctx.font_size *% rising_ratio;
  })
(*
(* !!!! ---- global variable ---- !!!! *)

let global_hash_env : (string, location) Hashtbl.t = Hashtbl.create 32
*)

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
    | BaseType(FloatType)   -> "float" ^ qstn
    | BaseType(StringType)  -> "string" ^ qstn
    | BaseType(TextRowType) -> "inline-text" ^ qstn
    | BaseType(TextColType) -> "block-text" ^ qstn
    | BaseType(BoxRowType)  -> "inline-boxes" ^ qstn
    | BaseType(BoxColType)  -> "block-boxes" ^ qstn
    | BaseType(FontType)    -> "font" ^ qstn
    | BaseType(ContextType) -> "context" ^ qstn
    | BaseType(PrePathType) -> "pre-path" ^ qstn
    | BaseType(PathType)    -> "path" ^ qstn
    | BaseType(LengthType)  -> "length" ^ qstn
    | BaseType(GraphicsType) -> "graphics" ^ qstn
    | BaseType(DocumentType) -> "document" ^ qstn
    | BaseType(MathType)     -> "math" ^ qstn

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
    | VertDetailedCommandType(tylist)   ->
        let slist = List.map string_of_mono_type_basic tylist in
        "(" ^ (String.concat ", " slist) ^ ") vert-detailed-command"


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
