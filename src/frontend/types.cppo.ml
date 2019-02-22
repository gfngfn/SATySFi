
open LengthInterface
open GraphicBase

exception ParseErrorDetail of string
exception IllegalArgumentLength of Range.t * int * int


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


type header_element =
  | HeaderRequire of string
  | HeaderImport  of string


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


type quantifiability = Quantifiable | Unquantifiable
[@@deriving show]


module Level
: sig
   type t  [@@deriving show]
   val bottom : t
   val succ : t -> t
   val less_than : t -> t -> bool
  end
= struct
    type t = int
    [@@deriving show]

    let bottom = 0

    let succ lev = lev + 1

    let less_than lev1 lev2 = (lev1 < lev2)
  end


type level = Level.t
[@@deriving show]


module OptionRowVarID
: sig
    type t  [@@deriving show]
    val initialize : unit -> unit
    val fresh : level -> t
    val equal : t -> t -> bool
    val get_level : t -> level
    val set_level : t -> level -> t
    val show_direct : t -> string
  end
= struct
    type t = {
      level : level;
      number : int;
    }
    [@@deriving show]

    let current_number = ref 0

    let initialize () =
      current_number := 0

    let equal orv1 orv2 =
      (orv1.number = orv2.number)

    let fresh lev =
      incr current_number;
      { level = lev; number = !current_number; }

    let get_level orv =
      orv.level

    let set_level orv lev =
      { level = lev; number = orv.number; }

    let show_direct orv =
      "$" ^ (string_of_int orv.number) ^ "[" ^ (Level.show orv.level) ^ "]"
  end


module FreeID_
: sig
    type 'a t_  [@@deriving show]
    val get_level : 'a t_ -> level
    val set_level : 'a t_ -> level -> unit
    val initialize : unit -> unit
    val fresh : 'a -> quantifiability -> level -> unit -> 'a t_
    val equal : 'a t_ -> 'a t_ -> bool
    val is_quantifiable : 'a t_ -> bool
    val set_quantifiability : quantifiability -> 'a t_ -> unit
    val get_kind : 'a t_ -> 'a
    val set_kind : 'a -> 'a t_ -> unit
    val update_kind : ('a -> 'a) -> 'a t_ -> unit
    val show_direct : ('a -> string) -> 'a t_ -> string
  end
= struct
    type 'k t_ = {
      id                      : int;
      mutable kind            : 'k;
      mutable quantifiability : quantifiability;
      mutable level           : level;
    }
    [@@deriving show]

    let get_level tvid = tvid.level

    let set_level tvid lev =
      tvid.level <- lev

    let current_id = ref 0

    let initialize () =
      current_id := 0

    let fresh kd qtfbl lev () =
      incr current_id;
      {
        id              = !current_id;
        kind            = kd;
        quantifiability = qtfbl;
        level           = lev;
      }

    let equal tvid1 tvid2 =
      tvid1.id = tvid2.id

    let is_quantifiable tvid =
      match tvid.quantifiability with
      | Quantifiable   -> true
      | Unquantifiable -> false

    let set_quantifiability qtfbl tvid =
      tvid.quantifiability <- qtfbl

    let get_kind tvid = tvid.kind

    let set_kind kd tvid =
      tvid.kind <- kd

    let update_kind f tvid =
      tvid.kind <- f tvid.kind

    let show_direct f tvid =
      let idmain = tvid.id in
      let lev = tvid.level in
      let kd = tvid.kind in
      match tvid.quantifiability with
      | Quantifiable   -> (string_of_int idmain) ^ "[Q" ^ (Level.show lev) ^ "::" ^ (f kd) ^ "]"
      | Unquantifiable -> (string_of_int idmain) ^ "[U" ^ (Level.show lev) ^ "::" ^ (f kd) ^ "]"

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


module StoreIDHashTable = Hashtbl.Make(StoreID)


module EvalVarIDMap = Map.Make(EvalVarID)


type manual_type = Range.t * manual_type_main
and manual_type_main =
  | MTypeName        of (manual_type list) * module_name list * type_name
  | MTypeParam       of var_name
  | MFuncType        of manual_type list * manual_type * manual_type
  | MProductType     of manual_type list
  | MRecordType      of manual_type Assoc.t
      [@printer (fun fmt _ -> Format.fprintf fmt "MRecordType(...)")]
  | MHorzCommandType of manual_command_argument_type list
  | MVertCommandType of manual_command_argument_type list
  | MMathCommandType of manual_command_argument_type list
[@@deriving show]

and manual_command_argument_type =
  | MMandatoryArgumentType of manual_type
  | MOptionalArgumentType  of manual_type

type manual_kind =
  | MUniversalKind
  | MRecordKind    of manual_type Assoc.t
      [@printer (fun fmt _ -> Format.fprintf fmt "MRecordKind(...)")]
[@@deriving show]

type base_type =
  | EnvType
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
  | ContextType
  | PrePathType
  | PathType
  | GraphicsType
  | ImageType
  | DocumentType
  | MathType
  | RegExpType
  | TextInfoType
[@@deriving show]


let base_type_hash_table =
  let ht = Hashtbl.create 32 in
  begin
    List.iter (fun (s, bt) -> Hashtbl.add ht s bt) [
      ("unit"        , UnitType    );
      ("bool"        , BoolType    );
      ("int"         , IntType     );
      ("float"       , FloatType   );
      ("length"      , LengthType  );
      ("string"      , StringType  );
      ("inline-text" , TextRowType );
      ("block-text"  , TextColType );
      ("inline-boxes", BoxRowType  );
      ("block-boxes" , BoxColType  );
      ("context"     , ContextType );
      ("pre-path"    , PrePathType );
      ("path"        , PathType    );
      ("graphics"    , GraphicsType);
      ("image"       , ImageType   );
      ("document"    , DocumentType);
      ("math"        , MathType    );
      ("regexp"      , RegExpType  );
      ("text-info"   , TextInfoType);
    ];
    ht
  end


type ('a, 'b) typ = Range.t * ('a, 'b) type_main
and ('a, 'b) type_main =
  | BaseType        of base_type
  | FuncType        of ('a, 'b) option_row * ('a, 'b) typ * ('a, 'b) typ
  | ListType        of ('a, 'b) typ
  | RefType         of ('a, 'b) typ
  | ProductType     of (('a, 'b) typ) list
  | TypeVariable    of 'a
  | SynonymType     of (('a, 'b) typ) list * TypeID.t * ('a, 'b) typ
  | VariantType     of (('a, 'b) typ) list * TypeID.t
  | RecordType      of (('a, 'b) typ) Assoc.t
      [@printer (fun fmt _ -> Format.fprintf fmt "RecordType(...)")]
  | HorzCommandType of (('a, 'b) command_argument_type) list
  | VertCommandType of (('a, 'b) command_argument_type) list
  | MathCommandType of (('a, 'b) command_argument_type) list
  | CodeType        of ('a, 'b) typ

and ('a, 'b) command_argument_type =
  | MandatoryArgumentType of ('a, 'b) typ
  | OptionalArgumentType  of ('a, 'b) typ

and ('a, 'b) kind =
  | UniversalKind
  | RecordKind of (('a, 'b) typ) Assoc.t
      [@printer (fun fmt _ -> Format.fprintf fmt "RecordKind(...)")]

and ('a, 'b) option_row =
  | OptionRowCons     of ('a, 'b) typ * ('a, 'b) option_row
  | OptionRowEmpty
  | OptionRowVariable of 'b

and mono_option_row_variable_info =
  | MonoORFree of OptionRowVarID.t
  | MonoORLink of mono_option_row

and poly_option_row_variable_info =
  | PolyORFree of mono_option_row_variable_info ref

and mono_type_variable_info =
  | MonoFree of mono_kind FreeID_.t_
  | MonoLink of mono_type

and poly_type_variable_info =
  | PolyFree  of mono_type_variable_info ref
  | PolyBound of ((poly_type_variable_info, poly_option_row_variable_info) kind) BoundID_.t_

and mono_type = (mono_type_variable_info ref, mono_option_row_variable_info ref) typ

and poly_type_body = (poly_type_variable_info, poly_option_row_variable_info) typ

and poly_type =
  | Poly of poly_type_body

and mono_kind = (mono_type_variable_info ref, mono_option_row_variable_info ref) kind

and poly_kind = (poly_type_variable_info, poly_option_row_variable_info) kind

and mono_option_row = (mono_type_variable_info ref, mono_option_row_variable_info ref) option_row
[@@deriving show]

type mono_command_argument_type = (mono_type_variable_info ref, mono_option_row_variable_info ref) command_argument_type


module FreeID =
  struct
    include FreeID_
    type t = mono_kind FreeID_.t_
  end


module BoundID =
  struct
    include BoundID_
    type t = poly_kind BoundID_.t_
  end

(* ---- untyped ---- *)

let pp_sep fmt () =
  Format.fprintf fmt ";@ "


type stage =
  | Persistent0
  | Stage0
  | Stage1

type pre = {
  level           : level;
  quantifiability : quantifiability;  (* may omitted in the future *)
  stage           : stage;
}


let string_of_stage = function
  | Persistent0 -> "persistent stage"
  | Stage0      -> "stage 0"
  | Stage1      -> "stage 1"


type untyped_letrec_binding =
  UTLetRecBinding of manual_type option * Range.t * var_name * untyped_abstract_tree

and untyped_input_horz_element = Range.t * untyped_input_horz_element_main
  [@printer (fun fmt (_, utihmain) -> Format.fprintf fmt "%a" pp_untyped_input_horz_element_main utihmain)]

and untyped_input_horz_element_main =
  | UTInputHorzText         of string
      [@printer (fun fmt s -> Format.fprintf fmt "IT:%s" s)]
  | UTInputHorzEmbedded     of untyped_abstract_tree * untyped_command_argument list
      [@printer (fun fmt (utast, lst) -> Format.fprintf fmt "IC:%a %a" pp_untyped_abstract_tree utast (Format.pp_print_list ~pp_sep pp_untyped_command_argument) lst)]
  | UTInputHorzContent      of untyped_abstract_tree
  | UTInputHorzEmbeddedMath of untyped_abstract_tree

and untyped_input_vert_element = Range.t * untyped_input_vert_element_main
  [@printer (fun fmt (_, utivmain) -> Format.fprintf fmt "%a" pp_untyped_input_vert_element_main utivmain)]

and untyped_input_vert_element_main =
  | UTInputVertEmbedded of untyped_abstract_tree * untyped_command_argument list
      [@printer (fun fmt (utast, lst) -> Format.fprintf fmt "BC:%a %a" pp_untyped_abstract_tree utast (Format.pp_print_list ~pp_sep pp_untyped_command_argument) lst)]
  | UTInputVertContent  of untyped_abstract_tree

and 'a untyped_path_component =
  | UTPathLineTo        of 'a
  | UTPathCubicBezierTo of untyped_abstract_tree * untyped_abstract_tree * 'a

and untyped_abstract_tree =
  Range.t * untyped_abstract_tree_main
    [@printer (fun fmt (_, utastmain) -> Format.fprintf fmt "%a" pp_untyped_abstract_tree_main utastmain)]

and untyped_abstract_tree_main =
(* -- basic value -- *)
  | UTUnitConstant
      [@printer (fun fmt () -> Format.fprintf fmt "U:()")]
  | UTBooleanConstant      of bool
  | UTIntegerConstant      of int
  | UTFloatConstant        of float
  | UTLengthDescription    of float * length_unit_name
      [@printer (fun fmt (fl, lun) -> Format.fprintf fmt "L:%f%s" fl lun)]
  | UTStringEmpty
  | UTStringConstant       of string
      [@printer (fun fmt s -> Format.fprintf fmt "S:\"%s\"" s)]
(* -- inputs -- *)
  | UTInputHorz            of untyped_input_horz_element list
  | UTInputVert            of untyped_input_vert_element list
  | UTConcat               of untyped_abstract_tree * untyped_abstract_tree
  | UTLambdaHorz           of Range.t * var_name * untyped_abstract_tree
  | UTLambdaVert           of Range.t * var_name * untyped_abstract_tree
  | UTLambdaMath           of untyped_abstract_tree
(* -- graphics -- *)
  | UTPath                 of untyped_abstract_tree * (untyped_abstract_tree untyped_path_component) list * (unit untyped_path_component) option
(* -- horizontal box list -- *)
  | UTHorz                 of HorzBox.horz_box list
  | UTHorzConcat           of untyped_abstract_tree * untyped_abstract_tree
(* -- vertical box list -- *)
  | UTVert                 of HorzBox.vert_box list
  | UTVertConcat           of untyped_abstract_tree * untyped_abstract_tree
(* -- list value -- *)
  | UTListCons             of untyped_abstract_tree * untyped_abstract_tree
  | UTEndOfList
(* -- tuple value -- *)
  | UTTuple               of untyped_abstract_tree list
(* -- record value -- *)
  | UTRecord               of (field_name * untyped_abstract_tree) list
  | UTAccessField          of untyped_abstract_tree * field_name
  | UTUpdateField          of untyped_abstract_tree * field_name * untyped_abstract_tree
(* -- fundamental -- *)
  | UTContentOf            of (module_name list) * var_name
      [@printer (fun fmt (_, vn) -> Format.fprintf fmt "%s" vn)]
  | UTApply                of untyped_abstract_tree * untyped_abstract_tree
      [@printer (fun fmt (u1, u2) -> Format.fprintf fmt "(%a %a)" pp_untyped_abstract_tree u1 pp_untyped_abstract_tree u2)]
  | UTApplyOmission        of untyped_abstract_tree
  | UTApplyOptional        of untyped_abstract_tree * untyped_abstract_tree
  | UTLetRecIn             of untyped_letrec_binding list * untyped_abstract_tree
  | UTLetNonRecIn          of manual_type option * untyped_pattern_tree * untyped_abstract_tree * untyped_abstract_tree
  | UTIfThenElse           of untyped_abstract_tree * untyped_abstract_tree * untyped_abstract_tree
  | UTFunction             of (Range.t * var_name) list * untyped_pattern_tree * untyped_abstract_tree
  | UTOpenIn               of Range.t * module_name * untyped_abstract_tree
  | UTFinishHeaderFile
  | UTFinishStruct
(* -- pattern match -- *)
  | UTPatternMatch         of untyped_abstract_tree * untyped_pattern_branch list
  | UTConstructor          of constructor_name * untyped_abstract_tree
      [@printer (fun fmt (cn, u) -> Format.fprintf fmt "%s(%a)" cn pp_untyped_abstract_tree u)]
(* -- declaration of type and module -- *)
  | UTDeclareVariantIn     of untyped_mutual_variant_cons * untyped_abstract_tree
  | UTModule               of Range.t * module_name * manual_signature option * untyped_abstract_tree * untyped_abstract_tree
(* -- implerative -- *)
  | UTLetMutableIn         of Range.t * var_name * untyped_abstract_tree * untyped_abstract_tree
  | UTSequential           of untyped_abstract_tree * untyped_abstract_tree
  | UTWhileDo              of untyped_abstract_tree * untyped_abstract_tree
  | UTOverwrite            of Range.t * var_name * untyped_abstract_tree
(* -- lightweight itemize -- *)
  | UTItemize              of untyped_itemize
(* -- math -- *)
  | UTMath                 of untyped_math
(* -- for lightweight command definition -- *)
  | UTLexHorz              of untyped_abstract_tree * untyped_abstract_tree
  | UTLexVert              of untyped_abstract_tree * untyped_abstract_tree
(* -- multi-stage constructs -- *)
  | UTNext                 of untyped_abstract_tree
  | UTPrev                 of untyped_abstract_tree

and constraints = (var_name * manual_kind) list

and manual_signature_content =
  | SigType   of untyped_type_argument list * type_name
  | SigValue  of var_name * manual_type * constraints
  | SigDirect of var_name * manual_type * constraints
(*
  | SigModule of module_name * manual_signature
*)

and manual_signature = manual_signature_content list

and untyped_itemize =
  | UTItem of untyped_abstract_tree * (untyped_itemize list)

and untyped_constructor_dec = Range.t * constructor_name * manual_type

and untyped_mutual_variant_cons =
  | UTMutualVariantCons    of untyped_type_argument list * Range.t * type_name * untyped_constructor_dec list * untyped_mutual_variant_cons
  | UTMutualSynonymCons    of untyped_type_argument list * Range.t * type_name * manual_type * untyped_mutual_variant_cons
  | UTEndOfMutualVariant

and untyped_pattern_tree = Range.t * untyped_pattern_tree_main
and untyped_pattern_tree_main =
  | UTPIntegerConstant     of int
  | UTPBooleanConstant     of bool
  | UTPStringConstant      of string
  | UTPUnitConstant
  | UTPListCons            of untyped_pattern_tree * untyped_pattern_tree
  | UTPEndOfList
  | UTPTuple               of untyped_pattern_tree list
  | UTPWildCard
  | UTPVariable            of var_name
  | UTPAsVariable          of var_name * untyped_pattern_tree
  | UTPConstructor         of constructor_name * untyped_pattern_tree

and untyped_pattern_branch =
  | UTPatternBranch     of untyped_pattern_tree * untyped_abstract_tree
  | UTPatternBranchWhen of untyped_pattern_tree * untyped_abstract_tree * untyped_abstract_tree

and untyped_unkinded_type_argument = Range.t * var_name

and untyped_type_argument = Range.t * var_name * manual_kind

and untyped_math = Range.t * untyped_math_main

and untyped_math_main =
  | UTMChar        of string
  | UTMSuperScript of untyped_math * untyped_math
  | UTMSubScript   of untyped_math * untyped_math
  | UTMCommand     of untyped_abstract_tree * untyped_command_argument list
  | UTMList        of untyped_math list
  | UTMEmbed       of untyped_abstract_tree

and untyped_command_argument =
  | UTMandatoryArgument of untyped_abstract_tree
  | UTOptionalArgument  of untyped_abstract_tree
  | UTOmission          of Range.t
[@@deriving show { with_path = false; }]

type untyped_letrec_pattern_branch =
  | UTLetRecPatternBranch of untyped_pattern_tree list * untyped_abstract_tree

type untyped_argument =
  | UTPatternArgument  of untyped_pattern_tree
  | UTOptionalArgument of Range.t * var_name

type untyped_let_binding = manual_type option * untyped_pattern_tree * untyped_argument list * untyped_abstract_tree

(* ---- typed ---- *)

type 'a input_horz_element_scheme =
  | InputHorzText         of string
  | InputHorzEmbedded     of 'a
  | InputHorzContent      of 'a
  | InputHorzEmbeddedMath of 'a
[@@deriving show { with_path = false; }]

type 'a input_vert_element_scheme =
  | InputVertEmbedded of 'a
  | InputVertContent  of 'a
[@@deriving show { with_path = false; }]

type ('a, 'b) path_component_scheme =
  | PathLineTo        of 'a
  | PathCubicBezierTo of 'b * 'b * 'a
[@@deriving show { with_path = false; }]

type base_constant =
  | BCUnit
  | BCBool     of bool
  | BCInt      of int
  | BCFloat    of float
  | BCLength   of length
  | BCString   of string
  | BCRegExp   of Str.regexp
      [@printer (fun fmt _ -> Format.fprintf fmt "<regexp>")]
  | BCPath     of path list
      [@printer (fun fmt _ -> Format.fprintf fmt "<path>")]
  | BCPrePath  of PrePath.t
      [@printer (fun fmt _ -> Format.fprintf fmt "<pre-path>")]
  | BCImageKey of ImageInfo.key
      [@printer (fun fmt _ -> Format.fprintf fmt "<image-key>")]
  | BCHorz     of HorzBox.horz_box list
  | BCVert     of HorzBox.vert_box list
  | BCGraphics of (HorzBox.intermediate_horz_box list) GraphicD.element
      [@printer (fun fmt _ -> Format.fprintf fmt "<graphics>")]
  | BCTextModeContext of TextBackend.text_mode_context
  | BCDocument        of HorzBox.page_size * HorzBox.page_content_scheme_func * HorzBox.page_parts_scheme_func * HorzBox.vert_box list
[@@deriving show { with_path = false; }]

type letrec_binding =
  | LetRecBinding of EvalVarID.t * pattern_branch

and environment = location EvalVarIDMap.t * (syntactic_value StoreIDHashTable.t) ref
  [@printer (fun fmt _ -> Format.fprintf fmt "<env>")]

and location = syntactic_value ref

and vmenv = environment * (syntactic_value array) list

and compiled_input_horz_element =
  | CompiledInputHorzText         of string
  | CompiledInputHorzEmbedded     of instruction list
  | CompiledInputHorzContent      of instruction list
  | CompiledInputHorzEmbeddedMath of instruction list

and compiled_intermediate_input_horz_element =
  | CompiledImInputHorzText         of string
  | CompiledImInputHorzEmbedded     of instruction list
  | CompiledImInputHorzContent of compiled_intermediate_input_horz_element list * vmenv
  | CompiledImInputHorzEmbeddedMath of instruction list

and compiled_input_vert_element =
  | CompiledInputVertEmbedded of instruction list
  | CompiledInputVertContent  of instruction list

and compiled_intermediate_input_vert_element =
  | CompiledImInputVertEmbedded of instruction list
  | CompiledImInputVertContent  of compiled_intermediate_input_vert_element list * vmenv

and ir_input_horz_element =
  | IRInputHorzText         of string
  | IRInputHorzEmbedded     of ir
  | IRInputHorzContent      of ir
  | IRInputHorzEmbeddedMath of ir


and ir_input_vert_element =
  | IRInputVertEmbedded of ir
  | IRInputVertContent  of ir

and 'a ir_path_component =
  | IRPathLineTo        of 'a
  | IRPathCubicBezierTo of ir * ir * 'a

and 'a compiled_path_component =
  | CompiledPathLineTo of 'a
  | CompiledPathCubicBezierTo of instruction list * instruction list * 'a

and varloc =
  | GlobalVar of location * EvalVarID.t * int ref
  | LocalVar  of int * int * EvalVarID.t * int ref

and ir =
  | IRConstant              of syntactic_value
  | IRTerminal
  | IRInputHorz             of ir_input_horz_element list
  | IRInputVert             of ir_input_vert_element list
  | IRRecord                of Assoc.key list * ir list
      [@printer (fun fmt _ -> Format.fprintf fmt "IRRecord(...)")]
  | IRAccessField           of ir * field_name
  | IRUpdateField           of ir * field_name * ir
  | IRLetRecIn              of (varloc * ir) list * ir
  | IRLetNonRecIn           of ir * ir_pattern_tree * ir
  | IRContentOf             of varloc
  | IRIfThenElse            of ir * ir * ir
  | IRFunction              of int * varloc list * ir_pattern_tree list * ir
  | IRApply                 of int * ir * ir list
  | IRApplyPrimitive        of instruction * int * ir list
  | IRApplyOptional         of ir * ir
  | IRApplyOmission         of ir
  | IRTuple                 of int * ir list
  | IRPatternMatch          of Range.t * ir * ir_pattern_branch list
  | IRNonValueConstructor   of constructor_name * ir
  | IRLetMutableIn          of varloc * ir * ir
  | IRSequential            of ir * ir
  | IRWhileDo               of ir * ir
  | IROverwrite             of varloc * ir
  | IRDereference           of ir
  | IRModule                of ir * ir
  | IRPath                  of ir * ir ir_path_component list * (unit ir_path_component) option

  | IRCodeCombinator        of (code_value list -> code_value) * int * ir list
  | IRCodeRecord            of Assoc.key list * ir list
      [@printer (fun fmt _ -> Format.fprintf fmt "IRCodeRecord(...)")]

and ir_pattern_branch =
  | IRPatternBranch      of ir_pattern_tree * ir
  | IRPatternBranchWhen  of ir_pattern_tree * ir * ir

and ir_pattern_tree =
  | IRPUnitConstant
  | IRPBooleanConstant      of bool
  | IRPIntegerConstant      of int
  | IRPStringConstant       of string
  | IRPListCons             of ir_pattern_tree * ir_pattern_tree
  | IRPEndOfList
  | IRPTupleCons             of ir_pattern_tree * ir_pattern_tree
  (*| IRPTupleCons            of int * ir_pattern_tree list*)
  | IRPEndOfTuple
  | IRPWildCard
  | IRPVariable             of varloc
  | IRPAsVariable           of varloc * ir_pattern_tree
  | IRPConstructor          of constructor_name * ir_pattern_tree

and instruction =
  | OpAccessField of field_name
  | OpUpdateField of field_name
  | OpForward of int
  | OpApply of int
  | OpApplyT of int
  | OpApplyOptional
  | OpApplyOmission
  | OpBindGlobal of syntactic_value ref * EvalVarID.t * int
  | OpBindLocal of int * int * EvalVarID.t * int
  | OpBindClosuresRec of (varloc * instruction list) list
  | OpBranch of instruction list
      [@printer (fun fmt _ -> Format.fprintf fmt "OpBranch(...)")]
  | OpBranchIf of instruction list
      [@printer (fun fmt _ -> Format.fprintf fmt "OpBranchIf(...)")]
  | OpBranchIfNot of instruction list
      [@printer (fun fmt _ -> Format.fprintf fmt "OpBranchIfNot(...)")]
  | OpLoadGlobal of syntactic_value ref * EvalVarID.t * int
      [@printer ((fun fmt (r, evid, refs) -> Format.fprintf fmt "OpLoadGlobal(%s)" (EvalVarID.show_direct evid)))]
  | OpLoadLocal of int * int * EvalVarID.t * int
  | OpDereference
      (* !! pdf, no-interp *)
  | OpDup
  | OpError of string
  | OpMakeConstructor of constructor_name
  | OpMakeRecord of Assoc.key list
      [@printer (fun fmt _ -> Format.fprintf fmt "OpMakeRecord(...)")]
  | OpMakeTuple of int
  | OpPop
  | OpPush of syntactic_value
  | OpPushEnv
  | OpCheckStackTopBool of bool * instruction list
      [@printer (fun fmt _ -> Format.fprintf fmt "OpCheckStackTopBool(...)")]
  | OpCheckStackTopCtor of constructor_name * instruction list
      [@printer (fun fmt _ -> Format.fprintf fmt "OpCheckStackTopCtor(...)")]
  | OpCheckStackTopEndOfList of instruction list
      [@printer (fun fmt _ -> Format.fprintf fmt "OpCheckStackTopEndOfList(...)")]
  | OpCheckStackTopInt of int * instruction list
      [@printer (fun fmt _ -> Format.fprintf fmt "OpCheckStackTopInt(...)")]
  | OpCheckStackTopListCons of instruction list
      [@printer (fun fmt _ -> Format.fprintf fmt "OpCheckStackTopListCons(...)")]
  | OpCheckStackTopStr of string * instruction list
      [@printer (fun fmt _ -> Format.fprintf fmt "OpCheckStackTopStr(...)")]
  | OpCheckStackTopTupleCons of instruction list
      [@printer (fun fmt _ -> Format.fprintf fmt "OpCheckStackTopTupleCons(...)")]
  | OpClosure of varloc list * int * int * instruction list
  | OpClosureInputHorz of compiled_input_horz_element list
  | OpClosureInputVert of compiled_input_vert_element list
  | OpBindLocationGlobal of syntactic_value ref * EvalVarID.t
  | OpBindLocationLocal of int * int * EvalVarID.t
  | OpUpdateGlobal of syntactic_value ref * EvalVarID.t
      [@printer (fun fmt _ -> Format.fprintf fmt "OpUpdateGlobal(...)")]
  | OpUpdateLocal of int * int * EvalVarID.t
      [@printer (fun fmt _ -> Format.fprintf fmt "OpUpdateLocal(...)")]
  | OpSel of instruction list * instruction list
      [@printer (fun fmt _ -> Format.fprintf fmt "OpSel(...)")]

  | OpBackendMathList of int
      (* !! no-ircode *)

  | OpPath of ((instruction list) compiled_path_component) list * (unit compiled_path_component) option
      [@printer (fun fmt _ -> Format.fprintf fmt "OpPath(...)")]
      (* !! no-interp, no-ircode *)

  | OpInsertArgs of syntactic_value list
  | OpApplyCodeCombinator of (code_value list -> code_value) * int
  | OpCodeMakeRecord of Assoc.key list
      [@printer (fun fmt _ -> Format.fprintf fmt "OpCodeMakeRecord(...)")]
  | OpCodeMathList of int
  | OpCodeMakeTuple of int
#include "__insttype.gen.ml"

and intermediate_input_horz_element =
  | ImInputHorzText         of string
  | ImInputHorzEmbedded     of abstract_tree
  | ImInputHorzContent      of intermediate_input_horz_element list * environment
  | ImInputHorzEmbeddedMath of abstract_tree

and intermediate_input_vert_element =
  | ImInputVertEmbedded of abstract_tree
  | ImInputVertContent  of intermediate_input_vert_element list * environment

and syntactic_value =
  | Nil  (* -- just for brief use -- *)
  | BaseConstant of base_constant
  | Constructor  of constructor_name * syntactic_value
  | List         of syntactic_value list
  | Tuple        of syntactic_value list
  | RecordValue  of syntactic_value Assoc.t
      [@printer (fun fmt _ -> Format.fprintf fmt "<record-value>")]
  | Location     of StoreID.t
  | MathValue    of math list
  | Context      of input_context
  | CodeValue    of code_value

  | EvaluatedEnvironment of environment

(* -- for the naive interpreter, i.e. 'evaluator.cppo.ml' -- *)
  | Closure          of EvalVarID.t list * pattern_branch * environment
  | PrimitiveClosure of pattern_branch * environment * int * (abstract_tree list -> abstract_tree)
  | InputHorzClosure of intermediate_input_horz_element list * environment
  | InputVertClosure of intermediate_input_vert_element list * environment

(* -- for the SECD machine, i.e. 'vm.cppo.ml' -- *)
  | CompiledClosure          of varloc list * int * syntactic_value list * int * instruction list * vmenv
  | CompiledPrimitiveClosure of int * syntactic_value list * int * instruction list * vmenv * (abstract_tree list -> abstract_tree)
  | CompiledInputHorzClosure of compiled_intermediate_input_horz_element list * vmenv
  | CompiledInputVertClosure of compiled_intermediate_input_vert_element list * vmenv

and abstract_tree =
  | ASTBaseConstant       of base_constant
  | ASTEndOfList
  | ASTMath               of math list
  | FinishHeaderFile
  | FinishStruct
(* -- input texts -- *)
  | InputHorz             of input_horz_element list
  | InputVert             of input_vert_element list
(* -- graphics -- *)
  | Path                  of abstract_tree * (abstract_tree path_component) list * (unit path_component) option
(* -- record value -- *)
  | Record                of abstract_tree Assoc.t
      [@printer (fun fmt _ -> Format.fprintf fmt "Record(...)")]
  | AccessField           of abstract_tree * field_name
  | UpdateField           of abstract_tree * field_name * abstract_tree
(* -- fundamental -- *)
  | LetRecIn              of letrec_binding list * abstract_tree
  | LetNonRecIn           of pattern_tree * abstract_tree * abstract_tree
  | ContentOf             of Range.t * EvalVarID.t
  | IfThenElse            of abstract_tree * abstract_tree * abstract_tree
  | Function              of EvalVarID.t list * pattern_branch
  | Apply                 of abstract_tree * abstract_tree
  | ApplyOptional         of abstract_tree * abstract_tree
  | ApplyOmission         of abstract_tree
(* -- pattern match -- *)
  | PatternMatch          of Range.t * abstract_tree * pattern_branch list
  | NonValueConstructor   of constructor_name * abstract_tree
(* -- imperative -- *)
  | LetMutableIn          of EvalVarID.t * abstract_tree * abstract_tree
  | Dereference           of abstract_tree
  | Sequential            of abstract_tree * abstract_tree
  | WhileDo               of abstract_tree * abstract_tree
  | Overwrite             of EvalVarID.t * abstract_tree
(* -- module system -- *)
  | Module                of abstract_tree * abstract_tree
  | BackendMathList             of abstract_tree list
  | PrimitiveTuple        of abstract_tree list
(* -- staging constructs -- *)
  | Next                  of abstract_tree
  | Prev                  of abstract_tree
#include "__attype.gen.ml"

and input_horz_element = abstract_tree input_horz_element_scheme

and input_vert_element = abstract_tree input_vert_element_scheme

and 'a path_component = ('a, abstract_tree) path_component_scheme

and pattern_branch =
  | PatternBranch      of pattern_tree * abstract_tree
  | PatternBranchWhen  of pattern_tree * abstract_tree * abstract_tree

and pattern_tree =
  | PUnitConstant
  | PBooleanConstant      of bool
  | PIntegerConstant      of int
  | PStringConstant       of string
  | PListCons             of pattern_tree * pattern_tree
  | PEndOfList
  | PTuple                of pattern_tree list
  | PWildCard
  | PVariable             of EvalVarID.t
  | PAsVariable           of EvalVarID.t * pattern_tree
  | PConstructor          of constructor_name * pattern_tree

and input_context = HorzBox.context_main * context_sub

and context_sub = {
  math_command : math_command_func;
  dummy : unit;
}

and math_command_func =
  | MathCommand of syntactic_value
      (* (input_context -> math list -> HorzBox.horz_box list) *)

and math_element_main =
  | MathChar         of bool * Uchar.t list
      [@printer (fun fmt _ -> Format.fprintf fmt "<math-char>")]
      (* --
         (1) whether it is a big operator
         (2) Unicode code point (currently singular)
         -- *)
  | MathCharWithKern of bool * Uchar.t list * HorzBox.math_char_kern_func * HorzBox.math_char_kern_func
      [@printer (fun fmt _ -> Format.fprintf fmt "<math-char'>")]
      (* --
         (1) whether it is a big operator
         (2) Unicode code point (currently singular)
         (3) left-hand-side kerning function
         (4) right-hand-side kerning function
         --*)
  | MathEmbeddedText of (input_context -> HorzBox.horz_box list)

and math_element =
  | MathElement           of HorzBox.math_kind * math_element_main
  | MathVariantChar       of string
  | MathVariantCharDirect of HorzBox.math_kind * bool * HorzBox.math_variant_style
      [@printer (fun fmt _ -> Format.fprintf fmt "<math-variant-char-direct>")]
      (* --
         (1) math class
         (2) whether it is a big operator
         (3) Unicode code point for all math_char_class
         -- *)

and math_context_change =
  | MathChangeColor         of color
  | MathChangeMathCharClass of HorzBox.math_char_class

and math =
  | MathPure              of math_element
  | MathPullInScripts     of HorzBox.math_kind * HorzBox.math_kind * ((math list) option -> (math list) option -> math list)
  | MathChangeContext     of math_context_change * math list
  | MathGroup             of HorzBox.math_kind * HorzBox.math_kind * math list
  | MathSubscript         of math list * math list
  | MathSuperscript       of math list * math list
  | MathFraction          of math list * math list
  | MathRadicalWithDegree of math list * math list
  | MathRadical           of HorzBox.radical * math list
  | MathParen             of HorzBox.paren * HorzBox.paren * math list
  | MathParenWithMiddle   of HorzBox.paren * HorzBox.paren * HorzBox.paren * (math list) list
  | MathUpperLimit        of math list * math list
  | MathLowerLimit        of math list * math list

and code_value =
  | CdBaseConstant  of base_constant
  | CdEndOfList
  | CdMath          of math list
  | CdFinishHeaderFile
  | CdFinishStruct
  | CdInputHorz     of code_input_horz_element list
  | CdInputVert     of code_input_vert_element list
  | CdContentOf     of Range.t * EvalVarID.t
  | CdLetRecIn      of code_letrec_binding list * code_value
  | CdLetNonRecIn   of pattern_tree * code_value * code_value
  | CdFunction      of EvalVarID.t list * code_pattern_branch
  | CdApply         of code_value * code_value
  | CdApplyOptional of code_value * code_value
  | CdApplyOmission of code_value
  | CdIfThenElse    of code_value * code_value * code_value
  | CdRecord        of code_value Assoc.t
      [@printer (fun fmt _ -> Format.fprintf fmt "CdRecord(...)")]
  | CdAccessField   of code_value * field_name
  | CdUpdateField   of code_value * field_name * code_value
  | CdLetMutableIn  of EvalVarID.t * code_value * code_value
  | CdSequential    of code_value * code_value
  | CdOverwrite     of EvalVarID.t * code_value
  | CdWhileDo       of code_value * code_value
  | CdDereference   of code_value
  | CdPatternMatch  of Range.t * code_value * code_pattern_branch list
  | CdConstructor   of constructor_name * code_value
  | CdTuple         of code_value list
  | CdPath          of code_value * (code_value code_path_component) list * (unit code_path_component) option
  | CdMathList      of code_value list
  | CdModule        of code_value * code_value
#include "__codetype.gen.ml"

and code_input_horz_element = code_value input_horz_element_scheme

and code_input_vert_element = code_value input_vert_element_scheme

and 'a code_path_component = ('a, code_value) path_component_scheme

and code_letrec_binding =
  | CdLetRecBinding of EvalVarID.t * code_pattern_branch

and code_pattern_branch =
  | CdPatternBranch     of pattern_tree * code_value
  | CdPatternBranchWhen of pattern_tree * code_value * code_value
[@@deriving show { with_path = false; }]


let get_range (rng, _) = rng


let overwrite_range_of_type ((_, tymain) : mono_type) (rng : Range.t) = (rng, tymain)


let lift_argument_type f = function
  | MandatoryArgumentType(ty) -> MandatoryArgumentType(f ty)
  | OptionalArgumentType(ty)  -> OptionalArgumentType(f ty)


let lift_manual_common f = function
  | MMandatoryArgumentType(mnty) -> f mnty
  | MOptionalArgumentType(mnty)  -> f mnty


let rec unlink ((_, tymain) as ty) =
  match tymain with
  | TypeVariable({contents = MonoLink(ty)}) -> unlink ty
  | _                                       -> ty


let rec erase_range_of_type (ty : mono_type) : mono_type =
  let iter = erase_range_of_type in
  let rng = Range.dummy "erased" in
  let (_, tymain) = ty in
    match tymain with
    | TypeVariable(tvref) ->
        begin
          match !tvref with
          | MonoFree(tvid) -> FreeID.update_kind erase_range_of_kind tvid; (rng, tymain)
          | MonoLink(ty)   -> erase_range_of_type ty
        end

    | BaseType(_)                       -> (rng, tymain)
    | FuncType(optrow, tydom, tycod)    -> (rng, FuncType(erase_range_of_option_row optrow, iter tydom, iter tycod))
    | ProductType(tylist)               -> (rng, ProductType(List.map iter tylist))
    | RecordType(tyasc)                 -> (rng, RecordType(Assoc.map_value iter tyasc))
    | SynonymType(tylist, tyid, tyreal) -> (rng, SynonymType(List.map iter tylist, tyid, iter tyreal))
    | VariantType(tylist, tyid)         -> (rng, VariantType(List.map iter tylist, tyid))
    | ListType(tycont)                  -> (rng, ListType(iter tycont))
    | RefType(tycont)                   -> (rng, RefType(iter tycont))
    | HorzCommandType(tylist)           -> (rng, HorzCommandType(List.map (lift_argument_type iter) tylist))
    | VertCommandType(tylist)           -> (rng, VertCommandType(List.map (lift_argument_type iter) tylist))
    | MathCommandType(tylist)           -> (rng, MathCommandType(List.map (lift_argument_type iter) tylist))
    | CodeType(tysub)                   -> (rng, CodeType(iter tysub))


and erase_range_of_kind (kd : mono_kind) =
  match kd with
  | UniversalKind   -> UniversalKind
  | RecordKind(asc) -> RecordKind(Assoc.map_value erase_range_of_type asc)


and erase_range_of_option_row (optrow : mono_option_row) =
  match optrow with
  | OptionRowEmpty          -> optrow
  | OptionRowCons(ty, tail) -> OptionRowCons(erase_range_of_type ty, erase_range_of_option_row tail)
  | OptionRowVariable({contents = MonoORLink(optrow)}) -> erase_range_of_option_row optrow
  | OptionRowVariable({contents = MonoORFree(_)})      -> optrow


module BoundIDHashTable = Hashtbl.Make(
  struct
    type t = BoundID.t
    let equal = BoundID.eq
    let hash = Hashtbl.hash
  end)

module FreeIDHashTable = Hashtbl.Make(
  struct
    type t = FreeID.t
    let equal = FreeID.equal
    let hash = Hashtbl.hash
  end)


let rec instantiate_aux bid_ht lev qtfbl (rng, ptymain) =
  let aux = instantiate_aux bid_ht lev qtfbl in
  let aux_or = instantiate_option_row_aux bid_ht lev qtfbl in
    match ptymain with
    | TypeVariable(ptvi) ->
        begin
          match ptvi with
          | PolyFree(tvref) ->
              (rng, TypeVariable(tvref))

          | PolyBound(bid) ->
              begin
                match BoundIDHashTable.find_opt bid_ht bid with
                | Some(tvrefnew) ->
                    (rng, TypeVariable(tvrefnew))

                | None ->
                    let kd = BoundID.get_kind bid in
                    let kdfree = instantiate_kind_aux bid_ht lev qtfbl kd in
                    let tvid = FreeID.fresh kdfree qtfbl lev () in
                    let tvref = ref (MonoFree(tvid)) in
                    begin
                      BoundIDHashTable.add bid_ht bid tvref;
                      (rng, TypeVariable(tvref))
                    end
              end
        end
    | FuncType(optrow, tydom, tycod)    -> (rng, FuncType(aux_or optrow, aux tydom, aux tycod))
    | ProductType(tylist)               -> (rng, ProductType(List.map aux tylist))
    | RecordType(tyasc)                 -> (rng, RecordType(Assoc.map_value aux tyasc))
    | SynonymType(tylist, tyid, tyreal) -> (rng, SynonymType(List.map aux tylist, tyid, aux tyreal))
    | VariantType(tylist, tyid)         -> (rng, VariantType(List.map aux tylist, tyid))
    | ListType(tysub)                   -> (rng, ListType(aux tysub))
    | RefType(tysub)                    -> (rng, RefType(aux tysub))
    | BaseType(bty)                     -> (rng, BaseType(bty))
    | HorzCommandType(tylist)           -> (rng, HorzCommandType(List.map (lift_argument_type aux) tylist))
    | VertCommandType(tylist)           -> (rng, VertCommandType(List.map (lift_argument_type aux) tylist))
    | MathCommandType(tylist)           -> (rng, MathCommandType(List.map (lift_argument_type aux) tylist))
    | CodeType(tysub)                   -> (rng, CodeType(aux tysub))


and instantiate_kind_aux bid_ht lev qtfbl (kd : poly_kind) : mono_kind =
  let aux = instantiate_aux bid_ht lev qtfbl in
    match kd with
    | UniversalKind     -> UniversalKind
    | RecordKind(tyasc) -> RecordKind(Assoc.map_value aux tyasc)


and instantiate_option_row_aux bid_ht lev qtfbl optrow : mono_option_row =
  let aux = instantiate_aux bid_ht lev qtfbl in
  let aux_or = instantiate_option_row_aux bid_ht lev qtfbl in
    match optrow with
    | OptionRowEmpty                         -> OptionRowEmpty
    | OptionRowCons(pty, tail)               -> OptionRowCons(aux pty, aux_or tail)
    | OptionRowVariable(PolyORFree(orviref)) -> OptionRowVariable(orviref)


let instantiate (lev : level) (qtfbl : quantifiability) ((Poly(pty)) : poly_type) : mono_type =
  let bid_ht : (mono_type_variable_info ref) BoundIDHashTable.t = BoundIDHashTable.create 32 in
  instantiate_aux bid_ht lev qtfbl pty


let instantiate_kind (lev : level) (qtfbl : quantifiability) (pkd : poly_kind) : mono_kind =
  let bid_ht : (mono_type_variable_info ref) BoundIDHashTable.t = BoundIDHashTable.create 32 in
  instantiate_kind_aux bid_ht lev qtfbl pkd


let lift_poly_general (ptv : FreeID.t -> bool) (porv : OptionRowVarID.t -> bool) (ty : mono_type) : poly_type_body =
  let tvidht = FreeIDHashTable.create 32 in
  let rec iter (rng, tymain) =
    match tymain with
    | TypeVariable(tvref) ->
        begin
          match !tvref with
          | MonoLink(tyl) ->
              iter tyl

          | MonoFree(tvid) ->
              let ptvi =
                if not (ptv tvid) then
                  PolyFree(tvref)
                else
                  begin
                    match FreeIDHashTable.find_opt tvidht tvid with
                    | Some(bid) ->
                        PolyBound(bid)

                    | None ->
                        let kd = FreeID.get_kind tvid in
                        let kdgen = generalize_kind kd in
                        let bid = BoundID.fresh kdgen () in
                        FreeIDHashTable.add tvidht tvid bid;
                        PolyBound(bid)
                  end
              in
                (rng, TypeVariable(ptvi))
        end

    | FuncType(optrow, tydom, tycod)    -> (rng, FuncType(generalize_option_row optrow, iter tydom, iter tycod))
    | ProductType(tylist)               -> (rng, ProductType(List.map iter tylist))
    | RecordType(tyasc)                 -> (rng, RecordType(Assoc.map_value iter tyasc))
    | SynonymType(tylist, tyid, tyreal) -> (rng, SynonymType(List.map iter tylist, tyid, iter tyreal))
    | VariantType(tylist, tyid)         -> (rng, VariantType(List.map iter tylist, tyid))
    | ListType(tysub)                   -> (rng, ListType(iter tysub))
    | RefType(tysub)                    -> (rng, RefType(iter tysub))
    | BaseType(bty)                     -> (rng, BaseType(bty))
    | HorzCommandType(tylist)           -> (rng, HorzCommandType(List.map (lift_argument_type iter) tylist))
    | VertCommandType(tylist)           -> (rng, VertCommandType(List.map (lift_argument_type iter) tylist))
    | MathCommandType(tylist)           -> (rng, MathCommandType(List.map (lift_argument_type iter) tylist))
    | CodeType(tysub)                   -> (rng, CodeType(iter tysub))

  and generalize_kind kd =
    match kd with
    | UniversalKind     -> UniversalKind
    | RecordKind(tyasc) -> RecordKind(Assoc.map_value iter tyasc)

  and generalize_option_row optrow =
    match optrow with
    | OptionRowEmpty             -> OptionRowEmpty
    | OptionRowCons(ty, tail)    -> OptionRowCons(iter ty, generalize_option_row tail)

    | OptionRowVariable(orviref) ->
        begin
          match !orviref with
          | MonoORFree(orv) ->
              if porv orv then
                OptionRowEmpty
              else
                OptionRowVariable(PolyORFree(orviref))

          | MonoORLink(optraw) ->
              generalize_option_row optrow
        end
  in
  iter ty


let check_level lev (ty : mono_type) =
  let rec iter (_, tymain) =
    match tymain with
    | TypeVariable(tvref) ->
        begin
          match !tvref with
          | MonoLink(ty) -> iter ty
          | MonoFree(tvid) -> Level.less_than lev (FreeID.get_level tvid)
        end

    | ProductType(tylst)             -> List.for_all iter tylst
    | RecordType(tyasc)              -> Assoc.fold_value (fun b ty -> b && iter ty) true tyasc
    | FuncType(optrow, tydom, tycod) -> iter_or optrow && iter tydom && iter tycod
    | RefType(tycont)                -> iter tycont
    | BaseType(_)                    -> true
    | ListType(tycont)               -> iter tycont
    | VariantType(tylst, _)          -> List.for_all iter tylst
    | SynonymType(tylst, _, tyact)   -> List.for_all iter tylst && iter tyact

    | HorzCommandType(cmdargtylst)
    | VertCommandType(cmdargtylst)
    | MathCommandType(cmdargtylst)
      ->
        List.for_all iter_cmd cmdargtylst

    | CodeType(tysub)                -> iter tysub

  and iter_cmd = function
    | MandatoryArgumentType(ty) -> iter ty
    | OptionalArgumentType(ty)  -> iter ty

  and iter_or = function
    | OptionRowEmpty -> true

    | OptionRowCons(ty, tail) -> iter ty && iter_or tail

    | OptionRowVariable(orviref) ->
        begin
          match !orviref with
          | MonoORFree(orv)    -> Level.less_than lev (OptionRowVarID.get_level orv)
          | MonoORLink(optrow) -> iter_or optrow
        end

  in
  iter ty


let generalize (lev : level) (ty : mono_type) =
  let ptv tvid =
    let bkd =
      let kd = FreeID.get_kind tvid in
      match kd with
      | UniversalKind   -> true
      | RecordKind(asc) -> Assoc.fold_value (fun b ty -> b && (check_level lev ty)) true asc
    in
    FreeID.is_quantifiable tvid && Level.less_than lev (FreeID.get_level tvid) && bkd
  in
  let porv orv =
    not (Level.less_than lev (OptionRowVarID.get_level orv))
  in
  Poly(lift_poly_general ptv porv ty)


let lift_poly_body =
  lift_poly_general (fun _ -> false) (fun _ -> false)


let lift_poly (ty : mono_type) : poly_type =
  Poly(lift_poly_body ty)


let rec unlift_aux pty =
  let aux = unlift_aux in
  let (rng, ptymain) = pty in
  let ptymainnew =
    match ptymain with
    | BaseType(bt) -> BaseType(bt)

    | TypeVariable(ptvi) ->
        begin
          match ptvi with
          | PolyFree(tvref) -> TypeVariable(tvref)
          | PolyBound(_)    -> raise Exit
        end

    | FuncType(optrow, pty1, pty2)    -> FuncType(unlift_aux_or optrow, aux pty1, aux pty2)
    | ProductType(ptylst)             -> ProductType(List.map aux ptylst)
    | RecordType(ptyasc)              -> RecordType(Assoc.map_value aux ptyasc)
    | ListType(ptysub)                -> ListType(aux ptysub)
    | RefType(ptysub)                 -> RefType(aux ptysub)
    | VariantType(ptylst, tyid)       -> VariantType(List.map aux ptylst, tyid)
    | SynonymType(ptylst, tyid, ptya) -> SynonymType(List.map aux ptylst, tyid, aux ptya)
    | HorzCommandType(catyl)          -> HorzCommandType(List.map unlift_aux_cmd catyl)
    | VertCommandType(catyl)          -> VertCommandType(List.map unlift_aux_cmd catyl)
    | MathCommandType(catyl)          -> MathCommandType(List.map unlift_aux_cmd catyl)
    | CodeType(ptysub)                -> CodeType(aux ptysub)
  in
  (rng, ptymainnew)


and unlift_aux_cmd = function
  | MandatoryArgumentType(pty) -> MandatoryArgumentType(unlift_aux pty)
  | OptionalArgumentType(pty)  -> OptionalArgumentType(unlift_aux pty)


and unlift_aux_or = function
  | OptionRowEmpty                         -> OptionRowEmpty
  | OptionRowCons(pty, tail)               -> OptionRowCons(unlift_aux pty, unlift_aux_or tail)
  | OptionRowVariable(PolyORFree(orviref)) -> OptionRowVariable(orviref)


let unlift_poly (pty : poly_type_body) : mono_type option =
  try Some(unlift_aux pty) with
  | Exit -> None


let unlift_option_row poptrow =
  try Some(unlift_aux_or poptrow) with
  | Exit -> None


let add_to_environment (env : environment) (evid : EvalVarID.t) (rfast : location) : environment =
  let (valenv, stenvref) = env in
    (valenv |> EvalVarIDMap.add evid rfast, stenvref)


let find_in_environment (env : environment) (evid : EvalVarID.t) : location option =
  let (valenv, _) = env in
    valenv |> EvalVarIDMap.find_opt evid


let register_location (env : environment) (value : syntactic_value) : StoreID.t =
  let (_, stenvref) = env in
  let stid = StoreID.fresh () in
  StoreIDHashTable.add (!stenvref) stid value;
  stid


let update_location (env : environment) (stid : StoreID.t) (value : syntactic_value) : unit =
  let (_, stenvref) = env in
  let stenv = !stenvref in
  if StoreIDHashTable.mem stenv stid then
    StoreIDHashTable.replace stenv stid value
  else
    assert false


let find_location_value (env : environment) (stid : StoreID.t) : syntactic_value option =
  let (_, stenvref) = env in
  StoreIDHashTable.find_opt (!stenvref) stid


let map_input_horz f ihlst =
  ihlst |> List.map (function
  | InputHorzText(s)           -> InputHorzText(s)
  | InputHorzEmbedded(ast)     -> InputHorzEmbedded(f ast)
  | InputHorzContent(ast)      -> InputHorzContent(f ast)
  | InputHorzEmbeddedMath(ast) -> InputHorzEmbeddedMath(f ast)
  )


let map_input_vert f ivlst =
  ivlst |> List.map (function
  | InputVertContent(ast)  -> InputVertContent(f ast)
  | InputVertEmbedded(ast) -> InputVertEmbedded(f ast)
  )


let map_path_component f g = function
  | PathLineTo(ast) ->
      PathLineTo(g ast)

  | PathCubicBezierTo(ast1, ast2, ast3) ->
      let v1 = f ast1 in
      let v2 = f ast2 in
      let v3 = g ast3 in
      PathCubicBezierTo(v1, v2, v3)


let rec unlift_code (code : code_value) : abstract_tree =
  let rec aux code =
    match code with
    | CdBaseConstant(bc)                   -> ASTBaseConstant(bc)
    | CdEndOfList                          -> ASTEndOfList
    | CdMath(mlst)                         -> ASTMath(mlst)
    | CdFinishHeaderFile                   -> FinishHeaderFile
    | CdFinishStruct                       -> FinishStruct
    | CdInputHorz(cdihlst)                 -> InputHorz(cdihlst |> map_input_horz aux)
    | CdInputVert(cdivlst)                 -> InputVert(cdivlst |> map_input_vert aux)
    | CdContentOf(rng, evid)               -> ContentOf(rng, evid)
    | CdLetRecIn(cdrecbinds, code1)        -> LetRecIn(List.map aux_letrec_binding cdrecbinds, aux code1)
    | CdLetNonRecIn(pat, code1, code2)     -> LetNonRecIn(pat, aux code1, aux code2)
    | CdFunction(evids, cdpatbr)           -> Function(evids, aux_pattern_branch cdpatbr)
    | CdApply(code1, code2)                -> Apply(aux code1, aux code2)
    | CdApplyOptional(code1, code2)        -> ApplyOptional(aux code1, aux code2)
    | CdApplyOmission(code1)               -> ApplyOmission(aux code1)
    | CdIfThenElse(code1, code2, code3)    -> IfThenElse(aux code1, aux code2, aux code3)
    | CdRecord(cdasc)                      -> Record(Assoc.map_value aux cdasc)
    | CdAccessField(code1, fldnm)          -> AccessField(aux code1, fldnm)
    | CdUpdateField(code1, fldnm, code2)   -> UpdateField(aux code1, fldnm, aux code2)
    | CdLetMutableIn(evid, code1, code2)   -> LetMutableIn(evid, aux code1, aux code2)
    | CdSequential(code1, code2)           -> Sequential(aux code1, aux code2)
    | CdOverwrite(evid, code1)             -> Overwrite(evid, aux code1)
    | CdDereference(code1)                 -> Dereference(aux code1)
    | CdWhileDo(code1, code2)              -> WhileDo(aux code1, aux code2)
    | CdPatternMatch(rng, code1, cdpatbrs) -> PatternMatch(rng, aux code1, List.map aux_pattern_branch cdpatbrs)
    | CdConstructor(constrnm, code1)       -> NonValueConstructor(constrnm, aux code1)
    | CdTuple(codelst)                     -> PrimitiveTuple(List.map aux codelst)
    | CdPath(code1, cdpath, cdcycleopt)    -> Path(aux code1, aux_path cdpath, aux_cycle cdcycleopt)
    | CdMathList(codes)                    -> BackendMathList(List.map aux codes)
    | CdModule(code1, code2)               -> Module(aux code1, aux code2)
#include "__unliftcode.gen.ml"

  and aux_letrec_binding (CdLetRecBinding(evid, cdpatbr)) =
    LetRecBinding(evid, aux_pattern_branch cdpatbr)

  and aux_pattern_branch = function
    | CdPatternBranch(pat, code)            -> PatternBranch(pat, aux code)
    | CdPatternBranchWhen(pat, code, codeB) -> PatternBranchWhen(pat, aux code, aux codeB)

  and aux_path cdpath =
    List.map (map_path_component aux aux) cdpath

  and aux_cycle cdcycleopt =
    cdcycleopt |> BatOption.map (map_path_component aux (fun () -> ()))

  in
  aux code


module MathContext
: sig
    type t
    val make : input_context -> t
    val context_for_text : t -> input_context
    val context_main : t -> HorzBox.context_main
    val convert_math_variant_char : input_context -> string -> HorzBox.math_kind * Uchar.t list
    val color : t -> color
    val set_color : color -> t -> t
    val enter_script : t -> t
    val math_char_class : t -> HorzBox.math_char_class
    val set_math_char_class : HorzBox.math_char_class -> t -> t
    val is_in_base_level : t -> bool
    val actual_font_size : t -> (HorzBox.math_font_abbrev -> FontFormat.math_decoder) -> length
    val base_font_size : t -> length
    val math_font_abbrev : t -> HorzBox.math_font_abbrev
  end
= struct
    type level =
      | BaseLevel
      | ScriptLevel
      | ScriptScriptLevel

    type t =
      {
        mc_font_abbrev    : HorzBox.math_font_abbrev;
        mc_base_font_size : length;
        mc_level_int      : int;
        mc_level          : level;
        context_for_text  : input_context;
      }

    let make (ictx : input_context) : t =
      let (ctx, _) = ictx in
        {
          mc_font_abbrev    = ctx.HorzBox.math_font;
          mc_base_font_size = ctx.HorzBox.font_size;
          mc_level_int      = 0;
          mc_level          = BaseLevel;
          context_for_text  = ictx;
        }

    let convert_math_variant_char ((ctx, _) : input_context) (s : string) =
      let open HorzBox in
      let mcclsmap = ctx.math_variant_char_map in
      let mccls = ctx.math_char_class in
      let mkmap = ctx.math_class_map in
        match mkmap |> MathClassMap.find_opt s with
        | Some(uchlstaft, mk) ->
            (mk, uchlstaft)

        | None ->
            let uchlst = InternalText.to_uchar_list (InternalText.of_utf8 s) in
            let uchlstaft =
              uchlst |> List.map (fun uch ->
                match mcclsmap |> HorzBox.MathVariantCharMap.find_opt (uch, mccls) with
                | Some(uchaft) -> uchaft
                | None         -> uch
              )
            in
              (MathOrdinary, uchlstaft)

    let context_for_text (mctx : t) =
      mctx.context_for_text
        (* temporary; maybe should update font size *)

    let context_main (mctx : t) =
      let (ctx, _) = mctx.context_for_text in
        ctx

    let color (mctx : t) =
      let (ctx, _) = mctx.context_for_text in
        ctx.HorzBox.text_color

    let set_color (color : color) (mctx : t) =
      let (ctx, v) = mctx.context_for_text in
      let ctxnew = { ctx with HorzBox.text_color = color; } in
        { mctx with context_for_text = (ctxnew, v); }

    let math_char_class (mctx : t) =
      let (ctx, _) = mctx.context_for_text in
        ctx.HorzBox.math_char_class

    let set_math_char_class mccls (mctx : t) =
      let (ctx, v) = mctx.context_for_text in
      let ctxnew = { ctx with HorzBox.math_char_class = mccls } in
        { mctx with context_for_text = (ctxnew, v) }

    let enter_script mctx =
      let levnew = mctx.mc_level_int + 1 in
      match mctx.mc_level with
      | BaseLevel         -> { mctx with mc_level = ScriptLevel;       mc_level_int = levnew; }
      | ScriptLevel       -> { mctx with mc_level = ScriptScriptLevel; mc_level_int = levnew; }
      | ScriptScriptLevel -> { mctx with                               mc_level_int = levnew; }

    let is_in_base_level mctx =
      match mctx.mc_level with
      | BaseLevel -> true
      | _         -> false

    let actual_font_size mctx (mdf : HorzBox.math_font_abbrev -> FontFormat.math_decoder) =
      let bfsize = mctx.mc_base_font_size in
      let md = mdf mctx.mc_font_abbrev in
      let mc = FontFormat.get_math_constants md in
      match mctx.mc_level with
      | BaseLevel         -> bfsize
      | ScriptLevel       -> bfsize *% mc.FontFormat.script_scale_down
      | ScriptScriptLevel -> bfsize *% mc.FontFormat.script_script_scale_down

    let base_font_size mctx =
      mctx.mc_base_font_size

    let math_font_abbrev mctx =
      mctx.mc_font_abbrev

  end


type math_context = MathContext.t


(* -- following are all for debugging -- *)

let string_of_record_type (type a) (type b) (f : (a, b) typ -> string) (asc : ((a, b) typ) Assoc.t) =
  let rec aux lst =
    match lst with
    | []                     -> " -- "
    | (fldnm, tystr) :: []   -> fldnm ^ " : " ^ (f tystr)
    | (fldnm, tystr) :: tail -> fldnm ^ " : " ^ (f tystr) ^ "; " ^ (aux tail)
  in
    "(|" ^ (aux (Assoc.to_list asc)) ^ "|)"


let string_of_kind (type a) (type b) (f : (a, b) typ -> string) (kdstr : (a, b) kind) =
  let rec aux lst =
    match lst with
    | []                     -> " -- "
    | (fldnm, tystr) :: []   -> fldnm ^ " : " ^ (f tystr)
    | (fldnm, tystr) :: tail -> fldnm ^ " : " ^ (f tystr) ^ "; " ^ (aux tail)
  in
    match kdstr with
    | UniversalKind   -> "U"
    | RecordKind(asc) -> "(|" ^ (aux (Assoc.to_list asc)) ^ "|)"


let rec string_of_type_basic tvf orvf tystr : string =
  let iter = string_of_type_basic tvf orvf in
  let (rng, tymain) = tystr in
  let qstn = if Range.is_dummy rng then "%" else "" in
    match tymain with
    | BaseType(EnvType)     -> "env" ^ qstn
    | BaseType(UnitType)    -> "unit" ^ qstn
    | BaseType(BoolType)    -> "bool" ^ qstn
    | BaseType(IntType)     -> "int" ^ qstn
    | BaseType(FloatType)   -> "float" ^ qstn
    | BaseType(StringType)  -> "string" ^ qstn
    | BaseType(TextRowType) -> "inline-text" ^ qstn
    | BaseType(TextColType) -> "block-text" ^ qstn
    | BaseType(BoxRowType)  -> "inline-boxes" ^ qstn
    | BaseType(BoxColType)  -> "block-boxes" ^ qstn
    | BaseType(ContextType) -> "context" ^ qstn
    | BaseType(PrePathType) -> "pre-path" ^ qstn
    | BaseType(PathType)    -> "path" ^ qstn
    | BaseType(LengthType)  -> "length" ^ qstn
    | BaseType(GraphicsType) -> "graphics" ^ qstn
    | BaseType(ImageType)    -> "image" ^ qstn
    | BaseType(DocumentType) -> "document" ^ qstn
    | BaseType(MathType)     -> "math" ^ qstn
    | BaseType(RegExpType)   -> "regexp" ^ qstn
    | BaseType(TextInfoType) -> "text-info" ^ qstn

    | VariantType(tyarglist, tyid) ->
        (string_of_type_argument_list_basic tvf orvf tyarglist) ^ (TypeID.show_direct tyid) (* temporary *) ^ "@" ^ qstn

    | SynonymType(tyarglist, tyid, tyreal) ->
        (string_of_type_argument_list_basic tvf orvf tyarglist) ^ (TypeID.show_direct tyid) ^ "@ (= " ^ (iter tyreal) ^ ")"

    | FuncType(optrow, tydom, tycod) ->
        let stropts = string_of_option_row_basic tvf orvf optrow in
        let strdom = iter tydom in
        let strcod = iter tycod in
          stropts ^
          begin match tydom with
          | (_, FuncType(_, _, _)) -> "(" ^ strdom ^ ")"
          | _                      -> strdom
          end ^ " ->" ^ qstn ^ " " ^ strcod

    | ListType(tycont) ->
        let strcont = iter tycont in
        let (_, tycontmain) = tycont in
          begin match tycontmain with
          | FuncType(_, _, _)
          | ProductType(_)
          | ListType(_)
          | RefType(_)
          | VariantType(_ :: _, _)
(*          | TypeSynonym(_ :: _, _, _) *)
              -> "(" ^ strcont ^ ")"
          | _ -> strcont
          end ^ " list" ^ qstn

    | RefType(tycont) ->
        let strcont = iter tycont in
        let (_, tycontmain) = tycont in
          begin match tycontmain with
          | FuncType(_, _, _)
          | ProductType(_)
          | ListType(_)
          | RefType(_)
          | VariantType(_ :: _, _)
(*          | TypeSynonym(_ :: _, _, _) *)
              -> "(" ^ strcont ^ ")"

          | _ -> strcont
          end ^ " ref" ^ qstn

    | CodeType(tysub) ->
        let strsub = iter tysub in
        let (_, tysubmain) = tysub in
        "&" ^ begin
          match tysubmain with
          | BaseType(_)
          | VariantType([], _) -> strsub

          | _ -> "(" ^ strsub ^ ")"
        end

    | ProductType(tylist) ->
        string_of_type_list_basic tvf orvf tylist

    | TypeVariable(tvi) ->
        tvf qstn tvi

    | RecordType(asc) ->
        string_of_record_type iter asc

    | HorzCommandType(tylist) ->
        let slist = List.map (string_of_command_argument_type tvf orvf) tylist in
        "[" ^ (String.concat "; " slist) ^ "] horz-command"

    | VertCommandType(tylist)   ->
        let slist = List.map (string_of_command_argument_type tvf orvf) tylist in
        "[" ^ (String.concat "; " slist) ^ "] vert-command"

    | MathCommandType(tylist)   ->
        let slist = List.map (string_of_command_argument_type tvf orvf) tylist in
        "[" ^ (String.concat "; " slist) ^ "] math-command"


and string_of_option_row_basic tvf orvf = function
  | OptionRowEmpty -> ""

  | OptionRowVariable(orvi) -> orvf orvi

  | OptionRowCons(ty, tail) ->
      let strtysub = string_of_type_basic tvf orvf ty in
      let strty =
        let (_, tymain) = ty in
        match tymain with
        | FuncType(_, _, _)
        | ProductType(_)
        | ListType(_)
        | RefType(_)
        | VariantType(_ :: _, _)
          -> "(" ^ strtysub ^ ")"

        | _ -> strtysub
      in
      strty ^ "?-> " ^ (string_of_option_row_basic tvf orvf tail)


and string_of_command_argument_type tvf orvf = function
  | MandatoryArgumentType(ty) -> string_of_type_basic tvf orvf ty
  | OptionalArgumentType(ty)  -> "(" ^ (string_of_type_basic tvf orvf ty) ^ ")?"


and string_of_type_argument_list_basic tvf orvf tyarglist =
  match tyarglist with
  | []           -> ""
  | head :: tail ->
      let strhd = string_of_type_basic tvf orvf head in
      let strtl = string_of_type_argument_list_basic tvf orvf tail in
      let (_, headmain) = head in
        begin
          match headmain with
          | FuncType(_, _, _)
          | ProductType(_)
            (* | TypeSynonym(_ :: _, _, _) *) (* temporary *)
          | ListType(_)
          | RefType(_)
          | VariantType(_ :: _, _)
              -> "(" ^ strhd ^ ")"
          | _ -> strhd
        end ^ " " ^ strtl


and string_of_type_list_basic tvf orvf tylist =
  match tylist with
  | []           -> ""
  | head :: []   ->
      let strhd = string_of_type_basic tvf orvf head in
      let (_, headmain) = head in
        begin
          match headmain with
          | ProductType(_)
          | FuncType(_, _, _)
              -> "(" ^ strhd ^ ")"
          | _ -> strhd
        end
  | head :: tail ->
      let strhd = string_of_type_basic tvf orvf head in
      let strtl = string_of_type_list_basic tvf orvf tail in
      let (_, headmain) = head in
        begin
          match headmain with
          | ProductType(_)
          | FuncType(_, _, _)
              -> "(" ^ strhd ^ ")"
          | _ -> strhd
        end ^ " * " ^ strtl


let rec tvf_mono qstn tvref =
  match !tvref with
  | MonoLink(tyl)  -> "$(" ^ (string_of_mono_type_basic tyl) ^ ")"
  | MonoFree(tvid) -> "'" ^ (FreeID.show_direct (string_of_kind string_of_mono_type_basic) tvid) ^ qstn


and orvf_mono orvref =
  match !orvref with
  | MonoORFree(orv)    -> (OptionRowVarID.show_direct orv) ^ "?-> "
  | MonoORLink(optrow) -> string_of_option_row_basic tvf_mono orvf_mono optrow


and string_of_mono_type_basic ty =
    string_of_type_basic tvf_mono orvf_mono ty


let rec string_of_poly_type_basic (Poly(pty)) =
  let tvf_poly qstn ptvi =
    match ptvi with
    | PolyBound(bid) ->
        "'#" ^ (BoundID.show_direct (string_of_kind (fun ty -> string_of_poly_type_basic (Poly(ty)))) bid) ^ qstn

    | PolyFree(tvref) -> tvf_mono qstn tvref
  in
  let ortvf orvi =
    match orvi with
    | PolyORFree(orvref) -> orvf_mono orvref
  in
    string_of_type_basic tvf_poly ortvf pty


and string_of_kind_basic kd = string_of_kind string_of_mono_type_basic kd

(*
let rec string_of_manual_type (_, mtymain) =
  let iter = string_of_manual_type in
  let iter_cmd = string_of_manual_command_argument_type in
  match mtymain with
  | MTypeName(mtylst, mdlnmlst, tynm) -> (String.concat " " (List.map iter mtylst)) ^ " " ^ (String.concat "." (List.append mdlnmlst [tynm]))
  | MTypeParam(tpnm)          -> "'" ^ tpnm
  | MFuncType(mtyopts, mtydom, mtycod) -> (String.concat "" (List.map iter mtyopts)) (iter mtydom) ^ " -> " ^ (iter mtycod)
  | MOptFuncType(mtydom, mtycod) -> "(" ^ (iter mtydom) ^ ")? -> " ^ (iter mtycod)
  | MProductType(mtylst)      -> (String.concat " * " (List.map iter mtylst))
  | MRecordType(mtyasc)       -> "(|" ^ (String.concat "; " (List.map (fun (fldnm, mty) -> fldnm ^ " : " ^ (iter mty)) (Assoc.to_list mtyasc))) ^ "|)"
  | MHorzCommandType(mncatylst) -> "[" ^ (String.concat "; " (List.map iter_cmd mncatylst)) ^ "] inline-cmd"
  | MVertCommandType(mncatylst) -> "[" ^ (String.concat "; " (List.map iter_cmd mncatylst)) ^ "] block-cmd"
  | MMathCommandType(mncatylst) -> "[" ^ (String.concat "; " (List.map iter_cmd mncatylst)) ^ "] math-cmd"


and string_of_manual_command_argument_type = function
  | MMandatoryArgumentType(mnty) -> string_of_manual_type mnty
  | MOptionalArgumentType(mnty)  -> "(" ^ (string_of_manual_type mnty) ^ ")?"
*)
