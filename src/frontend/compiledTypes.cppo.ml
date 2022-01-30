
open SyntaxBase
open Types


type compiled_value =
  | CVNil
  | CVBaseConstant of base_constant
  | CVConstructor  of constructor_name * compiled_value
  | CVList         of compiled_value list
  | CVTuple        of compiled_value list
  | CVRecordValue  of compiled_value LabelMap.t
      [@printer (fun fmt _ -> Format.fprintf fmt "<record-value>")]
  | CVLocation     of StoreID.t
  | CVMathValue    of math list
  | CVContext      of input_context
  | CVCodeValue    of code_value
  | CVCodeSymbol   of CodeSymbol.t

(* -- for the SECD machine, i.e. 'vm.cppo.ml' -- *)
  | CompiledClosure          of varloc LabelMap.t * int * compiled_value list * int * instruction list * vmenv
  | CompiledPrimitiveClosure of int * compiled_value list * int * instruction list * vmenv * (abstract_tree list -> abstract_tree)
  | CompiledInputHorzClosure of compiled_intermediate_input_horz_element list * vmenv
  | CompiledInputVertClosure of compiled_intermediate_input_vert_element list * vmenv

and compiled_environment =
  compiled_location EvalVarIDMap.t * (compiled_value StoreIDHashTable.t) ref
    [@printer (fun fmt _ -> Format.fprintf fmt "<c-env>")]

and compiled_location =
  compiled_value ref

and vmenv =
  compiled_environment * (compiled_value array) list

and compiled_input_horz_element =
  | CompiledInputHorzText         of string
  | CompiledInputHorzEmbedded     of instruction list
  | CompiledInputHorzContent      of instruction list
  | CompiledInputHorzEmbeddedMath of instruction list
  | CompiledInputHorzEmbeddedCodeText of string

and compiled_intermediate_input_horz_element =
  | CompiledImInputHorzText         of string
  | CompiledImInputHorzEmbedded     of instruction list
  | CompiledImInputHorzContent of compiled_intermediate_input_horz_element list * vmenv
  | CompiledImInputHorzEmbeddedMath of instruction list
  | CompiledImInputHorzEmbeddedCodeText of string

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
  | IRInputHorzEmbeddedCodeText of string

and ir_input_vert_element =
  | IRInputVertEmbedded of ir
  | IRInputVertContent  of ir

and varloc =
  | GlobalVar of compiled_location * EvalVarID.t * int ref
  | LocalVar  of int * int * EvalVarID.t * int ref

and ir =
  | IRConstant              of compiled_value
  | IRTerminal
  | IRInputHorz             of ir_input_horz_element list
  | IRInputVert             of ir_input_vert_element list
  | IRRecord                of label list * ir list
  | IRAccessField           of ir * label
  | IRUpdateField           of ir * label * ir
  | IRLetRecIn              of (varloc * ir) list * ir
  | IRLetNonRecIn           of ir * ir_pattern_tree * ir
  | IRContentOf             of varloc
  | IRPersistent            of varloc
  | IRSymbolOf              of varloc
  | IRIfThenElse            of ir * ir * ir
  | IRFunction              of int * varloc LabelMap.t * ir_pattern_tree list * ir
  | IRApply                 of int * ir * ir list
  | IRApplyPrimitive        of instruction * int * ir list
  | IRApplyOptional         of ir * ir
  | IRApplyOmission         of ir
  | IRTuple                 of int * ir list
  | IRPatternMatch          of Range.t * ir * ir_pattern_branch list
  | IRNonValueConstructor   of constructor_name * ir
  | IRLetMutableIn          of varloc * ir * ir
  | IROverwrite             of varloc * ir
  | IRDereference           of ir

  | IRCodeCombinator        of (code_value list -> code_value) * int * ir list
  | IRCodeRecord            of label list * ir list
  | IRCodeInputHorz         of (ir input_horz_element_scheme) list
  | IRCodeInputVert         of (ir input_vert_element_scheme) list
  | IRCodePatternMatch      of Range.t * ir * ir_pattern_branch list
  | IRCodeLetRecIn          of ir_letrec_binding list * ir
  | IRCodeLetNonRecIn       of ir_pattern_tree * ir * ir
  | IRCodeFunction          of varloc LabelMap.t * ir_pattern_tree * ir
  | IRCodeLetMutableIn      of varloc * ir * ir
  | IRCodeOverwrite         of varloc * ir

and 'a ir_letrec_binding_scheme =
  | IRLetRecBinding of varloc * 'a ir_pattern_branch_scheme

and ir_letrec_binding =
  ir ir_letrec_binding_scheme

and 'a ir_pattern_branch_scheme =
  | IRPatternBranch      of ir_pattern_tree * 'a
  | IRPatternBranchWhen  of ir_pattern_tree * 'a * 'a

and ir_pattern_branch =
  ir ir_pattern_branch_scheme

and ir_pattern_tree =
  | IRPUnitConstant
  | IRPBooleanConstant      of bool
  | IRPIntegerConstant      of int
  | IRPStringConstant       of string
  | IRPListCons             of ir_pattern_tree * ir_pattern_tree
  | IRPEndOfList
  | IRPTuple                of ir_pattern_tree TupleList.t
  | IRPWildCard
  | IRPVariable             of varloc
  | IRPAsVariable           of varloc * ir_pattern_tree
  | IRPConstructor          of constructor_name * ir_pattern_tree

and instruction =
  | OpAccessField of label
  | OpUpdateField of label
  | OpForward of int
  | OpApply of int
  | OpApplyT of int
  | OpApplyOptional
  | OpApplyOmission
  | OpBindGlobal of compiled_location * EvalVarID.t * int
  | OpBindLocal of int * int * EvalVarID.t * int
  | OpBindClosuresRec of (varloc * instruction list) list
  | OpBranch of instruction list
      [@printer (fun fmt _ -> Format.fprintf fmt "OpBranch(...)")]
  | OpBranchIf of instruction list
      [@printer (fun fmt _ -> Format.fprintf fmt "OpBranchIf(...)")]
  | OpBranchIfNot of instruction list
      [@printer (fun fmt _ -> Format.fprintf fmt "OpBranchIfNot(...)")]
  | OpLoadGlobal of compiled_location * EvalVarID.t * int
      [@printer ((fun fmt (r, evid, refs) -> Format.fprintf fmt "OpLoadGlobal(%s)" (EvalVarID.show_direct evid)))]
  | OpLoadLocal of int * int * EvalVarID.t * int
  | OpDereference
      (* !! pdf, no-interp *)
  | OpDup
  | OpError of string
  | OpMakeConstructor of constructor_name
  | OpMakeRecord of label list
  | OpMakeTuple of int
  | OpPop
  | OpPush of compiled_value
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
  | OpClosure of varloc LabelMap.t * int * int * instruction list
  | OpClosureInputHorz of compiled_input_horz_element list
  | OpClosureInputVert of compiled_input_vert_element list
  | OpBindLocationGlobal of compiled_location * EvalVarID.t
  | OpBindLocationLocal of int * int * EvalVarID.t
  | OpUpdateGlobal of compiled_location * EvalVarID.t
      [@printer (fun fmt _ -> Format.fprintf fmt "OpUpdateGlobal(...)")]
  | OpUpdateLocal of int * int * EvalVarID.t
      [@printer (fun fmt _ -> Format.fprintf fmt "OpUpdateLocal(...)")]
  | OpSel of instruction list * instruction list
      [@printer (fun fmt _ -> Format.fprintf fmt "OpSel(...)")]

  | OpBackendMathList of int
      (* !! no-ircode *)

  | OpInsertArgs of compiled_value list
  | OpApplyCodeCombinator of (code_value list -> code_value) * int
  | OpCodeMakeRecord of label list
  | OpCodeMathList of int
  | OpCodeMakeTuple of int
  | OpCodeMakeInputHorz of ((instruction list) input_horz_element_scheme) list
  | OpCodeMakeInputVert of ((instruction list) input_vert_element_scheme) list
  | OpCodePatternMatch  of Range.t * ((instruction list) ir_pattern_branch_scheme) list
  | OpCodeLetRec        of ((instruction list) ir_letrec_binding_scheme) list * instruction list
  | OpCodeLetNonRec     of ir_pattern_tree * instruction list * instruction list
  | OpCodeFunction      of varloc LabelMap.t * ir_pattern_tree * instruction list
  | OpCodeLetMutable    of varloc * instruction list * instruction list
  | OpCodeOverwrite     of instruction list
  | OpConvertSymbolToCode
#include "__insttype.gen.ml"


let add_to_environment (env : compiled_environment) (evid : EvalVarID.t) (loc : compiled_location) : compiled_environment =
  let (valenv, stenvref) = env in
  (valenv |> EvalVarIDMap.add evid loc, stenvref)


let find_in_environment (env : compiled_environment) (evid : EvalVarID.t) : compiled_location option =
  let (valenv, _) = env in
  valenv |> EvalVarIDMap.find_opt evid
