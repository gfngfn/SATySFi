
open MyUtil
open LengthInterface
open GraphicBase
open Types
open EvalUtil

exception EvalError of string


let report_dynamic_error msg =
  raise (EvalError(msg))


type nom_input_horz_element =
  | NomInputHorzText     of string
  | NomInputHorzEmbedded of abstract_tree
  | NomInputHorzThunk    of syntactic_value * abstract_tree
  | NomInputHorzContent  of nom_input_horz_element list * environment

(*
let make_length_from_description flt unitnm =
  match unitnm with  (* temporary; ad-hoc handling of unit names *)
  | "pt"   -> Length.of_pdf_point flt
  | "cm"   -> Length.of_centimeter flt
  | "mm"   -> Length.of_millimeter flt
  | "inch" -> Length.of_inch flt
  | _      -> report_bug_vm "LengthDescription; unknown unit name"
*)

let lex_horz_text (ctx : HorzBox.context_main) (s_utf8 : string) : HorzBox.horz_box list =
  let uchlst = InternalText.to_uchar_list (InternalText.of_utf8 s_utf8) in
  HorzBox.([HorzPure(PHCInnerString(ctx, uchlst))])


let find_symbol (env : environment) (evid : EvalVarID.t) : CodeSymbol.t option =
  match find_in_environment env evid with
  | Some(rfvalue) ->
      begin
        match !rfvalue with
        | CodeSymbol(symb) -> Some(symb)
        | v                -> report_bug_value "not a symbol" v
      end

  | None ->
      None


let generate_symbol_for_eval_var_id (evid : EvalVarID.t) (env : environment) : environment * CodeSymbol.t =
  let symb =
    let varnm = EvalVarID.get_varnm evid in
    let rng = EvalVarID.get_range evid in
    CodeSymbol.fresh (rng, "symbol for " ^ varnm)
  in
  let rfvalue = ref (CodeSymbol(symb)) in
  let envnew = add_to_environment env evid rfvalue in
  (envnew, symb)


let rec reduce_beta ?optional:(ast_labmap : abstract_tree LabelMap.t = LabelMap.empty) (value1 : syntactic_value) (value2 : syntactic_value) =
  match value1 with
  | Closure(evid_labmap, patbr, env1) ->
      let env1 =
        LabelMap.fold (fun label evid env ->
          let loc =
            match ast_labmap |> LabelMap.find_opt label with
            | None ->
                ref (Constructor("None", const_unit))

            | Some(ast0) ->
                let (value0, _) = interpret_0 env ast0 in
                ref (Constructor("Some", value0))
          in
          add_to_environment env evid loc
        ) evid_labmap env1
      in
      fst @@ select_pattern (Range.dummy "Apply") env1 value2 [patbr]

  | PrimitiveClosure(patbr, env1, _, _) ->
      fst @@ select_pattern (Range.dummy "Apply") env1 value2 [patbr]

  | _ ->
      report_bug_value "reduce_beta: not a function" value1


and reduce_beta_list value1 valueargs =
  List.fold_left reduce_beta value1 valueargs


and interpret_point env ast =
  let (value, _) = interpret_0 env ast in
  get_point value


and interpret_0_path (env : environment) pathcomplst cycleopt =
  let pathelemlst =
    pathcomplst |> List.map (function
      | PathLineTo(astpt) ->
          let pt = interpret_point env astpt in
          LineTo(pt)

      | PathCubicBezierTo(astpt1, astpt2, astpt) ->
          let pt1 = interpret_point env astpt1 in
          let pt2 = interpret_point env astpt2 in
          let pt = interpret_point env astpt in
          CubicBezierTo(pt1, pt2, pt)
    )
  in
  let closingopt =
    cycleopt |> option_map (function
      | PathLineTo(()) ->
          LineTo(())

      | PathCubicBezierTo(astpt1, astpt2, ()) ->
          let pt1 = interpret_point env astpt1 in
          let pt2 = interpret_point env astpt2 in
          CubicBezierTo(pt1, pt2, ())
    )
  in
    (pathelemlst, closingopt)


and interpret_0_input_horz_content (env : environment) (ihlst : input_horz_element list) =
  ihlst |> List.map (function
    | InputHorzText(s) ->
        ImInputHorzText(s)

    | InputHorzEmbedded(astabs) ->
        ImInputHorzEmbedded(astabs)

    | InputHorzEmbeddedMath(astmath) ->
        ImInputHorzEmbeddedMath(astmath)

    | InputHorzEmbeddedCodeText(s) ->
        ImInputHorzEmbeddedCodeText(s)

    | InputHorzContent(ast) ->
        let (value, _) = interpret_0 env ast in
        begin
          match value with
          | InputHorzClosure(imihlst, envsub) ->
              ImInputHorzContent(imihlst, envsub)

          | _ ->
              report_bug_reduction "interpret_input_horz_content" ast value
        end
  )

and interpret_0_input_vert_content (env : environment) (ivlst : input_vert_element list) =
  ivlst |> List.map (function
    | InputVertEmbedded(astabs) ->
        ImInputVertEmbedded(astabs)

    | InputVertContent(ast) ->
        let (value, _) = interpret_0 env ast in
        begin
          match value with
          | InputVertClosure(imivlst, envsub) ->
              ImInputVertContent(imivlst, envsub)

          | _ ->
              report_bug_reduction "interpret_input_vert_content" ast value
        end
  )


and interpret_0 (env : environment) (ast : abstract_tree) : syntactic_value * environment option =
  let return v = (v, None) in
  match ast with

(* ---- basic value ---- *)

  | ASTBaseConstant(bc) ->
      return @@ BaseConstant(bc)

  | ASTEndOfList ->
      return @@ List([])

  | ASTMath(mlst) ->
      return @@ MathValue(mlst)

  | FinishHeaderFile ->
      (EvaluatedEnvironment, Some(env))

  | FinishStruct ->
      (EvaluatedEnvironment, Some(env))

  | InputHorz(ihlst) ->
      let imihlst = interpret_0_input_horz_content env ihlst in
      return @@ InputHorzClosure(imihlst, env)
        (* -- lazy evaluation; evaluates embedded variables only -- *)

  | InputVert(ivlst) ->
      let imivlst = interpret_0_input_vert_content env ivlst in
      return @@ InputVertClosure(imivlst, env)
        (* -- lazy evaluation; evaluates embedded variables only -- *)

(* -- fundamentals -- *)

  | ContentOf(rng, evid)
  | Persistent(rng, evid) ->
      begin
        match find_in_environment env evid with
        | Some(rfvalue) ->
            let value = !rfvalue in
            return @@ value

        | None ->
            report_bug_ast ("ContentOf: variable '" ^ (EvalVarID.show_direct evid) ^ "' (at " ^ (Range.to_string rng) ^ ") not found") ast
      end

  | LetRecIn(recbinds, ast2) ->
      let envnew = add_letrec_bindings_to_environment env recbinds in
      interpret_0 envnew ast2

  | LetNonRecIn(pat, ast1, ast2) ->
      let (value1, _) = interpret_0 env ast1 in
      select_pattern (Range.dummy "LetNonRecIn") env value1 [PatternBranch(pat, ast2)]

  | Function(evids, patbrs) ->
      return @@ Closure(evids, patbrs, env)

  | Apply(ast_labmap, ast1, ast2) ->
      let (value1, _) = interpret_0 env ast1 in
      let (value2, _) = interpret_0 env ast2 in
      return @@ reduce_beta ~optional:ast_labmap value1 value2

  | IfThenElse(astb, ast1, ast2) ->
      let (valueb, _) = interpret_0 env astb in
      let b = get_bool valueb in
      if b then interpret_0 env ast1 else interpret_0 env ast2

(* ---- record ---- *)

  | Record(asc) ->
      return @@ RecordValue(asc |> LabelMap.map (interpret_0_value env))

  | AccessField(ast1, fldnm) ->
      let (value1, _) = interpret_0 env ast1 in
      begin
        match value1 with
        | RecordValue(asc1) ->
            begin
              match asc1 |> LabelMap.find_opt fldnm with
              | None    -> report_bug_reduction ("AccessField: field '" ^ fldnm ^ "' not found") ast1 value1
              | Some(v) -> return @@ v
            end

        | _ ->
            report_bug_reduction "AccessField: not a Record" ast1 value1
      end

  | UpdateField(ast1, fldnm, ast2) ->
      let (value1, _) = interpret_0 env ast1 in
      let (value2, _) = interpret_0 env ast2 in
      begin
        match value1 with
        | RecordValue(asc1) ->
            let asc1new =
              match asc1 |> LabelMap.find_opt fldnm with
              | None    -> report_bug_reduction ("UpdateField: field '" ^ fldnm ^ "' not found") ast1 value1
              | Some(_) -> asc1 |> LabelMap.add fldnm value2
            in
            return @@ RecordValue(asc1new)

        | _ ->
            report_bug_reduction "UpdateField: not a Record" ast1 value1
      end

(* ---- imperatives ---- *)

  | LetMutableIn(evid, astini, astaft) ->
      let (valueini, _) = interpret_0 env astini in
      let stid = register_location env valueini in
      let envnew = add_to_environment env evid (ref (Location(stid))) in
      interpret_0 envnew astaft

  | Overwrite(evid, astnew) ->
      begin
        match find_in_environment env evid with
        | Some(rfvalue) ->
            let value = !rfvalue in
            begin
              match value with
              | Location(stid) ->
                  let (valuenew, _) = interpret_0 env astnew in
                  update_location env stid valuenew;
                  return @@ const_unit

              | _ ->
                  report_bug_value "Overwrite: value is not a Location" value
            end

        | None ->
            report_bug_ast ("Overwrite: mutable value '" ^ (EvalVarID.show_direct evid) ^ "' not found") ast
      end

  | Dereference(astcont) ->
      let (valuecont, _) = interpret_0 env astcont in
      begin
        match valuecont with
        | Location(stid) ->
            begin
              match find_location_value env stid with
              | Some(value) -> return @@ value
              | None        -> report_bug_reduction "Dereference; not found" astcont valuecont
            end

        | _ ->
            report_bug_reduction "Dereference" astcont valuecont
      end

  | PatternMatch(rng, astobj, patbrs) ->
      let (valueobj, _) = interpret_0 env astobj in
      select_pattern rng env valueobj patbrs

  | NonValueConstructor(constrnm, astcont) ->
      let (valuecont, _) = interpret_0 env astcont in
      return @@ Constructor(constrnm, valuecont)

  | Module(astmdl, astaft) ->
      let (value, envopt) = interpret_0 env astmdl in
      begin
        match value with
        | EvaluatedEnvironment ->
            begin
              match envopt with
              | Some(envfinal) -> interpret_0 envfinal astaft
              | None           -> report_bug_vm "no environment returned"
            end

        | _ ->
            report_bug_reduction "Module" astmdl value
      end

  | BackendMathList(astmlst) ->
      let mlstlst =
        List.map (fun astm -> get_math (interpret_0_value env astm)) astmlst
      in  (* slightly doubtful in terms of evaluation strategy *)
      return @@ MathValue(List.concat mlstlst)

  | PrimitiveTuple(asts) ->
      let values = asts |> TupleList.map (interpret_0_value env) in
        (* -- should be left-to-right -- *)
      return @@ Tuple(values |> TupleList.to_list)

(* -- staging constructs -- *)

  | Prev(_) ->
      report_bug_ast "Prev(_) at stage 0" ast

  | Next(ast1) ->
      let (code1, envopt1) = interpret_1 env ast1 in
      (CodeValue(code1), envopt1)

#include "__evaluator_0.gen.ml"

and interpret_0_value env ast =
  fst @@ interpret_0 env ast


and interpret_1 (env : environment) (ast : abstract_tree) : code_value * environment option =
  let return cd = (cd, None) in
  match ast with

  | ASTBaseConstant(bc) ->
      return @@ CdBaseConstant(bc)

  | ASTMath(mlst) ->
      return @@ CdMath(mlst)

  | ASTEndOfList ->
      return @@ CdEndOfList

  | FinishHeaderFile ->
      (CdFinishHeaderFile, Some(env))

  | FinishStruct ->
      (CdFinishStruct, Some(env))

  | InputHorz(ihlst) ->
      let cdihlst = ihlst |> map_input_horz (interpret_1_value env) in
      return @@ CdInputHorz(cdihlst)

  | InputVert(ivlst) ->
      let cdivlst = ivlst |> map_input_vert (interpret_1_value env) in
      return @@ CdInputVert(cdivlst)

  | ContentOf(rng, evid) ->
      begin
        match find_symbol env evid with
        | Some(symb) -> return @@ CdContentOf(rng, symb)
        | None       -> report_bug_ast "symbol not found" ast
      end

 | Persistent(rng, evid) ->
     return @@ CdPersistent(rng, evid)

  | LetRecIn(recbinds, ast2) ->
      let (env, zippedacc) =
      (* -- generate the symbols for the identifiers and add them to the environment -- *)
        recbinds |> List.fold_left (fun (env, zippedacc) recbind ->
          let LetRecBinding(evid, _) = recbind in
          let (env, symb) = generate_symbol_for_eval_var_id evid env in
          (env, Alist.extend zippedacc (symb, recbind))
        ) (env, Alist.empty)
      in
      let cdrecbinds =
        zippedacc |> Alist.to_list |> List.map (fun (symb, LetRecBinding(evid, patbr)) ->
          let cdpatbr = interpret_1_pattern_branch env patbr in
          CdLetRecBinding(symb, cdpatbr)
        )
      in
      let (code2, envopt2) = interpret_1 env ast2 in
      (CdLetRecIn(cdrecbinds, code2), envopt2)

  | LetNonRecIn(pattr, ast1, ast2) ->
      let (code1, _) = interpret_1 env ast1 in
      let (env, cdpattr) = interpret_1_pattern_tree env pattr in
        (* -- generate the symbols corresponding to the variables in the pattern
              and add them to the environment -- *)
      let (code2, envopt2) = interpret_1 env ast2 in
      (CdLetNonRecIn(cdpattr, code1, code2), envopt2)

  | Function(evid_labmap, patbr) ->
      let (env, symb_labmap) =
        LabelMap.fold (fun label evid (env, symb_labmap) ->
          let (env, symb) = generate_symbol_for_eval_var_id evid env in
          (env, symb_labmap |> LabelMap.add label symb)
        ) evid_labmap (env, LabelMap.empty)
      in
      let cdpatbr = interpret_1_pattern_branch env patbr in
      return @@ CdFunction(symb_labmap, cdpatbr)

  | Apply(ast_labmap, ast1, ast2) ->
      let (code1, _) = interpret_1 env ast1 in
      let (code2, _) = interpret_1 env ast2 in
      let code_labmap =
        ast_labmap |> LabelMap.map (fun ast0 ->
          let (code0, _) = interpret_1 env ast0 in
          code0
        )
      in
      return @@ CdApply(code_labmap, code1, code2)

  | IfThenElse(ast0, ast1, ast2) ->
      let (code0, _) = interpret_1 env ast0 in
      let (code1, _) = interpret_1 env ast1 in
      let (code2, _) = interpret_1 env ast2 in
      return @@ CdIfThenElse(code0, code1, code2)

  | Record(asc) ->
      let cdasc = asc |> LabelMap.map (interpret_1_value env) in
      return @@ CdRecord(cdasc)

  | AccessField(ast1, fldnm) ->
      let (code1, _) = interpret_1 env ast1 in
      return @@ CdAccessField(code1, fldnm)

  | UpdateField(ast1, fldnm, ast2) ->
      let (code1, _) = interpret_1 env ast1 in
      let (code2, _) = interpret_1 env ast2 in
      return @@ CdUpdateField(code1, fldnm, code2)

  | LetMutableIn(evid, ast1, ast2) ->
      let (env, symb) = generate_symbol_for_eval_var_id evid env in
      let (code1, _) = interpret_1 env ast1 in
      let (code2, envopt2) = interpret_1 env ast2 in
      (CdLetMutableIn(symb, code1, code2), envopt2)

  | Overwrite(evid, ast1) ->
      begin
        match find_symbol env evid with
        | Some(symb) ->
            let (code1, _) = interpret_1 env ast1 in
            return @@ CdOverwrite(symb, code1)

        | None ->
            report_bug_ast "symbol not found" ast
      end

  | Dereference(ast1) ->
      let (code1, _) = interpret_1 env ast1 in
      return @@ CdDereference(code1)

  | PatternMatch(rng, ast1, patbrs) ->
      let (code1, _) = interpret_1 env ast1 in
      let cdpatbrs = patbrs |> List.map (interpret_1_pattern_branch env) in
      return @@ CdPatternMatch(rng, code1, cdpatbrs)

  | NonValueConstructor(constrnm, ast1) ->
      let (code1, _) = interpret_1 env ast1 in
      return @@ CdConstructor(constrnm, code1)

  | Module(ast1, ast2) ->
      let (code1, envopt1) = interpret_1 env ast1 in
      begin
        match envopt1 with
        | Some(env) ->
            let (code2, envopt2) = interpret_1 env ast2 in
            (CdModule(code1, code2), envopt2)

        | None ->
            report_bug_ast "not a module contents" ast
      end

  | BackendMathList(astlst) ->
      let codelst = astlst |> List.map (interpret_1_value env) in
      return @@ CdMathList(codelst)

  | PrimitiveTuple(asts) ->
      let codes = TupleList.map (interpret_1_value env) asts in
        (* -- should be left-to-right -- *)
      return @@ CdTuple(codes)

  | Prev(ast1) ->
      let (value1, envopt1) = interpret_0 env ast1 in
      begin
        match value1 with
        | CodeValue(code) -> (code, envopt1)
        | _               -> report_bug_reduction "Prev; not a code value" ast value1
      end

  | Next(_) ->
      report_bug_ast "Next(_) at stage 1" ast

#include "__evaluator_1.gen.ml"

and interpret_1_value env ast =
  fst @@ interpret_1 env ast


and interpret_1_pattern_branch env = function
  | PatternBranch(pattr, ast) ->
      let (env, cdpattr) = interpret_1_pattern_tree env pattr in
      CdPatternBranch(cdpattr, interpret_1_value env ast)

  | PatternBranchWhen(pattr, ast, ast1) ->
      let (env, cdpattr) = interpret_1_pattern_tree env pattr in
      CdPatternBranchWhen(cdpattr, interpret_1_value env ast, interpret_1_value env ast1)


and interpret_1_pattern_tree env = function
  | PUnitConstant       -> (env, CdPUnitConstant)
  | PBooleanConstant(b) -> (env, CdPBooleanConstant(b))
  | PIntegerConstant(n) -> (env, CdPIntegerConstant(n))
  | PStringConstant(s)  -> (env, CdPStringConstant(s))

  | PListCons(pattr1, pattr2) ->
      let (env, cdpattr1) = interpret_1_pattern_tree env pattr1 in
      let (env, cdpattr2) = interpret_1_pattern_tree env pattr2 in
      (env, CdPListCons(cdpattr1, cdpattr2))

  | PEndOfList ->
      (env, CdPEndOfList)

  | PTuple(pattrs) ->
      let (env, cdpattracc) =
        pattrs |> TupleList.to_list |> List.fold_left (fun (env, cdpattracc) pattr ->
          let (env, cdpattr) = interpret_1_pattern_tree env pattr in
          (env, Alist.extend cdpattracc cdpattr)
        ) (env, Alist.empty)
      in
      let cdpattrs =
        match Alist.to_list cdpattracc with
        | c1 :: c2 :: cs -> TupleList.make c1 c2 cs
        | _              -> assert false
      in
      (env, CdPTuple(cdpattrs))

  | PWildCard ->
      (env, CdPWildCard)

  | PVariable(evid) ->
      let (env, symb) = generate_symbol_for_eval_var_id evid env in
      (env, CdPVariable(symb))

  | PAsVariable(evid, pattr) ->
      let (env, symb) = generate_symbol_for_eval_var_id evid env in
      let (env, cdpattr) = interpret_1_pattern_tree env pattr in
      (env, CdPAsVariable(symb, cdpattr))

  | PConstructor(ctor, pattr) ->
      let (env, cdpattr) = interpret_1_pattern_tree env pattr in
      (env, CdPConstructor(ctor, cdpattr))


and interpret_text_mode_intermediate_input_vert env (valuetctx : syntactic_value) (imivlst : intermediate_input_vert_element list) : syntactic_value =
  let rec interpret_commands env (imivlst : intermediate_input_vert_element list) =
    imivlst |> List.map (fun imiv ->
      match imiv with
      | ImInputVertEmbedded(astabs) ->
          let (valueabs, _) = interpret_0 env astabs in
          let valuevert = reduce_beta valueabs valuetctx in
          get_string valuevert

      | ImInputVertContent(imivlstsub, envsub) ->
          interpret_commands envsub imivlstsub

    ) |> String.concat ""
  in
  let s = interpret_commands env imivlst in
  make_string s


and interpret_text_mode_intermediate_input_horz (env : environment) (valuetctx : syntactic_value) (imihlst : intermediate_input_horz_element list) : syntactic_value =

  let tctx = get_text_mode_context valuetctx in

  let rec normalize (imihlst : intermediate_input_horz_element list) =
    imihlst |> List.fold_left (fun acc imih ->
      match imih with
      | ImInputHorzEmbedded(astabs) ->
          let nmih = NomInputHorzEmbedded(astabs) in
          Alist.extend acc nmih

      | ImInputHorzText(s2) ->
          begin
            match Alist.chop_last acc with
            | Some(accrest, NomInputHorzText(s1)) -> (Alist.extend accrest (NomInputHorzText(s1 ^ s2)))
            | _                                   -> (Alist.extend acc (NomInputHorzText(s2)))
          end

      | ImInputHorzEmbeddedMath(astmath) ->
          failwith "Evaluator_> math; remains to be supported."
(*
          let nmih = NomInputHorzThunk(Apply(Apply(Value(valuemcmd), Value(valuectx)), astmath)) in
            Alist.extend acc nmih
*)
      | ImInputHorzEmbeddedCodeText(s) ->
          failwith "Evaluator_> code text; remains to be supported."

      | ImInputHorzContent(imihlstsub, envsub) ->
          let nmihlstsub = normalize imihlstsub in
          let nmih = NomInputHorzContent(nmihlstsub, envsub) in
          Alist.extend acc nmih

    ) Alist.empty |> Alist.to_list
  in

  let rec interpret_commands env (nmihlst : nom_input_horz_element list) : string =
    nmihlst |> List.map (fun nmih ->
      match nmih with
      | NomInputHorzEmbedded(astabs) ->
          let (valueabs, _) = interpret_0 env astabs in
          let valueret = reduce_beta valueabs valuetctx in
          get_string valueret

      | NomInputHorzThunk(valuecmd, astarg) ->
          let (valuearg, _) = interpret_0 env astarg in
          let valueret = reduce_beta valuecmd valuearg in
          get_string valueret

      | NomInputHorzText(s) ->
          let uchlst = InternalText.to_uchar_list (InternalText.of_utf8 s) in
          let uchlstret = tctx |> TextBackend.stringify uchlst in
          InternalText.to_utf8 (InternalText.of_uchar_list uchlstret)

      | NomInputHorzContent(nmihlstsub, envsub) ->
          interpret_commands envsub nmihlstsub

    ) |> String.concat ""
  in

  let nmihlst = normalize imihlst in
  let s = interpret_commands env nmihlst in
  make_string s


and interpret_pdf_mode_intermediate_input_vert env (valuectx : syntactic_value) (imivlst : intermediate_input_vert_element list) : syntactic_value =
  let rec interpret_commands env (imivlst : intermediate_input_vert_element list) =
    imivlst |> List.map (fun imiv ->
      match imiv with
      | ImInputVertEmbedded(astabs) ->
          let (valueabs, _) = interpret_0 env astabs in
          let valuevert = reduce_beta valueabs valuectx in
          get_vert valuevert

      | ImInputVertContent(imivlstsub, envsub) ->
          interpret_commands envsub imivlstsub

    ) |> List.concat
  in
  let imvblst = interpret_commands env imivlst in
  make_vert imvblst


and interpret_pdf_mode_intermediate_input_horz (env : environment) (valuectx : syntactic_value) (imihlst : intermediate_input_horz_element list) : syntactic_value =

  let (ctx, ctxsub) = get_context valuectx in
  let valuemcmd = make_math_command_func ctxsub.math_command in

  let rec normalize (imihlst : intermediate_input_horz_element list) =
    imihlst |> List.fold_left (fun acc imih ->
      match imih with
      | ImInputHorzEmbedded(astabs) ->
          let nmih = NomInputHorzEmbedded(astabs) in
          Alist.extend acc nmih

      | ImInputHorzText(s2) ->
          begin
            match Alist.chop_last acc with
            | Some(accrest, NomInputHorzText(s1)) -> (Alist.extend accrest (NomInputHorzText(s1 ^ s2)))
            | _                                   -> (Alist.extend acc (NomInputHorzText(s2)))
          end

      | ImInputHorzEmbeddedMath(astmath) ->
          let valuemcmdctx = reduce_beta valuemcmd valuectx in
          let nmih = NomInputHorzThunk(valuemcmdctx, astmath) in
          Alist.extend acc nmih

      | ImInputHorzEmbeddedCodeText(s) ->
          begin
            match ctxsub.code_text_command with
            | DefaultCodeTextCommand ->
                let nmih = NomInputHorzText(s) in
                Alist.extend acc nmih

            | CodeTextCommand(valuectcmd) ->
                let valuectcmdctx = reduce_beta valuectcmd valuectx in
                let nmih = NomInputHorzThunk(valuectcmdctx, ASTBaseConstant(BCString(s))) in
                Alist.extend acc nmih
          end

      | ImInputHorzContent(imihlstsub, envsub) ->
          let nmihlstsub = normalize imihlstsub in
          let nmih = NomInputHorzContent(nmihlstsub, envsub) in
          Alist.extend acc nmih

    ) Alist.empty |> Alist.to_list
  in

  let rec interpret_commands env (nmihlst : nom_input_horz_element list) : HorzBox.horz_box list =
    nmihlst |> List.map (fun nmih ->
      match nmih with
      | NomInputHorzEmbedded(astabs) ->
          let (valueabs, _) = interpret_0 env astabs in
          let valuehorz = reduce_beta valueabs valuectx in
          get_horz valuehorz

      | NomInputHorzThunk(valuemcmdctx, astmath) ->
          let (valuemath, _) = interpret_0 env astmath in
          let valuehorz = reduce_beta valuemcmdctx valuemath in
          get_horz valuehorz

      | NomInputHorzText(s) ->
          lex_horz_text ctx s

      | NomInputHorzContent(nmihlstsub, envsub) ->
          interpret_commands envsub nmihlstsub

    ) |> List.concat
  in

  let nmihlst = normalize imihlst in
  let hblst = interpret_commands env nmihlst in
  make_horz hblst


and select_pattern (rng : Range.t) (env : environment) (valueobj : syntactic_value) (patbrs : pattern_branch list) : syntactic_value * environment option =
  let iter = select_pattern rng env valueobj in
  match patbrs with
  | [] ->
      report_dynamic_error ("no matches (" ^ (Range.to_string rng) ^ ")")

  | PatternBranch(pat, astto) :: tail ->
      begin
        match check_pattern_matching env pat valueobj with
        | Some(envnew) -> interpret_0 envnew astto
        | None         -> iter tail
      end

  | PatternBranchWhen(pat, astcond, astto) :: tail ->
      begin
        match check_pattern_matching env pat valueobj with
        | Some(envnew) ->
            let cond = get_bool (interpret_0_value envnew astcond) in
            if cond then interpret_0 envnew astto else iter tail

        | None ->
            iter tail
      end


and check_pattern_matching (env : environment) (pat : pattern_tree) (valueobj : syntactic_value) : environment option =
  match (pat, valueobj) with
  | (PIntegerConstant(pnc), BaseConstant(BCInt(nc))) ->
      if pnc = nc then Some(env) else None

  | (PBooleanConstant(pbc), BaseConstant(BCBool(bc))) ->
      if pbc = bc then Some(env) else None

  | (PStringConstant(psc), BaseConstant(BCString(str2))) ->
      if String.equal psc str2 then Some(env) else None

  | (PUnitConstant, BaseConstant(BCUnit)) -> Some(env)
  | (PWildCard, _)                        -> Some(env)

  | (PVariable(evid), _) ->
      let envnew = add_to_environment env evid (ref valueobj) in
      Some(envnew)

  | (PAsVariable(evid, psub), sub) ->
      let envnew = add_to_environment env evid (ref sub) in
      check_pattern_matching envnew psub sub

  | (PEndOfList, List([])) ->
      Some(env)

  | (PListCons(phd, ptl), List(vhd :: vtail)) ->
      let open OptionMonad in
      check_pattern_matching env phd vhd >>= fun envhd ->
      check_pattern_matching envhd ptl (List(vtail))

  | (PTuple(ps), Tuple(vlst)) ->
      let open OptionMonad in
      begin
        try
          List.fold_left2 (fun envopt p v ->
            envopt >>= fun env ->
            check_pattern_matching env p v
          ) (Some(env)) (ps |> TupleList.to_list) vlst
        with
        | Invalid_argument(_) -> None
      end

  | (PConstructor(cnm1, psub), Constructor(cnm2, sub))
    when cnm1 = cnm2 ->
      check_pattern_matching env psub sub

  | _ ->
      None


and add_letrec_bindings_to_environment (env : environment) (recbinds : letrec_binding list) : environment =
  let tris =
    recbinds |> List.map (function LetRecBinding(evid, patbr) ->
      let loc = ref Nil in
      (evid, loc, patbr)
    )
  in
  let env =
    tris |> List.fold_left (fun env (evid, loc, _) ->
      add_to_environment env evid loc
    ) env
  in
  tris |> List.iter (fun (evid, loc, patbr) ->
    loc := Closure(LabelMap.empty, patbr, env)
  );
  env


let interpret_bindings_0 (env : environment) (binds : binding list) : environment =
  failwith "TODO: Evaluator.interpret_bindings_0"


let interpret_bindings_1 (env : environment) (binds : binding list) : code_binding list * environment =
  failwith "TODO: Evaluator.interpret_bindings_1"
