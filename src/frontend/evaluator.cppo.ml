
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
  | NomInputHorzThunk    of abstract_tree
  | NomInputHorzContent  of nom_input_horz_element list * environment


let make_length_from_description flt unitnm =
  match unitnm with  (* temporary; ad-hoc handling of unit names *)
  | "pt"   -> Length.of_pdf_point flt
  | "cm"   -> Length.of_centimeter flt
  | "mm"   -> Length.of_millimeter flt
  | "inch" -> Length.of_inch flt
  | _      -> report_bug_vm "LengthDescription; unknown unit name"


let lex_horz_text (ctx : HorzBox.context_main) (s_utf8 : string) : HorzBox.horz_box list =
  let uchlst = InternalText.to_uchar_list (InternalText.of_utf8 s_utf8) in
  HorzBox.([HorzPure(PHCInnerString(ctx, uchlst))])


let rec reduce_beta value1 value2 =
  match value1 with
  | FuncWithEnvironment(evids, patbr, env1) ->
      let env1 =
        evids |> List.fold_left (fun env evid ->
          let loc = ref (Constructor("None", UnitConstant)) in
          add_to_environment env evid loc
        ) env1
      in
      select_pattern (Range.dummy "Apply") env1 value2 [patbr]

  | PrimitiveWithEnvironment(patbr, env1, _, _) ->
      select_pattern (Range.dummy "Apply") env1 value2 [patbr]

  | _ ->
      report_bug_value "reduce_beta: not a function" value1


and reduce_beta_list value1 valueargs =
  List.fold_left reduce_beta value1 valueargs


and interpret_point env ast =
  let value = interpret_0 env ast in
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

    | InputHorzContent(ast) ->
        let value = interpret_0 env ast in
        begin
          match value with
          | InputHorzWithEnvironment(imihlst, envsub) ->
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
        let value = interpret_0 env ast in
        begin
          match value with
          | InputVertWithEnvironment(imivlst, envsub) ->
              ImInputVertContent(imivlst, envsub)

          | _ ->
              report_bug_reduction "interpret_input_vert_content" ast value
        end
  )


and interpret_0 (env : environment) (ast : abstract_tree) =
  match ast with

(* ---- basic value ---- *)

  | Value(v) -> v

  | FinishHeaderFile ->
      EvaluatedEnvironment(env)

  | FinishStruct ->
      EvaluatedEnvironment(env)

  | InputHorz(ihlst) ->
      let imihlst = interpret_0_input_horz_content env ihlst in
      InputHorzWithEnvironment(imihlst, env)
        (* -- lazy evaluation; evaluates embedded variables only -- *)

  | InputVert(ivlst) ->
      let imivlst = interpret_0_input_vert_content env ivlst in
      InputVertWithEnvironment(imivlst, env)
        (* -- lazy evaluation; evaluates embedded variables only -- *)

  | LengthDescription(flt, unitnm) ->
      let len = make_length_from_description flt unitnm in
      LengthConstant(len)

(* -- fundamentals -- *)

  | ContentOf(rng, evid) ->
      begin
        match find_in_environment env evid with
        | Some(rfvalue) ->
            let value = !rfvalue in
            value

        | None ->
            report_bug_ast ("ContentOf: variable '" ^ (EvalVarID.show_direct evid) ^ "' (at " ^ (Range.to_string rng) ^ ") not found") ast
      end

  | LetRecIn(recbinds, ast2) ->
      let envnew = add_letrec_bindings_to_environment env recbinds in
      interpret_0 envnew ast2

  | LetNonRecIn(pat, ast1, ast2) ->
      let value1 = interpret_0 env ast1 in
      select_pattern (Range.dummy "LetNonRecIn") env value1 [PatternBranch(pat, ast2)]

  | Function(evids, patbrs) ->
      FuncWithEnvironment(evids, patbrs, env)

  | Apply(ast1, ast2) ->
      let value1 = interpret_0 env ast1 in
      let value2 = interpret_0 env ast2 in
      reduce_beta value1 value2

  | ApplyOptional(ast1, ast2) ->
      let value1 = interpret_0 env ast1 in
      begin
        match value1 with
        | FuncWithEnvironment(evid :: evids, patbrs, env1) ->
            let value2 = interpret_0 env ast2 in
            let loc = ref (Constructor("Some", value2)) in
            let env1 = add_to_environment env1 evid loc in
            FuncWithEnvironment(evids, patbrs, env1)

        | _ -> report_bug_reduction "ApplyOptional: not a function with optional parameter" ast1 value1
      end

  | ApplyOmission(ast1) ->
      let value1 = interpret_0 env ast1 in
      begin
        match value1 with
        | FuncWithEnvironment(evid :: evids, patbrs, env1) ->
            let env1 = add_to_environment env1 evid (ref (Constructor("None", UnitConstant))) in
            FuncWithEnvironment(evids, patbrs, env1)

        | _ ->
            report_bug_reduction "ApplyOmission: not a function with optional parameter" ast1 value1
      end

  | IfThenElse(astb, ast1, ast2) ->
      let b = get_bool (interpret_0 env astb) in
      if b then interpret_0 env ast1 else interpret_0 env ast2

(* ---- record ---- *)

  | Record(asc) ->
      RecordValue(Assoc.map_value (interpret_0 env) asc)

  | AccessField(ast1, fldnm) ->
      let value1 = interpret_0 env ast1 in
      begin
        match value1 with
        | RecordValue(asc1) ->
            begin
              match Assoc.find_opt asc1 fldnm with
              | None    -> report_bug_reduction ("AccessField: field '" ^ fldnm ^ "' not found") ast1 value1
              | Some(v) -> v
            end

        | _ ->
            report_bug_reduction "AccessField: not a Record" ast1 value1
      end

  | UpdateField(ast1, fldnm, ast2) ->
      let value1 = interpret_0 env ast1 in
      let value2 = interpret_0 env ast2 in
      begin
        match value1 with
        | RecordValue(asc1) ->
            let asc1new =
              match Assoc.find_opt asc1 fldnm with
              | None    -> report_bug_reduction ("UpdateField: field '" ^ fldnm ^ "' not found") ast1 value1
              | Some(_) -> Assoc.add asc1 fldnm value2
            in
            RecordValue(asc1new)

        | _ ->
            report_bug_reduction "UpdateField: not a Record" ast1 value1
      end

(* ---- imperatives ---- *)

  | LetMutableIn(evid, astini, astaft) ->
      let valueini = interpret_0 env astini in
      let stid = register_location env valueini in
      let envnew = add_to_environment env evid (ref (Location(stid))) in
      interpret_0 envnew astaft

  | Sequential(ast1, ast2) ->
      let value1 = interpret_0 env ast1 in
      let value2 = interpret_0 env ast2 in
      begin
        match value1 with
        | UnitConstant -> value2
        | _            -> report_bug_reduction "Sequential: first operand value is not a UnitConstant" ast1 value1
      end

  | Overwrite(evid, astnew) ->
      begin
        match find_in_environment env evid with
        | Some(rfvalue) ->
            let value = !rfvalue in
            begin
              match value with
              | Location(stid) ->
                  let valuenew = interpret_0 env astnew in
                  update_location env stid valuenew;
                  UnitConstant

              | _ ->
                  report_bug_value "Overwrite: value is not a Location" value
            end

        | None ->
            report_bug_ast ("Overwrite: mutable value '" ^ (EvalVarID.show_direct evid) ^ "' not found") ast
      end

  | WhileDo(astb, astc) ->
      let b = get_bool (interpret_0 env astb) in
      if b then
        let _ = interpret_0 env astc in interpret_0 env (WhileDo(astb, astc))
      else
        UnitConstant

  | Dereference(astcont) ->
      let valuecont = interpret_0 env astcont in
      begin
        match valuecont with
        | Location(stid) ->
            begin
              match find_location_value env stid with
              | Some(value) -> value
              | None        -> report_bug_reduction "Dereference; not found" astcont valuecont
            end

        | _ ->
            report_bug_reduction "Dereference" astcont valuecont
      end

  | PatternMatch(rng, astobj, patbrs) ->
      let valueobj = interpret_0 env astobj in
      select_pattern rng env valueobj patbrs

  | NonValueConstructor(constrnm, astcont) ->
      let valuecont = interpret_0 env astcont in
      Constructor(constrnm, valuecont)

  | Module(astmdl, astaft) ->
      let value = interpret_0 env astmdl in
      begin
        match value with
        | EvaluatedEnvironment(envfinal) -> interpret_0 envfinal astaft
        | _                              -> report_bug_reduction "Module" astmdl value
      end

  | BackendMathList(astmlst) ->
      let mlstlst =
        List.map (fun astm -> get_math (interpret_0 env astm)) astmlst
      in  (* slightly doubtful in terms of evaluation strategy *)
      MathValue(List.concat mlstlst)

  | PrimitiveTupleCons(asthd, asttl) ->
      let valuehd = interpret_0 env asthd in
      let valuetl = interpret_0 env asttl in
      TupleCons(valuehd, valuetl)

  | Path(astpt0, pathcomplst, cycleopt) ->
      let pt0 = interpret_point env astpt0 in
      let (pathelemlst, closingopt) = interpret_0_path env pathcomplst cycleopt in
      PathValue([GeneralPath(pt0, pathelemlst, closingopt)])

(* -- staging constructs -- *)

  | Prev(_) ->
      report_bug_ast "Prev(_) at stage 0" ast

  | Next(ast1) ->
      let code1 = interpret_1 env ast1 in
      CodeValue(code1)

#include "__evaluator_0.gen.ml"


and interpret_1 (env : environment) (ast : abstract_tree) =
  match ast with

  | Value(v) ->
      CdValue(v)

  | FinishHeaderFile ->
      CdFinishHeaderFile

  | FinishStruct ->
      CdFinishStruct

  | InputHorz(ihlst) ->
      let cdihlst = ihlst |> map_input_horz (interpret_1 env) in
      CdInputHorz(cdihlst)

  | InputVert(ivlst) ->
      let cdivlst = ivlst |> map_input_vert (interpret_1 env) in
      CdInputVert(cdivlst)

  | LengthDescription(flt, unitnm) ->
      let len = make_length_from_description flt unitnm in
      CdValue(LengthConstant(len))

  | ContentOf(rng, evid) ->
      CdContentOf(rng, evid)

  | LetRecIn(recbinds, ast2) ->
      let cdrecbinds =
        recbinds |> List.map (function LetRecBinding(evid, patbr) ->
          let cdpatbr = interpret_1_pattern_branch env patbr in
          CdLetRecBinding(evid, cdpatbr)
        )
      in
      let code2 = interpret_1 env ast2 in
      CdLetRecIn(cdrecbinds, code2)

  | LetNonRecIn(pat, ast1, ast2) ->
      let code1 = interpret_1 env ast1 in
      let code2 = interpret_1 env ast2 in
      CdLetNonRecIn(pat, code1, code2)

  | Function(evids, patbr) ->
      let cdpatbr =  interpret_1_pattern_branch env patbr in
      CdFunction(evids, cdpatbr)

  | Apply(ast1, ast2) ->
      let code1 = interpret_1 env ast1 in
      let code2 = interpret_1 env ast2 in
      CdApply(code1, code2)

  | ApplyOptional(ast1, ast2) ->
      let code1 = interpret_1 env ast1 in
      let code2 = interpret_1 env ast2 in
      CdApplyOptional(code1, code2)

  | ApplyOmission(ast1) ->
      let code1 = interpret_1 env ast1 in
      CdApplyOmission(code1)

  | IfThenElse(ast0, ast1, ast2) ->
      let code0 = interpret_1 env ast0 in
      let code1 = interpret_1 env ast1 in
      let code2 = interpret_1 env ast2 in
      CdIfThenElse(code0, code1, code2)

  | Record(asc) ->
      let cdasc = Assoc.map_value (interpret_1 env) asc in
      CdRecord(cdasc)

  | AccessField(ast1, fldnm) ->
      let code1 = interpret_1 env ast1 in
      CdAccessField(code1, fldnm)

  | UpdateField(ast1, fldnm, ast2) ->
      let code1 = interpret_1 env ast1 in
      let code2 = interpret_1 env ast2 in
      CdUpdateField(code1, fldnm, code2)

  | LetMutableIn(evid, ast1, ast2) ->
      let code1 = interpret_1 env ast1 in
      let code2 = interpret_1 env ast2 in
      CdLetMutableIn(evid, code1, code2)

  | Sequential(ast1, ast2) ->
      let code1 = interpret_1 env ast1 in
      let code2 = interpret_1 env ast2 in
      CdSequential(code1, code2)

  | Overwrite(evid, ast1) ->
      let code1 = interpret_1 env ast1 in
      CdOverwrite(evid, code1)

  | WhileDo(ast1, ast2) ->
      let code1 = interpret_1 env ast1 in
      let code2 = interpret_1 env ast2 in
      CdWhileDo(code1, code2)

  | Dereference(ast1) ->
      let code1 = interpret_1 env ast1 in
      CdDereference(code1)

  | PatternMatch(rng, ast1, patbrs) ->
      let code1 = interpret_1 env ast1 in
      let cdpatbrs = patbrs |> List.map (interpret_1_pattern_branch env) in
      CdPatternMatch(rng, code1, cdpatbrs)

  | NonValueConstructor(constrnm, ast1) ->
      let code1 = interpret_1 env ast1 in
      CdConstructor(constrnm, code1)

  | Module(ast1, ast2) ->
      let code1 = interpret_1 env ast1 in
      let code2 = interpret_1 env ast2 in
      CdModule(code1, code2)

  | BackendMathList(astlst) ->
      let codelst = astlst |> List.map (interpret_1 env) in
      CdMathList(codelst)

  | PrimitiveTupleCons(ast1, ast2) ->
      let code1 = interpret_1 env ast1 in
      let code2 = interpret_1 env ast2 in
      CdTupleCons(code1, code2)

  | Path(astpt0, pathcomplst, cycleopt) ->
      let codept0 = interpret_1 env astpt0 in
      let cdpathcomplst = pathcomplst |> List.map (map_path_component (interpret_1 env) (interpret_1 env)) in
      let cdcycleopt =
        cycleopt |> BatOption.map (map_path_component (interpret_1 env) (fun () -> ())
        )
      in
      CdPath(codept0, cdpathcomplst, cdcycleopt)

  | Prev(ast1) ->
      let value1 = interpret_0 env ast1 in
      begin
        match value1 with
        | CodeValue(code) -> code
        | _               -> report_bug_reduction "Prev; not a code value" ast value1
      end

  | Next(_) ->
      report_bug_ast "Next(_) at stage 1" ast

#include "__evaluator_1.gen.ml"


and interpret_1_pattern_branch env = function
  | PatternBranch(pattr, ast)           -> CdPatternBranch(pattr, interpret_1 env ast)
  | PatternBranchWhen(pattr, ast, ast1) -> CdPatternBranchWhen(pattr, interpret_1 env ast, interpret_1 env ast1)


and interpret_text_mode_intermediate_input_vert env (valuetctx : syntactic_value) (imivlst : intermediate_input_vert_element list) : syntactic_value =
  let rec interpret_commands env (imivlst : intermediate_input_vert_element list) =
    imivlst |> List.map (fun imiv ->
      match imiv with
      | ImInputVertEmbedded(astabs) ->
          let valuevert = interpret_0 env (Apply(astabs, Value(valuetctx))) in
          get_string valuevert

      | ImInputVertContent(imivlstsub, envsub) ->
          interpret_commands envsub imivlstsub

    ) |> String.concat ""
  in
  let s = interpret_commands env imivlst in
  StringConstant(s)


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
          let valueret = interpret_0 env (Apply(astabs, Value(valuetctx))) in
          get_string valueret

      | NomInputHorzThunk(ast) ->
          let valueret = interpret_0 env ast in
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
  StringConstant(s)


and interpret_pdf_mode_intermediate_input_vert env (valuectx : syntactic_value) (imivlst : intermediate_input_vert_element list) : syntactic_value =
  let rec interpret_commands env (imivlst : intermediate_input_vert_element list) =
    imivlst |> List.map (fun imiv ->
      match imiv with
      | ImInputVertEmbedded(astabs) ->
          let valuevert = interpret_0 env (Apply(astabs, Value(valuectx))) in
          get_vert valuevert

      | ImInputVertContent(imivlstsub, envsub) ->
          interpret_commands envsub imivlstsub

    ) |> List.concat
  in
  let imvblst = interpret_commands env imivlst in
  Vert(imvblst)


and interpret_pdf_mode_intermediate_input_horz (env : environment) (valuectx : syntactic_value) (imihlst : intermediate_input_horz_element list) : syntactic_value =

  let (ctx, valuemcmd) = get_context valuectx in

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
          let nmih = NomInputHorzThunk(Apply(Apply(Value(valuemcmd), Value(valuectx)), astmath)) in
          Alist.extend acc nmih

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
          let valuehorz = interpret_0 env (Apply(astabs, Value(valuectx))) in
          get_horz valuehorz

      | NomInputHorzThunk(ast) ->
          let valuehorz = interpret_0 env ast in
          get_horz valuehorz

      | NomInputHorzText(s) ->
          lex_horz_text ctx s

      | NomInputHorzContent(nmihlstsub, envsub) ->
          interpret_commands envsub nmihlstsub

    ) |> List.concat
  in

  let nmihlst = normalize imihlst in
  let hblst = interpret_commands env nmihlst in
  Horz(hblst)


and select_pattern (rng : Range.t) (env : environment) (valueobj : syntactic_value) (patbrs : pattern_branch list) =
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
            let cond = get_bool (interpret_0 envnew astcond) in
            if cond then interpret_0 envnew astto else iter tail

        | None ->
            iter tail
      end


and check_pattern_matching (env : environment) (pat : pattern_tree) (valueobj : syntactic_value) : environment option =
  match (pat, valueobj) with
  | (PIntegerConstant(pnc), IntegerConstant(nc)) -> if pnc = nc then Some(env) else None
  | (PBooleanConstant(pbc), BooleanConstant(bc)) -> if pbc = bc then Some(env) else None

  | (PStringConstant(ast1), value2) ->
      let str1 = get_string (interpret_0 env ast1) in
      let str2 = get_string value2 in
      if String.equal str1 str2 then Some(env) else None

  | (PUnitConstant, UnitConstant) -> Some(env)
  | (PWildCard, _)                -> Some(env)

  | (PVariable(evid), _) ->
      let envnew = add_to_environment env evid (ref valueobj) in
      Some(envnew)

  | (PAsVariable(evid, psub), sub) ->
      let envnew = add_to_environment env evid (ref sub) in
      check_pattern_matching envnew psub sub

  | (PEndOfList, EndOfList) ->
      Some(env)

  | (PListCons(phd, ptl), ListCons(hd, tl)) ->
      let open OptionMonad in
      check_pattern_matching env phd hd >>= fun envhd ->
      check_pattern_matching envhd ptl tl

  | (PEndOfTuple, EndOfTuple) ->
      Some(env)

  | (PTupleCons(phd, ptl), TupleCons(hd, tl)) ->
      let open OptionMonad in
      check_pattern_matching env phd hd >>= fun envhd ->
      check_pattern_matching envhd ptl tl

  | (PConstructor(cnm1, psub), Constructor(cnm2, sub))
    when cnm1 = cnm2 ->
      check_pattern_matching env psub sub

  | _ ->
      None


and add_letrec_bindings_to_environment (env : environment) (recbinds : letrec_binding list) : environment =
  let trilst =
    recbinds |> List.map (function LetRecBinding(evid, patbr) ->
      let loc = ref Nil in
      (evid, loc, patbr)
    )
  in
  let envnew =
    trilst @|> env @|> List.fold_left (fun envacc (evid, loc, _) ->
      add_to_environment envacc evid loc
    )
  in
  trilst |> List.iter (fun (evid, loc, patbr) ->
    loc := FuncWithEnvironment([], patbr, envnew)
  );
  envnew
