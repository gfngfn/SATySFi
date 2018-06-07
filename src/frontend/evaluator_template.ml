
module Types = Types_
open MyUtil
open LengthInterface
open Types
open EvalUtil

exception EvalError of string


let report_dynamic_error msg =
  raise (EvalError(msg))


type nom_input_horz_element =
  | NomInputHorzText     of string
  | NomInputHorzEmbedded of abstract_tree * abstract_tree list
  | NomInputHorzContent  of nom_input_horz_element list * environment


let lex_horz_text (ctx : HorzBox.context_main) (s_utf8 : string) : HorzBox.horz_box list =
  let uchlst = InternalText.to_uchar_list (InternalText.of_utf8 s_utf8) in
    HorzBox.([HorzPure(PHCInnerString(ctx, uchlst))])


let rec reduce_beta envf evid valuel astdef =
  let envnew = add_to_environment envf evid (ref valuel) in
    interpret envnew astdef


and reduce_beta_list valuef valuearglst =
  match valuearglst with
  | [] ->
      valuef

  | valuearg :: astargtail ->
      begin
        match valuef with
        | FuncWithEnvironment(patbrs, envf) ->
            let valuefnew = select_pattern (Range.dummy "reduce_beta_list") envf valuearg patbrs in
              reduce_beta_list valuefnew astargtail

        | _ -> report_bug_value "reduce_beta_list" valuef
      end


and interpret_point env ast =
  let value = interpret env ast in
    get_point value


and interpret_path env pathcomplst cycleopt =
  let pathelemlst =
    pathcomplst |> List.map (function
      | PathLineTo(astpt) ->
          let pt = interpret_point env astpt in
            GraphicData.LineTo(pt)

      | PathCubicBezierTo(astpt1, astpt2, astpt) ->
          let pt1 = interpret_point env astpt1 in
          let pt2 = interpret_point env astpt2 in
          let pt = interpret_point env astpt in
            GraphicData.CubicBezierTo(pt1, pt2, pt)
    )
  in
  let closingopt =
    cycleopt |> option_map (function
      | PathLineTo(()) ->
          GraphicData.LineTo(())

      | PathCubicBezierTo(astpt1, astpt2, ()) ->
          let pt1 = interpret_point env astpt1 in
          let pt2 = interpret_point env astpt2 in
            GraphicData.CubicBezierTo(pt1, pt2, ())
    )
  in
    (pathelemlst, closingopt)


and interpret_input_horz_content env (ihlst : input_horz_element list) =
  ihlst |> List.map (function
    | InputHorzText(s) ->
        ImInputHorzText(s)

    | InputHorzEmbedded(astcmd, astarglst) ->
        ImInputHorzEmbedded(astcmd, astarglst)

    | InputHorzEmbeddedMath(astmath) ->
        ImInputHorzEmbeddedMath(astmath)

    | InputHorzContent(ast) ->
        let value = interpret env ast in
        begin
          match value with
          | InputHorzWithEnvironment(imihlst, envsub) ->
              ImInputHorzContent(imihlst, envsub)

          | _ -> report_bug_reduction "interpret_input_horz_content" ast value
        end
  )

and interpret_input_vert_content env (ivlst : input_vert_element list) =
  ivlst |> List.map (function
    | InputVertEmbedded(astcmd, astarglst) ->
        ImInputVertEmbedded(astcmd, astarglst)

    | InputVertContent(ast) ->
        let value = interpret env ast in
        begin
          match value with
          | InputVertWithEnvironment(imivlst, envsub) ->
              ImInputVertContent(imivlst, envsub)

          | _ -> report_bug_reduction "interpret_input_vert_content" ast value
        end
  )


and interpret env ast =
  match ast with

(* ---- basic value ---- *)

  | Value(v) -> v

  | FinishHeaderFile -> EvaluatedEnvironment(env)

  | FinishStruct -> EvaluatedEnvironment(env)

  | InputHorz(ihlst) ->
      let imihlst = interpret_input_horz_content env ihlst in
        InputHorzWithEnvironment(imihlst, env)
          (* -- lazy evaluation; evaluates embedded variables only -- *)

  | InputVert(ivlst) ->
      let imivlst = interpret_input_vert_content env ivlst in
        InputVertWithEnvironment(imivlst, env)
          (* -- lazy evaluation; evaluates embedded variables only -- *)

  | LengthDescription(flt, unitnm) ->
      let len =
        match unitnm with  (* temporary; ad-hoc handling of unit names *)
        | "pt"   -> Length.of_pdf_point flt
        | "cm"   -> Length.of_centimeter flt
        | "mm"   -> Length.of_millimeter flt
        | "inch" -> Length.of_inch flt
        | _      -> report_bug_ast "LengthDescription; unknown unit name" ast
      in
        LengthConstant(len)

  | LambdaVert(evid, astdef) -> LambdaVertWithEnvironment(evid, astdef, env)

  | LambdaHorz(evid, astdef) -> LambdaHorzWithEnvironment(evid, astdef, env)

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
        interpret envnew ast2

  | LetNonRecIn(pat, ast1, ast2) ->
      let value1 = interpret env ast1 in
        select_pattern (Range.dummy "LetNonRecIn") env value1 [PatternBranch(pat, ast2)]

  | Function(patbrs) ->
      FuncWithEnvironment(patbrs, env)

  | Apply(ast1, ast2) ->
      let value1 = interpret env ast1 in
      begin
        match value1 with
        | FuncWithEnvironment(patbrs, env1) ->
            let value2 = interpret env ast2 in
              select_pattern (Range.dummy "Apply") env1 value2 patbrs

        | PrimitiveWithEnvironment(patbrs, env1, _, _) ->
            let value2 = interpret env ast2 in
                          select_pattern (Range.dummy "Apply") env1 value2 patbrs

        | _ -> report_bug_reduction "Apply: not a function" ast1 value1
      end

  | IfThenElse(astb, ast1, ast2) ->
      let b = get_bool (interpret env astb) in
        if b then interpret env ast1 else interpret env ast2

(* ---- record ---- *)

  | Record(asc) ->
      RecordValue(Assoc.map_value (interpret env) asc)

  | AccessField(ast1, fldnm) ->
      let value1 = interpret env ast1 in
      begin
        match value1 with
        | RecordValue(asc1) ->
            begin
              match Assoc.find_opt asc1 fldnm with
              | None    -> report_bug_reduction ("AccessField: field '" ^ fldnm ^ "' not found") ast1 value1
              | Some(v) -> v
            end

        | _ -> report_bug_reduction "AccessField: not a Record" ast1 value1
      end

(* ---- imperatives ---- *)

  | LetMutableIn(evid, astini, astaft) ->
      let valueini = interpret env astini in
      let stid = register_location env valueini in
      let envnew = add_to_environment env evid (ref (Location(stid))) in
        interpret envnew astaft

  | Sequential(ast1, ast2) ->
      let value1 = interpret env ast1 in
      let value2 = interpret env ast2 in
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
                  let valuenew = interpret env astnew in
                    begin
                      update_location env stid valuenew;
                      UnitConstant
                    end
              | _ -> report_bug_value "Overwrite: value is not a Location" value
            end

        | None ->
            report_bug_ast ("Overwrite: mutable value '" ^ (EvalVarID.show_direct evid) ^ "' not found") ast
      end

  | WhileDo(astb, astc) ->
      let b = get_bool (interpret env astb) in
      if b then
        let _ = interpret env astc in interpret env (WhileDo(astb, astc))
      else
        UnitConstant

  | Dereference(astcont) ->
      let valuecont = interpret env astcont in
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
      let valueobj = interpret env astobj in
        select_pattern rng env valueobj patbrs

  | NonValueConstructor(constrnm, astcont) ->
      let valuecont = interpret env astcont in
        Constructor(constrnm, valuecont)

  | Module(astmdl, astaft) ->
      let value = interpret env astmdl in
      begin
        match value with
        | EvaluatedEnvironment(envfinal) -> interpret envfinal astaft
        | _                              -> report_bug_reduction "Module" astmdl value
      end

  | BackendMathList(astmlst) ->
      let mlstlst =
        List.map (fun astm -> get_math (interpret env astm)) astmlst
      in  (* slightly doubtful in terms of evaluation strategy *)
        MathValue(List.concat mlstlst)

  | PrimitiveTupleCons(asthd, asttl) ->
      let valuehd = interpret env asthd in
      let valuetl = interpret env asttl in
        TupleCons(valuehd, valuetl)

  | Path(astpt0, pathcomplst, cycleopt) ->
      let pt0 = interpret_point env astpt0 in
      let (pathelemlst, closingopt) = interpret_path env pathcomplst cycleopt in
        PathValue([GraphicData.GeneralPath(pt0, pathelemlst, closingopt)])

(**** include: __evaluator.ml ****)


and interpret_intermediate_input_vert env (valuectx : syntactic_value) (imivlst : intermediate_input_vert_element list) : syntactic_value =
  let rec interpret_commands env (imivlst : intermediate_input_vert_element list) =
    imivlst |> List.map (fun imiv ->
      match imiv with
      | ImInputVertEmbedded(astcmd, astarglst) ->
          let valuecmd = interpret env astcmd in
          begin
            match valuecmd with
            | LambdaVertWithEnvironment(evid, astdef, envf) ->
                let valuedef = reduce_beta envf evid valuectx astdef in
                let valuearglst =
                  astarglst |> List.fold_left (fun acc astarg ->
                    let valuearg = interpret env astarg in
                      Alist.extend acc valuearg
                  ) Alist.empty |> Alist.to_list
                    (* -- left-to-right evaluation -- *)
                in
                let valueret = reduce_beta_list valuedef valuearglst in
                  get_vert valueret

            | _ -> report_bug_reduction "interpret_intermediate_input_vert:1" astcmd valuecmd
          end

      | ImInputVertContent(imivlstsub, envsub) ->
          interpret_commands envsub imivlstsub

    ) |> List.concat
  in
  let imvblst = interpret_commands env imivlst in
    Vert(imvblst)


and interpret_intermediate_input_horz (env : environment) (valuectx : syntactic_value) (imihlst : intermediate_input_horz_element list) : syntactic_value =

  let (ctx, valuemcmd) = get_context valuectx in

  let rec normalize (imihlst : intermediate_input_horz_element list) =
    imihlst |> List.fold_left (fun acc imih ->
      match imih with
      | ImInputHorzEmbedded(astcmd, astarglst) ->
          let nmih = NomInputHorzEmbedded(astcmd, astarglst) in
            Alist.extend acc nmih

      | ImInputHorzText(s2) ->
          begin
            match Alist.chop_last acc with
            | Some(accrest, NomInputHorzText(s1)) -> (Alist.extend accrest (NomInputHorzText(s1 ^ s2)))
            | _                                   -> (Alist.extend acc (NomInputHorzText(s2)))
          end

      | ImInputHorzEmbeddedMath(astmath) ->
          let nmih = NomInputHorzEmbedded(Value(valuemcmd), [astmath]) in
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
      | NomInputHorzEmbedded(astcmd, astarglst) ->
          let valuecmd = interpret env astcmd in
          begin
            match valuecmd with
            | LambdaHorzWithEnvironment(evid, astdef, envf) ->
                let valuedef = reduce_beta envf evid valuectx astdef in
                let valuearglst =
                  astarglst |> List.fold_left (fun acc astarg ->
                    let valuearg = interpret env astarg in
                      Alist.extend acc valuearg
                  ) Alist.empty |> Alist.to_list
                    (* -- left-to-right evaluation -- *)
                in
                let valueret = reduce_beta_list valuedef valuearglst in
                let hblst = get_horz valueret in
                  hblst

            | _ -> report_bug_reduction "interpret_input_horz" astcmd valuecmd
          end

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
      let (b, envnew) = check_pattern_matching env pat valueobj in
        if b then
          interpret envnew astto
        else
          iter tail

  | PatternBranchWhen(pat, astcond, astto) :: tail ->
      let (b, envnew) = check_pattern_matching env pat valueobj in
      let cond = get_bool (interpret envnew astcond) in
        if b && cond then
          interpret envnew astto
        else
          iter tail


and check_pattern_matching (env : environment) (pat : pattern_tree) (valueobj : syntactic_value) =
  let return b = (b, env) in
  match (pat, valueobj) with
  | (PIntegerConstant(pnc), IntegerConstant(nc)) -> return (pnc = nc)
  | (PBooleanConstant(pbc), BooleanConstant(bc)) -> return (pbc = bc)

  | (PStringConstant(ast1), value2) ->
      let str1 = get_string (interpret env ast1) in
      let str2 = get_string value2 in
        return (String.equal str1 str2)

  | (PUnitConstant, UnitConstant) -> return true
  | (PWildCard, _)                -> return true

  | (PVariable(evid), _) ->
      let envnew = add_to_environment env evid (ref valueobj) in
        (true, envnew)

  | (PAsVariable(evid, psub), sub) ->
      let envnew = add_to_environment env evid (ref sub) in
        check_pattern_matching envnew psub sub

  | (PEndOfList, EndOfList) -> return true

  | (PListCons(phd, ptl), ListCons(hd, tl)) ->
      let (bhd, envhd) = check_pattern_matching env phd hd in
      let (btl, envtl) = check_pattern_matching envhd ptl tl in
      if bhd && btl then
        (true, envtl)
      else
        return false

  | (PEndOfTuple, EndOfTuple) -> return true

  | (PTupleCons(phd, ptl), TupleCons(hd, tl)) ->
      let (bhd, envhd) = check_pattern_matching env phd hd in
      let (btl, envtl) = check_pattern_matching envhd ptl tl in
      if bhd && btl then
        (true, envtl)
      else
        return false

  | (PConstructor(cnm1, psub), Constructor(cnm2, sub))
      when cnm1 = cnm2 -> check_pattern_matching env psub sub

  | _ -> return false


and add_letrec_bindings_to_environment (env : environment) (recbinds : letrec_binding list) : environment =
  let trilst =
    recbinds |> List.map (function LetRecBinding(evid, patbrs) ->
      let loc = ref StringEmpty in
      (evid, loc, patbrs)
    )
  in
  let envnew =
    trilst @|> env @|> List.fold_left (fun envacc (evid, loc, _) ->
      add_to_environment envacc evid loc
    )
  in
  trilst |> List.iter (fun (evid, loc, patbrs) ->
    loc := FuncWithEnvironment(patbrs, envnew)
  );
  envnew
