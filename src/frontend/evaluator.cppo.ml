
open MyUtil
open LengthInterface
open GraphicBase
open SyntaxBase
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
        | CodeSymbol(symb) ->
            Some(symb)

        | CodeValue(CdContentOf(_, symb)) ->
            Some(symb)

        | v ->
            report_bug_value (Printf.sprintf "not a symbol (%s)" (EvalVarID.show_direct evid)) v
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


let rec reduce_beta ?optional:(val_labmap : syntactic_value LabelMap.t = LabelMap.empty) (value1 : syntactic_value) (value2 : syntactic_value) =
  match value1 with
  | Closure(evid_labmap, patbr, env1) ->
      let env1 =
        LabelMap.fold (fun label evid env ->
          let loc =
            match val_labmap |> LabelMap.find_opt label with
            | None ->
                ref (Constructor("None", const_unit))

            | Some(value0) ->
                ref (Constructor("Some", value0))
          in
          add_to_environment env evid loc
        ) evid_labmap env1
      in
      select_pattern (Range.dummy "Apply") env1 value2 [patbr]

  | PrimitiveClosure(patbr, env1, _, _) ->
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

    | InputHorzEmbeddedCodeText(s) ->
        ImInputHorzEmbeddedCodeText(s)

    | InputHorzContent(ast) ->
        let value = interpret_0 env ast in
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
        let value = interpret_0 env ast in
        begin
          match value with
          | InputVertClosure(imivlst, envsub) ->
              ImInputVertContent(imivlst, envsub)

          | _ ->
              report_bug_reduction "interpret_input_vert_content" ast value
        end
  )


and interpret_0 (env : environment) (ast : abstract_tree) : syntactic_value =
  match ast with

(* Basic values: *)

  | ASTBaseConstant(bc) ->
      BaseConstant(bc)

  | ASTEndOfList ->
      List([])

  | ASTMath(ms) ->
      MathValue(ms)

  | InputHorz(ihs) ->
      let imihs = interpret_0_input_horz_content env ihs in
      InputHorzClosure(imihs, env)
        (* Lazy evaluation; evaluates embedded variables only *)

  | InputVert(ivs) ->
      let imivs = interpret_0_input_vert_content env ivs in
      InputVertClosure(imivs, env)
        (* Lazy evaluation; evaluates embedded variables only *)

(* Fundamentals: *)

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
      let env = add_letrec_bindings_to_environment env recbinds in
      interpret_0 env ast2

  | LetNonRecIn(pat, ast1, ast2) ->
      let value1 = interpret_0 env ast1 in
      select_pattern (Range.dummy "LetNonRecIn") env value1 [ PatternBranch(pat, ast2) ]

  | Function(evids, patbrs) ->
      Closure(evids, patbrs, env)

  | Apply(ast_labmap, ast1, ast2) ->
      let val_labmap = ast_labmap |> LabelMap.map (interpret_0 env) in
      let value1 = interpret_0 env ast1 in
      let value2 = interpret_0 env ast2 in
      reduce_beta ~optional:val_labmap value1 value2

  | IfThenElse(ast0, ast1, ast2) ->
      let value0 = interpret_0 env ast0 in
      let b = get_bool value0 in
      if b then interpret_0 env ast1 else interpret_0 env ast2

(* Records: *)

  | Record(asc) ->
      RecordValue(asc |> LabelMap.map (interpret_0 env))

  | AccessField(ast1, fldnm) ->
      let value1 = interpret_0 env ast1 in
      begin
        match value1 with
        | RecordValue(asc1) ->
            begin
              match asc1 |> LabelMap.find_opt fldnm with
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
              match asc1 |> LabelMap.find_opt fldnm with
              | None    -> report_bug_reduction ("UpdateField: field '" ^ fldnm ^ "' not found") ast1 value1
              | Some(_) -> asc1 |> LabelMap.add fldnm value2
            in
            RecordValue(asc1new)

        | _ ->
            report_bug_reduction "UpdateField: not a Record" ast1 value1
      end

(* Imperatives: *)

  | LetMutableIn(evid, ast_ini, ast_after) ->
      let value_ini = interpret_0 env ast_ini in
      let stid = register_location env value_ini in
      let env = add_to_environment env evid (ref (Location(stid))) in
      interpret_0 env ast_after

  | Overwrite(evid, ast_new) ->
      begin
        match find_in_environment env evid with
        | Some(rfvalue) ->
            let value = !rfvalue in
            begin
              match value with
              | Location(stid) ->
                  let value_new = interpret_0 env ast_new in
                  update_location env stid value_new;
                  const_unit

              | _ ->
                  report_bug_value "Overwrite: value is not a Location" value
            end

        | None ->
            report_bug_ast ("Overwrite: mutable value '" ^ (EvalVarID.show_direct evid) ^ "' not found") ast
      end

  | Dereference(ast_cont) ->
      let value_cont = interpret_0 env ast_cont in
      begin
        match value_cont with
        | Location(stid) ->
            begin
              match find_location_value env stid with
              | Some(value) -> value
              | None        -> report_bug_reduction "Dereference; not found" ast_cont value_cont
            end

        | _ ->
            report_bug_reduction "Dereference" ast_cont value_cont
      end

  | PatternMatch(rng, astobj, patbrs) ->
      let valueobj = interpret_0 env astobj in
      select_pattern rng env valueobj patbrs

  | NonValueConstructor(constrnm, ast_cont) ->
      let value_cont = interpret_0 env ast_cont in
      Constructor(constrnm, value_cont)

  | BackendMathList(asts) ->
      let ms = asts |> List.map (fun ast -> get_math (interpret_0 env ast)) |> List.concat in
      MathValue(ms)

  | PrimitiveTuple(asts) ->
      let values = asts |> TupleList.map (interpret_0 env) in
        (* Should be left-to-right *)
      Tuple(values |> TupleList.to_list)

(* Staging constructs: *)

  | Prev(_) ->
      report_bug_ast "Prev(_) at stage 0" ast

  | Next(ast1) ->
      let code1 = interpret_1 env ast1 in
      CodeValue(code1)

  | Persistent(_) ->
      report_bug_ast "Persistent(_) at stage 0" ast

  | Lift(ast1) ->
      failwith "TODO: Lift"
(*
      let value1 = interpret_0 env ast1 in
      CodeValue(CdPersistent(value1))
*)

  | ASTCodeSymbol(symb) ->
      report_bug_ast "ASTCodeSymbol(_) at stage 0" ast

#include "__evaluator_0.gen.ml"


and interpret_1 (env : environment) (ast : abstract_tree) : code_value =
  match ast with
  | ASTBaseConstant(bc) ->
      CdBaseConstant(bc)

  | ASTMath(mlst) ->
      CdMath(mlst)

  | ASTEndOfList ->
      CdEndOfList

  | InputHorz(ihs) ->
      let cdihs = ihs |> map_input_horz (interpret_1 env) in
      CdInputHorz(cdihs)

  | InputVert(ivs) ->
      let cdivs = ivs |> map_input_vert (interpret_1 env) in
      CdInputVert(cdivs)

  | ContentOf(rng, evid) ->
      begin
        match find_in_environment env evid with
        | Some(rfvalue) ->
            begin
              match !rfvalue with
              | CodeSymbol(symb) ->
                  CdContentOf(rng, symb)

              | CodeValue(cv) ->
                  cv

              | v ->
                  report_bug_value
                    (Printf.sprintf "not a code value (%s, used at %s)"
                      (EvalVarID.show_direct evid) (Range.show rng))
                    v
            end

        | None ->
            report_bug_ast ("not found (" ^ Range.show rng ^ ")") ast
      end

  | LetRecIn(recbinds, ast2) ->
      let (env, cdrecbinds) = interpret_letrec_bindings_1 env recbinds in
      let code2 = interpret_1 env ast2 in
      CdLetRecIn(cdrecbinds, code2)

  | LetNonRecIn(pattr, ast1, ast2) ->
      let code1 = interpret_1 env ast1 in
      let (env, cdpattr) = interpret_1_pattern_tree env pattr in
        (* Generate the symbols corresponding to the variables in the pattern
           and add them to the environment *)
      let code2 = interpret_1 env ast2 in
      CdLetNonRecIn(cdpattr, code1, code2)

  | Function(evid_labmap, patbr) ->
      let (env, symb_labmap) =
        LabelMap.fold (fun label evid (env, symb_labmap) ->
          let (env, symb) = generate_symbol_for_eval_var_id evid env in
          (env, symb_labmap |> LabelMap.add label symb)
        ) evid_labmap (env, LabelMap.empty)
      in
      let cdpatbr = interpret_1_pattern_branch env patbr in
      CdFunction(symb_labmap, cdpatbr)

  | Apply(ast_labmap, ast1, ast2) ->
      let code_labmap = ast_labmap |> LabelMap.map (interpret_1 env) in
      let code1 = interpret_1 env ast1 in
      let code2 = interpret_1 env ast2 in
      CdApply(code_labmap, code1, code2)

  | IfThenElse(ast0, ast1, ast2) ->
      let code0 = interpret_1 env ast0 in
      let code1 = interpret_1 env ast1 in
      let code2 = interpret_1 env ast2 in
      CdIfThenElse(code0, code1, code2)

  | Record(asc) ->
      let cdasc = asc |> LabelMap.map (interpret_1 env) in
      CdRecord(cdasc)

  | AccessField(ast1, field) ->
      let code1 = interpret_1 env ast1 in
      CdAccessField(code1, field)

  | UpdateField(ast1, field, ast2) ->
      let code1 = interpret_1 env ast1 in
      let code2 = interpret_1 env ast2 in
      CdUpdateField(code1, field, code2)

  | LetMutableIn(evid, ast1, ast2) ->
      let (env, symb) = generate_symbol_for_eval_var_id evid env in
      let code1 = interpret_1 env ast1 in
      let code2 = interpret_1 env ast2 in
      CdLetMutableIn(symb, code1, code2)

  | Overwrite(evid, ast1) ->
      begin
        match find_symbol env evid with
        | Some(symb) ->
            let code1 = interpret_1 env ast1 in
            CdOverwrite(symb, code1)

        | None ->
            report_bug_ast "symbol not found" ast
      end

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

  | BackendMathList(asts) ->
      let codes = asts |> List.map (interpret_1 env) in
      CdMathList(codes)

  | PrimitiveTuple(asts) ->
      let codes = asts |> TupleList.map (interpret_1 env) in
      CdTuple(codes)

  | Prev(ast1) ->
      let value1 = interpret_0 env ast1 in
      begin
        match value1 with
        | CodeValue(code) -> code
        | _               -> report_bug_reduction "Prev; not a code value" ast value1
      end

  | Next(_) ->
      report_bug_ast "Next(_) at stage 1" ast

  | Persistent(rng, evid) ->
      CdPersistent(rng, evid)

  | Lift(_) ->
      report_bug_ast "Lift(_) at stage 1" ast

  | ASTCodeSymbol(symb) ->
      CdContentOf(Range.dummy "ASTCodeSymbol", symb)

#include "__evaluator_1.gen.ml"


and interpret_1_pattern_branch (env : environment) = function
  | PatternBranch(pattr, ast) ->
      let (env, cdpattr) = interpret_1_pattern_tree env pattr in
      CdPatternBranch(cdpattr, interpret_1 env ast)

  | PatternBranchWhen(pattr, ast, ast1) ->
      let (env, cdpattr) = interpret_1_pattern_tree env pattr in
      CdPatternBranchWhen(cdpattr, interpret_1 env ast, interpret_1 env ast1)


and interpret_1_pattern_tree (env : environment) = function
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


and interpret_text_mode_intermediate_input_vert (env : environment) (value_tctx : syntactic_value) (imivs : intermediate_input_vert_element list) : syntactic_value =
  let rec interpret_commands (env : environment) (imivs : intermediate_input_vert_element list) =
    imivs |> List.map (fun imiv ->
      match imiv with
      | ImInputVertEmbedded(ast_abs) ->
          let value_abs = interpret_0 env ast_abs in
          let value_vert = reduce_beta value_abs value_tctx in
          get_string value_vert

      | ImInputVertContent(imivs_sub, env_sub) ->
          interpret_commands env_sub imivs_sub

    ) |> String.concat ""
  in
  let s = interpret_commands env imivs in
  make_string s


and interpret_text_mode_intermediate_input_horz (env : environment) (value_tctx : syntactic_value) (imihs : intermediate_input_horz_element list) : syntactic_value =

  let tctx = get_text_mode_context value_tctx in

  let rec normalize (imihs : intermediate_input_horz_element list) =
    imihs |> List.fold_left (fun acc imih ->
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
          failwith "TODO: Evaluator_> math; remains to be supported."

      | ImInputHorzEmbeddedCodeText(s) ->
          failwith "TODO: Evaluator_> code text; remains to be supported."

      | ImInputHorzContent(imihs_sub, env_sub) ->
          let nmihs_sub = normalize imihs_sub in
          let nmih = NomInputHorzContent(nmihs_sub, env_sub) in
          Alist.extend acc nmih

    ) Alist.empty |> Alist.to_list
  in

  let rec interpret_commands (env : environment) (nmihs : nom_input_horz_element list) : string =
    nmihs |> List.map (fun nmih ->
      match nmih with
      | NomInputHorzEmbedded(ast_abs) ->
          let value_abs = interpret_0 env ast_abs in
          let value_ret = reduce_beta value_abs value_tctx in
          get_string value_ret

      | NomInputHorzThunk(value_cmd, ast_arg) ->
          let value_arg = interpret_0 env ast_arg in
          let value_ret = reduce_beta value_cmd value_arg in
          get_string value_ret

      | NomInputHorzText(s) ->
          let uchs = InternalText.to_uchar_list (InternalText.of_utf8 s) in
          let uchs_ret = tctx |> TextBackend.stringify uchs in
          InternalText.to_utf8 (InternalText.of_uchar_list uchs_ret)

      | NomInputHorzContent(nmihs_sub, env_sub) ->
          interpret_commands env_sub nmihs_sub

    ) |> String.concat ""
  in

  let nmihs = normalize imihs in
  let s = interpret_commands env nmihs in
  make_string s


and interpret_pdf_mode_intermediate_input_vert (env : environment) (value_ctx : syntactic_value) (imivs : intermediate_input_vert_element list) : syntactic_value =
  let rec interpret_commands (env : environment) (imivs : intermediate_input_vert_element list) =
    imivs |> List.map (fun imiv ->
      match imiv with
      | ImInputVertEmbedded(ast_abs) ->
          let value_abs = interpret_0 env ast_abs in
          let value_vert = reduce_beta value_abs value_ctx in
          get_vert value_vert

      | ImInputVertContent(imivs_sub, env_sub) ->
          interpret_commands env_sub imivs_sub

    ) |> List.concat
  in
  let imvbs = interpret_commands env imivs in
  make_vert imvbs


and interpret_pdf_mode_intermediate_input_horz (env : environment) (value_ctx : syntactic_value) (imihs : intermediate_input_horz_element list) : syntactic_value =

  let (ctx, ctxsub) = get_context value_ctx in
  let value_mcmd = make_math_command_func ctxsub.math_command in

  let rec normalize (imihs : intermediate_input_horz_element list) =
    imihs |> List.fold_left (fun acc imih ->
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

      | ImInputHorzEmbeddedMath(ast_math) ->
          let value_mcmdctx = reduce_beta value_mcmd value_ctx in
          let nmih = NomInputHorzThunk(value_mcmdctx, ast_math) in
          Alist.extend acc nmih

      | ImInputHorzEmbeddedCodeText(s) ->
          begin
            match ctxsub.code_text_command with
            | DefaultCodeTextCommand ->
                let nmih = NomInputHorzText(s) in
                Alist.extend acc nmih

            | CodeTextCommand(value_ctcmd) ->
                let value_ctcmdctx = reduce_beta value_ctcmd value_ctx in
                let nmih = NomInputHorzThunk(value_ctcmdctx, ASTBaseConstant(BCString(s))) in
                Alist.extend acc nmih
          end

      | ImInputHorzContent(imihs_sub, env_sub) ->
          let nmihs_sub = normalize imihs_sub in
          let nmih = NomInputHorzContent(nmihs_sub, env_sub) in
          Alist.extend acc nmih

    ) Alist.empty |> Alist.to_list
  in

  let rec interpret_commands (env : environment) (nmihs : nom_input_horz_element list) : HorzBox.horz_box list =
    nmihs |> List.map (fun nmih ->
      match nmih with
      | NomInputHorzEmbedded(ast_abs) ->
          let value_abs = interpret_0 env ast_abs in
          let value_horz = reduce_beta value_abs value_ctx in
          get_horz value_horz

      | NomInputHorzThunk(value_mcmdctx, ast_math) ->
          let value_math = interpret_0 env ast_math in
          let value_horz = reduce_beta value_mcmdctx value_math in
          get_horz value_horz

      | NomInputHorzText(s) ->
          lex_horz_text ctx s

      | NomInputHorzContent(nmihs_sub, env_sub) ->
          interpret_commands env_sub nmihs_sub

    ) |> List.concat
  in

  let nmihs = normalize imihs in
  let hbs = interpret_commands env nmihs in
  make_horz hbs


and select_pattern (rng : Range.t) (env : environment) (value_obj : syntactic_value) (patbrs : pattern_branch list) : syntactic_value =
  let iter = select_pattern rng env value_obj in
  match patbrs with
  | [] ->
      report_dynamic_error ("no matches (" ^ (Range.to_string rng) ^ ")")

  | PatternBranch(pat, ast_to) :: tail ->
      begin
        match check_pattern_matching env pat value_obj with
        | Some(env_new) -> interpret_0 env_new ast_to
        | None          -> iter tail
      end

  | PatternBranchWhen(pat, ast_cond, ast_to) :: tail ->
      begin
        match check_pattern_matching env pat value_obj with
        | Some(env_new) ->
            let cond = get_bool (interpret_0 env_new ast_cond) in
            if cond then interpret_0 env_new ast_to else iter tail

        | None ->
            iter tail
      end


and check_pattern_matching (env : environment) (pat : pattern_tree) (value_obj : syntactic_value) : environment option =
  match (pat, value_obj) with
  | (PIntegerConstant(pnc), BaseConstant(BCInt(nc))) ->
      if pnc = nc then Some(env) else None

  | (PBooleanConstant(pbc), BaseConstant(BCBool(bc))) ->
      if pbc = bc then Some(env) else None

  | (PStringConstant(psc), BaseConstant(BCString(str2))) ->
      if String.equal psc str2 then Some(env) else None

  | (PUnitConstant, BaseConstant(BCUnit)) ->
      Some(env)

  | (PWildCard, _) ->
      Some(env)

  | (PVariable(evid), _) ->
      let env = add_to_environment env evid (ref value_obj) in
      Some(env)

  | (PAsVariable(evid, psub), _) ->
      let open OptionMonad in
      check_pattern_matching env psub value_obj >>= fun env ->
      let env = add_to_environment env evid (ref value_obj) in
      Some(env)

  | (PEndOfList, List([])) ->
      Some(env)

  | (PListCons(pat_head, pat_tail), List(v_head :: vs_tail)) ->
      let open OptionMonad in
      check_pattern_matching env pat_head v_head >>= fun env ->
      check_pattern_matching env pat_tail (List(vs_tail))

  | (PTuple(ps), Tuple(vs)) ->
      let open OptionMonad in
      begin
        try
          List.fold_left2 (fun envopt p v ->
            envopt >>= fun env ->
            check_pattern_matching env p v
          ) (Some(env)) (ps |> TupleList.to_list) vs
        with
        | Invalid_argument(_) -> None
      end

  | (PConstructor(cnm1, psub), Constructor(cnm2, sub)) ->
      if String.equal cnm1 cnm2 then
        check_pattern_matching env psub sub
      else
        None

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


and interpret_letrec_bindings_1 (env : environment) (recbinds : letrec_binding list) : environment * code_letrec_binding list =
  (* Generate the symbols for the identifiers and add them to the environment: *)
  let (env, zippedacc) =
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
  (env, cdrecbinds)


let interpret_bindings_0 (env : environment) (binds : binding list) : environment * code_rec_or_nonrec list =
  let (env, acc) =
    binds |> List.fold_left (fun (env, acc) (Bind(stage, rec_or_nonrec)) ->
      match stage with
      | Persistent0 | Stage0 ->
          let env =
            match rec_or_nonrec with
            | NonRec(evid, ast) ->
                let value = interpret_0 env ast in
                add_to_environment env evid (ref value)

            | Rec(recbinds) ->
                add_letrec_bindings_to_environment env recbinds

            | Mutable(evid, ast_ini) ->
                let value_ini = interpret_0 env ast_ini in
                let stid = register_location env value_ini in
                add_to_environment env evid (ref (Location(stid)))
          in
          (env, acc)

      | Stage1 ->
          begin
            match rec_or_nonrec with
            | NonRec(evid, ast) ->
                let code = interpret_1 env ast in
                let (env, symb) = generate_symbol_for_eval_var_id evid env in
                (env, Alist.extend acc (CdNonRec(symb, code)))

            | Rec(recbinds) ->
                let (env, cdrecbinds) = interpret_letrec_bindings_1 env recbinds in
                (env, Alist.extend acc (CdRec(cdrecbinds)))

            | Mutable(evid, ast) ->
                let code = interpret_1 env ast in
                let (env, symb) = generate_symbol_for_eval_var_id evid env in
                (env, Alist.extend acc (CdMutable(symb, code)))
          end

    ) (env, Alist.empty)
  in
  (env, acc |> Alist.to_list)
