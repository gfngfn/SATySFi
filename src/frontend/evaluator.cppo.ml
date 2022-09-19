
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
  | NomInputHorzText           of string
  | NomInputHorzCommandClosure of horz_command_closure


let convert_command_application_to_application (e_cmd : abstract_tree) (args : (abstract_tree LabelMap.t * abstract_tree) list) : abstract_tree =
  args |> List.fold_left (fun e_acc (e_labmap, e_arg) ->
    Apply(e_labmap, e_acc, e_arg)
  ) e_cmd


let lex_horz_text (ctx : HorzBox.context_main) (s_utf8 : string) : HorzBox.horz_box list =
  let uchs = InternalText.to_uchar_list (InternalText.of_utf8 s_utf8) in
  HorzBox.([ HorzPure(PHCInnerString(ctx, uchs)) ])


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


let rec reduce_beta ~msg ?optional:(val_labmap : syntactic_value LabelMap.t = LabelMap.empty) (value1 : syntactic_value) (value2 : syntactic_value) : syntactic_value =
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
      select_pattern (Range.dummy "Apply") env1 value2 [ patbr ]

  | PrimitiveClosure(patbr, env1, _, _) ->
      select_pattern (Range.dummy "Apply") env1 value2 [ patbr ]

  | _ ->
      report_bug_value (Printf.sprintf "reduce_beta (%s): not a function" msg) value1


and reduce_beta_list ~msg (value1 : syntactic_value) (value_args : syntactic_value list) : syntactic_value =
  List.fold_left (reduce_beta ~msg ~optional:LabelMap.empty) value1 value_args


and interpret_0_input_horz_content (env : environment) (ihs : input_horz_element list) : input_horz_value_element list =
  ihs |> List.map (function
    | InputHorzText(s) ->
        [ InputHorzValueText(s) ]

    | InputHorzApplyCommand{ command = ast_cmd; arguments = args } ->
        let ast = convert_command_application_to_application ast_cmd args in
        let value = interpret_0 env ast in
        let hclosure = get_horz_command_closure value in
        [ InputHorzValueCommandClosure(hclosure) ]

    | InputHorzEmbeddedMath(ast_math) ->
        let value = interpret_0 env ast_math in
        let imvs = get_math_text ~msg:"InputHorzEmbeddedMath" value in
        [ InputHorzValueEmbeddedMath(imvs) ]

    | InputHorzEmbeddedCodeArea(s) ->
        [ InputHorzValueEmbeddedCodeArea(s) ]

    | InputHorzContent(ast) ->
        let value = interpret_0 env ast in
        get_horz_text value

  ) |> List.concat


and interpret_0_input_vert_content (env : environment) (ivs : input_vert_element list) : input_vert_value_element list =
  ivs |> List.map (function
    | InputVertApplyCommand{ command = ast_cmd; arguments = args } ->
        let ast = convert_command_application_to_application ast_cmd args in
        let value = interpret_0 env ast in
        let vclosure = get_vert_command_closure value in
        [ InputVertValueCommandClosure(vclosure) ]

    | InputVertContent(ast) ->
        let value = interpret_0 env ast in
        get_vert_text value

  ) |> List.concat


and interpret_0_input_math_content (env : environment) (ims : input_math_element list) : input_math_value_element list =
  ims |> List.map (fun im ->
    let InputMathElement{ base = imbase; sub = ims_sub_opt; sup = ims_sup_opt } = im in
    let imvs_sub_opt = ims_sub_opt |> Option.map (interpret_0_input_math_content env) in
    let imvs_sup_opt = ims_sup_opt |> Option.map (interpret_0_input_math_content env) in
    match imbase with
    | InputMathChar(uch) ->
        InputMathValueElement{
          base = InputMathValueChar(uch);
          sub  = imvs_sub_opt;
          sup  = imvs_sup_opt;
        }

    | InputMathApplyCommand{
        command   = ast_cmd;
        arguments = args;
      } ->
        let ast = convert_command_application_to_application ast_cmd args in
        let value = interpret_0 env ast in
        let mclosure = get_math_command_closure value in
        InputMathValueElement{
          base = InputMathValueEmbedded(mclosure);
          sub  = imvs_sub_opt;
          sup  = imvs_sup_opt;
        }

    | InputMathContent(ast) ->
        let value = interpret_0 env ast in
        let imvs = get_math_text ~msg:"InputMathContent" value in
        let opt =
          match imvs with
          | [ imv0 ] ->
              let
                InputMathValueElement{
                  base = imvbase0;
                  sub  = imvs0_sub_opt;
                  sup  = imvs0_sup_opt;
                } = imv0
              in
              let open OptionMonad in
              begin
                match (imvs0_sub_opt, imvs_sub_opt) with
                | (Some(_), Some(_)) -> None
                | (Some(_), None)    -> Some(imvs0_sub_opt)
                | (None, _)          -> Some(imvs_sub_opt)
              end >>= fun imvs_sub_opt ->
              begin
                match (imvs0_sup_opt, imvs_sup_opt) with
                | (Some(_), Some(_)) -> None
                | (Some(_), None)    -> Some(imvs0_sup_opt)
                | (None, _)          -> Some(imvs_sup_opt)
              end >>= fun imvs_sup_opt ->
              Some(InputMathValueElement{
                base = imvbase0;
                sub  = imvs_sub_opt;
                sup  = imvs_sup_opt;
              })

          | _ ->
              None
        in
        begin
          match opt with
          | None ->
              InputMathValueElement{
                base = InputMathValueGroup(imvs);
                sub  = imvs_sub_opt;
                sup  = imvs_sup_opt;
              }

          | Some(imv) ->
              imv
        end
  )


and interpret_0 (env : environment) (ast : abstract_tree) : syntactic_value =
  match ast with

(* Basic values: *)

  | ASTBaseConstant(bc) ->
      BaseConstant(bc)

  | ASTEndOfList ->
      List([])

  | InputHorz(ihs) ->
      let ihvs = interpret_0_input_horz_content env ihs in
      InputHorzValue(ihvs)

  | InputVert(ivs) ->
      let ivvs = interpret_0_input_vert_content env ivs in
      InputVertValue(ivvs)

  | InputMath(ims) ->
      let imvs = interpret_0_input_math_content env ims in
      InputMathValue(imvs)

  | LambdaHorz(evid_ctx, ast0) ->
      let hclosure =
        HorzCommandClosureSimple{
          context_binder = evid_ctx;
          body           = ast0;
          environment    = env;
        }
      in
      HorzCommandClosure(hclosure)

  | LambdaVert(evid_ctx, ast0) ->
      let vclosure =
        VertCommandClosureSimple{
          context_binder = evid_ctx;
          body           = ast0;
          environment    = env;
        }
      in
      VertCommandClosure(vclosure)

  | LambdaMath(evid_ctx, evid_pair_opt, ast0) ->
      let mclosure =
        match evid_pair_opt with
        | None ->
            MathCommandClosureSimple{
              context_binder = evid_ctx;
              body           = ast0;
              environment    = env;
            }

        | Some((evid_sub, evid_sup)) ->
            MathCommandClosureWithScripts{
              context_binder = evid_ctx;
              sub_binders    = evid_sub;
              sup_binders    = evid_sup;
              body           = ast0;
              environment    = env;
            }
      in
      MathCommandClosure(mclosure)

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
      reduce_beta ~msg:"Apply" ~optional:val_labmap value1 value2

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

  | ASTEndOfList ->
      CdEndOfList

  | InputHorz(ihs) ->
      let cdihs = ihs |> map_input_horz (interpret_1 env) in
      CdInputHorz(cdihs)

  | InputVert(ivs) ->
      let cdivs = ivs |> map_input_vert (interpret_1 env) in
      CdInputVert(cdivs)

  | InputMath(ims) ->
      let cdims = ims |> map_input_math (interpret_1 env) in
      CdInputMath(cdims)

  | LambdaHorz(evid_ctx, ast0) ->
      let (env, symb_ctx) = generate_symbol_for_eval_var_id evid_ctx env in
      let code0 = interpret_1 env ast0 in
      CdLambdaHorz(symb_ctx, code0)

  | LambdaVert(evid_ctx, ast0) ->
      let (env, symb_ctx) = generate_symbol_for_eval_var_id evid_ctx env in
      let code0 = interpret_1 env ast0 in
      CdLambdaVert(symb_ctx, code0)

  | LambdaMath(evid_ctx, evid_pair_opt, ast0) ->
      let (env, symb_ctx) = generate_symbol_for_eval_var_id evid_ctx env in
      let (env, symb_pair_opt) =
        match evid_pair_opt with
        | None ->
            (env, None)

        | Some((evid_sub, evid_sup)) ->
            let (env, symb_sub) = generate_symbol_for_eval_var_id evid_sub env in
            let (env, symb_sup) = generate_symbol_for_eval_var_id evid_sup env in
            (env, Some((symb_sub, symb_sup)))
      in
      let code0 = interpret_1 env ast0 in
      CdLambdaMath(symb_ctx, symb_pair_opt, code0)

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


and read_text_mode_math_text (value_tctx : syntactic_value) (imvs : input_math_value_element list) : syntactic_value =

  let (_tctx, tctxsub) = get_text_mode_context value_tctx in
  let value_mscriptsf = make_math_scripts_func tctxsub.text_mode_math_scripts_func in
  let loc_tctx = ref value_tctx in

  let rec iter (imvs : input_math_value_element list) =
    imvs |> List.map (fun imv ->
      let InputMathValueElement{ base; sub; sup } = imv in
      match base with
      | InputMathValueChar(uch) ->
          let s_base = InternalText.to_utf8 (InternalText.of_uchar_list [ uch ]) in
            (* TODO: define the conversion of chars and use it here *)
          let s_sub_opt = sub |> Option.map iter in
          let s_sup_opt = sup |> Option.map iter in
          let value =
            reduce_beta_list ~msg:"InputMathValueChar" value_mscriptsf [
              make_string s_base;
              make_option make_string s_sub_opt;
              make_option make_string s_sup_opt;
            ]
          in
          get_string value

      | InputMathValueEmbedded(mclosure) ->
          begin
            match mclosure with
            | MathCommandClosureSimple{
                context_binder = evid_ctx;
                body           = ast_body;
                environment    = env;
              } ->
                let s_base =
                  let value =
                    let env = add_to_environment env evid_ctx loc_tctx in
                    interpret_0 env ast_body
                  in
                  get_string value
                in
                let s_sub_opt = sub |> Option.map iter in
                let s_sup_opt = sup |> Option.map iter in
                let value =
                  reduce_beta_list ~msg:"InputMathValueEmbedded" value_mscriptsf [
                    make_string s_base;
                    make_option make_string s_sub_opt;
                    make_option make_string s_sup_opt;
                  ]
                in
                get_string value

            | MathCommandClosureWithScripts{
                context_binder = evid_ctx;
                sub_binders    = evid_sub;
                sup_binders    = evid_sup;
                body           = ast;
                environment    = env;
              } ->
                let value =
                  let value_sub = make_option make_math_text sub in
                  let value_sup = make_option make_math_text sup in
                  let env = add_to_environment env evid_ctx loc_tctx in
                  let env = add_to_environment env evid_sub (ref value_sub) in
                  let env = add_to_environment env evid_sup (ref value_sup) in
                  interpret_0 env ast
                in
                get_string value
          end

      | InputMathValueGroup(imvs_group) ->
          let s_base = iter imvs_group in
          let s_sub_opt = sub |> Option.map iter in
          let s_sup_opt = sup |> Option.map iter in
          let value =
            reduce_beta_list ~msg:"InputMathValueGroup" value_mscriptsf [
              make_string s_base;
              make_option make_string s_sub_opt;
              make_option make_string s_sup_opt;
            ]
          in
          get_string value

    ) |> String.concat ""
  in
  let s = iter imvs in
  make_string s


and read_text_mode_vert_text (value_tctx : syntactic_value) (ivvs : input_vert_value_element list) : syntactic_value =

  let loc_tctx = ref value_tctx in

  let rec interpret_commands (ivvs : input_vert_value_element list) =
    ivvs |> List.map (function
    | InputVertValueCommandClosure(vclosure) ->
        let
          VertCommandClosureSimple{
            context_binder = evid_ctx;
            body           = ast_body;
            environment    = env;
          } = vclosure
        in
        let value =
          let env = add_to_environment env evid_ctx loc_tctx in
          interpret_0 env ast_body
        in
        get_string value

    ) |> String.concat ""
  in
  let s = interpret_commands ivvs in
  make_string s


and read_text_mode_horz_text (value_tctx : syntactic_value) (ihvs : input_horz_value_element list) : syntactic_value =

  let (tctx, tctxsub) = get_text_mode_context value_tctx in
  let value_mcmd = make_math_command_func tctxsub.text_mode_math_command in
  let loc_tctx = ref value_tctx in

  (* Merges adjacent `InputHorzValueText`s into single `NomInputHorzText`. *)
  let rec normalize (ihvs : input_horz_value_element list) =
    ihvs |> List.fold_left (fun acc ihv ->
      match ihv with
      | InputHorzValueCommandClosure(hclosure) ->
          let nmih = NomInputHorzCommandClosure(hclosure) in
          Alist.extend acc nmih

      | InputHorzValueText(s2) ->
          begin
            match Alist.chop_last acc with
            | Some((accrest, NomInputHorzText(s1))) -> (Alist.extend accrest (NomInputHorzText(s1 ^ s2)))
            | _                                     -> (Alist.extend acc (NomInputHorzText(s2)))
          end

      | InputHorzValueEmbeddedMath(imvs) ->
          let value =
            reduce_beta ~msg:"InputHorzValueEmbeddedMath" value_mcmd (InputMathValue(imvs))
          in
          let hclosure = get_horz_command_closure value in
          Alist.extend acc (NomInputHorzCommandClosure(hclosure))

      | InputHorzValueEmbeddedCodeArea(s) ->
          begin
            match make_code_text_command_func tctxsub.text_mode_code_text_command with
            | None ->
                Alist.extend acc (NomInputHorzText(s))

            | Some(value_ctcmd) ->
                let value =
                  reduce_beta ~msg:"InputHorzValueEmbeddedCodeArea" value_ctcmd (BaseConstant(BCString(s)))
                in
                let hclosure = get_horz_command_closure value in
                Alist.extend acc (NomInputHorzCommandClosure(hclosure))
          end

    ) Alist.empty |> Alist.to_list
  in

  let rec interpret_commands (nmihs : nom_input_horz_element list) : string =
    nmihs |> List.map (fun nmih ->
      match nmih with
      | NomInputHorzCommandClosure(hclosure) ->
          let
            HorzCommandClosureSimple{
              context_binder = evid_ctx;
              body           = ast_body;
              environment    = env;
            } = hclosure
          in
          let value =
            let env = add_to_environment env evid_ctx loc_tctx in
            interpret_0 env ast_body
          in
          get_string value

      | NomInputHorzText(s) ->
          let uchs = InternalText.to_uchar_list (InternalText.of_utf8 s) in
          let uchs_ret = tctx |> TextBackend.stringify uchs in
          InternalText.to_utf8 (InternalText.of_uchar_list uchs_ret)

    ) |> String.concat ""
  in

  let nmihs = normalize ihvs in
  let s = interpret_commands nmihs in
  make_string s


and append_sub_and_super_scripts (ictx : input_context) ~base:(mbs_base : math_box list) ~sub:(mbs_sub_opt : (math_box list) option) ~sup:(mbs_sup_opt : (math_box list) option) : math_box list =
  match (mbs_sub_opt, mbs_sup_opt) with
  | (None, None) ->
      mbs_base

  | (Some(mbs_sub), None) ->
      [ MathBoxSubscript{ context = ictx; base = mbs_base; sub = mbs_sub } ]

  | (None, Some(mbs_sup)) ->
      [ MathBoxSuperscript{ context = ictx; base = mbs_base; sup = mbs_sup } ]

  | (Some(mbs_sub), Some(mbs_sup)) ->
      [
        MathBoxSuperscript{
          context = ictx;
          base    = [ MathBoxSubscript{ context = ictx; base = mbs_base; sub = mbs_sub } ];
          sup     = mbs_sup;
        };
      ]


and read_pdf_mode_math_text (ictx : input_context) (imvs : input_math_value_element list) : math_box list =
  let rec iter (ictx : input_context) (imvs : input_math_value_element list) =
    imvs |> List.map (fun imv ->
      let InputMathValueElement{ base; sub; sup } = imv in
      match base with
      | InputMathValueChar(uch) ->
          let (mk, uch_aft) = MathContext.convert_math_variant_char ictx uch in
          let mbs_base =
            [
              MathBoxAtom{
                kind = mk;
                main = MathChar{ context = ictx; is_big = false; chars = [ uch_aft ] };
              };
            ]
          in
          let ctx_scripts = MathContext.(ictx |> make |> enter_script FontInfo.get_math_constants |> context_for_text) in
          let mbs_sub_opt = sub |> Option.map (iter ctx_scripts) in
          let mbs_sup_opt = sup |> Option.map (iter ctx_scripts) in
          append_sub_and_super_scripts ictx ~base:mbs_base ~sub:mbs_sub_opt ~sup:mbs_sup_opt

      | InputMathValueEmbedded(mclosure) ->
          begin
            match mclosure with
            | MathCommandClosureSimple{
                context_binder = evid_ctx;
                body           = ast;
                environment    = env;
              } ->
                let value =
                  let env = add_to_environment env evid_ctx (ref (Context(ictx))) in
                  interpret_0 env ast
                in
                let mbs_base = get_math_boxes value in
                let ctx_scripts = MathContext.(ictx |> make |> enter_script FontInfo.get_math_constants |> context_for_text) in
                let mbs_sub_opt = sub |> Option.map (iter ctx_scripts) in
                let mbs_sup_opt = sup |> Option.map (iter ctx_scripts) in
                append_sub_and_super_scripts ictx ~base:mbs_base ~sub:mbs_sub_opt ~sup:mbs_sup_opt

            | MathCommandClosureWithScripts{
                context_binder = evid_ctx;
                sub_binders    = evid_sub;
                sup_binders    = evid_sup;
                body           = ast;
                environment    = env;
              } ->
                let value =
                  let value_sub = make_option make_math_text sub in
                  let value_sup = make_option make_math_text sup in
                  let env = add_to_environment env evid_ctx (ref (Context(ictx))) in
                  let env = add_to_environment env evid_sub (ref value_sub) in
                  let env = add_to_environment env evid_sup (ref value_sup) in
                  interpret_0 env ast
                in
                get_math_boxes value
          end

      | InputMathValueGroup(imvs_group) ->
          let mbs_base = iter ictx imvs_group in
          let ctx_scripts = MathContext.(ictx |> make |> enter_script FontInfo.get_math_constants |> context_for_text) in
          let mbs_sub_opt = sub |> Option.map (iter ctx_scripts) in
          let mbs_sup_opt = sup |> Option.map (iter ctx_scripts) in
          append_sub_and_super_scripts ictx ~base:mbs_base ~sub:mbs_sub_opt ~sup:mbs_sup_opt

    ) |> List.concat
  in
  iter ictx imvs


and read_pdf_mode_vert_text (value_ctx : syntactic_value) (ivvs : input_vert_value_element list) : syntactic_value =

  let loc_ctx = ref value_ctx in

  let rec interpret_commands (ivvs : input_vert_value_element list) =
    ivvs |> List.map (function
    | InputVertValueCommandClosure(vclosure) ->
        let
          VertCommandClosureSimple{
            context_binder = evid_ctx;
            body           = ast_body;
            environment    = env;
          } = vclosure
        in
        let value =
          let env = add_to_environment env evid_ctx loc_ctx in
          interpret_0 env ast_body
        in
        get_vert_boxes value

    ) |> List.concat
  in
  let imvbs = interpret_commands ivvs in
  make_vert imvbs


and read_pdf_mode_horz_text (ictx : input_context) (ihvs : input_horz_value_element list) : syntactic_value =

  let (ctx, ctxsub) = ictx in
  let value_mcmd = make_math_command_func ctxsub.math_command in
  let loc_ctx = ref (Context(ictx)) in

  let rec normalize (imihs : input_horz_value_element list) =
    imihs |> List.fold_left (fun acc imih ->
      match imih with
      | InputHorzValueCommandClosure(hclosure) ->
          Alist.extend acc (NomInputHorzCommandClosure(hclosure))

      | InputHorzValueText(s2) ->
          begin
            match Alist.chop_last acc with
            | Some(accrest, NomInputHorzText(s1)) -> (Alist.extend accrest (NomInputHorzText(s1 ^ s2)))
            | _                                   -> (Alist.extend acc (NomInputHorzText(s2)))
          end

      | InputHorzValueEmbeddedMath(imvs) ->
          let value =
            reduce_beta ~msg:"InputHorzValueEmbeddedMath" value_mcmd (InputMathValue(imvs))
          in
          let hclosure = get_horz_command_closure value in
          Alist.extend acc (NomInputHorzCommandClosure(hclosure))

      | InputHorzValueEmbeddedCodeArea(s) ->
          begin
            match make_code_text_command_func ctxsub.code_text_command with
            | None ->
                Alist.extend acc (NomInputHorzText(s))

            | Some(value_ctcmd) ->
                let value =
                  reduce_beta ~msg:"InputHorzValueEmbeddedCodeArea" value_ctcmd (BaseConstant(BCString(s)))
                in
                let hclosure = get_horz_command_closure value in
                Alist.extend acc (NomInputHorzCommandClosure(hclosure))
          end

    ) Alist.empty |> Alist.to_list
  in

  let rec interpret_commands (nmihs : nom_input_horz_element list) : HorzBox.horz_box list =
    nmihs |> List.map (fun nmih ->
      match nmih with
      | NomInputHorzCommandClosure(hclosure) ->
          let
            HorzCommandClosureSimple{
              context_binder = evid_ctx;
              body           = ast_body;
              environment    = env;
            } = hclosure
          in
          let value =
            let env = add_to_environment env evid_ctx loc_ctx in
            interpret_0 env ast_body
          in
          get_horz_boxes value

      | NomInputHorzText(s) ->
          lex_horz_text ctx s

    ) |> List.concat
  in

  let nmihs = normalize ihvs in
  let hbs = interpret_commands nmihs in
  make_horz hbs


(* Selects the topmost pattern in `patbrs` that matches `value_obj`,
   evaluates the corresponding expression, and returns the resulting value.
   Raises an exception when no pattern matches `value_obj`. *)
and select_pattern (rng : Range.t) (env : environment) (value_obj : syntactic_value) (patbrs : pattern_branch list) : syntactic_value =
  let rec iter = function
    | [] ->
        report_dynamic_error (Format.asprintf "no matches (%s, %a)" (Range.to_string rng) pp_syntactic_value value_obj)

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
  in
  iter patbrs


(* Checks whether pattern `pat` matches value `value_obj`.
   Returns the environment extended by `pat` and `value_obj` from `env` if `pat` matches `value_obj`,
   or returns `None` otherwise. *)
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
