
open MyUtil
open LengthInterface
open GraphicBase
open SyntaxBase
open Types
open TypeConv
open EvalUtil


exception ExecError of string

type stack = syntactic_value list


let report_dynamic_error (msg : string) =
  raise (ExecError(msg))


type compiled_nom_input_horz_element =
  | CompiledNomInputHorzText     of string
  | CompiledNomInputHorzEmbedded of instruction list
  | CompiledNomInputHorzThunk    of instruction list
  | CompiledNomInputHorzContent  of compiled_nom_input_horz_element list * vmenv


let local_get_value (env : vmenv) (lv : int) (off : int) : syntactic_value =
  let (_, frames) = env in
    if lv = 0 then
      (List.hd frames).(off)
    else
      (List.nth frames lv).(off)


let local_set_value (env : vmenv) (lv : int) (off : int) (value : syntactic_value) : unit =
  let (_, frames) = env in
    if lv = 0 then
      (List.hd frames).(off) <- value
    else
      (List.nth frames lv).(off) <- value


let vmenv_global (env : vmenv) : environment =
  let (global, _) = env in
    global


let newframe (env : vmenv) (size : int) : vmenv =
  let (global, local) = env in
    (global, (Array.make size Nil) :: local)


let newframe_recycle (env : vmenv) (preenv : vmenv) (size : int) : vmenv =
  let (global, local) = env in
    match preenv with
    | (_, prefrm :: _) ->
        if size > Array.length prefrm then
          (global, (Array.make size Nil) :: local)
        else
          (global, prefrm :: local)

    | _ ->
        (global, (Array.make size Nil) :: local)


let lex_horz_text (ctx : HorzBox.context_main) (s_utf8 : string) : HorzBox.horz_box list =
  let uchlst = InternalText.to_uchar_list (InternalText.of_utf8 s_utf8) in
  HorzBox.([HorzPure(PHCInnerString(ctx, uchlst))])


let popn (stack : stack) (n : int) : syntactic_value list * stack =
  let rec iter st n acc =
    if n = 0 then
      (acc, st)
    else
      match st with
      | x :: xs -> iter xs (n - 1) (x :: acc)
      | []      -> report_bug_vm "stack underflow!"
  in
  iter stack n []


let make_binding_op (var : varloc) : instruction =
  match var with
  | GlobalVar(loc, evid, refs)    -> OpBindGlobal(loc, evid, !refs)
  | LocalVar(lv, off, evid, refs) -> OpBindLocal(lv, off, evid, !refs)


let rec exec_input_horz_content env ihlst =
  let imihlist = ihlst |> List.map (function
    | CompiledInputHorzText(s) ->
        CompiledImInputHorzText(s)

    | CompiledInputHorzEmbedded(code) ->
        CompiledImInputHorzEmbedded(code)

    | CompiledInputHorzEmbeddedMath(code) ->
        CompiledImInputHorzEmbeddedMath(code)

    | CompiledInputHorzEmbeddedCodeText(s) ->
        CompiledImInputHorzEmbeddedCodeText(s)

    | CompiledInputHorzContent(code) ->
        let value = exec_value [] env code [] in
        begin
          match value with
          | CompiledInputHorzClosure(imihlst, envsub) ->
              CompiledImInputHorzContent(imihlst, envsub)

          | _ -> report_bug_vm "exec_input_horz_content"
        end

  ) in
    CompiledInputHorzClosure(imihlist, env)


and exec_input_vert_content env ivlst =
  let imivlst = ivlst |> List.map (function
    | CompiledInputVertEmbedded(code) ->
        CompiledImInputVertEmbedded(code)

    | CompiledInputVertContent(code) ->
        let value = exec_value [] env code [] in
        begin
          match value with
          | CompiledInputVertClosure(imivlst, envsub) ->
              CompiledImInputVertContent(imivlst, envsub)

          | _ -> report_bug_vm "exec_input_vert_content"
        end

  ) in
    CompiledInputVertClosure(imivlst, env)


and exec_code_input_horz env irihlst =
  irihlst |> List.map (function
  | InputHorzText(s) ->
      InputHorzText(s)

  | InputHorzEmbedded(instrs) ->
      let value = exec_value [] env instrs [] in
      let cv = get_code value in
      InputHorzEmbedded(cv)

  | InputHorzEmbeddedMath(instrs) ->
      let value = exec_value [] env instrs [] in
      let cv = get_code value in
      InputHorzEmbeddedMath(cv)

  | InputHorzEmbeddedCodeText(s) ->
      InputHorzEmbeddedCodeText(s)

  | InputHorzContent(instrs) ->
      let value = exec_value [] env instrs [] in
      let cv = get_code value in
      InputHorzContent(cv)
  )


and exec_code_input_vert env irivlst =
  irivlst |> List.map (function
  | InputVertEmbedded(instrs) ->
      let value = exec_value [] env instrs [] in
      let cv = get_code value in
      InputVertEmbedded(cv)

  | InputVertContent(instrs) ->
      let value = exec_value [] env instrs [] in
      let cv = get_code value in
      InputVertContent(cv)
  )


and exec_text_mode_intermediate_input_vert (env : vmenv) (value_tctx : syntactic_value) (imivs : compiled_intermediate_input_vert_element list) : syntactic_value =
  let rec interpret_commands (env : vmenv) (imivs : compiled_intermediate_input_vert_element list) =
    imivs |> List.map (fun imiv ->
        match imiv with
        | CompiledImInputVertEmbedded(code) ->
            let value_ret = exec_value [ value_tctx ] env (List.append code [ OpApplyT(1) ]) [] in
            get_string value_ret

        | CompiledImInputVertContent(imivs_sub, env_sub) ->
            interpret_commands env_sub imivs_sub

      ) |> String.concat ""
  in
  let s = interpret_commands env imivs in
  make_string s


and exec_text_mode_intermediate_input_horz (env : vmenv) (value_tctx : syntactic_value) (imihs : compiled_intermediate_input_horz_element list) : syntactic_value =

  let tctx = get_text_mode_context value_tctx in

  let rec normalize (imihs : compiled_intermediate_input_horz_element list) =
    imihs |> List.fold_left (fun acc imih ->
      match imih with
      | CompiledImInputHorzEmbedded(code) ->
          let nmih = CompiledNomInputHorzEmbedded(code) in
          Alist.extend acc nmih

      | CompiledImInputHorzText(s2) ->
          begin
            match Alist.chop_last acc with
            | Some(accrest, CompiledNomInputHorzText(s1)) ->
                (Alist.extend accrest (CompiledNomInputHorzText(s1 ^ s2)))

            | _ ->
                (Alist.extend acc (CompiledNomInputHorzText(s2)))
          end

      | CompiledImInputHorzEmbeddedMath(mathcode) ->
          failwith "TODO: math; remains to be supported."

      | CompiledImInputHorzEmbeddedCodeText(s) ->
          failwith "TODO: code text; remains to be supported."

      | CompiledImInputHorzContent(imihs, env_sub) ->
          let nmihs_sub = normalize imihs in
          let nmih = CompiledNomInputHorzContent(nmihs_sub, env_sub) in
          Alist.extend acc nmih

    ) Alist.empty |> Alist.to_list
  in

  let rec interpret_commands (env : vmenv) (nmihs : compiled_nom_input_horz_element list) : string =
    nmihs |> List.map (fun nmih ->
      match nmih with
      | CompiledNomInputHorzEmbedded(code) ->
          let value_ret = exec_value [ value_tctx ] env (List.append code [ OpApplyT(1) ]) [] in
          get_string value_ret

      | CompiledNomInputHorzThunk(code) ->
          let value_ret = exec_value [] env code [] in
          get_string value_ret

      | CompiledNomInputHorzText(s) ->
          let uchlst = InternalText.to_uchar_list (InternalText.of_utf8 s) in
          let uchlstret = TextBackend.stringify uchlst tctx in
            InternalText.to_utf8 (InternalText.of_uchar_list uchlstret)

      | CompiledNomInputHorzContent(nmihlstsub, envsub) ->
          interpret_commands envsub nmihlstsub

    ) |> String.concat ""
  in

  let nmihs = normalize imihs in
  let s = interpret_commands env nmihs in
  make_string s


and exec_pdf_mode_intermediate_input_vert (env : vmenv) (value_ctx : syntactic_value) (imivs : compiled_intermediate_input_vert_element list) : syntactic_value =
  let rec interpret_commands (env : vmenv) (imivs : compiled_intermediate_input_vert_element list) =
    imivs |> List.map (fun imiv ->
        match imiv with
        | CompiledImInputVertEmbedded(code) ->
            let valueret = exec_value [ value_ctx ] env (List.append code [OpApplyT(1)]) [] in
            get_vert valueret

        | CompiledImInputVertContent(imivs_sub, env_sub) ->
            interpret_commands env_sub imivs_sub

      ) |> List.concat
  in
  let imvbs = interpret_commands env imivs in
  make_vert imvbs


and exec_pdf_mode_intermediate_input_horz (env : vmenv) (value_ctx : syntactic_value) (imihs : compiled_intermediate_input_horz_element list) : syntactic_value =

  let (ctx, ctxsub) = get_context value_ctx in

  let rec normalize (imihs : compiled_intermediate_input_horz_element list) =
    imihs |> List.fold_left (fun acc imih ->
      match imih with
      | CompiledImInputHorzEmbedded(code) ->
          let nmih = CompiledNomInputHorzEmbedded(code) in
          Alist.extend acc nmih

      | CompiledImInputHorzText(s2) ->
          begin
            match Alist.chop_last acc with
            | Some(accrest, CompiledNomInputHorzText(s1)) ->
                (Alist.extend accrest (CompiledNomInputHorzText(s1 ^ s2)))

            | _ ->
                (Alist.extend acc (CompiledNomInputHorzText(s2)))
          end

      | CompiledImInputHorzEmbeddedMath(mathcode) ->
          let MathCommand(valuemcmd) = ctxsub.math_command in
          let nmih =
            CompiledNomInputHorzThunk(
              List.append mathcode [
                OpPush(value_ctx);
                OpForward(1);  (* Put the context argument under the math argument. *)
                OpPush(valuemcmd);
                OpApplyT(2)
              ])
          in
          Alist.extend acc nmih

      | CompiledImInputHorzEmbeddedCodeText(s) ->
          begin
            match ctxsub.code_text_command with
            | DefaultCodeTextCommand ->
                let nmih = CompiledNomInputHorzText(s) in
                Alist.extend acc nmih

            | CodeTextCommand(valuectcmd) ->
                let nmih =
                  CompiledNomInputHorzThunk([
                    OpPush(value_ctx);
                    OpPush(BaseConstant(BCString(s)));
                    OpPush(valuectcmd);
                    OpApplyT(2)
                  ])
                in
                Alist.extend acc nmih
          end

      | CompiledImInputHorzContent(imihlst, envsub) ->
          let nmihlstsub = normalize imihlst in
          let nmih = CompiledNomInputHorzContent(nmihlstsub, envsub) in
            Alist.extend acc nmih

    ) Alist.empty |> Alist.to_list
  in

  let rec interpret_commands (env : vmenv) (nmihs : compiled_nom_input_horz_element list) : HorzBox.horz_box list =
    nmihs |> List.map (fun nmih ->
      match nmih with
      | CompiledNomInputHorzEmbedded(code) ->
          let value_ret = exec_value [ value_ctx ] env (List.append code [ OpApplyT(1) ]) [] in
          get_horz value_ret

      | CompiledNomInputHorzThunk(code) ->
          let value_ret = exec_value [] env code [] in
          get_horz value_ret

      | CompiledNomInputHorzText(s) ->
          lex_horz_text ctx s

      | CompiledNomInputHorzContent(nmihlstsub, envsub) ->
          interpret_commands envsub nmihlstsub

    ) |> List.concat
  in

  let nmihs = normalize imihs in
  let hbs = interpret_commands env nmihs in
  make_horz hbs


and exec_application (env : vmenv) (vf : syntactic_value) (vargs : syntactic_value list) : syntactic_value =
  let len = List.length vargs in
    if len = 0 then
      vf
    else
      exec_value (vf :: (List.rev vargs)) env [ OpApplyT(len) ] []


and generate_symbol_and_add_to_environment (env : vmenv) (var : varloc) : vmenv * CodeSymbol.t =
  failwith "TODO: generate_symbol_and_add_to_environment"
(*
  let symb =
    let evid =
      match var with
      | GlobalVar(_, evid, _)   -> evid
      | LocalVar(_, _, evid, _) -> evid
    in
    let varnm = EvalVarID.get_varnm evid in
    let rng = EvalVarID.get_range evid in
    CodeSymbol.fresh (rng, "symbol for " ^ varnm)
  in
  let bindop = make_binding_op var in
  let cv = CodeSymbol(symb) in
  let _ = exec [] env [OpPush(cv); bindop; OpPushEnv] [] in
  match envopt with
  | Some(env) -> (env, symb)
  | None      -> assert false
*)


and exec_code_pattern_tree (env : vmenv) (irpat : ir_pattern_tree) : vmenv * code_pattern_tree =
  match irpat with
  | IRPUnitConstant       -> (env, CdPUnitConstant)
  | IRPBooleanConstant(b) -> (env, CdPBooleanConstant(b))
  | IRPIntegerConstant(n) -> (env, CdPIntegerConstant(n))
  | IRPStringConstant(s)  -> (env, CdPStringConstant(s))

  | IRPListCons(irpat1, irpat2) ->
      let (env, cdpat1) = exec_code_pattern_tree env irpat1 in
      let (env, cdpat2) = exec_code_pattern_tree env irpat2 in
      (env, CdPListCons(cdpat1, cdpat2))

  | IRPEndOfList ->
      (env, CdPEndOfList)

  | IRPTuple(irpats) ->
      let (env, cdpatacc) =
        irpats |> TupleList.to_list |> List.fold_left (fun (env, cdpatacc) irpat ->
          let (env, cdpat) = exec_code_pattern_tree env irpat in
          (env, Alist.extend cdpatacc cdpat)
        ) (env, Alist.empty)
      in
      let cdpats =
        match Alist.to_list cdpatacc with
        | cdpat1 :: cdpat2 :: cdpats -> TupleList.make cdpat1 cdpat2 cdpats
        | _                          -> assert false
      in
      (env, CdPTuple(cdpats))

  | IRPWildCard ->
      (env, CdPWildCard)

  | IRPVariable(var) ->
      let (env, symb) = generate_symbol_and_add_to_environment env var in
      (env, CdPVariable(symb))

  | IRPAsVariable(var, irpat1) ->
      let (env, symb) = generate_symbol_and_add_to_environment env var in
      let (env, cdpat1) = exec_code_pattern_tree env irpat1 in
      (env, CdPAsVariable(symb, cdpat1))

  | IRPConstructor(ctornm, irpat1) ->
      let (env, cdpat1) = exec_code_pattern_tree env irpat1 in
      (env, CdPConstructor(ctornm, cdpat1))


and exec_code_pattern_branch (env : vmenv) (comppatbr : (instruction list) ir_pattern_branch_scheme) =
  match comppatbr with
  | IRPatternBranch(irpat, instrs1) ->
      let (env, cdpat) = exec_code_pattern_tree env irpat in
      let value1 = exec_value [] env instrs1 [] in
      let cv1 = get_code value1 in
      CdPatternBranch(cdpat, cv1)

  | IRPatternBranchWhen(irpat, instrs0, instrs1) ->
      let (env, cdpat) = exec_code_pattern_tree env irpat in
      let value0 = exec_value [] env instrs0 [] in
      let cv0 = get_code value0 in
      let value1 = exec_value [] env instrs1 [] in
      let cv1 = get_code value1 in
      CdPatternBranchWhen(cdpat, cv0, cv1)


and exec_value (stack : stack) (env : vmenv) (code : instruction list) dump : syntactic_value =
  exec stack env code dump


and exec (stack : stack) (env : vmenv) (code : instruction list) dump : syntactic_value =
  match code with
  | [] ->
      begin
        match dump with
        | (pe, pc) :: rest ->
            exec stack pe pc rest

        | [] ->
            begin match stack with
              | v :: _ -> v
              | [] -> report_bug_vm "stack underflow!"
            end
      end

  | op :: code ->
      (*Format.printf "\nexec ---> %a\n" pp_instruction c;*)
      exec_op op stack env code dump


and exec_code (genv : environment) (code : instruction list) : syntactic_value =
  exec [] (genv, []) code []


and exec_op (op : instruction) (stack : stack) (env : vmenv) (code : instruction list) dump =
  match op with
  | OpDereference ->
      begin
        match stack with
        | value_cont :: stack ->
            let ret =
              match value_cont with
              | Location(stid) ->
                  begin
                    match find_location_value (vmenv_global env) stid with
                    | Some(value) -> value
                    | None        -> report_bug_vm "Dereference"
                  end

              | _ ->
                  report_bug_vm "Dereference"
            in exec (ret :: stack) env code dump

        | _ -> report_bug_vm "invalid argument for OpDereference"
      end

  | OpAccessField(field) ->
      begin
        match stack with
        | value1 :: stack ->
            begin
              match value1 with
              | RecordValue(asc1) ->
                  begin
                    match asc1 |> LabelMap.find_opt field with
                    | Some(v) -> exec (v :: stack) env code dump
                    | None    -> report_bug_vm (Printf.sprintf "OpAccessField: field '%s' not found" field)
                  end

              | _ ->
                  report_bug_vm "OpAccessField: not a Record"
            end

        | _ -> report_bug_vm "invalid argument for OpAccessField"
      end

  | OpUpdateField(field) ->
      begin
        match stack with
        | value2 :: value1 :: stack ->
            begin
              match value1 with
              | RecordValue(asc1) ->
                  let asc1new =
                    match asc1 |> LabelMap.find_opt field with
                    | Some(_) -> asc1 |> LabelMap.add field value2
                    | None    -> report_bug_vm (Printf.sprintf "OpUpdateField: field '%s' not found" field)
                  in
                  let v = RecordValue(asc1new) in
                  exec (v :: stack) env code dump

              | _ ->
                report_bug_vm "OpUpdateField: not a Record"
            end

        | _ -> report_bug_vm "invalid argument for OpUpdateField"
      end

  | OpForward(n) ->
      begin
            begin
              let (vs, stack) = popn stack n in
              match stack with
              | v0 :: stack -> exec (v0 :: List.rev_append vs stack) env code dump
              | _           -> report_bug_vm "Forward: stack underflow"
            end

      end

  | OpApply(n) ->
      failwith "TODO (enhance): Vm, OpApply"
(*
      begin
        match stack with
        | (f, _) :: stack ->
            begin
              match f with
              | CompiledClosure(optvars, arity, pargs, framesize, body, env1) ->
                  let body =
                    optvars |> List.fold_left (fun acc optvar ->
                      let bindop = make_binding_op optvar in
                      OpPush(Constructor("None", const_unit)) :: bindop :: acc
                    ) body
                  in
                  if arity = n then
                    if pargs = [] then
                      exec stack (newframe env1 framesize) body ((env, code) :: dump)
                    else
                      let (args, stack) = popn stack n in
                      let allargs = List.rev_map make_entry (pargs @ args) in
                      exec (allargs @ stack) (newframe env1 framesize) body ((env, code) :: dump)

                  else if arity > n then
                    let (args, stack) = popn stack n in
                    let applied = CompiledClosure([], arity - n, pargs @ args, framesize, body, env1) in
                    exec (make_entry applied :: stack) env code dump

                  else
                    let (surplus, stack) = popn stack (n - arity) in
                    let (args, stack) = popn stack arity in
                    let allargs = List.rev_map make_entry (pargs @ args) in
                    exec (allargs @ stack) (newframe env1 framesize) body ((env, OpInsertArgs(surplus) :: OpApply(n - arity) :: code) :: dump)

              | CompiledPrimitiveClosure(arity, [], framesize, body, env1, astf) ->
                  if arity = n then
                    exec stack (newframe env1 framesize) body ((env, code) :: dump)

                  else if arity > n then
                    let (args, stack) = popn stack n in
                    let applied = CompiledClosure([], arity - n, args, framesize, body, env1) in
                    exec (make_entry applied :: stack) env code dump

                  else
                    let (surplus, stack) = popn stack (n - arity) in
                    exec stack (newframe env1 framesize) body ((env, OpInsertArgs(surplus) :: OpApply(n - arity) :: code) :: dump)

               | _ ->
                   report_bug_vm_value "Apply: not a function" f
            end

        | _ -> report_bug_vm "invalid argument for OpApply"
      end
*)

  | OpApplyT(n) ->
      failwith "TODO (enhance): Vm, OpApplyT"
(*
      begin
        match stack with
        | (f, _) :: stack ->
            begin
              match f with
              | CompiledClosure(optvars, arity, pargs, framesize, body, env1) ->
                  let body =
                    optvars |> List.fold_left (fun acc optvar ->
                      let bindop = make_binding_op optvar in
                      OpPush(Constructor("None", const_unit)) :: bindop :: acc
                    ) body
                  in
                  if arity = n then
                    if pargs = [] then
                      exec stack (newframe env1 framesize) body dump
                    else
                      let (args, stack) = popn stack n in
                      let allargs = List.rev_map make_entry (pargs @ args) in
                      exec (allargs @ stack) (newframe env1 framesize) body dump

                  else if arity > n then
                    let (args, stack) = popn stack n in
                    let applied = CompiledClosure([], arity - n, pargs @ args, framesize, body, env1) in
                    exec (make_entry applied :: stack) env code dump

                  else
                    let (surplus, stack) = popn stack (n - arity) in
                    let (args, stack) = popn stack arity in
                    let allargs = List.rev_map make_entry (pargs @ args) in
                    exec (allargs @ stack) (newframe env1 framesize) body ((env, OpInsertArgs(surplus) :: OpApplyT(n - arity) :: code) :: dump)

               | CompiledPrimitiveClosure(arity, [], framesize, body, env1, astf) ->
                   if arity = n then
                     exec stack (newframe env1 framesize) body dump

                   else if arity > n then
                     let (args, stack) = popn stack n in
                     let applied = CompiledClosure([], arity - n, args, framesize, body, env1) in
                     exec (make_entry applied :: stack) env code dump

                   else
                     let (surplus, stack) = popn stack (n-arity) in
                     exec stack (newframe env1 framesize) body ((env, OpInsertArgs(surplus) :: OpApplyT(n - arity) :: code) :: dump)

               | _ ->
                   report_bug_vm_value "ApplyT: not a function" f
            end

        | _ -> report_bug_vm "invalid argument for OpApplyT"
      end
*)

  | OpApplyOptional ->
      failwith "TODO (enhance): Vm, OpApplyOptional"
(*
      begin
        match stack with
        | (v, _) :: (f, _) :: stack ->
            begin
              match f with
              | CompiledClosure(var :: vars, arity, pargs, framesize, body, env1) ->
                  let bindop = make_binding_op var in
                  let body = OpPush(Constructor("Some", v)) :: bindop :: body in
                  let fnew = CompiledClosure(vars, arity, pargs, framesize, body, env1) in
                  exec (make_entry fnew :: stack) env code dump

              | _ ->
                  report_bug_vm_value "ApplyOptional: not a function with optional arguments" f
            end

        | _ -> report_bug_vm "invalid argument for OpApplyOptional"
      end
*)

  | OpApplyOmission ->
      failwith "TODO (enhance): Vm, OpApplyOmission"
(*
      begin
        match stack with
        | (f, _) :: stack ->
            begin
              match f with
              | CompiledClosure(var :: vars, arity, pargs, framesize, body, env1) ->
                  let bindop = make_binding_op var in
                  let body = OpPush(Constructor("None", const_unit)) :: bindop :: body in
                  let fnew = CompiledClosure(vars, arity, pargs, framesize, body, env1) in
                  exec (make_entry fnew :: stack) env code dump

              | _ ->
                  report_bug_vm_value "ApplyOmission: not a function with optional arguments" f
            end

        | _ -> report_bug_vm "invalid argument for OpApplyOmission"
      end
*)

  | OpBindGlobal(loc, evid, refs) ->
      begin
        match stack with
        | value :: stack ->
            begin
              loc := value;
              exec stack env code dump
            end

        | _ -> report_bug_vm "invalid argument for OpBindGlobal"
      end

  | OpBindLocal(lv, offset, evid, refs) ->
      begin
            begin
              match stack with
              | value :: stack ->
                  local_set_value env lv offset value;
                  exec stack env code dump

              | _ ->
                  report_bug_vm ("BindLocal(" ^ (EvalVarID.show_direct evid) ^ ")")
            end

      end

  | OpBindClosuresRec(binds) ->
      begin
            begin
              binds |> List.iter (fun (var, code) ->
                let recfunc = exec_value [] env code [] in
                  match var with
                  | GlobalVar(loc, evid, refs) -> loc := recfunc
                  | LocalVar(lv, offset, evid, refs) -> local_set_value env lv offset recfunc
              );
              exec stack env code dump
            end

      end

  | OpBranch(body) ->
      begin
            begin
              exec stack env body dump
            end

      end

  | OpBranchIf(body) ->
      begin
        match stack with
        | value :: stack ->
            let b = get_bool value in
            if b then
              exec stack env body dump
            else
              exec stack env code dump

        | _ -> report_bug_vm "invalid argument for OpBranchIf"
      end

  | OpBranchIfNot(body) ->
      begin
        match stack with
        | value :: stack ->
            let b = get_bool value in
            if b then
              exec stack env code dump
            else
              exec stack env body dump

        | _ -> report_bug_vm "invalid argument for OpBranchIfNot"
      end

  | OpLoadGlobal(loc, evid, refs) ->
      begin
            begin
              let v = !loc in
              exec (v :: stack) env code dump
            end

      end

  | OpLoadLocal(lv, offset, evid, refs) ->
      begin
            begin
              let v = local_get_value env lv offset in
              exec (v :: stack) env code dump
            end

      end

  | OpDup ->
      let entry =
        try List.hd stack with
        | Invalid_argument(_) -> report_bug_vm "Dup: stack underflow"
      in
      exec (entry :: stack) env code dump

  | OpError(msg) ->
      raise (ExecError(msg))

  | OpMakeConstructor(ctornm) ->
      begin
        match stack with
        | value_cont :: stack ->
            exec (Constructor(ctornm, value_cont) :: stack) env code dump

        | [] ->
            report_bug_vm "invalid argument for OpMakeConstructor"
      end

  | OpMakeRecord(keys) ->
      let rec collect keys labmap (stack : stack) =
        match keys with
        | [] ->
            (labmap, stack)

        | key :: rest ->
            begin
              match stack with
              | value :: stack_new -> collect rest (labmap |> LabelMap.add key value) stack_new
              | []                 -> report_bug_vm "MakeRecord: stack underflow"
            end
      in
      let (asc, stack) = collect (List.rev keys) LabelMap.empty stack in
      exec (RecordValue(asc) :: stack) env code dump

  | OpMakeTuple(len) ->
        let rec iter n acc (stack : stack) =
          if n <= 0 then
            (acc, stack)
          else
            match stack with
            | value :: stack_new -> iter (n - 1) (value :: acc) stack_new
            | []                 -> report_bug_vm "MakeTuple: stack underflow"
        in
        let (vs, stack) = iter len [] stack in
        exec (Tuple(vs) :: stack) env code dump

  | OpPop ->
      let stack =
        try List.tl stack with
        | Invalid_argument(_) -> report_bug_vm "Pop: stack underflow"
      in
      exec stack env code dump

  | OpPush(v) ->
      exec (v :: stack) env code dump

  | OpPushEnv ->
      failwith "TODO (enhance): OpPushEnv"

  | OpCheckStackTopBool(b, next) ->
      begin
        match stack with
        | value :: stack ->
            let b0 = get_bool value in
            if b = b0 then
              exec stack env code dump
            else
              exec stack env next dump

        | _ ->
            report_bug_vm "invalid argument for OpCheckStackTopBool"
      end

  | OpCheckStackTopCtor(ctor_nm, next) ->
      begin
        match stack with
        | value :: stack ->
            begin
              match value with
              | Constructor(nm, sub) when nm = ctor_nm -> exec (sub :: stack) env code dump
              | _                                      -> exec stack env next dump
            end

        | _ -> report_bug_vm "invalid argument for OpCheckStackTopCtor"
      end

  | OpCheckStackTopEndOfList(next) ->
      begin
        match stack with
        | value :: stack ->
            begin
              match value with
              | List([]) -> exec stack env code dump
              | _        -> exec stack env next dump
            end

        | _ ->
            report_bug_vm "invalid argument for OpCheckStackTopEndOfList"
      end

  | OpCheckStackTopInt(i, next) ->
      begin
        match stack with
        | value :: stack ->
            let i0 = get_int value in
            if i = i0 then
              exec stack env code dump
            else
              exec stack env next dump

        | _ ->
            report_bug_vm "invalid argument for OpCheckStackTopInt"
      end

  | OpCheckStackTopListCons(next) ->
      begin
        match stack with
        | value :: stack ->
            begin
              match value with
              | List(car :: cdr) -> exec (car :: List(cdr) :: stack) env code dump
              | _                -> exec stack env next dump
            end

        | _ ->
            report_bug_vm "invalid argument for OpCheckStackTopListCons"
      end

  | OpCheckStackTopStr(str, next) ->
      begin
        match stack with
        | value :: stack ->
            let s0 = get_string value in
            if s0 = str then
              exec stack env code dump
            else
              exec stack env next dump

        | _ ->
            report_bug_vm "invalid argument for OpCheckStackTopStr"
      end

  | OpCheckStackTopTupleCons(next) ->
      begin
        match stack with
        | value :: stack ->
            begin
              match value with
              | Tuple(car :: cdr) -> exec (car :: Tuple(cdr) :: stack) env code dump
              | _                 -> exec stack env next dump
            end

        | _ ->
            report_bug_vm "invalid argument for OpCheckStackTopTupleCons"
      end

  | OpClosure(varloc_labmap, arity, framesize, body) ->
      let value = CompiledClosure(varloc_labmap, arity, [], framesize, body, env) in
      exec (value :: stack) env code dump

  | OpClosureInputHorz(imihs) ->
      let imihclos = exec_input_horz_content env imihs in
      exec (imihclos :: stack) env code dump

  | OpClosureInputVert(imivs) ->
      let imivclos = exec_input_vert_content env imivs in
      exec (imivclos :: stack) env code dump

  | OpBindLocationGlobal(loc, evid) ->
      begin
        match stack with
        | value_ini :: stack ->
            begin
              let stid = register_location (vmenv_global env) value_ini in
              loc := Location(stid);
              exec stack env code dump
            end

        | _ ->
            report_bug_vm "invalid argument for OpBindLocationGlobal"
      end

  | OpBindLocationLocal(lv, offset, evid) ->
      begin
        match stack with
        | value_ini :: stack ->
            begin
              let stid = register_location (vmenv_global env) value_ini in
              local_set_value env lv offset (Location(stid));
              exec stack env code dump
            end

        | _ ->
            report_bug_vm "invalid argument for OpBindLocationLocal"
      end

  | OpUpdateGlobal(loc, evid) ->
      begin
        match stack with
        | value_new :: stack ->
            begin
              match !loc with
              | Location(stid) ->
                  begin
                    update_location (vmenv_global env) stid value_new;
                    exec (const_unit :: stack) env code dump
                  end

              | _ ->
                  report_bug_vm "UpdateGlobal"
            end

        | _ -> report_bug_vm "invalid argument for OpUpdateGlobal"
      end

  | OpUpdateLocal(lv, offset, evid) ->
      begin
        match stack with
        | value_new :: stack ->
            begin
              match local_get_value env lv offset with
              | Location(stid) ->
                   begin
                     update_location (vmenv_global env) stid value_new;
                     exec (const_unit :: stack) env code dump
                   end

              | _ ->
                  report_bug_vm "UpdateLocal"
            end

        | _ -> report_bug_vm "invalid argument for OpUpdateLocal"
      end

  | OpSel(tpart, fpart) ->
      begin
        match stack with
        | value :: stack ->
            let b = get_bool value in
            if b then
              exec stack env tpart dump
            else
              exec stack env fpart dump

        | _ -> report_bug_vm "invalid argument for OpSel"
      end

  | OpBackendMathList(n) ->
      let rec iter n (stack : stack) acc =
        if n <= 0 then
          (acc, stack)
        else
          match stack with
          | MathValue(m) :: stack_new -> iter (n - 1) stack_new (m :: acc)
          | _                         -> report_bug_vm "BackendMathList"
      in
      let (maths, stack) = iter n stack [] in
      let value = MathValue(List.concat maths) in
      exec (value :: stack) env code dump

  | OpInsertArgs(vs) ->
      begin
        match stack with
        | func :: stack ->
            begin
              exec (func :: (List.rev_append vs stack)) env code dump
            end

        | _ ->
            report_bug_vm "invalid argument for OpInsertArgs"
      end

  | OpApplyCodeCombinator(codef, arity) ->
      let (values, stack) = popn stack arity in
      let codes = values |> List.map get_code in
      let code_ret = codef codes in
      exec (CodeValue(code_ret) :: stack) env code dump

  | OpCodeMakeRecord(keys) ->
      let rec collect keys cdlabmap (stack : stack) =
        match keys with
        | [] ->
            (cdlabmap, stack)

        | key :: rest ->
            begin
              match stack with
              | CodeValue(cv) :: stack_new -> collect rest (cdlabmap |> LabelMap.add key cv) stack_new
              | _                          -> report_bug_vm "CodeMakeRecord"
            end
      in
      let (cdlabmap, stack) = collect (List.rev keys) LabelMap.empty stack in
      exec (CodeValue(CdRecord(cdlabmap)) :: stack) env code dump

  | OpCodeMathList(n) ->
      let rec iter n (stack : stack) acc =
        if n <= 0 then
          (acc, stack)
        else
          match stack with
          | CodeValue(cv) :: stack_new -> iter (n - 1) stack_new (cv :: acc)
          | _                          -> report_bug_vm "CodeMathList"
      in
      let (cvs, stack) = iter n stack [] in
      exec (CodeValue(CdMathList(cvs)) :: stack) env code dump

  | OpCodeMakeTuple(n) ->
      let rec iter n acc (stack : stack) =
        if n <= 0 then
          (acc, stack)
        else
          match stack with
          | CodeValue(cv) :: stack_new -> iter (n - 1) (cv :: acc) stack_new
          | _                          -> report_bug_vm "CodeMakeTuple"
      in
      let (cvs, stack) = iter n [] stack in
      let cvs =
        match cvs with
        | cv1 :: cv2 :: cvrest -> TupleList.make cv1 cv2 cvrest
        | _                    -> assert false
      in
      exec (CodeValue(CdTuple(cvs)) :: stack) env code dump

  | OpCodeMakeInputHorz(compihs) ->
      let cdihs = exec_code_input_horz env compihs in
      exec (CodeValue(CdInputHorz(cdihs)) :: stack) env code dump

  | OpCodeMakeInputVert(compivlst) ->
      let cdivs = exec_code_input_vert env compivlst in
      exec (CodeValue(CdInputVert(cdivs)) :: stack) env code dump

  | OpCodePatternMatch(rng, comppatbrs) ->
      begin
        match stack with
        | CodeValue(cv0) :: stack ->
            let cdpatbrs = List.map (exec_code_pattern_branch env) comppatbrs in
            let value = CodeValue(CdPatternMatch(rng, cv0, cdpatbrs)) in
            exec (value :: stack) env code dump

        | _ ->
            report_bug_vm "CodePatternMatch: stack underflow"
      end

  | OpCodeLetRec(comprecbinds, instrs2) ->
      let (pairacc, env) =
        comprecbinds |> List.fold_left (fun (pairacc, env) comprecbind ->
          let IRLetRecBinding(var, _) = comprecbind in
          let (env, symb) = generate_symbol_and_add_to_environment env var in
          (Alist.extend pairacc (symb, comprecbind), env)
        ) (Alist.empty, env)
      in
      let (cdrecbindacc, env) =
        pairacc |> Alist.to_list |> List.fold_left (fun (cdrecbindacc, env) (symb, comprecbind) ->
          let IRLetRecBinding(_, comppatbr) = comprecbind in
          let cdpatbr = exec_code_pattern_branch env comppatbr in
          let cdrecbind = CdLetRecBinding(symb, cdpatbr) in
          (Alist.extend cdrecbindacc cdrecbind, env)
        ) (Alist.empty, env)
      in
      let value2 = exec [] env instrs2 [] in
      let cv2 = get_code value2 in
      exec (CodeValue(CdLetRecIn(Alist.to_list cdrecbindacc, cv2)) :: stack) env code dump

  | OpCodeLetNonRec(irpat, instrs1, instrs2) ->
      let value1 = exec_value [] env instrs1 [] in
      let cv1 = get_code value1 in
      let (env, cdpat) = exec_code_pattern_tree env irpat in
      let value2 = exec [] env instrs2 [] in
      let cv2 = get_code value2 in
      exec (CodeValue(CdLetNonRecIn(cdpat, cv1, cv2)) :: stack) env code dump

  | OpCodeFunction(varloc_labmap, irpat, instrs1) ->
      let (symb_labmap, env) =
        LabelMap.fold (fun label varloc (symb_labmap, env) ->
          let (env, symb) = generate_symbol_and_add_to_environment env varloc in
          (symb_labmap |> LabelMap.add label symb, env)
        ) varloc_labmap (LabelMap.empty, env)
      in
      let (env, cdpat) = exec_code_pattern_tree env irpat in
      let value1 = exec_value [] env instrs1 [] in
      let cv1 = get_code value1 in
      exec (CodeValue(CdFunction(symb_labmap, CdPatternBranch(cdpat, cv1))) :: stack) env code dump

  | OpCodeLetMutable(var, instrs1, instrs2) ->
      let (env, symb) = generate_symbol_and_add_to_environment env var in
      let value1 = exec_value [] env instrs1 [] in
      let cv1 = get_code value1 in
      let value2 = exec [] env instrs2 [] in
      let cv2 = get_code value2 in
      exec (CodeValue(CdLetMutableIn(symb, cv1, cv2)) :: stack) env code dump

  | OpCodeOverwrite(instrs1) ->
      begin
        match stack with
        | CodeSymbol(symb) :: stack ->
            let value1 = exec_value [] env instrs1 [] in
            let cv1 = get_code value1 in
            exec (CodeValue(CdOverwrite(symb, cv1)) :: stack) env code dump

        | _ ->
            report_bug_vm "not a symbol (OpCodeOverwrite)"
      end

  | OpConvertSymbolToCode ->
      begin
        match stack with
        | CodeSymbol(symb) :: stack ->
            let rng = Range.dummy "OpConvertSymbolToCode" in
            exec (CodeValue(CdContentOf(rng, symb)) :: stack) env code dump

        | v :: _ ->
            report_bug_vm_value "not a code symbol" v

        | [] ->
            report_bug_vm "stack underflow (OpConvertSymbolToCode)"
      end

#include "__vm.gen.ml"
