(* -unused-value-declaration *)
[@@@ocaml.warning "-32"]

open LengthInterface
open GraphicBase
open SyntaxBase
open Types
open EvalUtil


exception ExecError of string

type stack_entry = syntactic_value * vmenv option

type stack = stack_entry list


let make_entry (v : syntactic_value) : stack_entry =
  (v, None)


let report_dynamic_error msg =
  raise (ExecError(msg))

(*
type compiled_nom_inline_text_element =
  | CompiledNomInlineTextText     of string
  | CompiledNomInlineTextEmbedded of instruction list
  | CompiledNomInlineTextThunk    of instruction list
  | CompiledNomInlineTextContent  of compiled_nom_inline_text_element list * vmenv
*)

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
  HorzBox.([ HorzPure(PHCInnerString{ context = ctx; chars = uchlst }) ])


let popn (stack : stack) (n : int) : syntactic_value list * stack =
  let rec iter st n acc =
    if n = 0 then
      (acc, st)
    else
      match st with
      | (x, _) :: xs -> iter xs (n - 1) (x :: acc)
      | []           -> report_bug_vm "stack underflow!"
  in
  iter stack n []


let make_binding_op (var : varloc) : instruction =
  match var with
  | GlobalVar(loc, evid, refs)    -> OpBindGlobal(loc, evid, !refs)
  | LocalVar(lv, off, evid, refs) -> OpBindLocal(lv, off, evid, !refs)


let rec exec_inline_text_content _env _ihlst =
  failwith "TODO: exec_inline_text_content"
(*
  let imihlist = ihlst |> List.map (function
    | CompiledInlineTextText(s) ->
        CompiledImInlineTextText(s)

    | CompiledInlineTextEmbedded(code) ->
        CompiledImInlineTextEmbedded(code)

    | CompiledInlineTextEmbeddedMath(code) ->
        CompiledImInlineTextEmbeddedMath(code)

    | CompiledInlineTextEmbeddedCodeText(s) ->
        CompiledImInlineTextEmbeddedCodeText(s)

    | CompiledInlineTextContent(code) ->
        let value = exec_value [] env code [] in
        begin
          match value with
          | CompiledInlineTextClosure(imihlst, envsub) ->
              CompiledImInlineTextContent(imihlst, envsub)

          | _ -> report_bug_vm "exec_inline_text_content"
        end

  ) in
    CompiledInlineTextClosure(imihlist, env)
*)


and exec_block_text_content _env _ivlst =
  failwith "TODO: exec_code_vert_content"
(*
  let imivlst = ivlst |> List.map (function
    | CompiledBlockTextEmbedded(code) ->
        CompiledImBlockTextEmbedded(code)

    | CompiledBlockTextContent(code) ->
        let value = exec_value [] env code [] in
        begin
          match value with
          | CompiledBlockTextClosure(imivlst, envsub) ->
              CompiledImBlockTextContent(imivlst, envsub)

          | _ -> report_bug_vm "exec_block_text_content"
        end

  ) in
    CompiledBlockTextClosure(imivlst, env)
*)

and exec_code_inline_text _env _irihlst =
  failwith "TODO: exec_code_inline_text"
(*
  irihlst |> List.map (function
  | InlineTextText(s) ->
      InlineTextText(s)

  | InlineTextEmbedded(instrs) ->
      let value = exec_value [] env instrs [] in
      let cv = get_code value in
      InlineTextEmbedded(cv)

  | InlineTextEmbeddedMath(instrs) ->
      let value = exec_value [] env instrs [] in
      let cv = get_code value in
      InlineTextEmbeddedMath(cv)

  | InlineTextEmbeddedCodeText(s) ->
      InlineTextEmbeddedCodeText(s)

  | InlineTextContent(instrs) ->
      let value = exec_value [] env instrs [] in
      let cv = get_code value in
      InlineTextContent(cv)
  )
*)


and exec_code_block_text _env _irivlst =
  failwith "TODO: exec_code_block_text"
(*
  irivlst |> List.map (function
  | BlockTextEmbedded(instrs) ->
      let value = exec_value [] env instrs [] in
      let cv = get_code value in
      BlockTextEmbedded(cv)

  | BlockTextContent(instrs) ->
      let value = exec_value [] env instrs [] in
      let cv = get_code value in
      BlockTextContent(cv)
  )
*)


and exec_text_mode_intermediate_block_text (_env : vmenv) (_valuetctx : syntactic_value) (_imivlst : compiled_intermediate_block_text_element list) : syntactic_value =
  failwith "TODO: exec_text_mode_intermediate_block_text"
(*
  let rec interpret_commands env imivlst =
    imivlst |> List.map (fun imiv ->
        match imiv with
        | CompiledImBlockTextEmbedded(code) ->
            let valueret = exec_value [make_entry valuetctx] env (List.append code [OpApplyT(1)]) [] in
              get_string valueret

        | CompiledImBlockTextContent(imivlstsub, envsub) ->
            interpret_commands envsub imivlstsub

      ) |> String.concat ""
  in
  let s = interpret_commands env imivlst in
  make_string s
*)

and exec_text_mode_intermediate_inline_text (_env : vmenv) (_valuetctx : syntactic_value) (_imihlst : compiled_intermediate_inline_text_element list) : syntactic_value =
  failwith "TODO: exec_text_mode_intermediate_inline_text"
(*
  let (tctx, _ctxsub) = get_text_mode_context valuetctx in
    begin
      let rec normalize imihlst =
        imihlst |> List.fold_left (fun acc imih ->
            match imih with
            | CompiledImInlineTextEmbedded(code) ->
                let nmih = CompiledNomInlineTextEmbedded(code) in
                  Alist.extend acc nmih

            | CompiledImInlineTextText(s2) ->
                begin
                  match Alist.chop_last acc with
                  | Some(accrest, CompiledNomInlineTextText(s1)) -> (Alist.extend accrest (CompiledNomInlineTextText(s1 ^ s2)))
                  | _                                           -> (Alist.extend acc (CompiledNomInlineTextText(s2)))
                end

            | CompiledImInlineTextEmbeddedMath(_mathcode) ->
                failwith "TODO: (VM) math; remains to be supported."
(*
                let nmih = CompiledNomInlineTextThunk(List.append mathcode [OpPush(valuetctx); OpForward(1); OpPush(valuemcmd); OpApplyT(2)]) in
                  Alist.extend acc nmih
*)
            | CompiledImInlineTextEmbeddedCodeText(_s) ->
                failwith "TODO: (VM) code text; remains to be supported."

            | CompiledImInlineTextContent(imihlst, envsub) ->
                let nmihlstsub = normalize imihlst in
                let nmih = CompiledNomInlineTextContent(nmihlstsub, envsub) in
                  Alist.extend acc nmih

          ) Alist.empty |> Alist.to_list
      in

      let rec interpret_commands (env : vmenv) (nmihlst : compiled_nom_inline_text_element list) : string =
        nmihlst |> List.map (fun nmih ->
            match nmih with
            | CompiledNomInlineTextEmbedded(code) ->
                let valueret = exec_value [make_entry valuetctx] env (List.append code [OpApplyT(1)]) [] in
                  get_string valueret

            | CompiledNomInlineTextThunk(code) ->
                let valueret = exec_value [] env code [] in
                  get_string valueret

            | CompiledNomInlineTextText(s) ->
                let uchlst = InternalText.to_uchar_list (InternalText.of_utf8 s) in
                let uchlstret = TextBackend.stringify uchlst tctx in
                  InternalText.to_utf8 (InternalText.of_uchar_list uchlstret)

            | CompiledNomInlineTextContent(nmihlstsub, envsub) ->
                interpret_commands envsub nmihlstsub

          ) |> String.concat ""
      in

      let nmihlst = normalize imihlst in
      let s = interpret_commands env nmihlst in
      make_string s
    end
*)


and exec_pdf_mode_intermediate_math_text (_env : vmenv) (_ictx : input_context) (_imivlst : compiled_intermediate_math_text_element list) : syntactic_value =
  failwith "TODO: exec_pdf_mode_intermediate_math_text"


and exec_pdf_mode_intermediate_block_text (_env : vmenv) (_valuectx : syntactic_value) (_imivlst : compiled_intermediate_block_text_element list) : syntactic_value =
  failwith "TODO: exec_pdf_mode_intermediate_block_text"
(*
  let rec interpret_commands env imivlst =
    imivlst |> List.map (fun imiv ->
        match imiv with
        | CompiledImBlockTextEmbedded(code) ->
            let value = exec_value [ make_entry valuectx ] env (List.append code [ OpApplyT(1) ]) [] in
              get_vert_boxes value

        | CompiledImBlockTextContent(imivlstsub, envsub) ->
            interpret_commands envsub imivlstsub

      ) |> List.concat
  in
  let imvblst = interpret_commands env imivlst in
  make_vert imvblst
*)


and exec_pdf_mode_intermediate_inline_text (_env : vmenv) (_ictx : input_context) (_imihlst : compiled_intermediate_inline_text_element list) : syntactic_value =
  failwith "TODO: exec_pdf_mode_intermediate_inline_text"
(*
  let (ctx, ctxsub) = ictx in
    begin
      let rec normalize imihlst =
        imihlst |> List.fold_left (fun acc imih ->
            match imih with
            | CompiledImInlineTextEmbedded(code) ->
                let nmih = CompiledNomInlineTextEmbedded(code) in
                  Alist.extend acc nmih

            | CompiledImInlineTextText(s2) ->
                begin
                  match Alist.chop_last acc with
                  | Some(accrest, CompiledNomInlineTextText(s1)) -> (Alist.extend accrest (CompiledNomInlineTextText(s1 ^ s2)))
                  | _                                           -> (Alist.extend acc (CompiledNomInlineTextText(s2)))
                end

            | CompiledImInlineTextEmbeddedMath(mathcode) ->
                let MathCommand(valuemcmd) = ctxsub.math_command in
                let nmih =
                  CompiledNomInlineTextThunk(
                    List.append mathcode [
                      OpPush(Context(ictx));
                      OpForward(1);  (* -- put the context argument under the math argument -- *)
                      OpPush(valuemcmd);
                      OpApplyT(2)
                    ])
                in
                Alist.extend acc nmih

            | CompiledImInlineTextEmbeddedCodeText(s) ->
                begin
                  match ctxsub.code_text_command with
                  | DefaultCodeTextCommand ->
                      let nmih = CompiledNomInlineTextText(s) in
                      Alist.extend acc nmih

                  | CodeTextCommand(valuectcmd) ->
                      let nmih =
                        CompiledNomInlineTextThunk([
                          OpPush(Context(ictx));
                          OpPush(BaseConstant(BCString(s)));
                          OpPush(valuectcmd);
                          OpApplyT(2)
                        ])
                      in
                      Alist.extend acc nmih
                end

            | CompiledImInlineTextContent(imihlst, envsub) ->
                let nmihlstsub = normalize imihlst in
                let nmih = CompiledNomInlineTextContent(nmihlstsub, envsub) in
                  Alist.extend acc nmih

          ) Alist.empty |> Alist.to_list
      in

      let rec interpret_commands (env : vmenv) (nmihlst : compiled_nom_inline_text_element list) : HorzBox.horz_box list =
        nmihlst |> List.map (fun nmih ->
            match nmih with
            | CompiledNomInlineTextEmbedded(code) ->
                let value = exec_value [ make_entry (Context(ictx)) ] env (List.append code [ OpApplyT(1) ]) [] in
                get_horz_boxes value

            | CompiledNomInlineTextThunk(code) ->
                let value = exec_value [] env code [] in
                get_horz_boxes value

            | CompiledNomInlineTextText(s) ->
                lex_horz_text ctx s

            | CompiledNomInlineTextContent(nmihlstsub, envsub) ->
                interpret_commands envsub nmihlstsub

          ) |> List.concat
      in

      let nmihlst = normalize imihlst in
      let hblst = interpret_commands env nmihlst in
      make_horz hblst
    end
*)


and exec_application (env : vmenv) ~msg:_ (vf : syntactic_value) (vargs : syntactic_value list) : syntactic_value =
  let len = List.length vargs in
    if len = 0 then
      vf
    else
      exec_value (make_entry vf :: (List.rev_map make_entry vargs)) env [OpApplyT(len)] []


and generate_symbol_and_add_to_environment (env : vmenv) (var : varloc) : vmenv * CodeSymbol.t =
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
  let (_, envopt) = exec [] env [OpPush(cv); bindop; OpPushEnv] [] in
  match envopt with
  | Some(env) -> (env, symb)
  | None      -> assert false


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
  fst @@ exec stack env code dump


and exec (stack : stack) (env : vmenv) (code : instruction list) dump : stack_entry =
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


and exec_code (genv : environment) (code : instruction list) : syntactic_value * environment option =
  let (value, envopt) = exec [] (genv, []) code [] in
  let glenvopt =
    match envopt with
    | None      -> None
    | Some(env) -> Some(vmenv_global env)
  in
  (value, glenvopt)


and exec_op (op : instruction) (stack : stack) (env : vmenv) (code : instruction list) dump =
  match op with
  | OpDereference ->
      begin
        match stack with
        | (valuecont, _) :: stack ->
            let ret =
              match valuecont with
              | Location(stid) ->
                  begin
                    match find_location_value (vmenv_global env) stid with
                    | Some(value) -> make_entry value
                    | None        -> report_bug_vm "Dereference"
                  end

              | _ ->
                  report_bug_vm "Dereference"
            in exec (ret :: stack) env code dump

        | _ -> report_bug_vm "invalid argument for OpDereference"
      end

  | OpAccessField(field_nm) ->
      begin
        match stack with
        | (value1, _) :: stack ->
            begin
              match value1 with
              | RecordValue(asc1) ->
                  begin
                    match asc1 |> LabelMap.find_opt field_nm with
                    | Some(v) -> exec (make_entry v :: stack) env code dump
                    | None    -> report_bug_vm ("OpAccessField: field '" ^ field_nm ^ "' not found")
                  end

              | _ ->
                  report_bug_vm "OpAccessField: not a Record"
            end

        | _ -> report_bug_vm "invalid argument for OpAccessField"
      end

  | OpUpdateField(field_nm) ->
      begin
        match stack with
        | (value2, _) :: (value1, _) :: stack ->
            begin
              match value1 with
              | RecordValue(asc1) ->
                  let asc1new =
                    match asc1 |> LabelMap.find_opt field_nm with
                    | Some(_) -> asc1 |> LabelMap.add field_nm value2
                    | None    -> report_bug_vm ("OpUpdateField: field '" ^ field_nm ^ "' not found")
                  in
                  let v = RecordValue(asc1new) in
                  exec (make_entry v :: stack) env code dump

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
              | v0 :: stack -> exec (v0 :: List.rev_append (List.map make_entry vs) stack) env code dump
              | _           -> report_bug_vm "Forward: stack underflow"
            end

      end

  | OpApply(_n) ->
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

  | OpApplyT(_n) ->
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

  | OpBindGlobal(loc, _evid, _refs) ->
      begin
        match stack with
        | (v, _) :: stack ->
            begin
              loc := v;
              exec stack env code dump
            end

        | _ -> report_bug_vm "invalid argument for OpBindGlobal"
      end

  | OpBindLocal(lv, offset, evid, _refs) ->
      begin
            begin
              match stack with
              | (v, _) :: stack ->
                  local_set_value env lv offset v;
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
                  | GlobalVar(loc, _evid, _refs) -> loc := recfunc
                  | LocalVar(lv, offset, _evid, _refs) -> local_set_value env lv offset recfunc
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
        | (v, _) :: stack ->
            let b = get_bool v in
            if b then
              exec stack env body dump
            else
              exec stack env code dump

        | _ -> report_bug_vm "invalid argument for OpBranchIf"
      end

  | OpBranchIfNot(body) ->
      begin
        match stack with
        | (v, _) :: stack ->
            let b = get_bool v in
            if b then
              exec stack env code dump
            else
              exec stack env body dump

        | _ -> report_bug_vm "invalid argument for OpBranchIfNot"
      end

  | OpLoadGlobal(loc, _evid, _refs) ->
      begin
            begin
              let v = !loc in
              exec (make_entry v :: stack) env code dump
            end

      end

  | OpLoadLocal(lv, offset, _evid, _refs) ->
      begin
            begin
              let v = local_get_value env lv offset in
              exec (make_entry v :: stack) env code dump
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

  | OpMakeConstructor(ctor_nm) ->
      begin
        match stack with
        | (valuecont, _) :: stack ->
            let entry = make_entry @@ Constructor(ctor_nm, valuecont) in
            exec (entry :: stack) env code dump

        | _ ->
            report_bug_vm "invalid argument for OpMakeConstructor"
      end

  | OpMakeRecord(keylst) ->
      let rec collect keys asc st =
        match keys with
        | [] ->
            (asc, st)

        | k :: rest ->
            begin
              match st with
              | (v, _) :: stnew -> collect rest (asc |> LabelMap.add k v) stnew
              | _               -> report_bug_vm "MakeRecord: stack underflow"
            end
        in
        let (asc, stack) = collect (List.rev keylst) LabelMap.empty stack in
        let entry = make_entry @@ RecordValue(asc) in
        exec (entry :: stack) env code dump

  | OpMakeTuple(len) ->
        let rec iter n acc st =
          if n <= 0 then
            (acc, st)
          else
            match st with
            | (v, _) :: stnew -> iter (n - 1) (v :: acc) stnew
            | []              -> report_bug_vm "MakeTuple: stack underflow"
        in
        let (vlst, stack) = iter len [] stack in
        let entry = make_entry @@ Tuple(vlst) in
        exec (entry :: stack) env code dump

  | OpPop ->
      let stack =
        try List.tl stack with
        | Invalid_argument(_) -> report_bug_vm "Pop: stack underflow"
      in
      exec stack env code dump

  | OpPush(v) ->
      exec (make_entry v :: stack) env code dump

  | OpPushEnv ->
      failwith "TODO (enhance): OpPushEnv"

  | OpCheckStackTopBool(b, next) ->
      begin
        match stack with
        | (v, _) :: stack ->
            let b0 = get_bool v in
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
        | (v, _) :: stack ->
            begin
              match v with
              | Constructor(nm, sub) when nm = ctor_nm -> exec (make_entry sub :: stack) env code dump
              | _                                      -> exec stack env next dump
            end

        | _ -> report_bug_vm "invalid argument for OpCheckStackTopCtor"
      end

  | OpCheckStackTopEndOfList(next) ->
      begin
        match stack with
        | (v, _) :: stack ->
            begin
              match v with
              | List([]) -> exec stack env code dump
              | _        -> exec stack env next dump
            end

        | _ -> report_bug_vm "invalid argument for OpCheckStackTopEndOfList"
      end

  | OpCheckStackTopInt(i, next) ->
      begin
        match stack with
        | (v, _) :: stack ->
            let i0 = get_int v in
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
        | (v, _) :: stack ->
            begin
              match v with
              | List(car :: cdr) -> exec (make_entry car :: make_entry (List(cdr)) :: stack) env code dump
              | _                -> exec stack env next dump
            end

        | _ -> report_bug_vm "invalid argument for OpCheckStackTopListCons"
      end

  | OpCheckStackTopStr(str, next) ->
      begin
        match stack with
        | (v, _) :: stack ->
            let s0 = get_string v in
            if s0 = str then
              exec stack env code dump
            else
              exec stack env next dump

        | _ -> report_bug_vm "invalid argument for OpCheckStackTopStr"
      end

  | OpCheckStackTopTupleCons(next) ->
      begin
        match stack with
        | (v, _) :: stack ->
            begin
              match v with
              | Tuple(car :: cdr) -> exec (make_entry car :: make_entry (Tuple(cdr)) :: stack) env code dump
              | _                 -> exec stack env next dump
            end

        | _ -> report_bug_vm "invalid argument for OpCheckStackTopTupleCons"
      end

  | OpClosure(varloc_labmap, arity, framesize, body) ->
      let entry = make_entry @@ CompiledClosure(varloc_labmap, arity, [], framesize, body, env) in
      exec (entry :: stack) env code dump

  | OpClosureInlineText(imihlst) ->
      let imihclos = exec_inline_text_content env imihlst in
      exec (make_entry imihclos :: stack) env code dump

  | OpClosureBlockText(imivlst) ->
      let imivclos = exec_block_text_content env imivlst in
      exec (make_entry imivclos :: stack) env code dump

  | OpBindLocationGlobal(loc, _evid) ->
      begin
        match stack with
        | (valueini, _) :: stack ->
            begin
              let stid = register_location (vmenv_global env) valueini in
              loc := Location(stid);
              exec stack env code dump
            end

        | _ -> report_bug_vm "invalid argument for OpBindLocationGlobal"
      end

  | OpBindLocationLocal(lv, offset, _evid) ->
      begin
        match stack with
        | (valueini, _) :: stack ->
            begin
              let stid = register_location (vmenv_global env) valueini in
              local_set_value env lv offset (Location(stid));
              exec stack env code dump
            end

        | _ -> report_bug_vm "invalid argument for OpBindLocationLocal"
      end

  | OpUpdateGlobal(loc, _evid) ->
      begin
        match stack with
        | (valuenew, _) :: stack ->
            begin
              match !loc with
              | Location(stid) ->
                  begin
                    update_location (vmenv_global env) stid valuenew;
                    exec (make_entry const_unit :: stack) env code dump
                  end

              | _ ->
                  report_bug_vm "UpdateGlobal"
            end

        | _ -> report_bug_vm "invalid argument for OpUpdateGlobal"
      end

  | OpUpdateLocal(lv, offset, _evid) ->
      begin
        match stack with
        | (valuenew, _) :: stack ->
            begin
              match local_get_value env lv offset with
              | Location(stid) ->
                   begin
                     update_location (vmenv_global env) stid valuenew;
                     exec (make_entry const_unit :: stack) env code dump
                   end

              | _ ->
                  report_bug_vm "UpdateLocal"
            end

        | _ -> report_bug_vm "invalid argument for OpUpdateLocal"
      end

  | OpSel(tpart, fpart) ->
      begin
        match stack with
        | (v, _) :: stack ->
            let b = get_bool v in
            if b then
              exec stack env tpart dump
            else
              exec stack env fpart dump

        | _ -> report_bug_vm "invalid argument for OpSel"
      end

  | OpInsertArgs(lst) ->
      begin
        match stack with
        | func :: stack ->
            begin
              exec (func :: (List.rev_append (List.map make_entry lst) stack)) env code dump
            end

        | _ -> report_bug_vm "invalid argument for OpInsertArgs"
      end

  | OpApplyCodeCombinator(codef, arity) ->
      let (valuelst, stack) = popn stack arity in
      let codelst = valuelst |> List.map get_code in
      let coderet = codef codelst in
      exec (make_entry (CodeValue(coderet)) :: stack) env code dump

  | OpCodeMakeRecord(keylst) ->
      let rec collect keys asc st =
        match keys with
        | [] ->
            (asc, st)

        | k :: rest ->
            begin
              match st with
              | (CodeValue(cv), _) :: stnew -> collect rest (asc |> LabelMap.add k cv) stnew
              | _                           -> report_bug_vm "CodeMakeRecord"
            end
      in
      let (cdasc, stack) = collect (List.rev keylst) LabelMap.empty stack in
      let entry = make_entry @@ CodeValue(CdRecord(cdasc)) in
      exec (entry :: stack) env code dump

  | OpCodeMakeTuple(n) ->
      let rec iter n acc st =
        if n <= 0 then
          (acc, st)
        else
          match st with
          | (CodeValue(cv), _) :: stnew -> iter (n - 1) (cv :: acc) stnew
          | _                           -> report_bug_vm "CodeMakeTuple"
      in
      let (cvs, stack) = iter n [] stack in
      let cvs =
        match cvs with
        | cv1 :: cv2 :: cvrest -> TupleList.make cv1 cv2 cvrest
        | _                    -> assert false
      in
      let entry = make_entry @@ CodeValue(CdTuple(cvs)) in
      exec (entry :: stack) env code dump

  | OpCodeMakeInlineText(compihlst) ->
      let cdihlst = exec_code_inline_text env compihlst in
      let entry = make_entry @@ CodeValue(CdInlineText(cdihlst)) in
      exec (entry :: stack) env code dump

  | OpCodeMakeBlockText(compivlst) ->
      let cdivlst = exec_code_block_text env compivlst in
      let entry = make_entry @@ CodeValue(CdBlockText(cdivlst)) in
      exec (entry :: stack) env code dump

  | OpCodePatternMatch(rng, comppatbrs) ->
      begin
        match stack with
        | (CodeValue(cv0), _) :: stack ->
            let cdpatbrs = List.map (exec_code_pattern_branch env) comppatbrs in
            let entry = make_entry @@ CodeValue(CdPatternMatch(rng, cv0, cdpatbrs)) in
            exec (entry :: stack) env code dump

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
      let (value2, envopt) = exec [] env instrs2 [] in
      let cv2 = get_code value2 in
      let entry = (CodeValue(CdLetRecIn(Alist.to_list cdrecbindacc, cv2)), envopt) in
        (* -- returns the environment -- *)
      exec (entry :: stack) env code dump

  | OpCodeLetNonRec(irpat, instrs1, instrs2) ->
      let value1 = exec_value [] env instrs1 [] in
      let cv1 = get_code value1 in
      let (env, cdpat) = exec_code_pattern_tree env irpat in
      let (value2, envopt) = exec [] env instrs2 [] in
      let cv2 = get_code value2 in
      let entry = (CodeValue(CdLetNonRecIn(cdpat, cv1, cv2)), envopt) in
        (* -- returns the environment -- *)
      exec (entry :: stack) env code dump

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
      let entry = (CodeValue(CdFunction(symb_labmap, CdPatternBranch(cdpat, cv1))), None) in
      exec (entry :: stack) env code dump

  | OpCodeLetMutable(var, instrs1, instrs2) ->
      let (env, symb) = generate_symbol_and_add_to_environment env var in
      let value1 = exec_value [] env instrs1 [] in
      let cv1 = get_code value1 in
      let (value2, envopt) = exec [] env instrs2 [] in
      let cv2 = get_code value2 in
      let entry = (CodeValue(CdLetMutableIn(symb, cv1, cv2)), envopt) in
      exec (entry :: stack) env code dump

  | OpCodeOverwrite(instrs1) ->
      begin
        match stack with
        | (CodeSymbol(symb), _) :: stack ->
            let value1 = exec_value [] env instrs1 [] in
            let cv1 = get_code value1 in
            let entry = (CodeValue(CdOverwrite(symb, cv1)), None) in
            exec (entry :: stack) env code dump

        | _ ->
            report_bug_vm "not a symbol (OpCodeOverwrite)"
      end

  | OpConvertSymbolToCode ->
      begin
        match stack with
        | (CodeSymbol(symb), _) :: stack ->
            let rng = Range.dummy "OpConvertSymbolToCode" in
            let entry = (CodeValue(CdContentOf(rng, symb)), None) in
            exec (entry :: stack) env code dump

        | (v, _) :: _ ->
            report_bug_vm_value "not a code symbol" v

        | [] ->
            report_bug_vm "stack underflow (OpConvertSymbolToCode)"
      end

#include "__vm.gen.ml"
