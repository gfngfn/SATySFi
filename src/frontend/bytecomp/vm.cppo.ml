
open MyUtil
open LengthInterface
open GraphicBase
open Types
open EvalUtil

exception ExecError of string


let report_dynamic_error msg =
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


let popn stack n =
  let rec iter st n acc =
    if n = 0 then
      (acc, st)
    else
      match st with
      | x :: xs -> iter xs (n - 1) (x :: acc)
      | []      -> report_bug_vm "stack underflow!"
  in
    iter stack n []


let rec get_path env c_pathcomplst c_cycleopt =
  let pathelemlst =
    c_pathcomplst |> List.map (function
      | CompiledPathLineTo(ptcode) ->
          let pt = get_point @@ exec [] env ptcode [] in
            LineTo(pt)

      | CompiledPathCubicBezierTo(pt1code, pt2code, ptcode) ->
          let pt1 = get_point @@ exec [] env pt1code [] in
          let pt2 = get_point @@ exec [] env pt2code [] in
          let pt = get_point @@ exec [] env ptcode [] in
            CubicBezierTo(pt1, pt2, pt)
    )
  in
  let closingopt =
    c_cycleopt |> option_map (function
      | CompiledPathLineTo(()) ->
          LineTo(())

      | CompiledPathCubicBezierTo(pt1code, pt2code, ()) ->
          let pt1 = get_point @@ exec [] env pt1code [] in
          let pt2 = get_point @@ exec [] env pt2code [] in
            CubicBezierTo(pt1, pt2, ())
    )
  in
    (pathelemlst, closingopt)


and exec_input_horz_content env ihlst =
  let imihlist = ihlst |> List.map (function
    | CompiledInputHorzText(s) ->
        CompiledImInputHorzText(s)

    | CompiledInputHorzEmbedded(code) ->
        CompiledImInputHorzEmbedded(code)

    | CompiledInputHorzEmbeddedMath(code) ->
        CompiledImInputHorzEmbeddedMath(code)

    | CompiledInputHorzContent(code) ->
        let value = exec [] env code [] in
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
        let value = exec [] env code [] in
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
      let value = exec [] env instrs [] in
      let cv = get_code value in
      InputHorzEmbedded(cv)

  | InputHorzEmbeddedMath(instrs) ->
      let value = exec [] env instrs [] in
      let cv = get_code value in
      InputHorzEmbeddedMath(cv)

  | InputHorzContent(instrs) ->
      let value = exec [] env instrs [] in
      let cv = get_code value in
      InputHorzContent(cv)
  )


and exec_code_input_vert env irivlst =
  irivlst |> List.map (function
  | InputVertEmbedded(instrs) ->
      let value = exec [] env instrs [] in
      let cv = get_code value in
      InputVertEmbedded(cv)

  | InputVertContent(instrs) ->
      let value = exec [] env instrs [] in
      let cv = get_code value in
      InputVertContent(cv)
  )


and exec_text_mode_intermediate_input_vert (env : vmenv) (valuetctx : syntactic_value) (imivlst : compiled_intermediate_input_vert_element list) : syntactic_value =
  let rec interpret_commands env imivlst =
    imivlst |> List.map (fun imiv ->
        match imiv with
        | CompiledImInputVertEmbedded(code) ->
            let valueret = exec [valuetctx] env (List.append code [OpApplyT(1)]) [] in
              get_string valueret

        | CompiledImInputVertContent(imivlstsub, envsub) ->
            interpret_commands envsub imivlstsub

      ) |> String.concat ""
  in
  let s = interpret_commands env imivlst in
  make_string s


and exec_text_mode_intermediate_input_horz (env : vmenv) (valuetctx : syntactic_value) (imihlst : compiled_intermediate_input_horz_element list) : syntactic_value =
  let tctx = get_text_mode_context valuetctx in
    begin
      let rec normalize imihlst =
        imihlst |> List.fold_left (fun acc imih ->
            match imih with
            | CompiledImInputHorzEmbedded(code) ->
                let nmih = CompiledNomInputHorzEmbedded(code) in
                  Alist.extend acc nmih

            | CompiledImInputHorzText(s2) ->
                begin
                  match Alist.chop_last acc with
                  | Some(accrest, CompiledNomInputHorzText(s1)) -> (Alist.extend accrest (CompiledNomInputHorzText(s1 ^ s2)))
                  | _                                           -> (Alist.extend acc (CompiledNomInputHorzText(s2)))
                end

            | CompiledImInputHorzEmbeddedMath(mathcode) ->
                failwith "Vm_> math; remains to be supported."
(*
                let nmih = CompiledNomInputHorzThunk(List.append mathcode [OpPush(valuetctx); OpForward(1); OpPush(valuemcmd); OpApplyT(2)]) in
                  Alist.extend acc nmih
*)

            | CompiledImInputHorzContent(imihlst, envsub) ->
                let nmihlstsub = normalize imihlst in
                let nmih = CompiledNomInputHorzContent(nmihlstsub, envsub) in
                  Alist.extend acc nmih

          ) Alist.empty |> Alist.to_list
      in

      let rec interpret_commands (env : vmenv) (nmihlst : compiled_nom_input_horz_element list) : string =
        nmihlst |> List.map (fun nmih ->
            match nmih with
            | CompiledNomInputHorzEmbedded(code) ->
                let valueret = exec [valuetctx] env (List.append code [OpApplyT(1)]) [] in
                  get_string valueret

            | CompiledNomInputHorzThunk(code) ->
                let valueret = exec [] env code [] in
                  get_string valueret

            | CompiledNomInputHorzText(s) ->
                let uchlst = InternalText.to_uchar_list (InternalText.of_utf8 s) in
                let uchlstret = TextBackend.stringify uchlst tctx in
                  InternalText.to_utf8 (InternalText.of_uchar_list uchlstret)

            | CompiledNomInputHorzContent(nmihlstsub, envsub) ->
                interpret_commands envsub nmihlstsub

          ) |> String.concat ""
      in

      let nmihlst = normalize imihlst in
      let s = interpret_commands env nmihlst in
      make_string s
    end


and exec_pdf_mode_intermediate_input_vert (env : vmenv) (valuectx : syntactic_value) (imivlst : compiled_intermediate_input_vert_element list) : syntactic_value =
  let rec interpret_commands env imivlst =
    imivlst |> List.map (fun imiv ->
        match imiv with
        | CompiledImInputVertEmbedded(code) ->
            let valueret = exec [valuectx] env (List.append code [OpApplyT(1)]) [] in
              get_vert valueret

        | CompiledImInputVertContent(imivlstsub, envsub) ->
            interpret_commands envsub imivlstsub

      ) |> List.concat
  in
  let imvblst = interpret_commands env imivlst in
  make_vert imvblst


and exec_pdf_mode_intermediate_input_horz (env : vmenv) (valuectx : syntactic_value) (imihlst : compiled_intermediate_input_horz_element list) : syntactic_value =
  let (ctx, ctxsub) = get_context valuectx in
    begin
      let rec normalize imihlst =
        imihlst |> List.fold_left (fun acc imih ->
            match imih with
            | CompiledImInputHorzEmbedded(code) ->
                let nmih = CompiledNomInputHorzEmbedded(code) in
                  Alist.extend acc nmih

            | CompiledImInputHorzText(s2) ->
                begin
                  match Alist.chop_last acc with
                  | Some(accrest, CompiledNomInputHorzText(s1)) -> (Alist.extend accrest (CompiledNomInputHorzText(s1 ^ s2)))
                  | _                                           -> (Alist.extend acc (CompiledNomInputHorzText(s2)))
                end

            | CompiledImInputHorzEmbeddedMath(mathcode) ->
                let MathCommand(valuemcmd) = ctxsub.math_command in
                let nmih = CompiledNomInputHorzThunk(List.append mathcode [OpPush(valuectx); OpForward(1); OpPush(valuemcmd); OpApplyT(2)]) in
                  Alist.extend acc nmih

            | CompiledImInputHorzContent(imihlst, envsub) ->
                let nmihlstsub = normalize imihlst in
                let nmih = CompiledNomInputHorzContent(nmihlstsub, envsub) in
                  Alist.extend acc nmih

          ) Alist.empty |> Alist.to_list
      in

      let rec interpret_commands (env : vmenv) (nmihlst : compiled_nom_input_horz_element list) : HorzBox.horz_box list =
        nmihlst |> List.map (fun nmih ->
            match nmih with
            | CompiledNomInputHorzEmbedded(code) ->
                let valueret = exec [valuectx] env (List.append code [OpApplyT(1)]) [] in
                  get_horz valueret

            | CompiledNomInputHorzThunk(code) ->
                let valueret = exec [] env code [] in
                  get_horz valueret

            | CompiledNomInputHorzText(s) ->
                lex_horz_text ctx s

            | CompiledNomInputHorzContent(nmihlstsub, envsub) ->
                interpret_commands envsub nmihlstsub

          ) |> List.concat
      in

      let nmihlst = normalize imihlst in
      let hblst = interpret_commands env nmihlst in
      make_horz hblst
    end


and exec_application (env : vmenv) (vf : syntactic_value) (vargs : syntactic_value list) =
  let len = List.length vargs in
    if len = 0 then
      vf
    else
      exec (vf :: (List.rev vargs)) env [OpApplyT(len)] []


and exec_code_pattern_branch (env : vmenv) (comppatbr : (instruction list) pattern_branch_scheme) =
  failwith "remains to be implemented. (exec_code_pattern_branch)" (*
  match comppatbr with
  | PatternBranch(pat, instrs1) ->
      let value1 = exec [] env instrs1 [] in
      let cv1 = get_code value1 in
      CdPatternBranch(pat, cv1)

  | PatternBranchWhen(pat, instrs, instrs1) ->
      let value = exec [] env instrs [] in
      let cv = get_code value in
      let value1 = exec [] env instrs1 [] in
      let cv1 = get_code value1 in
      CdPatternBranchWhen(pat, cv, cv1)
  *)


and exec (stack : syntactic_value list) (env : vmenv) (code : instruction list) dump =
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


and exec_op (op : instruction) stack (env : vmenv) (code : instruction list) dump =
  match op with
  | OpDereference ->
      begin
        match stack with
        | valuecont :: stack ->
            let ret =
              match valuecont with
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

  | OpAccessField(field_nm) ->
      begin
        match stack with
        | value1 :: stack ->
            begin
              match value1 with
              | RecordValue(asc1) ->
                  begin
                    match Assoc.find_opt asc1 field_nm with
                    | Some(v) -> exec (v :: stack) env code dump
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
        | value2 :: value1 :: stack ->
            begin
              match value1 with
              | RecordValue(asc1) ->
                  let asc1new =
                    match Assoc.find_opt asc1 field_nm with
                    | Some(_) -> Assoc.add asc1 field_nm value2
                    | None    -> report_bug_vm ("OpUpdateField: field '" ^ field_nm ^ "' not found")
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
      begin
        match stack with
        | f :: stack ->
            begin
              match f with
              | CompiledClosure(optvars, arity, pargs, framesize, body, env1) ->
                  let body =
                    optvars |> List.fold_left (fun acc optvar ->
                      let bindop =
                        match optvar with
                        | GlobalVar(loc, evid, refs)    -> OpBindGlobal(loc, evid, !refs)
                        | LocalVar(lv, off, evid, refs) -> OpBindLocal(lv, off, evid, !refs)
                      in
                      OpPush(Constructor("None", const_unit)) :: bindop :: acc
                    ) body
                  in
                  if arity = n then
                    if pargs = [] then
                      exec stack (newframe env1 framesize) body ((env, code) :: dump)
                    else
                      let (args, stack) = popn stack n in
                      let allargs = List.rev (pargs @ args) in
                      exec (allargs @ stack) (newframe env1 framesize) body ((env, code) :: dump)

                  else if arity > n then
                    let (args, stack) = popn stack n in
                    let applied = CompiledClosure([], arity - n, pargs @ args, framesize, body, env1) in
                    exec (applied :: stack) env code dump

                  else
                    let (surplus, stack) = popn stack (n - arity) in
                    let (args, stack) = popn stack arity in
                    let allargs = List.rev (pargs @ args) in
                    exec (allargs @ stack) (newframe env1 framesize) body ((env, OpInsertArgs(surplus) :: OpApply(n - arity) :: code) :: dump)

              | CompiledPrimitiveClosure(arity, [], framesize, body, env1, astf) ->
                  if arity = n then
                    exec stack (newframe env1 framesize) body ((env, code) :: dump)

                  else if arity > n then
                    let (args, stack) = popn stack n in
                    let applied = CompiledClosure([], arity - n, args, framesize, body, env1) in
                    exec (applied :: stack) env code dump

                  else
                    let (surplus, stack) = popn stack (n - arity) in
                    exec stack (newframe env1 framesize) body ((env, OpInsertArgs(surplus) :: OpApply(n - arity) :: code) :: dump)

               | _ ->
                   report_bug_vm_value "Apply: not a function" f
            end

        | _ -> report_bug_vm "invalid argument for OpApply"
      end

  | OpApplyT(n) ->
      begin
        match stack with
        | f :: stack ->
            begin
              match f with
              | CompiledClosure(optvars, arity, pargs, framesize, body, env1) ->
                  let body =
                    optvars |> List.fold_left (fun acc optvar ->
                      let bindop =
                        match optvar with
                        | GlobalVar(loc, evid, refs)    -> OpBindGlobal(loc, evid, !refs)
                        | LocalVar(lv, off, evid, refs) -> OpBindLocal(lv, off, evid, !refs)
                      in
                      OpPush(Constructor("None", const_unit)) :: bindop :: acc
                    ) body
                  in
                  if arity = n then
                    if pargs = [] then
                      exec stack (newframe env1 framesize) body dump
                    else
                      let (args, stack) = popn stack n in
                      let allargs = List.rev (pargs @ args) in
                      exec (allargs @ stack) (newframe env1 framesize) body dump

                  else if arity > n then
                    let (args, stack) = popn stack n in
                    let applied = CompiledClosure([], arity - n, pargs @ args, framesize, body, env1) in
                    exec (applied :: stack) env code dump

                  else
                    let (surplus, stack) = popn stack (n - arity) in
                    let (args, stack) = popn stack arity in
                    let allargs = List.rev (pargs @ args) in
                    exec (allargs @ stack) (newframe env1 framesize) body ((env, OpInsertArgs(surplus) :: OpApplyT(n - arity) :: code) :: dump)

               | CompiledPrimitiveClosure(arity, [], framesize, body, env1, astf) ->
                   if arity = n then
                     exec stack (newframe env1 framesize) body dump

                   else if arity > n then
                     let (args, stack) = popn stack n in
                     let applied = CompiledClosure([], arity - n, args, framesize, body, env1) in
                     exec (applied :: stack) env code dump

                   else
                     let (surplus, stack) = popn stack (n-arity) in
                     exec stack (newframe env1 framesize) body ((env, OpInsertArgs(surplus) :: OpApplyT(n - arity) :: code) :: dump)

               | _ ->
                   report_bug_vm_value "ApplyT: not a function" f
            end

        | _ -> report_bug_vm "invalid argument for OpApplyT"
      end

  | OpApplyOptional ->
      begin
        match stack with
        | v :: f :: stack ->
            begin
              match f with
              | CompiledClosure(var :: vars, arity, pargs, framesize, body, env1) ->
                  let bindop =
                    match var with
                    | GlobalVar(loc, evid, refs)    -> OpBindGlobal(loc, evid, !refs)
                    | LocalVar(lv, off, evid, refs) -> OpBindLocal(lv, off, evid, !refs)
                  in
                  let body = OpPush(Constructor("Some", v)) :: bindop :: body in
                  let fnew = CompiledClosure(vars, arity, pargs, framesize, body, env1) in
                  exec (fnew :: stack) env code dump

              | _ ->
                  report_bug_vm_value "ApplyOptional: not a function with optional arguments" f
            end

        | _ -> report_bug_vm "invalid argument for OpApplyOptional"
      end

  | OpApplyOmission ->
      begin
        match stack with
        | f :: stack ->
            begin
              match f with
              | CompiledClosure(var :: vars, arity, pargs, framesize, body, env1) ->
                  let bindop =
                    match var with
                    | GlobalVar(loc, evid, refs)    -> OpBindGlobal(loc, evid, !refs)
                    | LocalVar(lv, off, evid, refs) -> OpBindLocal(lv, off, evid, !refs)
                  in
                  let body = OpPush(Constructor("None", const_unit)) :: bindop :: body in
                  let fnew = CompiledClosure(vars, arity, pargs, framesize, body, env1) in
                  exec (fnew :: stack) env code dump

              | _ ->
                  report_bug_vm_value "ApplyOmission: not a function with optional arguments" f
            end

        | _ -> report_bug_vm "invalid argument for OpApplyOmission"
      end

  | OpBindGlobal(loc, evid, refs) ->
      begin
        match stack with
        | v :: stack ->
            begin
              loc := v;
              exec stack env code dump
            end

        | _ -> report_bug_vm "invalid argument for OpBindGlobal"
      end

  | OpBindLocal(lv, offset, evid, refs) ->
      begin
            begin
              match stack with
              | v :: stack ->
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
                let recfunc = exec [] env code [] in
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
        | v :: stack ->
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
        | v :: stack ->
            let b = get_bool v in
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
      begin
            begin
              let v =
                try List.hd stack with
                | Invalid_argument(_) -> report_bug_vm "Dup: stack underflow"
              in
                exec (v :: stack) env code dump
            end

      end

  | OpError(msg) ->
      begin
            begin
              raise (ExecError(msg))
            end

      end

  | OpMakeConstructor(ctor_nm) ->
      begin
        match stack with
        | valuecont :: stack ->
            begin
              exec (Constructor(ctor_nm, valuecont) :: stack) env code dump
            end

        | _ -> report_bug_vm "invalid argument for OpMakeConstructor"
      end

  | OpMakeRecord(keylst) ->
      begin
            begin
              let rec collect keys asc st =
                match keys with
                | [] ->
                    (asc, st)

                | k :: rest ->
                    begin
                      match st with
                      | v :: stnew -> collect rest (Assoc.add asc k v) stnew
                      | _          -> report_bug_vm "MakeRecord: stack underflow"
                    end
                in
                let (asc, stack) = collect (List.rev keylst) Assoc.empty stack in
                exec (RecordValue(asc) :: stack) env code dump
            end

      end

  | OpMakeTuple(len) ->
      begin
            begin
              let rec iter n acc st =
                if n <= 0 then
                  (acc, st)
                else
                  match st with
                  | value :: stnew -> iter (n - 1) (value :: acc) stnew
                  | []             -> report_bug_vm "MakeTuple: stack underflow"
              in
              let (vlst, stack) = iter len [] stack in
              exec (Tuple(vlst) :: stack) env code dump
            end

      end

  | OpPop ->
      begin
            begin
              let stack =
                try List.tl stack with
                | Invalid_argument(_) -> report_bug_vm "Pop: stack underflow"
              in
              exec stack env code dump
            end

      end

  | OpPush(v) ->
      begin
            begin
              exec (v :: stack) env code dump
            end

      end

  | OpPushEnv ->
      begin
            begin
              exec (EvaluatedEnvironment(vmenv_global env) :: stack) env code dump
            end

      end

  | OpCheckStackTopBool(b, next) ->
      begin
        match stack with
        | v :: stack ->
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
        | v :: stack ->
            begin
              match v with
              | Constructor(nm, sub) when nm = ctor_nm -> exec (sub :: stack) env code dump
              | _                                      -> exec stack env next dump
            end

        | _ -> report_bug_vm "invalid argument for OpCheckStackTopCtor"
      end

  | OpCheckStackTopEndOfList(next) ->
      begin
        match stack with
        | v :: stack ->
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
        | v :: stack ->
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
        | v :: stack ->
            begin
              match v with
              | List(car :: cdr) -> exec (car :: List(cdr) :: stack) env code dump
              | _                -> exec stack env next dump
            end

        | _ -> report_bug_vm "invalid argument for OpCheckStackTopListCons"
      end

  | OpCheckStackTopStr(str, next) ->
      begin
        match stack with
        | v :: stack ->
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
        | v :: stack ->
            begin
              match v with
              | Tuple(car :: cdr) -> exec (car :: Tuple(cdr) :: stack) env code dump
              | _                 -> exec stack env next dump
            end

        | _ -> report_bug_vm "invalid argument for OpCheckStackTopTupleCons"
      end

  | OpClosure(optvars, arity, framesize, body) ->
      begin
            begin
              exec (CompiledClosure(optvars, arity, [], framesize, body, env) :: stack) env code dump
            end

      end

  | OpClosureInputHorz(imihlst) ->
      begin
            begin
              let imihclos = exec_input_horz_content env imihlst in
              exec (imihclos :: stack) env code dump
            end

      end

  | OpClosureInputVert(imivlst) ->
      begin
            begin
              let imivclos = exec_input_vert_content env imivlst in
              exec (imivclos :: stack) env code dump
            end

      end

  | OpBindLocationGlobal(loc, evid) ->
      begin
        match stack with
        | valueini :: stack ->
            begin
              let stid = register_location (vmenv_global env) valueini in
              loc := Location(stid);
              exec stack env code dump
            end

        | _ -> report_bug_vm "invalid argument for OpBindLocationGlobal"
      end

  | OpBindLocationLocal(lv, offset, evid) ->
      begin
        match stack with
        | valueini :: stack ->
            begin
              let stid = register_location (vmenv_global env) valueini in
              local_set_value env lv offset (Location(stid));
              exec stack env code dump
            end

        | _ -> report_bug_vm "invalid argument for OpBindLocationLocal"
      end

  | OpUpdateGlobal(loc, evid) ->
      begin
        match stack with
        | valuenew :: stack ->
            begin
              match !loc with
              | Location(stid) ->
                  begin
                    update_location (vmenv_global env) stid valuenew;
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
        | valuenew :: stack ->
            begin
              match local_get_value env lv offset with
              | Location(stid) ->
                   begin
                     update_location (vmenv_global env) stid valuenew;
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
        | v :: stack ->
            let b = get_bool v in
            if b then
              exec stack env tpart dump
            else
              exec stack env fpart dump

        | _ -> report_bug_vm "invalid argument for OpSel"
      end

  | OpBackendMathList(n) ->
      begin
            begin
              let rec iter n st acc =
                if n <= 0 then
                  (acc, st)
                else
                  match st with
                  | MathValue(m) :: stnew -> iter (n - 1) stnew (m :: acc)
                  | _                     -> report_bug_vm "BackendMathList"
              in
              let (mlst, stack) = iter n stack [] in
              exec (MathValue(List.concat mlst) :: stack) env code dump
            end

      end

  | OpPath(c_pathcomplst, c_cycleopt) ->
      begin
        match stack with
        | _tmp0 :: stack ->
            let pt0 = get_point _tmp0 in
            let ret =
              let (pathelemlst, closingopt) = get_path env c_pathcomplst c_cycleopt in
              make_path [GeneralPath(pt0, pathelemlst, closingopt)]
            in exec (ret :: stack) env code dump

        | _ -> report_bug_vm "invalid argument for OpPath"
      end

  | OpInsertArgs(lst) ->
      begin
        match stack with
        | func :: stack ->
            begin
              exec (func :: (List.rev_append lst stack)) env code dump
            end

        | _ -> report_bug_vm "invalid argument for OpInsertArgs"
      end

  | OpApplyCodeCombinator(codef, arity) ->
      let (valuelst, stack) = popn stack arity in
      let codelst = valuelst |> List.map get_code in
      let coderet = codef codelst in
      exec (CodeValue(coderet) :: stack) env code dump

  | OpCodeMakeRecord(keylst) ->
      let rec collect keys asc st =
        match keys with
        | [] ->
            (asc, st)

        | k :: rest ->
            begin
              match st with
              | CodeValue(cv) :: stnew -> collect rest (Assoc.add asc k cv) stnew
              | _                      -> report_bug_vm "CodeMakeRecord"
            end
      in
      let (cdasc, stack) = collect (List.rev keylst) Assoc.empty stack in
      exec (CodeValue(CdRecord(cdasc)) :: stack) env code dump

  | OpCodeMathList(n) ->
      let rec iter n st acc =
        if n <= 0 then
          (acc, st)
        else
          match st with
          | CodeValue(cv) :: stnew -> iter (n - 1) stnew (cv :: acc)
          | _                      -> report_bug_vm "CodeMathList"
      in
      let (cvlst, stack) = iter n stack [] in
      exec (CodeValue(CdMathList(cvlst)) :: stack) env code dump

  | OpCodeMakeTuple(n) ->
      let rec iter n acc st =
        if n <= 0 then
          (acc, st)
        else
          match st with
          | CodeValue(cv) :: stnew -> iter (n - 1) (cv :: acc) stnew
          | _                      -> report_bug_vm "CodeMakeTuple"
      in
      let (cvlst, stack) = iter n [] stack in
      exec (CodeValue(CdTuple(cvlst)) :: stack) env code dump

  | OpCodeMakeInputHorz(compihlst) ->
      let cdihlst = exec_code_input_horz env compihlst in
      exec (CodeValue(CdInputHorz(cdihlst)) :: stack) env code dump

  | OpCodeMakeInputVert(compivlst) ->
      let cdivlst = exec_code_input_vert env compivlst in
      exec (CodeValue(CdInputVert(cdivlst)) :: stack) env code dump

  | OpCodePatternMatch(rng, comppatbrs) ->
      begin
        match stack with
        | CodeValue(cv0) :: stack ->
            let cdpatbrs = List.map (exec_code_pattern_branch env) comppatbrs in
            exec (CodeValue(CdPatternMatch(rng, cv0, cdpatbrs)) :: stack) env code dump

        | _ ->
            report_bug_vm "CodePatternMatch: stack underflow"
      end

  | OpCodeLetRec(comprecbinds, instrs2) ->
      failwith "OpCodeLetRec; remains to be implemented. (exec_op)" (*
      let cdrecbinds =
        comprecbinds |> List.map (function LetRecBinding(evid, comppatbr) ->
          let cdpatbr = exec_code_pattern_branch env comppatbr in
          CdLetRecBinding(evid, cdpatbr)
        )
      in
      let value2 = exec [] env instrs2 [] in
      let cv2 = get_code value2 in
      exec (CodeValue(CdLetRecIn(cdrecbinds, cv2)) :: stack) env code dump
      *)

#include "__vm.gen.ml"
