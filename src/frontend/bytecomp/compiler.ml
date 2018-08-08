
module Types = Types_
open MyUtil
open LengthInterface
open Types

let report_bug_compiler msg =
  Format.printf "[Bug]@ %s:" msg;
  failwith ("bug: " ^ msg)

let report_bug_compiler_ast msg ast =
  Format.printf "[Bug]@ %s:" msg;
  Format.printf "%a" pp_abstract_tree ast;
  failwith ("bug: " ^ msg)


let rec compile_list irlist cont =
  let rec iter irlist cont =
    match irlist with
    | []         -> cont
    | ir :: tail -> iter tail (compile ir cont)
  in
    (* -- left-to-right evaluation -- *)
    iter (List.rev irlist) cont


and emit_appop arity cont inc_ctx =
  let arity = arity + (if inc_ctx then 1 else 0) in
  let appop = if cont = [] then OpApplyT(arity) else OpApply(arity) in
    if arity = 0 then
      cont
    else
      appop :: cont


and compile_input_horz_content (ihlst : ir_input_horz_element list) =
  let compiled_ihlist =
    ihlst |> List.map (function
      | IRInputHorzText(s) ->
          CompiledInputHorzText(s)

      | IRInputHorzEmbedded(irabs) ->
          let compiled = compile irabs [] in
(*
          let appcode = emit_appop (List.length irarglist) [] true in
          let cmdcode = compile ircmd appcode in
          let compiled = compile_list irarglist cmdcode in
*)
            CompiledInputHorzEmbedded(compiled)

      | IRInputHorzEmbeddedMath(irmath) ->
          CompiledInputHorzEmbeddedMath(compile irmath [])

      | IRInputHorzContent(ir) ->
          CompiledInputHorzContent(compile ir [])
    )
  in
    compiled_ihlist

and compile_input_vert_content (ivlst : ir_input_vert_element list) =
  let compiled_ivlist =
    ivlst |> List.map (function
      | IRInputVertEmbedded(irabs) ->
          let compiled = compile irabs [] in
(*
          let appcode = emit_appop (List.length irarglist) [] true in
          let cmdcode = compile ircmd appcode in
          let compiled = compile_list irarglist cmdcode in
*)
            CompiledInputVertEmbedded(compiled)

      | IRInputVertContent(ir) ->
          CompiledInputVertContent(compile ir [])
    )
  in
    compiled_ivlist

and compile_path pathcomplst (cycleopt : unit ir_path_component option) =
  let c_pathcomplst =
    pathcomplst |> List.map (function
      | IRPathLineTo(irpt) ->
          CompiledPathLineTo(compile irpt [])

      | IRPathCubicBezierTo(irpt1, irpt2, irpt) ->
          let pt1 = compile irpt1 [] in
          let pt2 = compile irpt2 [] in
          let pt = compile irpt [] in
            CompiledPathCubicBezierTo(pt1, pt2, pt)
    )
  in
  let c_cycleopt =
    cycleopt |> option_map (function
      | IRPathLineTo(()) ->
          CompiledPathLineTo(())

      | IRPathCubicBezierTo(irpt1, irpt2, ()) ->
          let pt1 = compile irpt1 [] in
          let pt2 = compile irpt2 [] in
            CompiledPathCubicBezierTo(pt1, pt2, ())
    )
  in
    (c_pathcomplst, c_cycleopt)


and compile (ir : ir) (cont : instruction list) =
  match ir with
  (* ---- basic value ---- *)

  | IRConstant(v) -> OpPush(v) :: cont

  | IRTerminal -> OpPushEnv :: cont

  | IRInputHorz(ihlst) ->
      OpClosureInputHorz(compile_input_horz_content ihlst) :: cont
    (* -- lazy evaluation; evaluates embedded variables only -- *)

  | IRInputVert(ivlst) ->
      OpClosureInputVert(compile_input_vert_content ivlst) :: cont
    (* -- lazy evaluation; evaluates embedded variables only -- *)

  (* -- fundamentals -- *)

  | IRContentOf(var) ->
      begin
        match var with
        | GlobalVar(loc, evid, refs) ->
          if !loc = Nil then
            OpLoadGlobal(loc, evid, !refs) :: cont
          else
            OpPush(!loc) :: cont

        | LocalVar(lv, off, evid, refs) -> OpLoadLocal(lv, off, evid, !refs) :: cont
      end

  | IRLetRecIn(recbinds, ast2) ->
      let binds =
        recbinds |> List.map (fun (var, ir) -> (var, compile ir []))
      in
        OpBindClosuresRec(binds) :: (compile ast2 cont)

  | IRLetNonRecIn(ir1, irpat, ir2) ->
      compile ir1 @@ compile_patsel (Range.dummy "LetNonRecIn") [IRPatternBranch(irpat, ir2)] cont

  | IRFunction(framesize, optvars, irpatlst, irbody) ->
      let body = compile irbody [] in
      let patlst = compile_patlist irpatlst body in
      let (optcode, n) = optimize_func_prologue patlst in
        if framesize - n = 0 then
          OpClosure(optvars, List.length irpatlst, 0, optcode) :: cont
        else
          OpClosure(optvars, List.length irpatlst, framesize, optcode) :: cont

  | IRApply(arity, ircallee, irargs) ->
      let n = List.length irargs in
      compile ircallee @@ (compile_list irargs @@ OpForward(n) :: emit_appop n cont false)

  | IRApplyPrimitive(op, arity, irargs) ->
      compile_list irargs (op :: cont)

  | IRApplyOptional(ircallee, iroptarg) ->
      compile ircallee @@ (compile iroptarg @@ OpApplyOptional :: cont)
(*
      failwith "IRApplyOptional: remains to be implemented"
*)

  | IRApplyOmission(irabs) ->
      compile irabs @@ OpApplyOmission :: cont
(*
      failwith "IRApplyOmission: remains to be implemented"
*)

  | IRTuple(len, iritems) ->
      compile_list iritems (OpMakeTuple(len) :: cont)

  | IRIfThenElse(irb, ir1, ir2) ->
      let tpart = compile ir1 cont in
      let fpart = compile ir2 cont in
        compile irb [OpSel(tpart, fpart)]

  (* ---- record ---- *)

  | IRRecord(keylst, irlst) ->
      compile_list irlst (OpMakeRecord(keylst) :: cont)

  | IRAccessField(ir1, fldnm) ->
      compile ir1 (OpAccessField(fldnm) :: cont)

  (* ---- imperatives ---- *)

  | IRLetMutableIn(var, irini, iraft) ->
      let bindop =
        match var with
        | GlobalVar(loc, evid, _)    -> OpBindLocationGlobal(loc, evid)
        | LocalVar(lv, off, evid, _) -> OpBindLocationLocal(lv, off, evid)
      in
        compile irini (bindop :: (compile iraft cont))

  | IRSequential(ir1, ir2) ->
      compile ir1 (OpPop :: compile ir2 cont)

  | IROverwrite(var, irnew) ->
      begin
        match var with
        | GlobalVar(loc, evid, _)    -> compile irnew (OpUpdateGlobal(loc, evid) :: cont)
        | LocalVar(lv, off, evid, _) -> compile irnew (OpUpdateLocal(lv, off, evid) :: cont)
      end

  | IRWhileDo(irb, irc) ->
      let cond = compile irb [OpBranchIfNot(OpPush(UnitConstant) :: cont)] in
      let body = compile irc cond in
        cond @ body

  (* ---- others ---- *)

  | IRPatternMatch(rng, irobj, patbrs) ->
      compile irobj @@ compile_patsel rng patbrs cont

  | IRNonValueConstructor(constrnm, ircont) ->
      compile ircont (OpMakeConstructor(constrnm) :: cont)

  | IRModule(irmdl, iraft) ->
      compile irmdl @@ compile iraft cont

  | IRPath(irpt0, pathcomplst, cycleopt) ->
      let (pathelemlst, closingopt) = compile_path pathcomplst cycleopt in
        compile irpt0 (OpPath(pathelemlst, closingopt) :: cont)


and compile_patsel (rng : Range.t) (patbrs : ir_pattern_branch list) (cont : instruction list) : instruction list =
  let consif cond a b =
    if cond then a :: b else b
  in
  let rec iter patbrsrev next n =
    match patbrsrev with
    | [] ->
        next

    | IRPatternBranch(pat, irto) :: tail ->
      let rest = compile irto cont in
      let body = consif (n <> 0) OpPop rest in
      let patchk = compile_patcheck pat next body in
        iter tail (consif (n <> 0) OpDup patchk) (n + 1)

    | IRPatternBranchWhen(pat, ircond, irto) :: tail ->
      let rest = compile irto cont in
      let body = consif (n <> 0) OpPop rest in
      let cond = compile ircond (OpBranchIfNot(next) :: body) in
      let patchk = compile_patcheck pat next cond in
        iter tail (consif (n <> 0) OpDup patchk) (n + 1)
  in
    iter (List.rev patbrs) [OpError("no matches (" ^ (Range.to_string rng) ^ ")")] 0


and compile_patlist (patlist : ir_pattern_tree list) (cont : instruction list) : instruction list =
  let next = [OpError("no matches")] in
  let rec iter patlist cont =
    match patlist with
    | []             -> cont
    | pat :: pattail -> iter pattail (compile_patcheck pat next cont)
  in
    (* -- right-to-left binding -- *)
    iter patlist cont


and compile_patcheck (pat : ir_pattern_tree) (next : instruction list) (cont : instruction list) : instruction list =
  let return inst = inst :: cont in
    match pat with
    | IRPIntegerConstant(pnc) -> return (OpCheckStackTopInt(pnc, next))
    | IRPBooleanConstant(pbc) -> return (OpCheckStackTopBool(pbc, next))
    | IRPStringConstant(str)  -> return (OpCheckStackTopStr(str, next))
    | IRPUnitConstant         -> return OpPop
    | IRPWildCard             -> return OpPop

    | IRPVariable(var) ->
        begin
          match var with
          | GlobalVar(loc, evid, refs)    -> return (OpBindGlobal(loc, evid, !refs))
          | LocalVar(lv, off, evid, refs) -> return (OpBindLocal(lv, off, evid, !refs))
        end

    | IRPAsVariable(var, psub) ->
        let bindop =
          match var with
          | GlobalVar(loc, evid, refs)    -> OpBindGlobal(loc, evid, !refs)
          | LocalVar(lv, off, evid, refs) -> OpBindLocal(lv, off, evid, !refs)
        in
        let code = compile_patcheck psub next cont in
          OpDup :: bindop :: code

    | IRPEndOfList -> return (OpCheckStackTopEndOfList(next))

    | IRPListCons(phd, ptl) ->
        let ctl = compile_patcheck ptl next cont in
        let chd = compile_patcheck phd (OpPop :: next) ctl in
          OpCheckStackTopListCons(next) :: chd

    | IRPEndOfTuple -> return OpPop

    | IRPTupleCons(phd, ptl) ->
        let ctl = compile_patcheck ptl next cont in
        let chd = compile_patcheck phd (OpPop :: next) ctl in
          OpCheckStackTopTupleCons(next) :: chd

    | IRPConstructor(cnm1, psub) ->
        let code = compile_patcheck psub next cont in
          OpCheckStackTopCtor(cnm1, next) :: code


and optimize_func_prologue code =
  let rec collect_bind code acc =
    match code with
    | (OpBindLocal(_, _, _, _) as hd) :: rest -> collect_bind rest (hd :: acc)
    | _                                       -> (acc, code)
  in
  let rec collect_load code acc =
    match code with
    | (OpLoadLocal(_, _, _, _) as hd) :: rest -> collect_load rest (hd :: acc)
    | _                                       -> (List.rev acc, code)
  in
  let rec consume bindopsrev loadops n =
    match (bindopsrev, loadops) with
    | (OpBindLocal(_, _, bid, 1) :: bs, OpLoadLocal(_, _, lid, 1) :: ls)
        when EvalVarID.equal bid lid ->
      (*Format.printf "omitted\n";*)
          consume bs ls (n + 1)

    | _ -> (List.rev_append bindopsrev loadops, n)
  in
  let (bindopsrev, rest) = collect_bind code [] in
  let (loadops, rest) = collect_load rest [] in
  (*Format.printf "bind/load = %d/%d\n" (List.length bindops) (List.length loadops);*)
  let (aftcode, n) = consume bindopsrev loadops 0 in
    (aftcode @ rest, n)
