
open MyUtil
open LengthInterface
open Types
open EvalUtil


let report_bug_compiler msg =
  Format.printf "[Bug]@ %s:" msg;
  failwith ("bug: " ^ msg)


let report_bug_compiler_ast msg ast =
  Format.printf "[Bug]@ %s:" msg;
  Format.printf "%a" pp_abstract_tree ast;
  failwith ("bug: " ^ msg)


let make_loading_op (var : varloc) : instruction =
  match var with
  | GlobalVar(loc, evid, refs) ->
      if !loc = Nil then
        OpLoadGlobal(loc, evid, !refs)
      else
        OpPush(!loc)

  | LocalVar(lv, off, evid, refs) ->
      OpLoadLocal(lv, off, evid, !refs)


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
        CompiledInputHorzEmbedded(compiled)

    | IRInputHorzEmbeddedMath(irmath) ->
        CompiledInputHorzEmbeddedMath(compile irmath [])

    | IRInputHorzEmbeddedCodeText(s) ->
        CompiledInputHorzEmbeddedCodeText(s)

    | IRInputHorzContent(ir) ->
        CompiledInputHorzContent(compile ir [])
    )
  in
  compiled_ihlist


and compile_code_input_horz (irihlst : (ir input_horz_element_scheme) list) =
  irihlst |> List.map (function
  | InputHorzText(s) ->
      InputHorzText(s)

  | InputHorzEmbedded(irabs) ->
      let compiled = compile irabs [] in
      InputHorzEmbedded(compiled)

  | InputHorzEmbeddedMath(irmath) ->
      let compiled = compile irmath [] in
      InputHorzEmbeddedMath(compiled)

  | InputHorzEmbeddedCodeText(s) ->
      InputHorzEmbeddedCodeText(s)

  | InputHorzContent(ir) ->
      let compiled = compile ir [] in
      InputHorzContent(compiled)
  )


and compile_input_vert_content (ivlst : ir_input_vert_element list) =
  let compiled_ivlist =
    ivlst |> List.map (function
    | IRInputVertEmbedded(irabs) ->
        let compiled = compile irabs [] in
        CompiledInputVertEmbedded(compiled)

    | IRInputVertContent(ir) ->
        CompiledInputVertContent(compile ir [])
    )
  in
  compiled_ivlist


and compile_code_input_vert (irivlst : (ir input_vert_element_scheme) list) =
  irivlst |> List.map (function
  | InputVertEmbedded(irabs) ->
      let compiled = compile irabs [] in
      InputVertEmbedded(compiled)

  | InputVertContent(ir) ->
      let compiled = compile ir [] in
      InputVertContent(compiled)
  )


and compile_code_pattern_branch (irpatbr : ir_pattern_branch) : (instruction list) ir_pattern_branch_scheme =
  match irpatbr with
  | IRPatternBranch(irpat, ir1) ->
      let compiled1 = compile ir1 [] in
      IRPatternBranch(irpat, compiled1)

  | IRPatternBranchWhen(irpat, ir, ir1) ->
      let compiled = compile ir [] in
      let compiled1 = compile ir1 [] in
      IRPatternBranchWhen(irpat, compiled, compiled1)


and compile_code_letrec_binding (IRLetRecBinding(var, irpatbr) : ir_letrec_binding) : (instruction list) ir_letrec_binding_scheme =
  let comppatbr = compile_code_pattern_branch irpatbr in
  IRLetRecBinding(var, comppatbr)


and compile (ir : ir) (cont : instruction list) =
  match ir with
  (* ---- basic value ---- *)

  | IRConstant(v) ->
      OpPush(v) :: cont

  | IRTerminal ->
      OpPushEnv :: cont

  | IRInputHorz(ihlst) ->
      OpClosureInputHorz(compile_input_horz_content ihlst) :: cont
    (* -- lazy evaluation; evaluates embedded variables only -- *)

  | IRInputVert(ivlst) ->
      OpClosureInputVert(compile_input_vert_content ivlst) :: cont
    (* -- lazy evaluation; evaluates embedded variables only -- *)

  (* -- fundamentals -- *)

  | IRContentOf(var) ->
      let loadop = make_loading_op var in
      loadop :: cont

  | IRPersistent(var) ->
      let evid =
        match var with
        | GlobalVar(_, evid, _)   -> evid
        | LocalVar(_, _, evid, _) -> evid
      in
      let rng = Range.dummy "IRPersistent" in
      OpPush(CodeValue(CdPersistent(rng, evid))) :: cont

  | IRSymbolOf(var) ->
      let loadop = make_loading_op var in
      loadop :: OpConvertSymbolToCode :: cont

  | IRLetRecIn(recbinds, ast2) ->
      let binds =
        recbinds |> List.map (fun (var, ir) -> (var, compile ir []))
      in
      OpBindClosuresRec(binds) :: (compile ast2 cont)

  | IRLetNonRecIn(ir1, irpat, ir2) ->
      compile ir1 @@ compile_patsel (Range.dummy "LetNonRecIn") [IRPatternBranch(irpat, ir2)] cont

  | IRFunction(framesize, varloc_labmap, irpatlst, irbody) ->
      let body = compile irbody [] in
      let patlst = compile_patlist irpatlst body in
      let (optcode, n) = optimize_func_prologue patlst in
      if framesize - n = 0 then
        OpClosure(varloc_labmap, List.length irpatlst, 0, optcode) :: cont
      else
        OpClosure(varloc_labmap, List.length irpatlst, framesize, optcode) :: cont

  | IRApply(arity, ircallee, irargs) ->
      let n = List.length irargs in
      compile ircallee @@ (compile_list irargs @@ OpForward(n) :: emit_appop n cont false)

  | IRApplyPrimitive(op, arity, irargs) ->
      compile_list irargs (op :: cont)

  | IRApplyOptional(ircallee, iroptarg) ->
      compile ircallee @@ (compile iroptarg @@ OpApplyOptional :: cont)

  | IRApplyOmission(irabs) ->
      compile irabs @@ OpApplyOmission :: cont

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

  | IRUpdateField(ir1, fldnm, ir2) ->
      compile ir1 (compile ir2 @@ OpUpdateField(fldnm) :: cont)

  (* ---- imperatives ---- *)

  | IRLetMutableIn(var, irini, iraft) ->
      let bindop =
        match var with
        | GlobalVar(loc, evid, _)    -> OpBindLocationGlobal(loc, evid)
        | LocalVar(lv, off, evid, _) -> OpBindLocationLocal(lv, off, evid)
      in
      compile irini (bindop :: (compile iraft cont))

  | IROverwrite(var, irnew) ->
      begin
        match var with
        | GlobalVar(loc, evid, _)    -> compile irnew (OpUpdateGlobal(loc, evid) :: cont)
        | LocalVar(lv, off, evid, _) -> compile irnew (OpUpdateLocal(lv, off, evid) :: cont)
      end

  | IRDereference(ir1) ->
      compile ir1 @@ OpDereference :: cont

  (* ---- others ---- *)

  | IRPatternMatch(rng, irobj, patbrs) ->
      compile irobj @@ compile_patsel rng patbrs cont

  | IRNonValueConstructor(constrnm, ircont) ->
      compile ircont (OpMakeConstructor(constrnm) :: cont)

  | IRModule(irmdl, iraft) ->
      compile irmdl @@ compile iraft cont

(* -- multi-stage -- *)

  | IRCodeCombinator(codef, arity, irargs) ->
      compile_list irargs (OpApplyCodeCombinator(codef, arity) :: cont)

  | IRCodeRecord(keylst, irargs) ->
      compile_list irargs (OpCodeMakeRecord(keylst) :: cont)

  | IRCodeInputHorz(ihlst) ->
      OpCodeMakeInputHorz(compile_code_input_horz ihlst) :: cont

  | IRCodeInputVert(ivlst) ->
      OpCodeMakeInputVert(compile_code_input_vert ivlst) :: cont

  | IRCodePatternMatch(rng, ir, irpatbrs) ->
      compile ir @@ OpCodePatternMatch(rng, List.map compile_code_pattern_branch irpatbrs) :: cont

  | IRCodeLetRecIn(irrecbinds, ir2) ->
      let instrs2 = compile ir2 [] in
      OpCodeLetRec(List.map compile_code_letrec_binding irrecbinds, instrs2) :: cont

  | IRCodeLetNonRecIn(irpat, ir1, ir2) ->
      let instrs1 = compile ir1 [] in
      let instrs2 = compile ir2 [] in
      OpCodeLetNonRec(irpat, instrs1, instrs2) :: cont

  | IRCodeFunction(varloc_labmap, irpat, ir1) ->
      let instrs1 = compile ir1 [] in
      OpCodeFunction(varloc_labmap, irpat, instrs1) :: cont

  | IRCodeLetMutableIn(var, ir1, ir2) ->
      let instrs1 = compile ir1 [] in
      let instrs2 = compile ir2 [] in
      OpCodeLetMutable(var, instrs1, instrs2) :: cont

  | IRCodeOverwrite(var, ir1) ->
      let loadop = make_loading_op var in
      let instrs1 = compile ir1 [] in
      loadop :: OpCodeOverwrite(instrs1) :: cont

  | IRCodeModule(ir1, ir2) ->
      let instrs1 = compile ir1 [] in
      let instrs2 = compile ir2 [] in
      OpCodeModule(instrs1, instrs2) :: cont

  | IRCodeFinishHeaderFile ->
      OpCodeFinishHeaderFile :: cont

  | IRCodeFinishStruct ->
      OpCodeFinishStruct :: cont


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

    | IRPEndOfList ->
        return (OpCheckStackTopEndOfList(next))

    | IRPListCons(phd, ptl) ->
        let ctl = compile_patcheck ptl next cont in
        let chd = compile_patcheck phd (OpPop :: next) ctl in
        OpCheckStackTopListCons(next) :: chd

    | IRPTuple(ps) ->
        let rec aux ps next cont =
          match ps with
          | [] ->
              return OpPop

          | phd :: ptl ->
              let ctl = aux ptl next cont in
              let chd = compile_patcheck phd (OpPop :: next) ctl in
              OpCheckStackTopTupleCons(next) :: chd
        in
        aux (ps |> TupleList.to_list) next cont

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
    | (OpLoadLocal(_, _, _, _) as hd) :: rest -> collect_load rest (Alist.extend acc hd)
    | _                                       -> (Alist.to_list acc, code)
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
  let (loadops, rest) = collect_load rest Alist.empty in
  (*Format.printf "bind/load = %d/%d\n" (List.length bindops) (List.length loadops);*)
  let (aftcode, n) = consume bindopsrev loadops 0 in
  (aftcode @ rest, n)
