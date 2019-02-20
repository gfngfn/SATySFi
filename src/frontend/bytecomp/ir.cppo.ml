
open MyUtil
open LengthInterface
open Types


let report_bug_ir msg =
  Format.printf "[Bug]@ %s:" msg;
  failwith ("bug: " ^ msg)

let report_bug_ir_ast msg ast =
  Format.printf "[Bug]@ %s:" msg;
  Format.printf "%a" pp_abstract_tree ast;
  failwith ("bug: " ^ msg)


type frame = {
  global : environment;
  vars   : varloc EvalVarIDMap.t;
  level  : int;
  size   : int;
}


let map_with_env f env lst =
  let rec iter env lst acc =
    match lst with
    | [] ->
        (Alist.to_list acc, env)

    | x :: xs ->
        let (r, envnew) = f env x in
          iter envnew xs (Alist.extend acc r)
  in
    iter env lst Alist.empty


let rec transform_input_horz_content (env : frame) (ihlst : input_horz_element list) : ir_input_horz_element list * frame =
  ihlst @|> env @|> map_with_env (fun env elem ->
    match elem with
    | InputHorzText(s) ->
        (IRInputHorzText(s), env)

    | InputHorzEmbedded(astabs) ->
        let (irabs, env) = transform env astabs in
          (IRInputHorzEmbedded(irabs), env)

    | InputHorzEmbeddedMath(astmath) ->
        let (irmath, env) = transform env astmath in
          (IRInputHorzEmbeddedMath(irmath), env)

    | InputHorzContent(ast) ->
        let (ir, env) = transform env ast in
          (IRInputHorzContent(ir), env)
  )


and transform_input_vert_content (env : frame) (ivlst : input_vert_element list) : ir_input_vert_element list * frame =
  ivlst @|> env @|> map_with_env (fun env elem ->
    match elem with
    | InputVertEmbedded(astabs) ->
        let (irabs, env) = transform env astabs in
          (IRInputVertEmbedded(irabs), env)

    | InputVertContent(ast) ->
        let (ir, env) = transform env ast in
          (IRInputVertContent(ir), env)
    )


and transform_path env pathcomplst cycleopt =
  let (irpathcomplst, env) =
    pathcomplst @|> env @|> map_with_env (fun env path ->
      match path with
      | PathLineTo(astpt) ->
          let (pt, env) = transform env astpt in
            (IRPathLineTo(pt), env)

      | PathCubicBezierTo(astpt1, astpt2, astpt) ->
          let (pt1, env) = transform env astpt1 in
          let (pt2, env) = transform env astpt2 in
          let (pt, env) = transform env astpt in
            (IRPathCubicBezierTo(pt1, pt2, pt), env)
    )
  in
  let (ircycleopt, env) =
    match cycleopt with
    | None ->
        (None, env)

    | Some(PathLineTo(())) ->
        (Some(IRPathLineTo(())), env)

    | Some(PathCubicBezierTo(astpt1, astpt2, ())) ->
        let (pt1, env) = transform env astpt1 in
        let (pt2, env) = transform env astpt2 in
          (Some(IRPathCubicBezierTo(pt1, pt2, ())), env)
  in
    (irpathcomplst, ircycleopt, env)


and transform_ast (env : environment) (ast : abstract_tree) : ir * environment =
  let (genv, _) = env in
  let initvars =
    EvalVarIDMap.fold (fun k v acc ->
      EvalVarIDMap.add k (GlobalVar(v, k, ref 0)) acc
    ) genv EvalVarIDMap.empty
  in
  let initframe = { global = env; vars = initvars; level = 0; size = 0; } in
  let (ir, frame) = transform initframe ast in
    (ir, frame.global)


and transform_list (env : frame) (astlst : abstract_tree list) : ir list * frame =
  map_with_env transform env astlst


and transform_primitive (env : frame) (astlst : abstract_tree list) (op : instruction) : ir * frame =
  let (irargs, env) = transform_list env astlst in
    (IRApplyPrimitive(op, List.length astlst, irargs), env)


and transform_patsel (env : frame) (patbrs : pattern_branch list) : ir_pattern_branch list * frame =
  let before_size = env.size in
  let max_size = ref before_size in
  let (irpatsel, envnew) =
    patbrs @|> env @|> map_with_env (fun env patbr ->
      match patbr with
      | PatternBranch(pat, astto) ->
          let env = { env with size = before_size; } in
          let (irpat, env) = transform_pattern env pat in
          let (irto, env) = transform env astto in
          max_size := max !max_size env.size;
            (IRPatternBranch(irpat, irto), env)

      | PatternBranchWhen(pat, astcond, astto) ->
          let env = { env with size = before_size; } in
          let (irpat, env) = transform_pattern env pat in
          let (ircond, env) = transform env astcond in
          let (irto, env) = transform env astto in
          max_size := max !max_size env.size;
            (IRPatternBranchWhen(irpat, ircond, irto), env)
    )
  in
    (irpatsel, { envnew with size = !max_size; })


and transform_pattern_list (env : frame) (patlst : pattern_tree list) : ir_pattern_tree list * frame =
  map_with_env transform_pattern env patlst


and transform_pattern (env : frame) (pat : pattern_tree) : ir_pattern_tree * frame =
  let return b = (b, env) in
    match pat with
    | PIntegerConstant(pnc) -> return (IRPIntegerConstant(pnc))
    | PBooleanConstant(pbc) -> return (IRPBooleanConstant(pbc))
    | PStringConstant(psc)  -> return (IRPStringConstant(psc))
    | PUnitConstant         -> return IRPUnitConstant
    | PWildCard             -> return IRPWildCard

    | PVariable(evid) ->
        let (var, env) = add_to_environment env evid  in
        (IRPVariable(var), env)

    | PAsVariable(evid, psub) ->
        let (var, env) = add_to_environment env evid  in
        let (bsub, env) = transform_pattern env psub in
        (IRPAsVariable(var, bsub), env)

    | PEndOfList -> return IRPEndOfList

    | PListCons(phd, ptl) ->
        let (bhd, envhd) = transform_pattern env phd in
        let (btl, envtl) = transform_pattern envhd ptl in
        (IRPListCons(bhd, btl), envtl)

    | PTuple(pats) ->
        let (bacc, env) =
          List.fold_left (fun (bacc, env) pat ->
            let (b, env) = transform_pattern env pat in
            (Alist.extend bacc b, env)
          ) (Alist.empty, env) pats
        in
        let irp =
          List.fold_right (fun b irp -> IRPTupleCons(b, irp)) (Alist.to_list bacc) IRPEndOfTuple
        in
        (irp, env)

    | PConstructor(cnm1, psub) ->
        let (bsub, env) = transform_pattern env psub in
          (IRPConstructor(cnm1, bsub), env)


and new_level (env : frame) =
  { env with level = env.level + 1; size = 0; }


and add_to_environment (env : frame) (evid : EvalVarID.t) : varloc * frame =
  let (var, newglobal) =
    if env.level = 0 then
      let loc = ref Nil in
        (GlobalVar(loc, evid, ref 0), Types.add_to_environment env.global evid loc)
    else
      (LocalVar(env.level, env.size, evid, ref 0), env.global)
  in
  let locvar =
    match var with
    | LocalVar(lv, off, id, refs) -> LocalVar(env.level - lv, off, id, refs)
    | GlobalVar(_, _, _)          -> var
  in
  let newvars = env.vars |> EvalVarIDMap.add evid var in
    (locvar, { env with global = newglobal; vars = newvars; size = env.size + 1; })


and find_in_environment (env : frame) (evid : EvalVarID.t) : varloc option =
  match env.vars |> EvalVarIDMap.find_opt evid with
  | Some(LocalVar(lv, off, id, refs)) -> incr refs; Some(LocalVar(env.level - lv, off, id, refs))
  | Some(GlobalVar(_, _, refs)) as v  -> incr refs; v
  | None                              -> None


and add_letrec_bindings_to_environment (env : frame) (recbinds : letrec_binding list) : (varloc * pattern_branch) list * frame =
  recbinds @|> env @|> map_with_env (fun env recbind ->
    let LetRecBinding(evid, patbr) = recbind in
    let (var, env) = add_to_environment env evid in
      ((var, patbr), env)
  )


and flatten_function (astfun : abstract_tree) : abstract_tree * pattern_tree list =
  let rec iter ast acc =
    match ast with
    | Function([], PatternBranch(pat, body)) -> iter body (Alist.extend acc pat)
    | _                                      -> (ast, Alist.to_list acc)
  in
    iter astfun Alist.empty


and flatten_application apast =
  let rec iter ast acc =
    match ast with
    | Apply(ast1, ast2) -> iter ast1 (ast2 :: acc)
    | _                 -> (ast, acc)
  in
    iter apast []

(*
and flatten_tuple asttup =
  match asttup with
  | PrimitiveTuple(astlst) -> astlst
  | _                      ->
  let rec iter ast acc =
    match ast with
    | PrimitiveTupleCons(hd, Value(EndOfTuple)) -> Alist.to_list (Alist.extend acc hd)
    | PrimitiveTupleCons(hd, tl)                -> iter tl (Alist.extend acc hd)
    | _                                         -> report_bug_ir_ast "malformed tuple!" asttup
  in
    iter asttup Alist.empty
*)

and transform_tuple env astlst =
  let (iritems, envnew) = map_with_env transform env astlst in
  let len = List.length astlst in
    (IRTuple(len, iritems), envnew)


and check_primitive (env : frame) (ast : abstract_tree) : (int * (abstract_tree list -> abstract_tree)) option =
  match ast with
  | ContentOf(_, evid) ->
      begin
        match Types.find_in_environment env.global evid with
        | Some(rfvalue) ->
            begin
              match !rfvalue with
              | CompiledPrimitiveWithEnvironment(arity, _, _, _, _, astf) -> Some((arity, astf))
              | _                                                         -> None
            end

        | None -> None
      end

  | _ -> None


and transform (env : frame) (ast : abstract_tree) : ir * frame =
  let return ir = (ir, env) in
    match ast with
    | Value(v) ->
        return (IRConstant(v))

    | FinishHeaderFile ->
        return IRTerminal

    | FinishStruct ->
        return IRTerminal

    | InputHorz(ihlst) ->
        let (imihlst, env) = transform_input_horz_content env ihlst in
          (IRInputHorz(imihlst), env)
        (* -- lazy evaluation; evaluates embedded variables only -- *)

    | InputVert(ivlst) ->
        let (imivlst, env) = transform_input_vert_content env ivlst in
          (IRInputVert(imivlst), env)
        (* -- lazy evaluation; evaluates embedded variables only -- *)

    | LengthDescription(flt, unitnm) ->
        let len =
          match unitnm with  (* temporary; ad-hoc handling of unit names *)
          | "pt"   -> Length.of_pdf_point flt
          | "cm"   -> Length.of_centimeter flt
          | "mm"   -> Length.of_millimeter flt
          | "inch" -> Length.of_inch flt
          | _      -> report_bug_ir_ast "LengthDescription; unknown unit name" ast
        in
          return (IRConstant(LengthConstant(len)))

    | BackendMathList(astmlst) ->
        transform_primitive env astmlst (OpBackendMathList(List.length astmlst))

    | Path(astpt0, pathcomplst, cycleopt) ->
        let (irpt0, env) = transform env astpt0 in
        let (pathelemlst, closingopt, env) = transform_path env pathcomplst cycleopt in
          (IRPath(irpt0, pathelemlst, closingopt), env)

    | PrimitiveTuple(astlst) ->
        transform_tuple env astlst

    (* -- fundamentals -- *)

    | ContentOf(rng, evid) ->
        begin
          match find_in_environment env evid with
          | Some(var) ->
              return (IRContentOf(var))

          | None ->
              report_bug_ir_ast ("ContentOf: variable '" ^ (EvalVarID.show_direct evid) ^ "' (at " ^ (Range.to_string rng) ^ ") not found") ast
        end

    | LetRecIn(recbinds, ast2) ->
        let (pairs, env) = add_letrec_bindings_to_environment env recbinds in
        let varir_lst =
          pairs |> List.map (fun pair ->
            let (var, patbr) = pair in
            let (ir, _) = transform env (Function([], patbr)) in
              (var, ir)
          )
        in
        let (ir2, env) = transform env ast2 in
          (IRLetRecIn(varir_lst, ir2), env)

    | LetNonRecIn(pat, ast1, ast2) ->
        let (ir1, env) = transform env ast1 in
        let (irpat, env) = transform_pattern env pat in
        let (ir2, env) = transform env ast2 in
          (IRLetNonRecIn(ir1, irpat, ir2), env)

    | Function([], _) ->
        let (body, args) = flatten_function ast in
        let funenv = new_level env in
        let (irargs, funenv) = transform_pattern_list funenv args in
        let (irbody, funenv) = transform funenv body in
          (IRFunction(funenv.size, [], irargs, irbody), env)

    | Function((_ :: _) as evids, PatternBranch(arg, body)) ->
        let funenv = new_level env in
        let (optvars, funenv) = map_with_env add_to_environment funenv evids in
        let (irarg, funenv) = transform_pattern funenv arg in
        let (irbody, funenv) = transform funenv body in
          (IRFunction(funenv.size, optvars, [irarg], irbody), env)

    | Function(_, PatternBranchWhen(_, _, _)) ->
        assert false

    | Apply(_, _) ->
        let (astcallee, astargs) = flatten_application ast in
          begin
            match check_primitive env astcallee with
            | Some((arity, astf))  when arity = List.length astargs ->
                transform env (astf astargs)

            | _ ->
                let (ircallee, env) = transform env astcallee in
                let (irargs, env) = transform_list env astargs in
                  (IRApply(List.length irargs, ircallee, irargs), env)
            end

    | ApplyOptional(ast1, ast2) ->
        let (ir1, env) = transform env ast1 in
        let (ir2, env) = transform env ast2 in
          (IRApplyOptional(ir1, ir2), env)

    | ApplyOmission(ast1) ->
        let (ir1, env) = transform env ast1 in
          (IRApplyOmission(ir1), env)

    | IfThenElse(astb, ast1, ast2) ->
        let (irb, env) = transform env astb in
        let before_size = env.size in
        let (ir1, env) = transform env ast1 in
        let ast1_size = env.size in
        let (ir2, env) = transform { env with size = before_size; } ast2 in
        let ast2_size = env.size in
          (IRIfThenElse(irb, ir1, ir2), { env with size = max ast1_size ast2_size; })

    (* ---- record ---- *)

    | Record(asc) ->
        let iter acc key ast =
          let (keylst, irlst, env) = acc in
          let (ir, env) = transform env ast in
            (key :: keylst, ir :: irlst, env)
        in
        let (keylst, irlst, env) = Assoc.fold iter ([], [], env) asc in
          (IRRecord(List.rev keylst, List.rev irlst), env)

    | AccessField(ast1, fldnm) ->
        let (ir1, env) = transform env ast1 in
          (IRAccessField(ir1, fldnm), env)

    | UpdateField(ast1, fldnm, ast2) ->
        let (ir1, env) = transform env ast1 in
        let (ir2, env) = transform env ast2 in
        (IRUpdateField(ir1, fldnm, ir2), env)

    (* ---- imperatives ---- *)

    | LetMutableIn(evid, astini, astaft) ->
        let (irini, env) = transform env astini in
        let (var, env) = add_to_environment env evid in
        let (iraft, env) = transform env astaft in
          (IRLetMutableIn(var, irini, iraft), env)

    | Sequential(ast1, ast2) ->
        let (ir1, env) = transform env ast1 in
        let (ir2, env) = transform env ast2 in
          (IRSequential(ir1, ir2), env)

    | Overwrite(evid, astnew) ->
        begin
          match find_in_environment env evid with
          | Some(var) ->
              let (irnew, env) = transform env astnew in
                (IROverwrite(var, irnew), env)

          | None ->
              report_bug_ir_ast ("Overwrite: mutable value '" ^ (EvalVarID.show_direct evid) ^ "' not found") ast
        end

    | Dereference(ast1) ->
        let (ir1, env) = transform env ast1 in
        (IRDereference(ir1), env)

    | WhileDo(astb, astc) ->
        let (irb, env) = transform env astb in
        let (irc, env) = transform env astc in
          (IRWhileDo(irb, irc), env)

    (* ---- others ---- *)

    | PatternMatch(rng, astobj, patbrs) ->
        let (irobj, env) = transform env astobj in
        let (irpatsel, env) = transform_patsel env patbrs in
          (IRPatternMatch(rng, irobj, irpatsel), env)

    | NonValueConstructor(constrnm, astcont) ->
        let (ircont, env) = transform env astcont in
          (IRNonValueConstructor(constrnm, ircont), env)

    | Module(astmdl, astaft) ->
        let (irmdl, env) = transform env astmdl in
        let (iraft, env) = transform env astaft in
          (IRModule(irmdl, iraft), env)

  (* -- staging construct -- *)

    | Prev(_) ->
        report_bug_ir "Prev(...) cannot occur at transform_ir"

    | Next(_) ->
        report_bug_ir "Next(...) cannot occur at transform_ir"

#include "__ir.gen.ml"
