
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


let map_with_env (type a) (type b) (f : frame -> a -> b * frame) (env : frame) (lst : a list) : b list * frame =
  let rec iter env lst acc =
    match lst with
    | [] ->
        (Alist.to_list acc, env)

    | x :: xs ->
        let (r, envnew) = f env x in
        iter envnew xs (Alist.extend acc r)
  in
  iter env lst Alist.empty


let rec transform_0_input_horz_content (env : frame) (ihlst : input_horz_element list) : ir_input_horz_element list * frame =
  ihlst @|> env @|> map_with_env (fun env elem ->
    match elem with
    | InputHorzText(s) ->
        (IRInputHorzText(s), env)

    | InputHorzEmbedded(astabs) ->
        let (irabs, env) = transform_0 env astabs in
        (IRInputHorzEmbedded(irabs), env)

    | InputHorzEmbeddedMath(astmath) ->
        let (irmath, env) = transform_0 env astmath in
        (IRInputHorzEmbeddedMath(irmath), env)

    | InputHorzContent(ast) ->
        let (ir, env) = transform_0 env ast in
        (IRInputHorzContent(ir), env)
  )


and transform_1_input_horz_content (env : frame) (ihlst : input_horz_element list) : (ir input_horz_element_scheme) list * frame =
  ihlst @|> env @|> map_with_env (fun env elem ->
    match elem with
    | InputHorzText(s) ->
        (InputHorzText(s), env)

    | InputHorzEmbedded(astabs) ->
        let (irabs, env) = transform_1 env astabs in
        (InputHorzEmbedded(irabs), env)

    | InputHorzEmbeddedMath(astmath) ->
        let (irmath, env) = transform_1 env astmath in
        (InputHorzEmbeddedMath(irmath), env)

    | InputHorzContent(ast) ->
        let (ir, env) = transform_1 env ast in
        (InputHorzContent(ir), env)
  )


and transform_0_input_vert_content (env : frame) (ivlst : input_vert_element list) : ir_input_vert_element list * frame =
  ivlst @|> env @|> map_with_env (fun env elem ->
    match elem with
    | InputVertEmbedded(astabs) ->
        let (irabs, env) = transform_0 env astabs in
        (IRInputVertEmbedded(irabs), env)

    | InputVertContent(ast) ->
        let (ir, env) = transform_0 env ast in
        (IRInputVertContent(ir), env)
    )


and transform_1_input_vert_content (env : frame) (ivlst : input_vert_element list) : (ir input_vert_element_scheme) list * frame =
  ivlst @|> env @|> map_with_env (fun env elem ->
    match elem with
    | InputVertEmbedded(astabs) ->
        let (irabs, env) = transform_1 env astabs in
        (InputVertEmbedded(irabs), env)

    | InputVertContent(ast) ->
        let (ir, env) = transform_1 env ast in
        (InputVertContent(ir), env)
  )


and transform_0_path env pathcomplst cycleopt =
  let (irpathcomplst, env) =
    pathcomplst @|> env @|> map_with_env (fun env path ->
      match path with
      | PathLineTo(astpt) ->
          let (pt, env) = transform_0 env astpt in
          (IRPathLineTo(pt), env)

      | PathCubicBezierTo(astpt1, astpt2, astpt) ->
          let (pt1, env) = transform_0 env astpt1 in
          let (pt2, env) = transform_0 env astpt2 in
          let (pt, env) = transform_0 env astpt in
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
        let (pt1, env) = transform_0 env astpt1 in
        let (pt2, env) = transform_0 env astpt2 in
        (Some(IRPathCubicBezierTo(pt1, pt2, ())), env)
  in
  (irpathcomplst, ircycleopt, env)


and transform_ast_0 (env : environment) (ast : abstract_tree) : ir * environment =
  let (genv, _) = env in
  let initvars =
    EvalVarIDMap.fold (fun k v acc ->
      EvalVarIDMap.add k (GlobalVar(v, k, ref 0)) acc
    ) genv EvalVarIDMap.empty
  in
  let initframe = { global = env; vars = initvars; level = 0; size = 0; } in
  let (ir, frame) = transform_0 initframe ast in
  (ir, frame.global)


and transform_ast_1 (env : environment) (ast : abstract_tree) : ir * environment =
  let (genv, _) = env in
  let initvars =
    EvalVarIDMap.fold (fun k v acc ->
      EvalVarIDMap.add k (GlobalVar(v, k, ref 0)) acc
    ) genv EvalVarIDMap.empty
  in
  let initframe = { global = env; vars = initvars; level = 0; size = 0; } in
  let (ir, frame) = transform_1 initframe ast in
  (ir, frame.global)


and transform_0_list (env : frame) (astlst : abstract_tree list) : ir list * frame =
  map_with_env transform_0 env astlst


and transform_0_primitive (env : frame) (astlst : abstract_tree list) (op : instruction) : ir * frame =
  let (irargs, env) = transform_0_list env astlst in
  (IRApplyPrimitive(op, List.length astlst, irargs), env)


and transform_1_list (env : frame) (astlst : abstract_tree list) : ir list * frame =
  map_with_env transform_1 env astlst


and transform_1_primitive (env : frame) (astlst : abstract_tree list) (op : instruction) : ir * frame =
  let (irargs, env) = transform_1_list env astlst in
  (IRApplyPrimitive(op, List.length astlst, irargs), env)

(*
and transform_code1 (env : frame) (astlst : abstract_tree list) (codef : code_value list -> code_value) : ir * frame =
  let (irargs, env) = transform_list env astlst in
  (IRCodeCombinator(codef, List.length astlst, irargs), env)
*)

and transform_0_patsel (env : frame) (patbrs : pattern_branch list) : ir_pattern_branch list * frame =
  let before_size = env.size in
  let max_size = ref before_size in
  let (irpatsel, envnew) =
    patbrs @|> env @|> map_with_env (fun env patbr ->
      match patbr with
      | PatternBranch(pat, astto) ->
          let env = { env with size = before_size; } in
          let (irpat, env) = transform_pattern env pat in
          let (irto, env) = transform_0 env astto in
          max_size := max !max_size env.size;
          (IRPatternBranch(irpat, irto), env)

      | PatternBranchWhen(pat, astcond, astto) ->
          let env = { env with size = before_size; } in
          let (irpat, env) = transform_pattern env pat in
          let (ircond, env) = transform_0 env astcond in
          let (irto, env) = transform_0 env astto in
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


and transform_0_tuple env astlst =
  let (iritems, envnew) = map_with_env transform_0 env astlst in
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
              | CompiledPrimitiveClosure(arity, _, _, _, _, astf) -> Some((arity, astf))
              | _                                                 -> None
            end

        | None ->
            None
      end

  | _ ->
      None

(*
and transform_1_horz_input ih : frame =
  let (acc, env) =
    ih |> List.fold_left (fun (acc, env) ihelem ->
      let (ir, env) =
        match ihelem with
        | InputHorzText(s)       -> (InputHorzText(s), env)
        | InputHorzEmbedded(ast) -> let (code, env) = transform_1 env ast in (InputHorzEmbedded(code))
      in
      (Alist.extend acc code, env)
    ) (Alist.empty, env)
  in
  (Alist.to_list acc, env)
*)

and code0 env cv =
  (IRCodeCombinator((fun _ -> cv), 0, []), env)

and code1 env cvf ast1 =
  let (ir1, env) = transform_1 env ast1 in
  let codef = function [cv1] -> cvf cv1 | _ -> report_bug_ir "code1" in
  (IRCodeCombinator(codef, 1, [ir1]), env)

and code2 env cvf ast1 ast2 =
  let (ir1, env) = transform_1 env ast1 in
  let (ir2, env) = transform_1 env ast2 in
  let codef = function [cv1; cv2] -> cvf cv1 cv2 | _ -> report_bug_ir "code2" in
  (IRCodeCombinator(codef, 2, [ir1; ir2]), env)

and code3 env cvf ast1 ast2 ast3 =
  let (ir1, env) = transform_1 env ast1 in
  let (ir2, env) = transform_1 env ast2 in
  let (ir3, env) = transform_1 env ast3 in
  let codef = function [cv1; cv2; cv3] -> cvf cv1 cv2 cv3 | _ -> report_bug_ir "code3" in
  (IRCodeCombinator(codef, 3, [ir1; ir2; ir3]), env)

and code4 env cvf ast1 ast2 ast3 ast4 =
  let (ir1, env) = transform_1 env ast1 in
  let (ir2, env) = transform_1 env ast2 in
  let (ir3, env) = transform_1 env ast3 in
  let (ir4, env) = transform_1 env ast4 in
  let codef = function [cv1; cv2; cv3; cv4] -> cvf cv1 cv2 cv3 cv4 | _ -> report_bug_ir "code4" in
  (IRCodeCombinator(codef, 4, [ir1; ir2; ir3; ir4]), env)

and code5 env cvf ast1 ast2 ast3 ast4 ast5 =
  let (ir1, env) = transform_1 env ast1 in
  let (ir2, env) = transform_1 env ast2 in
  let (ir3, env) = transform_1 env ast3 in
  let (ir4, env) = transform_1 env ast4 in
  let (ir5, env) = transform_1 env ast5 in
  let codef = function [cv1; cv2; cv3; cv4; cv5] -> cvf cv1 cv2 cv3 cv4 cv5 | _ -> report_bug_ir "code5" in
  (IRCodeCombinator(codef, 5, [ir1; ir2; ir3; ir4; ir5]), env)

and code6 env cvf ast1 ast2 ast3 ast4 ast5 ast6 =
  let (ir1, env) = transform_1 env ast1 in
  let (ir2, env) = transform_1 env ast2 in
  let (ir3, env) = transform_1 env ast3 in
  let (ir4, env) = transform_1 env ast4 in
  let (ir5, env) = transform_1 env ast5 in
  let (ir6, env) = transform_1 env ast6 in
  let codef = function [cv1; cv2; cv3; cv4; cv5; cv6] -> cvf cv1 cv2 cv3 cv4 cv5 cv6 | _ -> report_bug_ir "code6" in
  (IRCodeCombinator(codef, 6, [ir1; ir2; ir3; ir4; ir5; ir6]), env)


and code7 env cvf ast1 ast2 ast3 ast4 ast5 ast6 ast7 =
  let (ir1, env) = transform_1 env ast1 in
  let (ir2, env) = transform_1 env ast2 in
  let (ir3, env) = transform_1 env ast3 in
  let (ir4, env) = transform_1 env ast4 in
  let (ir5, env) = transform_1 env ast5 in
  let (ir6, env) = transform_1 env ast6 in
  let (ir7, env) = transform_1 env ast7 in
  let codef = function [cv1; cv2; cv3; cv4; cv5; cv6; cv7] -> cvf cv1 cv2 cv3 cv4 cv5 cv6 cv7 | _ -> report_bug_ir "code7" in
  (IRCodeCombinator(codef, 7, [ir1; ir2; ir3; ir4; ir5; ir6; ir7]), env)


and transform_1_pattern_branch (env : frame) (patbr : pattern_branch) : ir_pattern_branch * frame =
  match patbr with
  | PatternBranch(pat, ast1) ->
      let (irpat, env) = transform_pattern env pat in
      let (ir1, env) = transform_1 env ast1 in
      (IRPatternBranch(irpat, ir1), env)

  | PatternBranchWhen(pat, ast, ast1) ->
      let (irpat, env) = transform_pattern env pat in
      let (ir, env) = transform_1 env ast in
      let (ir1, env) = transform_1 env ast1 in
      (IRPatternBranchWhen(irpat, ir, ir1), env)


and transform_1 (env : frame) (ast : abstract_tree) : ir * frame =
  match ast with
  | ASTBaseConstant(bc) -> code0 env (CdBaseConstant(bc))
  | ASTEndOfList        -> code0 env CdEndOfList
  | ASTMath(mlst)       -> code0 env (CdMath(mlst))

  | FinishHeaderFile ->
      (IRCodeFinishHeaderFile, env)

  | FinishStruct ->
      (IRCodeFinishStruct, env)

  | InputHorz(ihlst) ->
      let (imihlst, env) = transform_1_input_horz_content env ihlst in
      (IRCodeInputHorz(imihlst), env)

  | InputVert(ivlst) ->
      let (imivlst, env) = transform_1_input_vert_content env ivlst in
      (IRCodeInputVert(imivlst), env)

  | Path(_) ->
      remains_to_be_implemented "transform_1: Path(_)"

  | Record(asc) ->
      let iter acc key ast =
        let (keyacc, iracc, env) = acc in
        let (ir, env) = transform_1 env ast in
        (Alist.extend keyacc key, Alist.extend iracc ir, env)
      in
      let (keyacc, iracc, env) = Assoc.fold iter (Alist.empty, Alist.empty, env) asc in
      (IRCodeRecord(Alist.to_list keyacc, Alist.to_list iracc), env)

  | AccessField(ast1, fldnm) ->
      code1 env (fun cv -> CdAccessField(cv, fldnm)) ast1

  | UpdateField(ast1, fldnm, ast2) ->
      code2 env (fun cv1 cv2 -> CdUpdateField(cv1, fldnm, cv2)) ast1 ast2

  | LetRecIn(recbinds, ast2) ->
      let (pairacc, env) =
        recbinds |> List.fold_left (fun (pairacc, env) recbind ->
          match recbind with
          | LetRecBinding(evid, _) ->
              let (var, env) = add_to_environment env evid in
              (Alist.extend pairacc (var, recbind), env)
        ) (Alist.empty, env)
      in
      let (irrecbinds, env) =
        pairacc |> Alist.to_list |> map_with_env (fun env (var, LetRecBinding(_, patbr)) ->
          let (irpatbrs, env) = transform_1_pattern_branch env patbr in
          (IRLetRecBinding(var, irpatbrs), env)
        ) env
      in
      let (ir2, env) = transform_1 env ast2 in
      (IRCodeLetRecIn(irrecbinds, ir2), env)

  | LetNonRecIn(pat, ast1, ast2) ->
      let (irpat, env) = transform_pattern env pat in
      let (ir1, env) = transform_1 env ast1 in
      let (ir2, env) = transform_1 env ast2 in
      (IRCodeLetNonRecIn(irpat, ir1, ir2), env)

  | ContentOf(rng, evid) ->
      begin
        match find_in_environment env evid with
        | Some(var) -> (IRSymbolOf(var), env)
        | None      -> failwith (Format.asprintf "%a not found" EvalVarID.pp evid)
      end

  | Persistent(rng, evid) ->
      begin
        match find_in_environment env evid with
        | Some(var) -> (IRPersistent(var), env)
        | None      -> assert false
      end

  | IfThenElse(ast0, ast1, ast2) ->
      code3 env (fun cv0 cv1 cv2 -> CdIfThenElse(cv0, cv1, cv2)) ast0 ast1 ast2

  | Function(evids, PatternBranch(pat, ast1)) ->
      let (optvaracc, env) =
        evids |> List.fold_left (fun (optvaracc, env) evid ->
          let (var, env) = add_to_environment env evid in
          (Alist.extend optvaracc var, env)
        ) (Alist.empty, env)
      in
      let (irpat, env) = transform_pattern env pat in
      let (ir1, env) = transform_1 env ast1 in
      (IRCodeFunction(Alist.to_list optvaracc, irpat, ir1), env)

  | Function(_, PatternBranchWhen(_, _, _)) ->
      assert false

  | Apply(ast1, ast2) ->
      code2 env (fun cv1 cv2 -> CdApply(cv1, cv2)) ast1 ast2

  | ApplyOptional(ast1, ast2) ->
      code2 env (fun cv1 cv2 -> CdApplyOptional(cv1, cv2)) ast1 ast2

  | ApplyOmission(ast1) ->
      code1 env (fun cv -> CdApplyOmission(cv)) ast1

  | PatternMatch(rng, ast1, patbrs) ->
      let (ir1, env) = transform_1 env ast1 in
      let (irpatbrs, env) = map_with_env transform_1_pattern_branch env patbrs in
      (IRCodePatternMatch(rng, ir1, irpatbrs), env)

  | NonValueConstructor(constrnm, ast1) ->
      code1 env (fun cv -> CdConstructor(constrnm, cv)) ast1

  | LetMutableIn(evid, ast1, ast2) ->
      let (var, env) = add_to_environment env evid in
      let (ir1, env) = transform_1 env ast1 in
      let (ir2, env) = transform_1 env ast2 in
      (IRCodeLetMutableIn(var, ir1, ir2), env)

  | Dereference(ast1) ->
      code1 env (fun cv -> CdDereference(cv)) ast1

  | Sequential(ast1, ast2) ->
      code2 env (fun cv1 cv2 -> CdSequential(cv1, cv2)) ast1 ast2

  | WhileDo(ast1, ast2) ->
      code2 env (fun cv1 cv2 -> CdWhileDo(cv1, cv2)) ast1 ast2

  | Overwrite(evid, ast1) ->
      begin
        match find_in_environment env evid with
        | Some(var) ->
            let (ir1, env) = transform_1 env ast1 in
            (IRCodeOverwrite(var, ir1), env)

        | None ->
            assert false
      end

  | Module(ast1, ast2) ->
      let (ir1, env) = transform_1 env ast1 in
      let (ir2, env) = transform_1 env ast2 in
      (IRCodeModule(ir1, ir2), env)

  | BackendMathList(astmlst) ->
      transform_1_primitive env astmlst (OpCodeMathList(List.length astmlst))

  | PrimitiveTuple(astlst) ->
      transform_1_primitive env astlst (OpCodeMakeTuple(List.length astlst))

  | Prev(ast1) ->
      transform_0 env ast1

  | Next(_) ->
      report_bug_ir "transform_1: Next at stage 1"

#include "__ir_1.gen.ml"


and transform_0 (env : frame) (ast : abstract_tree) : ir * frame =
  let return ir = (ir, env) in
  match ast with
  | ASTBaseConstant(bc) ->
      return (IRConstant(BaseConstant(bc)))

  | ASTMath(mlst) ->
      return (IRConstant(MathValue(mlst)))

  | ASTEndOfList ->
      return (IRConstant(List([])))

  | FinishHeaderFile ->
      return IRTerminal

  | FinishStruct ->
      return IRTerminal

  | InputHorz(ihlst) ->
      let (imihlst, env) = transform_0_input_horz_content env ihlst in
      (IRInputHorz(imihlst), env)
        (* -- lazy evaluation; evaluates embedded variables only -- *)

  | InputVert(ivlst) ->
      let (imivlst, env) = transform_0_input_vert_content env ivlst in
      (IRInputVert(imivlst), env)
        (* -- lazy evaluation; evaluates embedded variables only -- *)

  | BackendMathList(astmlst) ->
      transform_0_primitive env astmlst (OpBackendMathList(List.length astmlst))

  | Path(astpt0, pathcomplst, cycleopt) ->
      let (irpt0, env) = transform_0 env astpt0 in
      let (pathelemlst, closingopt, env) = transform_0_path env pathcomplst cycleopt in
        (IRPath(irpt0, pathelemlst, closingopt), env)

  | PrimitiveTuple(astlst) ->
      transform_0_tuple env astlst

(* -- fundamentals -- *)

  | ContentOf(rng, evid)
  | Persistent(rng, evid) ->
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
          let (ir, _) = transform_0 env (Function([], patbr)) in
          (var, ir)
        )
      in
      let (ir2, env) = transform_0 env ast2 in
      (IRLetRecIn(varir_lst, ir2), env)

  | LetNonRecIn(pat, ast1, ast2) ->
      let (ir1, env) = transform_0 env ast1 in
      let (irpat, env) = transform_pattern env pat in
      let (ir2, env) = transform_0 env ast2 in
      (IRLetNonRecIn(ir1, irpat, ir2), env)

  | Function([], _) ->
      let (body, args) = flatten_function ast in
      let funenv = new_level env in
      let (irargs, funenv) = transform_pattern_list funenv args in
      let (irbody, funenv) = transform_0 funenv body in
      (IRFunction(funenv.size, [], irargs, irbody), env)

  | Function((_ :: _) as evids, PatternBranch(arg, body)) ->
      let funenv = new_level env in
      let (optvars, funenv) = map_with_env add_to_environment funenv evids in
      let (irarg, funenv) = transform_pattern funenv arg in
      let (irbody, funenv) = transform_0 funenv body in
      (IRFunction(funenv.size, optvars, [irarg], irbody), env)

  | Function(_, PatternBranchWhen(_, _, _)) ->
      assert false

  | Apply(_, _) ->
      let (astcallee, astargs) = flatten_application ast in
        begin
          match check_primitive env astcallee with
          | Some((arity, astf))  when arity = List.length astargs ->
              transform_0 env (astf astargs)

          | _ ->
              let (ircallee, env) = transform_0 env astcallee in
              let (irargs, env) = transform_0_list env astargs in
              (IRApply(List.length irargs, ircallee, irargs), env)
        end

  | ApplyOptional(ast1, ast2) ->
      let (ir1, env) = transform_0 env ast1 in
      let (ir2, env) = transform_0 env ast2 in
      (IRApplyOptional(ir1, ir2), env)

  | ApplyOmission(ast1) ->
      let (ir1, env) = transform_0 env ast1 in
      (IRApplyOmission(ir1), env)

  | IfThenElse(astb, ast1, ast2) ->
      let (irb, env) = transform_0 env astb in
      let before_size = env.size in
      let (ir1, env) = transform_0 env ast1 in
      let ast1_size = env.size in
      let (ir2, env) = transform_0 { env with size = before_size; } ast2 in
      let ast2_size = env.size in
      (IRIfThenElse(irb, ir1, ir2), { env with size = max ast1_size ast2_size; })

(* ---- record ---- *)

  | Record(asc) ->
      let iter acc key ast =
        let (keyacc, iracc, env) = acc in
        let (ir, env) = transform_0 env ast in
        (Alist.extend keyacc key, Alist.extend iracc ir, env)
      in
      let (keyacc, iracc, env) = Assoc.fold iter (Alist.empty, Alist.empty, env) asc in
      (IRRecord(Alist.to_list keyacc, Alist.to_list iracc), env)

  | AccessField(ast1, fldnm) ->
      let (ir1, env) = transform_0 env ast1 in
      (IRAccessField(ir1, fldnm), env)

  | UpdateField(ast1, fldnm, ast2) ->
      let (ir1, env) = transform_0 env ast1 in
      let (ir2, env) = transform_0 env ast2 in
      (IRUpdateField(ir1, fldnm, ir2), env)

(* ---- imperatives ---- *)

  | LetMutableIn(evid, astini, astaft) ->
      let (irini, env) = transform_0 env astini in
      let (var, env) = add_to_environment env evid in
      let (iraft, env) = transform_0 env astaft in
      (IRLetMutableIn(var, irini, iraft), env)

  | Sequential(ast1, ast2) ->
      let (ir1, env) = transform_0 env ast1 in
      let (ir2, env) = transform_0 env ast2 in
      (IRSequential(ir1, ir2), env)

  | Overwrite(evid, astnew) ->
      begin
        match find_in_environment env evid with
        | Some(var) ->
            let (irnew, env) = transform_0 env astnew in
            (IROverwrite(var, irnew), env)

        | None ->
            report_bug_ir_ast ("Overwrite: mutable value '" ^ (EvalVarID.show_direct evid) ^ "' not found") ast
      end

  | Dereference(ast1) ->
      let (ir1, env) = transform_0 env ast1 in
      (IRDereference(ir1), env)

  | WhileDo(astb, astc) ->
      let (irb, env) = transform_0 env astb in
      let (irc, env) = transform_0 env astc in
      (IRWhileDo(irb, irc), env)

(* ---- others ---- *)

  | PatternMatch(rng, astobj, patbrs) ->
      let (irobj, env) = transform_0 env astobj in
      let (irpatsel, env) = transform_0_patsel env patbrs in
      (IRPatternMatch(rng, irobj, irpatsel), env)

  | NonValueConstructor(constrnm, astcont) ->
      let (ircont, env) = transform_0 env astcont in
      (IRNonValueConstructor(constrnm, ircont), env)

  | Module(astmdl, astaft) ->
      let (irmdl, env) = transform_0 env astmdl in
      let (iraft, env) = transform_0 env astaft in
      (IRModule(irmdl, iraft), env)

(* -- staging construct -- *)

  | Prev(_) ->
      report_bug_ir "Prev(...) cannot occur at transform_ir"

  | Next(ast1) ->
      transform_1 env ast1

#include "__ir_0.gen.ml"
