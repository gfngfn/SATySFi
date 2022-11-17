
open MyUtil
open SyntaxBase
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


let rec transform_0_inline_text_content (_env : frame) (_its : inline_text_element list) : ir_inline_text_element list * frame =
  failwith "TODO: transform_0_inline_text_content"
(*
  ihlst @|> env @|> map_with_env (fun env elem ->
    match elem with
    | InlineTextText(s) ->
        (IRInlineTextText(s), env)

    | InlineTextEmbedded(astabs) ->
        let (irabs, env) = transform_0 env astabs in
        (IRInlineTextEmbedded(irabs), env)

    | InlineTextEmbeddedMath(astmath) ->
        let (irmath, env) = transform_0 env astmath in
        (IRInlineTextEmbeddedMath(irmath), env)

    | InlineTextEmbeddedCodeText(s) ->
        (IRInlineTextEmbeddedCodeText(s), env)

    | InlineTextContent(ast) ->
        let (ir, env) = transform_0 env ast in
        (IRInlineTextContent(ir), env)
  )
*)

and transform_1_inline_text_content (_env : frame) (_its : inline_text_element list) : (ir inline_text_element_scheme) list * frame =
  failwith "TODO: transform_1_inline_text_content"
(*
  ihlst @|> env @|> map_with_env (fun env elem ->
    match elem with
    | InlineTextText(s) ->
        (InlineTextText(s), env)

    | InlineTextEmbedded(astabs) ->
        let (irabs, env) = transform_1 env astabs in
        (InlineTextEmbedded(irabs), env)

    | InlineTextEmbeddedMath(astmath) ->
        let (irmath, env) = transform_1 env astmath in
        (InlineTextEmbeddedMath(irmath), env)

    | InlineTextEmbeddedCodeText(s) ->
        (InlineTextEmbeddedCodeText(s), env)

    | InlineTextContent(ast) ->
        let (ir, env) = transform_1 env ast in
        (InlineTextContent(ir), env)
  )
*)


and transform_0_block_text_content (_env : frame) (_bts : block_text_element list) : ir_block_text_element list * frame =
  failwith "TODO: transform_0_block_text_content"
(*
  ivlst @|> env @|> map_with_env (fun env elem ->
    match elem with
    | BlockTextEmbedded(astabs) ->
        let (irabs, env) = transform_0 env astabs in
        (IRBlockTextEmbedded(irabs), env)

    | BlockTextContent(ast) ->
        let (ir, env) = transform_0 env ast in
        (IRBlockTextContent(ir), env)
    )
*)


and transform_1_block_text_content (_env : frame) (_bts : block_text_element list) : (ir block_text_element_scheme) list * frame =
  failwith "TODO: transform_1_block_text_content"
(*
  ivlst @|> env @|> map_with_env (fun env elem ->
    match elem with
    | BlockTextEmbedded(astabs) ->
        let (irabs, env) = transform_1 env astabs in
        (BlockTextEmbedded(irabs), env)

    | BlockTextContent(ast) ->
        let (ir, env) = transform_1 env ast in
        (BlockTextContent(ir), env)
  )
*)


and transform_0_math_text_content (_env : frame) (_ims : math_text_element list) : ir_math_text_element list * frame =
  failwith "TODO: transform_0_math_text_content"


and transform_1_math_text_content (_env : frame) (_ims : math_text_element list) : (ir math_text_element_scheme) list * frame =
  failwith "TODO: transform_1_math_text_content"


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
        pats |> TupleList.to_list |> List.fold_left (fun (bacc, env) pat ->
          let (b, env) = transform_pattern env pat in
          (Alist.extend bacc b, env)
        ) (Alist.empty, env)
      in
      let bs =
        match Alist.to_list bacc with
        | b1 :: b2 :: brest -> TupleList.make b1 b2 brest
        | _                 -> assert false
      in
      (IRPTuple(bs), env)

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
    | Function(evid_labmap, PatternBranch(pat, body)) ->
        if LabelMap.cardinal evid_labmap = 0 then
          iter body (Alist.extend acc pat)
        else
          (ast, Alist.to_list acc)

    | _ ->
        (ast, Alist.to_list acc)
  in
  iter astfun Alist.empty


and flatten_application apast =
  let rec iter ast acc =
    match ast with
    | Apply(ast_labmap, ast1, ast2) ->
        if LabelMap.cardinal ast_labmap = 0 then
          iter ast1 (ast2 :: acc)
        else
          (ast, acc)

    | _ ->
        (ast, acc)
  in
  iter apast []


and transform_0_tuple env (asts : abstract_tree TupleList.t) =
  let asts = asts |> TupleList.to_list in
  let (iritems, envnew) = map_with_env transform_0 env asts in
  let len = List.length asts in
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
        | InlineTextText(s)       -> (InlineTextText(s), env)
        | InlineTextEmbedded(ast) -> let (code, env) = transform_1 env ast in (InlineTextEmbedded(code))
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

  | InlineText(ihlst) ->
      let (imihlst, env) = transform_1_inline_text_content env ihlst in
      (IRCodeInlineText(imihlst), env)

  | BlockText(ivlst) ->
      let (imivlst, env) = transform_1_block_text_content env ivlst in
      (IRCodeBlockText(imivlst), env)

  | MathText(ims) ->
      let (imims, env) = transform_1_math_text_content env ims in
      (IRCodeMathText(imims), env)

  | LambdaInline(_, _) ->
      failwith "TODO: transform_1, LambdaInline"

  | LambdaBlock(_, _) ->
      failwith "TODO: transform_1, LambdaBlock"

  | LambdaMath(_, _, _) ->
      failwith "TODO: transform_1, LambdaMath"

  | Record(asc) ->
      let (keyacc, iracc, env) =
        LabelMap.fold (fun key ast acc ->
          let (keyacc, iracc, env) = acc in
          let (ir, env) = transform_1 env ast in
          (Alist.extend keyacc key, Alist.extend iracc ir, env)
        ) asc (Alist.empty, Alist.empty, env)
      in
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

  | ContentOf(_rng, evid) ->
      begin
        match find_in_environment env evid with
        | Some(var) -> (IRSymbolOf(var), env)
        | None      -> failwith (Format.asprintf "%a not found" EvalVarID.pp evid)
      end

  | IfThenElse(ast0, ast1, ast2) ->
      code3 env (fun cv0 cv1 cv2 -> CdIfThenElse(cv0, cv1, cv2)) ast0 ast1 ast2

  | Function(evid_labmap, PatternBranch(pat, ast1)) ->
      let (varloc_labmap, env) =
        LabelMap.fold (fun label evid (varloc_labmap, env) ->
          let (var, env) = add_to_environment env evid in
          (varloc_labmap |> LabelMap.add label var, env)
        ) evid_labmap (LabelMap.empty, env)
      in
      let (irpat, env) = transform_pattern env pat in
      let (ir1, env) = transform_1 env ast1 in
      (IRCodeFunction(varloc_labmap, irpat, ir1), env)

  | Function(_, PatternBranchWhen(_, _, _)) ->
      assert false

  | Apply(_ast_labmap, _ast1, _ast2) ->
      failwith "TODO (enhance): Ir, Apply"
(*
      code2 env (fun cv1 cv2 -> CdApply(cv1, cv2)) ast1 ast2
*)

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

  | Overwrite(evid, ast1) ->
      begin
        match find_in_environment env evid with
        | Some(var) ->
            let (ir1, env) = transform_1 env ast1 in
            (IRCodeOverwrite(var, ir1), env)

        | None ->
            assert false
      end

  | PrimitiveTuple(asts) ->
      let asts = asts |> TupleList.to_list in
      transform_1_primitive env asts (OpCodeMakeTuple(List.length asts))

  | Prev(ast1) ->
      transform_0 env ast1

  | Next(_) ->
      report_bug_ir "transform_1: Next at stage 1"

  | Persistent(_, _) ->
      failwith "TODO: Persistent"

  | Lift(_) ->
      report_bug_ir "transform_1: Lift at stage 1"

  | ASTCodeSymbol(_symb) ->
      report_bug_ir "transform_1: ASTCodeSymbol at stage 1"

  | LoadSingleFont(_) ->
      failwith "TODO: LoadSingleFont"

  | LoadCollectionFont(_) ->
      failwith "TODO: LoadCollectionFont"

  | CatchTest(_) ->
      assert false (* Cannot occur in input expressions. *)

#include "__ir_1.gen.ml"


and transform_0 (env : frame) (ast : abstract_tree) : ir * frame =
  let return ir = (ir, env) in
  match ast with
  | ASTBaseConstant(bc) ->
      return (IRConstant(BaseConstant(bc)))

  | ASTEndOfList ->
      return (IRConstant(List([])))

  | InlineText(ihlst) ->
      let (imihlst, env) = transform_0_inline_text_content env ihlst in
      (IRInlineText(imihlst), env)

  | BlockText(ivlst) ->
      let (imivlst, env) = transform_0_block_text_content env ivlst in
      (IRBlockText(imivlst), env)

  | MathText(ims) ->
      let (imims, env) = transform_0_math_text_content env ims in
      (IRMathText(imims), env)

  | LambdaInline(_, _) ->
      failwith "TODO: transform_0, LambdaInline"

  | LambdaBlock(_, _) ->
      failwith "TODO: transform_0, LambdaBlock"

  | LambdaMath(_, _, _) ->
      failwith "TODO: transform_0, LambdaMath"

  | PrimitiveTuple(asts) ->
      transform_0_tuple env asts
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
          let (ir, _) = transform_0 env (Function(LabelMap.empty, patbr)) in
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

  | Function(evid_labmap, PatternBranch(arg, body)) ->
      if LabelMap.cardinal evid_labmap = 0 then
        let (body, args) = flatten_function ast in
        let funenv = new_level env in
        let (irargs, funenv) = transform_pattern_list funenv args in
        let (irbody, funenv) = transform_0 funenv body in
        (IRFunction(funenv.size, LabelMap.empty, irargs, irbody), env)
      else
        let funenv = new_level env in
        let (varloc_labmap, funenv) =
          LabelMap.fold (fun label evid (varloc_labmap, funenv) ->
            let (varloc, funenv) = add_to_environment funenv evid in
            (varloc_labmap |> LabelMap.add label varloc, funenv)
          ) evid_labmap (LabelMap.empty, funenv)
        in
        let (irarg, funenv) = transform_pattern funenv arg in
        let (irbody, funenv) = transform_0 funenv body in
        (IRFunction(funenv.size, varloc_labmap, [irarg], irbody), env)

  | Function(_, PatternBranchWhen(_, _, _)) ->
      assert false

  | Apply(ast_labmap, _, _) ->
      if LabelMap.cardinal ast_labmap = 0 then
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
      else
        failwith "TODO (enhance): Ir, Apply"

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
      let (keyacc, iracc, env) =
        LabelMap.fold (fun key ast acc ->
          let (keyacc, iracc, env) = acc in
          let (ir, env) = transform_0 env ast in
          (Alist.extend keyacc key, Alist.extend iracc ir, env)
        ) asc (Alist.empty, Alist.empty, env)
      in
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

(* ---- others ---- *)

  | PatternMatch(rng, astobj, patbrs) ->
      let (irobj, env) = transform_0 env astobj in
      let (irpatsel, env) = transform_0_patsel env patbrs in
      (IRPatternMatch(rng, irobj, irpatsel), env)

  | NonValueConstructor(constrnm, astcont) ->
      let (ircont, env) = transform_0 env astcont in
      (IRNonValueConstructor(constrnm, ircont), env)

(* -- staging construct -- *)

  | Prev(_) ->
      report_bug_ir "Prev(...) cannot occur at transform_1"

  | Next(ast1) ->
      transform_1 env ast1

  | Persistent(_) ->
      report_bug_ir "Persistent(...) cannot occur at transform_1"

  | Lift(ast1) ->
      let (ir1, env) = transform_0 env ast1 in
      (IRLift(ir1), env)

  | ASTCodeSymbol(symb) ->
      return (IRConstant(CodeSymbol(symb)))

  | LoadSingleFont(_) ->
      failwith "TODO: LoadSingleFont"

  | LoadCollectionFont(_) ->
      failwith "TODO: LoadCollectionFont"

  | CatchTest(_) ->
      assert false (* Cannot occur in input expressions. *)

#include "__ir_0.gen.ml"
