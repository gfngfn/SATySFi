
open MyUtil
open LengthInterface
open Types

exception EvalError of string

type eval_input_horz_element =
  | EvInputHorzText     of string
  | EvInputHorzEmbedded of abstract_tree * abstract_tree list


let report_bug_evaluator msg =
  failwith msg


let rec make_argument_cons lst =
  match lst with
  | []           -> EndOfArgumentVariable
  | head :: tail -> ArgumentVariableCons(head, make_argument_cons tail)


let copy_environment (env : environment) = Hashtbl.copy env

let add_to_environment (env : environment) (evid : EvalVarID.t) (rfast : abstract_tree ref) = Hashtbl.add env evid rfast

let find_in_environment (env : environment) (evid : EvalVarID.t) = Hashtbl.find env evid


let lex_horz_text (ctx : HorzBox.input_context) (s_utf8 : string) : HorzBox.horz_box list =
  let uchlst = InternalText.to_uchar_list (InternalText.of_utf8 s_utf8) in
    HorzBox.([HorzPure(PHCInnerString(ctx, uchlst))])


let make_line_stack hblstlst =
  let open HorzBox in
  let wid =
    hblstlst |> List.fold_left (fun widacc hblst ->
      let (wid, _, _) = LineBreak.get_natural_metrics hblst in
       Length.max wid widacc
    ) Length.zero
  in
  let trilst = hblstlst |> List.map (fun hblst -> LineBreak.fit hblst wid) in
  let evvblst =
    trilst |> List.fold_left (fun evvbacc (evhblst, hgt, dpt) ->
      EvVertLine(hgt, dpt, evhblst) :: evvbacc
    ) [] |> List.rev
  in
    (wid, evvblst)


let adjust_to_first_line evvblst =
  let open HorzBox in
  let rec aux optinit totalhgtinit evvblst =
    evvblst |> List.fold_left (fun (opt, totalhgt) evvb ->
      match (evvb, opt) with
      | (EvVertLine(hgt, dpt, _), None)          -> (Some(totalhgt +% hgt), totalhgt +% hgt +% (Length.negate dpt))
      | (EvVertLine(hgt, dpt, _), _)             -> (opt, totalhgt +% hgt +% (Length.negate dpt))
      | (EvVertFixedEmpty(vskip), _)             -> (opt, totalhgt +% vskip)
      | (EvVertFrame(pads, _, _, evvblstsub), _) ->
          let totalhgtbefore = totalhgt +% pads.paddingT in
          let (optsub, totalhgtsub) = aux opt totalhgtbefore evvblstsub in
          let totalhgtafter = totalhgtsub +% pads.paddingB in
            (optsub, totalhgtafter)
    ) (optinit, totalhgtinit)
  in
    match aux None Length.zero evvblst with
    | (Some(hgt), totalhgt) -> (hgt, Length.negate (totalhgt -% hgt))
    | (None, totalhgt)      -> (Length.zero, Length.negate totalhgt)


let adjust_to_last_line evvblst =
    let open HorzBox in
    let rec aux optinit totalhgtinit evvblst =
      let evvblstrev = List.rev evvblst in
        evvblstrev |> List.fold_left (fun (opt, totalhgt) evvblast ->
            match (evvblast, opt) with
            | (EvVertLine(hgt, dpt, _), None)          -> (Some((Length.negate totalhgt) +% dpt), totalhgt +% (Length.negate dpt) +% hgt)
            | (EvVertLine(hgt, dpt, _), _)             -> (opt, totalhgt +% (Length.negate dpt) +% hgt)
            | (EvVertFixedEmpty(vskip), _)             -> (opt, totalhgt +% vskip)
            | (EvVertFrame(pads, _, _, evvblstsub), _) ->
                let totalhgtbefore = totalhgt +% pads.paddingB in
                let (optsub, totalhgtsub) = aux opt totalhgtbefore evvblstsub in
                let totalhgtafter = totalhgtsub +% pads.paddingT in
                  (optsub, totalhgtafter)
        ) (optinit, totalhgtinit)
    in
      match aux None Length.zero evvblst with
      | (Some(dpt), totalhgt) -> (totalhgt +% dpt, dpt)
      | (None, totalhgt)      -> (totalhgt, Length.zero)


let interpret_list interpretf (env : environment) extractf (ast : abstract_tree) =
  let rec aux acc value =
    match value with
    | ListCons(vhead, vtail) -> aux ((extractf vhead) :: acc) vtail
    | EndOfList              -> List.rev acc
    | _                      -> report_bug_evaluator "interpret_list"
  in
  let value = interpretf env ast in
    aux [] value


let rec reduce_beta envf evid valuel astdef =
  let envnew = copy_environment envf in
    begin
      add_to_environment envnew evid (ref valuel);
      interpret envnew astdef
    end


and reduce_beta_list env valuef astarglst =
  match astarglst with
  | []                   -> valuef
  | astarg :: astargtail ->
      begin
        match valuef with
        | FuncWithEnvironment(evid, astdef, envf) ->
            let valuearg = interpret env astarg in
            let valuefnew = reduce_beta envf evid valuearg astdef in
            reduce_beta_list env valuefnew astargtail

        | _ -> report_bug_evaluator "reduce_beta_list"
      end

(*
and interpret_horz_boxes env astrow =
  let valuerow = interpret env astrow in
  let rec aux value =
    match value with
    | Horz(hblst)                -> hblst
    | HorzConcat(value1, value2) -> List.append (aux value1) (aux value2)
    | _                          -> report_bug_evaluator ("interpret_horz_boxes; " ^ (Display.string_of_ast valuerow))
  in
    aux valuerow
*)

and interpret_vert env astvert =
  let valuevert = interpret env astvert in
    match valuevert with
    | Vert(imvblst) -> imvblst
    | _             -> report_bug_evaluator ("interpret_vert; " ^ (Display.string_of_ast valuevert))

and interpret_horz env asthorz =
  let valuehorz = interpret env asthorz in
    match valuehorz with
    | Horz(hblst) -> hblst
    | _           -> report_bug_evaluator ("interpret_horz; " ^ (Display.string_of_ast valuehorz))

and interpret_point env astpt =
  let valuept = interpret env astpt in
    match valuept with
    | TupleCons(LengthConstant(lenx), TupleCons(LengthConstant(leny), EndOfTuple)) -> (lenx, leny)
    | _ -> report_bug_evaluator ("interpret_point; " ^ (Display.string_of_ast valuept))


and interpret_prepath env astprepath =
  let valueprepath = interpret env astprepath in
    match valueprepath with
    | PrePathValue(prepath) -> prepath
    | _ -> report_bug_evaluator ("interpret_prepath; " ^ (Display.string_of_ast valueprepath))

and interpret_paddings env astpads =
  let valuepads = interpret env astpads in
  match valuepads with
  | TupleCons(LengthConstant(lenL),
      TupleCons(LengthConstant(lenR),
        TupleCons(LengthConstant(lenT),
          TupleCons(LengthConstant(lenB), EndOfTuple)))) ->
      {
        HorzBox.paddingL = lenL;
        HorzBox.paddingR = lenR;
        HorzBox.paddingT = lenT;
        HorzBox.paddingB = lenB;
      }

  | _ -> report_bug_evaluator ("interpret_paddings; " ^ (Display.string_of_ast valuepads))


and interpret_decoset env astdecoset =
  let valuedecoset = interpret env astdecoset in
  match valuedecoset with
  | TupleCons(valuedecoS,
      TupleCons(valuedecoH,
        TupleCons(valuedecoM,
          TupleCons(valuedecoT, EndOfTuple)))) ->
      (valuedecoS, valuedecoH, valuedecoM, valuedecoT)

  | _ -> report_bug_evaluator ("interpret_decoset; " ^ (Display.string_of_ast valuedecoset))


and interpret_path env pathcomplst cycleopt =
  let pathelemlst =
    pathcomplst |> List.map (function
      | PathLineTo(astpt) ->
          let pt = interpret_point env astpt in
            HorzBox.LineTo(pt)

      | PathCubicBezierTo(astpt1, astpt2, astpt) ->
          let pt1 = interpret_point env astpt1 in
          let pt2 = interpret_point env astpt2 in
          let pt = interpret_point env astpt in
            HorzBox.CubicBezierTo(pt1, pt2, pt)
    )
  in
  let closingopt =
    match cycleopt with
    | None -> None

    | Some(PathLineTo(())) -> Some(HorzBox.LineTo(()))

    | Some(PathCubicBezierTo(astpt1, astpt2, ())) ->
        let pt1 = interpret_point env astpt1 in
        let pt2 = interpret_point env astpt2 in
          Some(HorzBox.CubicBezierTo(pt1, pt2, ()))
  in
    (pathelemlst, closingopt)


and graphics_of_list valueg =
  let rec aux acc ast =
    match ast with
    | EndOfList                             -> List.rev acc
    | ListCons(GraphicsValue(pdfops), tail) -> aux (pdfops :: acc) tail
    | _                                     -> report_bug_evaluator ("make_frame_deco; "
                                                                     ^ (Display.string_of_ast ast))
  in
    List.concat (aux [] valueg)


and make_frame_deco env valuedeco =
  (fun (xpos, ypos) wid hgt dpt ->
    let valuepos = TupleCons(LengthConstant(xpos), TupleCons(LengthConstant(ypos), EndOfTuple)) in
    let valuewid = LengthConstant(wid) in
    let valuehgt = LengthConstant(hgt) in
    let valuedpt = LengthConstant(Length.negate dpt) in
      (* -- depth values for users are nonnegative -- *)
    let valueret = reduce_beta_list env valuedeco [valuepos; valuewid; valuehgt; valuedpt] in
      graphics_of_list valueret
  )

and make_paren env valueparenf : HorzBox.paren =
  (fun hgt dpt hgtaxis fontsize color ->
    let valuehgt      = LengthConstant(hgt) in
    let valuedpt      = LengthConstant(Length.negate dpt) in
      (* -- depth values for users are nonnegative -- *)
    let valuehgtaxis  = LengthConstant(hgtaxis) in
    let valuefontsize = LengthConstant(fontsize) in
    let valuecolor    = make_color_value color in
    let valueret = reduce_beta_list env valueparenf [valuehgt; valuedpt; valuehgtaxis; valuefontsize; valuecolor] in
      match valueret with
      | TupleCons(valuehorz, TupleCons(valuekernf, EndOfTuple)) ->
          let hblst = interpret_horz env valuehorz in
          let kernf = make_math_kern_func env valuekernf in
            (hblst, kernf)

      | _ ->
          report_bug_evaluator "make_paren"
  )

and make_math_kern_func env valuekernf : HorzBox.math_kern_func =
  (fun ypos ->
    let valueypos = LengthConstant(ypos) in
    let valueret = reduce_beta_list env valuekernf [valueypos] in
      interpret_length env valueret
  )

and make_math_char_kern_func env valuekernf : HorzBox.math_char_kern_func =
  (fun fontsize ypos ->
    let valuefontsize = LengthConstant(fontsize) in
    let valueypos = LengthConstant(ypos) in
    let valueret = reduce_beta_list env valuekernf [valuefontsize; valueypos] in
      interpret_length env valueret
  )

and make_inline_graphics env valueg =
  (fun (xpos, ypos) ->
    let valuepos = TupleCons(LengthConstant(xpos), TupleCons(LengthConstant(ypos), EndOfTuple)) in
    let valueret = reduce_beta_list env valueg [valuepos] in
      graphics_of_list valueret
  )


and interpret env ast =
  match ast with

(* ---- basic value ---- *)

  | StringEmpty                           -> ast
  | IntegerConstant(_)                    -> ast
  | FloatConstant(_)                      -> ast
  | StringConstant(_)                     -> ast
  | BooleanConstant(_)                    -> ast
  | UnitConstant                          -> ast
  | EvaluatedEnvironment(_)               -> ast
  | FuncWithEnvironment(_, _, _)          -> ast

  | InputHorz(ihlst)                      -> InputHorzWithEnvironment(ihlst, env)  (* -- lazy evaluation -- *)

  | InputHorzWithEnvironment(_, _)        -> ast

  | InputVert(ivlst)                      -> InputVertWithEnvironment(ivlst, env)  (* -- lazy evaluation -- *)

  | InputVertWithEnvironment(_, _)        -> ast

  | LengthDescription(flt, unitnm) ->
      let len =
        match unitnm with  (* temporary; ad-hoc handling of unit names *)
        | "pt"   -> Length.of_pdf_point flt
        | "cm"   -> Length.of_centimeter flt
        | "mm"   -> Length.of_millimeter flt
        | "inch" -> Length.of_inch flt
        | _      -> report_bug_evaluator "LengthDescription; unknown unit name"
      in
        LengthConstant(len)

  | LengthConstant(_) -> ast

  | Concat(ast1, ast2) ->
      let value1 = interpret env ast1 in
      let value2 = interpret env ast2 in
        begin
          match (value1, value2) with
          | (StringEmpty, _)                         -> value2
          | (_, StringEmpty)                         -> value1
          | (StringConstant(s1), StringConstant(s2)) -> StringConstant(s1 ^ s2)
          | _                                        -> report_bug_evaluator ("Concat: " ^ (Display.string_of_ast value1) ^ " and " ^ (Display.string_of_ast value2))
        end

(* ---- values for backend ---- *)

  | MathValue(_) -> ast

  | PrimitiveSetMathVariantToChar(asts, astmccls, astmathcls, astcp, astctx) ->
      let s = interpret_string env asts in
      let mccls = interpret_math_char_class env astmccls in
      let is_big = false in
      let mathcls = interpret_math_class env astmathcls in
      let cp = interpret_int env astcp in
      let ctx = interpret_context env astctx in
      let mvvalue = (mathcls, HorzBox.MathVariantToChar(is_big, Uchar.of_int cp)) in
      let mcclsmap = ctx.HorzBox.math_variant_char_map in
        Context(HorzBox.({ ctx with math_variant_char_map = mcclsmap |> MathVariantCharMap.add (s, mccls) mvvalue }))

  | BackendMathVariantCharDirect(astmathcls, astcp1, astcp2, astcp3, astcp4) ->   (* TEMPORARY; should extend more *)
      let mathcls = interpret_math_class env astmathcls in
      let is_big = false in
      let cp1 = interpret_int env astcp1 in  (* -- Italic -- *)
      let cp2 = interpret_int env astcp2 in  (* -- bold Italic -- *)
      let cp3 = interpret_int env astcp3 in  (* -- Roman -- *)
      let cp4 = interpret_int env astcp4 in  (* -- bold Roman -- *)
        MathValue(HorzBox.([MathPure(MathVariantCharDirect(mathcls, is_big,
          Uchar.of_int cp1, Uchar.of_int cp2, Uchar.of_int cp3, Uchar.of_int cp4))]))

  | BackendMathConcat(astm1, astm2) ->
      let mlst1 = interpret_math env astm1 in
      let mlst2 = interpret_math env astm2 in
        MathValue(List.append mlst1 mlst2)

  | BackendMathList(astmlst) ->
      let mlstlst = List.map (interpret_math env) astmlst in  (* slightly doubtful in terms of evaluation strategy *)
        MathValue(List.concat mlstlst)

  | BackendMathGroup(astmathcls1, astmathcls2, astm) ->
      let mathcls1 = interpret_math_class env astmathcls1 in
      let mathcls2 = interpret_math_class env astmathcls2 in
      let mlst = interpret_math env astm in
        MathValue([HorzBox.MathGroup(mathcls1, mathcls2, mlst)])

  | BackendMathSuperscript(astm1, astm2) ->
      let mlst1 = interpret_math env astm1 in
      let mlst2 = interpret_math env astm2 in
        MathValue([HorzBox.MathSuperscript(mlst1, mlst2)])

  | BackendMathSubscript(astm1, astm2) ->
      let mlst1 = interpret_math env astm1 in
      let mlst2 = interpret_math env astm2 in
        MathValue([HorzBox.MathSubscript(mlst1, mlst2)])

  | BackendMathFraction(astm1, astm2) ->
      let mlst1 = interpret_math env astm1 in
      let mlst2 = interpret_math env astm2 in
        MathValue([HorzBox.MathFraction(mlst1, mlst2)])

  | BackendMathRadical(astm1, astm2) ->
      let mlst1opt = interpret_option env (interpret_math env) astm1 in
      let mlst2 = interpret_math env astm2 in
      let radical = Primitives.default_radical in  (* temporary; should be variable *)
      begin
        match mlst1opt with
        | None        -> MathValue([HorzBox.MathRadical(radical, mlst2)])
        | Some(mlst1) -> MathValue([HorzBox.MathRadicalWithDegree(mlst1, mlst2)])
      end

  | BackendMathParen(astparenL, astparenR, astm1) ->
      let mlst1 = interpret_math env astm1 in
      let valueparenL = interpret env astparenL in
      let valueparenR = interpret env astparenR in
      let parenL = make_paren env valueparenL in
      let parenR = make_paren env valueparenR in
        MathValue([HorzBox.MathParen(parenL, parenR, mlst1)])

  | BackendMathUpperLimit(astm1, astm2) ->
      let mlst1 = interpret_math env astm1 in
      let mlst2 = interpret_math env astm2 in
        MathValue([HorzBox.MathUpperLimit(mlst1, mlst2)])

  | BackendMathLowerLimit(astm1, astm2) ->
      let mlst1 = interpret_math env astm1 in
      let mlst2 = interpret_math env astm2 in
        MathValue([HorzBox.MathLowerLimit(mlst1, mlst2)])

  | BackendMathChar(astmathcls, is_big, aststr) ->
      let mathcls = interpret_math_class env astmathcls in
      let s = interpret_string env aststr in
      let uchlst = (InternalText.to_uchar_list (InternalText.of_utf8 s)) in
      let mlst =
        uchlst |> List.map (fun uch ->
          HorzBox.(MathPure(MathElement(mathcls, MathChar(is_big, uch)))))
      in
        MathValue(mlst)

  | BackendMathCharWithKern(astmathcls, is_big, aststr, astkernfL, astkernfR) ->
      let mathcls = interpret_math_class env astmathcls in
      let s = interpret_string env aststr in
      let valuekernfL = interpret env astkernfL in
      let valuekernfR = interpret env astkernfR in
      let uchlst = (InternalText.to_uchar_list (InternalText.of_utf8 s)) in
      let kernfL = make_math_char_kern_func env valuekernfL in
      let kernfR = make_math_char_kern_func env valuekernfR in
      let kernf0 _ _ = Length.zero in
      let mlst =
        uchlst |> list_fold_adjacent (fun acc uch prevopt nextopt ->
          let math =
            match (prevopt, nextopt) with
            | (None   , None   ) -> HorzBox.(MathPure(MathElement(mathcls, MathCharWithKern(is_big, uch, kernfL, kernfR))))
            | (None   , Some(_)) -> HorzBox.(MathPure(MathElement(mathcls, MathCharWithKern(is_big, uch, kernfL, kernf0))))
            | (Some(_), None   ) -> HorzBox.(MathPure(MathElement(mathcls, MathCharWithKern(is_big, uch, kernf0, kernfR))))
            | (Some(_), Some(_)) -> HorzBox.(MathPure(MathElement(mathcls, MathChar(is_big, uch))))
          in
              (* -- provides left/right kern only with the leftmost/rightmost character -- *)
              (* needs reconsideration; maybe more natural if arbitrary number of code points can be handled as one glyph *)
            math :: acc
        ) [] |> List.rev
      in
        MathValue(mlst)

  | BackendMathText(astmathcls, astf) ->
      let mathcls = interpret_math_class env astmathcls in
      let valuef = interpret env astf in
      let hblstf ctx = interpret_horz env (Apply(valuef, Context(ctx))) in
        MathValue(HorzBox.([MathPure(MathElement(mathcls, MathEmbeddedText(hblstf)))]))

  | BackendMathColor(astcolor, astm) ->
      let color = interpret_color env astcolor in
      let mlst = interpret_math env astm in
        MathValue(HorzBox.([MathChangeContext(MathChangeColor(color), mlst)]))

  | BackendMathCharClass(astmccls, astm) ->
      let mccls = interpret_math_char_class env astmccls in
      let mlst = interpret_math env astm in
        MathValue(HorzBox.([MathChangeContext(MathChangeMathCharClass(mccls), mlst)]))

  | BackendEmbeddedMath(astctx, astm) ->
      let ctx = interpret_context env astctx in
      let mlst = interpret_math env astm in
      let mathctx = HorzBox.MathContext.make ctx in
      let hblst = Math.main mathctx mlst in
        Horz(hblst)

  | BackendTabular(asttabular) ->
      let interpret_row : abstract_tree -> HorzBox.cell list = interpret_list interpret env (interpret_cell env) in
      let tabular : HorzBox.row list = interpret_list interpret env interpret_row asttabular in
      let (evtabular, wid, hgt, dpt) = Tabular.main tabular in
        Horz(HorzBox.([HorzPure(PHGFixedTabular(wid, hgt, dpt, evtabular))]))

  | BackendRegisterPdfImage(aststr, astpageno) ->
      let srcpath = interpret_string env aststr in
      let pageno = interpret_int env astpageno in
      let imgkey = ImageInfo.add_pdf srcpath pageno in
        ImageKey(imgkey)

  | BackendRegisterOtherImage(aststr) ->
      let srcpath = interpret_string env aststr in
      let imgkey = ImageInfo.add_image srcpath in
        ImageKey(imgkey)

  | ImageKey(_) -> ast

  | BackendUseImageByWidth(astimg, astwid) ->
      let valueimg = interpret env astimg in
      let wid = interpret_length env astwid in
      begin
        match valueimg with
        | ImageKey(imgkey) ->
            let hgt = ImageInfo.get_height_from_width imgkey wid in
              Horz(HorzBox.([HorzPure(PHGFixedImage(wid, hgt, imgkey))]))

        | _ -> report_bug_evaluator "BackendUseImage"
      end

  | Path(astpt0, pathcomplst, cycleopt) ->
      let pt0 = interpret_point env astpt0 in
      let (pathelemlst, closingopt) = interpret_path env pathcomplst cycleopt in
        PathValue([HorzBox.GeneralPath(pt0, pathelemlst, closingopt)])

  | PathValue(_) -> ast

  | PathUnite(astpath1, astpath2) ->
      let pathlst1 = interpret_path_value env astpath1 in
      let pathlst2 = interpret_path_value env astpath2 in
        PathValue(List.append pathlst1 pathlst2)

  | PrePathValue(_) -> ast

  | PrePathBeginning(astpt0) ->
      let pt0 = interpret_point env astpt0 in
        PrePathValue(PrePath.start pt0)

  | PrePathLineTo(astpt1, astprepath) ->
      let pt1 = interpret_point env astpt1 in
      let prepath = interpret_prepath env astprepath in
        PrePathValue(prepath |> PrePath.line_to pt1)

  | PrePathCubicBezierTo(astptS, astptT, astpt1, astprepath) ->
      let ptS = interpret_point env astptS in
      let ptT = interpret_point env astptT in
      let pt1 = interpret_point env astpt1 in
      let prepath = interpret_prepath env astprepath in
        PrePathValue(prepath |> PrePath.bezier_to ptS ptT pt1)

  | PrePathTerminate(astprepath) ->
      let prepath = interpret_prepath env astprepath in
        PathValue([prepath |> PrePath.terminate])

  | PrePathCloseWithLine(astprepath) ->
      let prepath = interpret_prepath env astprepath in
        PathValue([prepath |> PrePath.close_with_line])

  | PrePathCloseWithCubicBezier(astptS, astptT, astprepath) ->
      let ptS = interpret_point env astptS in
      let ptT = interpret_point env astptT in
      let prepath = interpret_prepath env astprepath in
        PathValue([prepath |> PrePath.close_with_bezier ptS ptT])
(*
  | GraphicsContext(_) -> ast
*)
  | GraphicsValue(_) -> ast

  | FontDesignation(_) -> ast

  | Horz(_) -> ast

  | HorzConcat(ast1, ast2) ->
      let hblst1 = interpret_horz env ast1 in
      let hblst2 = interpret_horz env ast2 in
        Horz(List.append hblst1 hblst2)
(*
      begin
        match (value1, value2) with
        | (Horz(evhblst1), Horz(evhblst2)) -> Horz(List.append evhblst1 evhblst2)
        | _ -> report_bug_evaluator ("HorzConcat; " ^ (Display.string_of_ast value1) ^ ", " ^ (Display.string_of_ast value2))
        | (_, Horz([])) -> value1
        | (_, _)        -> HorzConcat(value1, value2)
      end
*)

  | Vert(_) -> ast

  | VertConcat(ast1, ast2) ->
      let imvblst1 = interpret_vert env ast1 in
      let imvblst2 = interpret_vert env ast2 in
        Vert(List.append imvblst1 imvblst2)
(*
      begin
        match (value1, value2) with
        | (Vert([]), _) -> value2
        | (_, Vert([])) -> value1
        | (_, _)        -> VertConcat(value1, value2)
      end
*)

  | Context(_) -> ast

  | UninitializedContext -> ast

  | LambdaVert(evid, astdef) -> LambdaVertWithEnvironment(evid, astdef, env)

  | LambdaVertWithEnvironment(_, _, _) -> ast

  | LambdaVertDetailed(evid, astdef) -> LambdaVertDetailedWithEnv(evid, astdef, env)      

  | LambdaVertDetailedWithEnv(_, _, _) -> ast

  | LambdaHorz(evid, astdef) -> LambdaHorzWithEnvironment(evid, astdef, env)      

  | LambdaHorzWithEnvironment(_, _, _) -> ast

  | HorzLex(astctx, ast1) ->
      let ctx = interpret_context env astctx in
      let value1 = interpret env ast1 in
      begin
        match value1 with
        | InputHorzWithEnvironment(ihlst, envi) -> interpret_input_horz envi ctx ihlst
        | _                                     -> report_bug_evaluator ("HorzLex; " ^ (Display.string_of_ast ast1) ^ " ->* " ^ (Display.string_of_ast value1))
      end

  | VertLex(astctx, ast1) ->
      let valuectx = interpret env astctx in
      let value1 = interpret env ast1 in
      begin
        match value1 with
        | InputVertWithEnvironment(ivlst, envi) -> interpret_input_vert envi valuectx ivlst
        | _                                     -> report_bug_evaluator "VertLex"
      end

  | BackendFont(astabbrev, astszrat, astrsrat) ->
      let font_abbrev = interpret_string env astabbrev in
      let size_ratio = interpret_float env astszrat in
      let rising_ratio = interpret_float env astrsrat in
        FontDesignation((font_abbrev, size_ratio, rising_ratio))

  | BackendLineBreaking(astctx, asthorz) ->
      let ctx = interpret_context env astctx in
      let hblst = interpret_horz env asthorz in
      let imvblst = HorzBox.(LineBreak.main ctx.paragraph_top ctx.paragraph_bottom ctx hblst) in
        Vert(imvblst)

  | BackendPageBreaking(astctx, astvert) ->
      let ctx = interpret_context env astctx in
      let imvblst = interpret_vert env astvert in
        DocumentValue(ctx, imvblst)

  | DocumentValue(_, _) -> ast

  | BackendVertFrame(astctx, astpads, astdecoset, astk) ->
      let ctx = interpret_context env astctx in
      let valuek = interpret env astk in
      let pads = interpret_paddings env astpads in
      let (valuedecoS, valuedecoH, valuedecoM, valuedecoT) = interpret_decoset env astdecoset in
      let valuectxsub =
        Context(HorzBox.({ ctx with paragraph_width = HorzBox.(ctx.paragraph_width -% pads.paddingL -% pads.paddingR) }))
      in
      let imvblst = interpret_vert env (Apply(valuek, valuectxsub)) in
        Vert(HorzBox.([
          ImVertTopMargin(true, ctx.paragraph_top);
          ImVertFrame(pads,
                      make_frame_deco env valuedecoS,
                      make_frame_deco env valuedecoH,
                      make_frame_deco env valuedecoM,
                      make_frame_deco env valuedecoT,
                      ctx.paragraph_width, imvblst);
          ImVertBottomMargin(true, ctx.paragraph_bottom);
        ]))  (* temporary; frame decorations should be variable *)

  | BackendEmbeddedVertTop(astctx, astlen, astk) ->
      let ctx = interpret_context env astctx in
      let wid = interpret_length env astlen in
      let valuek = interpret env astk in
      let valuectxsub =
        Context(HorzBox.({ ctx with paragraph_width = wid; }))
      in
      let imvblst = interpret_vert env (Apply(valuek, valuectxsub)) in
      let evvblst = PageBreak.solidify imvblst in

      let (hgt, dpt) = adjust_to_first_line evvblst in
      let () = PrintForDebug.embvertE (Format.sprintf "EmbeddedVert: height = %f, depth = %f" (Length.to_pdf_point hgt) (Length.to_pdf_point dpt)) in  (* for debug *)
        Horz(HorzBox.([HorzPure(PHGEmbeddedVert(wid, hgt, dpt, evvblst))]))

  | BackendEmbeddedVertBottom(astctx, astlen, astk) ->
      let ctx = interpret_context env astctx in
      let wid = interpret_length env astlen in
      let valuek = interpret env astk in
      let valuectxsub =
        Context(HorzBox.({ ctx with paragraph_width = wid; }))
      in
      let imvblst = interpret_vert env (Apply(valuek, valuectxsub)) in
      let evvblst = PageBreak.solidify imvblst in
      let (hgt, dpt) = adjust_to_last_line evvblst in
      let () = PrintForDebug.embvertE (Format.sprintf "EmbeddedVert: height = %f, depth = %f" (Length.to_pdf_point hgt) (Length.to_pdf_point dpt)) in  (* for debug *)
        Horz(HorzBox.([HorzPure(PHGEmbeddedVert(wid, hgt, dpt, evvblst))]))

  | BackendLineStackTop(astlst) ->
      let hblstlst = interpret_list interpret env (interpret_horz env) astlst in
      let (wid, evvblst) = make_line_stack hblstlst in
      let (hgt, dpt) = adjust_to_first_line evvblst in
        Horz(HorzBox.([HorzPure(PHGEmbeddedVert(wid, hgt, dpt, evvblst))]))

  | BackendLineStackBottom(astlst) ->
      let hblstlst = interpret_list interpret env (interpret_horz env) astlst in
      let (wid, evvblst) = make_line_stack hblstlst in
      let (hgt, dpt) = adjust_to_last_line evvblst in
        Horz(HorzBox.([HorzPure(PHGEmbeddedVert(wid, hgt, dpt, evvblst))]))

  | PrimitiveGetInitialContext(astpage, astpt, astwid, asthgt) ->
      let page = interpret_page env astpage in
      let (lmargin, tmargin) = interpret_point env astpt in
      let txtwid = interpret_length env astwid in
      let txthgt = interpret_length env asthgt in
(*
      let txtwid = Length.of_pdf_point 400. in  (* temporary; should be variable *)
      let txthgt = Length.of_pdf_point 650. in  (* temporary; should be variable *)
*)
      let pagesch =
        HorzBox.({
          page_size        = page;
          left_page_margin = lmargin;
          top_page_margin  = tmargin;
          area_width       = txtwid;
          area_height      = txthgt;
        })
      in
      let ctx = Primitives.get_initial_context pagesch in
        Context(ctx)

  | PrimitiveSetSpaceRatio(astratio, astctx) ->
      let ratio = interpret_float env astratio in
      let ctx = interpret_context env astctx in
        Context(HorzBox.({ ctx with space_natural = ratio; }))

  | PrimitiveSetFontSize(astsize, astctx) ->
      let size = interpret_length env astsize in
      let ctx = interpret_context env astctx in
        Context(HorzBox.({ ctx with font_size = size; }))

  | PrimitiveGetFontSize(astctx) ->
      let ctx = interpret_context env astctx in
        LengthConstant(ctx.HorzBox.font_size)

  | PrimitiveSetFont(astscript, astfont, astctx) ->
      let script = interpret_script env astscript in
      let font_info = interpret_font env astfont in
      let ctx = interpret_context env astctx in
      let font_scheme_new = HorzBox.(ctx.font_scheme |> ScriptSchemeMap.add script font_info) in
        Context(HorzBox.({ ctx with font_scheme = font_scheme_new; }))

  | PrimitiveGetFont(astscript, astctx) ->
      let script = interpret_script env astscript in
      let ctx = interpret_context env astctx in
      let fontwr = HorzBox.get_font_with_ratio ctx script in
        FontDesignation(fontwr)

  | PrimitiveSetMathFont(aststr, astctx) ->
      let mfabbrev = interpret_string env aststr in
      let ctx = interpret_context env astctx in
        Context(HorzBox.({ ctx with math_font = mfabbrev; }))

  | PrimitiveSetDominantScript(astscript, astctx) ->
      let script = interpret_script env astscript in
      let ctx = interpret_context env astctx in
        Context(HorzBox.({ ctx with dominant_script = script; }))

  | PrimitiveGetDominantScript(astctx) ->
      let ctx = interpret_context env astctx in
        make_script_value ctx.HorzBox.dominant_script

  | PrimitiveSetLangSys(astscript, astlangsys, astctx) ->
      let script = interpret_script env astscript in
      let langsys = interpret_language_system env astlangsys in
      let ctx = interpret_context env astctx in
        Context(HorzBox.({ ctx with langsys_scheme = ctx.langsys_scheme |> ScriptSchemeMap.add script langsys}))

  | PrimitiveGetLangSys(astscript, astctx) ->
      let script = interpret_script env astscript in
      let ctx = interpret_context env astctx in
      let langsys = HorzBox.get_language_system ctx script in
        make_language_system_value langsys

  | PrimitiveSetTextColor(astcolor, astctx) ->
      let color = interpret_color env astcolor in
      let ctx = interpret_context env astctx in
        Context(HorzBox.({ ctx with text_color = color; }))

  | PrimitiveSetLeading(astlen, astctx) ->
      let len = interpret_length env astlen in
      let ctx = interpret_context env astctx in
        Context(HorzBox.({ ctx with leading = len; }))

  | PrimitiveGetTextWidth(astctx) ->
      let ctx = interpret_context env astctx in
        LengthConstant(ctx.HorzBox.paragraph_width)

  | PrimitiveSetManualRising(astrising, astctx) ->
      let ctx = interpret_context env astctx in
      let rising = interpret_length env astrising in
        Context(HorzBox.({ ctx with manual_rising = rising; }))

  | PrimitiveEmbed(aststr) ->
      let str = interpret_string env aststr in
        InputHorzWithEnvironment([InputHorzText(str)], env)

  | BackendFixedEmpty(astwid) ->
      let wid = interpret_length env astwid in
        Horz([HorzBox.HorzPure(HorzBox.PHSFixedEmpty(wid))])

  | BackendOuterEmpty(astnat, astshrink, aststretch) ->
      let widnat = interpret_length env astnat in
      let widshrink = interpret_length env astshrink in
      let widstretch = interpret_length env aststretch in
        Horz([HorzBox.HorzPure(HorzBox.PHSOuterEmpty(widnat, widshrink, widstretch))])
(*
  | BackendFixedString(astctx, aststr) ->
      let ctx = interpret_font env astctx in
      let string_info =
        {
          HorzBox.font_abbrev = font_abbrev;
          HorzBox.font_size   = size;
          HorzBox.text_color  = HorzBox.DeviceGray(0.0);
        }
      in
      let purestr = interpret_string env aststr in
        Horz([HorzBox.HorzPure(HorzBox.PHFixedString(string_info, InternalText.to_uchar_list (InternalText.of_utf8 purestr)))])
*)
  | BackendOuterFrame(astpads, astdeco, astbr) ->
      let pads = interpret_paddings env astpads in
      let hblst = interpret_horz env astbr in
      let valuedeco = interpret env astdeco in
        Horz([HorzBox.HorzPure(HorzBox.PHGOuterFrame(
          pads,
          make_frame_deco env valuedeco (* Primitives.frame_deco_S *),
          hblst))])

  | BackendOuterFrameBreakable(astpads, astdecoset, astbr) ->
      let hblst = interpret_horz env astbr in
      let pads = interpret_paddings env astpads in
      let (valuedecoS, valuedecoH, valuedecoM, valuedecoT) = interpret_decoset env astdecoset in
        Horz([HorzBox.HorzFrameBreakable(
          pads, Length.zero, Length.zero,
          make_frame_deco env valuedecoS,
          make_frame_deco env valuedecoH,
          make_frame_deco env valuedecoM,
          make_frame_deco env valuedecoT,
          hblst
        )])

  | BackendInlineGraphics(astwid, asthgt, astdpt, astg) ->
      let wid = interpret_length env astwid in
      let hgt = interpret_length env asthgt in
      let dpt = interpret_length env astdpt in
      let valueg = interpret env astg in
      let graphics = make_inline_graphics env valueg in
        Horz(HorzBox.([HorzPure(PHGFixedGraphics(wid, hgt, Length.negate dpt, graphics))]))

  | PrimitiveGetNaturalWidth(asthorz) ->
      let hblst = interpret_horz env asthorz in
      let (wid, _, _) = LineBreak.get_natural_metrics hblst in
        LengthConstant(wid)

(* ---- list value ---- *)

  | EndOfList -> ast

  | ListCons(asthd, asttl) ->
      let valuehd = interpret env asthd in
      let valuetl = interpret env asttl in
        ListCons(valuehd, valuetl)

(* ---- tuple value ---- *)

  | EndOfTuple -> ast

  | TupleCons(asthd, asttl) ->
      let valuehd = interpret env asthd in
      let valuetl = interpret env asttl in
        TupleCons(valuehd, valuetl)

(* -- fundamentals -- *)

  | ContentOf(evid) ->
      let () = PrintForDebug.evalE ("ContentOf(" ^ (EvalVarID.show_direct evid) ^ ")") in  (* for debug *)
      begin
        try
          let content = !(find_in_environment env evid) in
          let () = PrintForDebug.evalE ("  -> " ^ (Display.string_of_ast content)) in  (* for debug *)
            content
        with
        | Not_found -> report_bug_evaluator ("ContentOf: variable '" ^ (EvalVarID.show_direct evid) ^ "' not found")
      end

  | LetIn(mutletcons, astrest) ->
      let envfunc = copy_environment env in
        begin
          add_mutuals_to_environment envfunc mutletcons ;
          interpret envfunc astrest
        end

  | LambdaAbstract(evid, ast) -> FuncWithEnvironment(evid, ast, env)

  | Apply(astf, astl) ->
      let () = PrintForDebug.evalE ("Apply(" ^ (Display.string_of_ast astf) ^ ", " ^ (Display.string_of_ast astl) ^ ")") in  (* for debug *)
      let fspec = interpret env astf in
        begin
          match fspec with
          | FuncWithEnvironment(evid, astdef, envf) ->
              let valuel = interpret env astl in
              reduce_beta envf evid valuel astdef

          | _ -> report_bug_evaluator "Apply: not a function"
        end

  | IfThenElse(astb, astf, astl) ->
      if interpret_bool env astb then interpret env astf else interpret env astl

(* ---- record ---- *)

  | Record(asc) -> Record(Assoc.map_value (interpret env) asc)

  | AccessField(ast1, fldnm) ->
      let value1 = interpret env ast1 in
      begin
        match value1 with
        | Record(asc1) -> Assoc.find asc1 fldnm
        | _            -> report_bug_evaluator "AccessField: not a Record"
      end

(* ---- class/id option ---- *)
(*
  | ApplyClassAndID(evidcls, evidid, clsnmast, idnmast, astf) ->
      let () = PrintForDebug.evaluator ("%1 " ^ (Display.string_of_ast astf) ^ "\n") in  (* for debug *)
      let valuef =  interpret env
                      (LetIn(MutualLetCons(evidcls, clsnmast, EndOfMutualLet),
                        LetIn(MutualLetCons(evidid, idnmast, EndOfMutualLet), astf))) in
      begin
        PrintForDebug.evaluator ("%2 " ^ (Display.string_of_ast valuef) ^ "\n") ;   (* for debug *)
        match valuef with
        | FuncWithEnvironment(varnm, astdef, envf) ->
            FuncWithEnvironment(varnm,
              LetIn(MutualLetCons(evidcls, clsnmast, EndOfMutualLet),
                LetIn(MutualLetCons(evidid, idnmast, EndOfMutualLet), astdef)
              ), envf)
        | other ->  valuef
      end
*)
(* ---- imperatives ---- *)

  | LetMutableIn(evid, astdflt, astaft) ->
      let valueini = interpret env astdflt in
      let loc = ref valueini in
      let envnew = copy_environment env in
        begin
          add_to_environment envnew evid (ref (Location(loc))) ;
          interpret envnew astaft
        end

  | Sequential(ast1, ast2) ->
      let () = PrintForDebug.evalE ("Sequential(" ^ (Display.string_of_ast ast1) ^ ", " ^ (Display.string_of_ast ast2) ^ ")") in  (* for debug *)
      let value1 = interpret env ast1 in
      let () = PrintForDebug.evalE ("value1 = " ^ (Display.string_of_ast value1)) in  (* for debug *)
      let value2 = interpret env ast2 in
      let () = PrintForDebug.evalE ("value2 = " ^ (Display.string_of_ast value2)) in  (* for debug *)
        begin
          match value1 with
          | UnitConstant -> value2
          | _            -> report_bug_evaluator "Sequential: first operand value is not a UnitConstant"
        end

  | Location(loc) -> Location(loc)

  | Overwrite(evid, astnew) ->
      begin
        try
          let rfvalue = find_in_environment env evid in
            match !rfvalue with
            | Location(loc) ->
                let newvalue = interpret env astnew in
                  begin
                    loc := newvalue ;
                    UnitConstant
                  end
            | _             -> report_bug_evaluator "Overwrite: value is not a Location"
        with
        | Not_found -> report_bug_evaluator ("Overwrite: mutable value '" ^ (EvalVarID.show_direct evid) ^ "' not found")
      end

  | WhileDo(astb, astc) ->
      if interpret_bool env astb then
        let _ = interpret env astc in interpret env (WhileDo(astb, astc))
      else
        UnitConstant

  | Reference(astcont) ->
      let valuecont = interpret env astcont in
        begin
          match valuecont with
          | Location(loc) -> !loc
          | _             -> report_bug_evaluator "Reference"
        end
(*
(* ---- final reference ---- *)

  | DeclareGlobalHash(astkey, astdflt) ->
      begin
        try
          let str_key = Out.main (interpret env astkey) in
          let valueini = interpret env astdflt in
          let loc = ref valueini in
            begin
              Hashtbl.add global_hash_env str_key (ref (Location(loc))) ;
              UnitConstant
            end
        with
        | Out.IllegalOut(_) -> raise (EvalError("this cannot hapen:\n    illegal hash key for 'declare-global-hash'"))
      end

  | OverwriteGlobalHash(astkey, astnew) ->
      begin
        try
          let str_key = Out.main (interpret env astkey) in
            try
              let rfvalue = Hashtbl.find global_hash_env str_key in
                match !rfvalue with
                | Location(loc) ->
                    let valuenew = interpret env astnew in
                      begin
                        loc := valuenew ;
                        UnitConstant
                      end
                | _             -> report_bug_evaluator "OverwriteGlobalHash: value is not a Location"
            with
            | Not_found -> raise (EvalError("undefined global hash key \"" ^ str_key ^ "\""))
        with
        | Out.IllegalOut(s) -> raise (EvalError("illegal argument for '<<-': " ^ s))
      end

  | ReferenceFinal(astkey) -> ReferenceFinal(interpret env astkey)
*)
(* ---- others ---- *)

  | FinishHeaderFile -> EvaluatedEnvironment(env)

  | FinishStruct     -> EvaluatedEnvironment(env)

  | PatternMatch(astobj, pmcons) ->
      let valueobj = interpret env astobj in select_pattern env valueobj pmcons

  | Constructor(constrnm, astcont) ->
      let valuecont = interpret env astcont in
        Constructor(constrnm, valuecont)

  | Module(astmdl, astaft) ->
      let value = interpret env astmdl in
      begin
        match value with
        | EvaluatedEnvironment(envfinal) -> interpret envfinal astaft
        | _                              -> report_bug_evaluator ("module did evaluate not to EvaluatedEnvironment; "
                                                                  ^ (Display.string_of_ast value))
      end

(* -- primitive operation -- *)

  | PrimitiveSame(ast1, ast2) ->
      let str1 = interpret_string env ast1 in
      let str2 = interpret_string env ast2 in
        BooleanConstant(String.equal str1 str2)


  | PrimitiveStringSub(aststr, astpos, astwid) ->
      let str = interpret_string env aststr in
      let pos = interpret_int env astpos in
      let wid = interpret_int env astwid in
        let resstr =
          try String.sub str pos wid with
          | Invalid_argument(s) -> raise (EvalError("illegal index for 'string-sub'"))
        in
          StringConstant(resstr)

  | PrimitiveStringLength(aststr) ->
      let str = interpret_string env aststr in
        IntegerConstant(String.length str)

  | PrimitiveStringUnexplode(astil) ->
      let ilst = interpret_list interpret env (interpret_int env) astil in
      let s =
        (List.map Uchar.of_int ilst) |> InternalText.of_uchar_list |> InternalText.to_utf8
      in
        StringConstant(s)

(*
  | PrimitiveInclude(astfile_name) ->
      ( try
          let str_file_name = Out.main env (interpret env astfile_name) in
          let file = open_in str_file_name in
          let parsed = Parser.main Lexer.cut_token (Lexing.from_channel file) in
            interpret env parsed
        with
        | Out.IllegalOut(s) -> raise (EvalError("illegal argument of \\include: " ^ s))
        | Sys_error(s) -> raise (EvalError("System error at \\include - " ^ s))
      )
*)
  | PrimitiveArabic(astnum) ->
      let num = interpret_int env astnum in StringConstant(string_of_int num)

  | PrimitiveFloat(ast1) ->
      let ic1 = interpret_int env ast1 in FloatConstant(float_of_int ic1)

  | PrimitiveDrawText(astpt, asttext) ->
      let pt = interpret_point env astpt in
      let valuetext = interpret env asttext in
      let hblst = interpret_horz env valuetext in
      let (evhblst, _, _) = LineBreak.natural hblst in
      let pdfops = HandlePdf.pdfops_of_evaled_horz_box pt evhblst in
        GraphicsValue(pdfops)

  | PrimitiveDrawStroke(astwid, astcolor, astpath) ->
      let wid = interpret_length env astwid in
      let color = interpret_color env astcolor in
      let pathlst = interpret_path_value env astpath in
      let pdfops = Graphics.pdfops_of_stroke wid color pathlst in
        GraphicsValue(pdfops)

  | PrimitiveDrawFill(astcolor, astpath) ->
      let color = interpret_color env astcolor in
      let pathlst = interpret_path_value env astpath in
      let pdfops = Graphics.pdfops_of_fill color pathlst in
        GraphicsValue(pdfops)

  | PrimitiveDrawDashedStroke(astwid, astdash, astcolor, astpath) ->
      let wid = interpret_length env astwid in
      let (len1, len2, len3) =
        astdash |> interpret_tuple3 env (fun value ->
          match value with
          | LengthConstant(len) -> len
          | _ -> report_bug_evaluator ("PrimitiveDrawDashedStroke; " ^ (Display.string_of_ast value))
        )
      in
      let color = interpret_color env astcolor in
      let pathlst = interpret_path_value env astpath in
      let pdfops = Graphics.pdfops_of_dashed_stroke wid (len1, len2, len3) color pathlst in
        GraphicsValue(pdfops)

  | Times(astl, astr) ->
      let numl = interpret_int env astl in
      let numr = interpret_int env astr in
        IntegerConstant(numl * numr)

  | Divides(astl, astr) ->
      let numl = interpret_int env astl in
      let numr = interpret_int env astr in
        begin
          try IntegerConstant(numl / numr) with
          | Division_by_zero -> raise (EvalError("division by zero"))
        end

  | Mod(astl, astr) ->
      let numl = interpret_int env astl in
      let numr = interpret_int env astr in
        begin
          try IntegerConstant(numl mod numr) with
          | Division_by_zero -> raise (EvalError("division by zero"))
        end

  | Plus(astl, astr) ->
      let numl = interpret_int env astl in
      let numr = interpret_int env astr in
        IntegerConstant(numl + numr)

  | Minus(astl, astr) ->
      let numl = interpret_int env astl in
      let numr = interpret_int env astr in
        IntegerConstant(numl - numr)

  | EqualTo(astl, astr) ->
      let numl = interpret_int env astl in
      let numr = interpret_int env astr in
        BooleanConstant(numl = numr)

  | GreaterThan(astl, astr) ->
      let numl = interpret_int env astl in
      let numr = interpret_int env astr in
        BooleanConstant(numl > numr)

  | LessThan(astl, astr) ->
      let numl = interpret_int env astl in
      let numr = interpret_int env astr in
        BooleanConstant(numl < numr)

  | LogicalAnd(astl, astr) ->
      let blnl = interpret_bool env astl in
      let blnr = interpret_bool env astr in
        BooleanConstant(blnl && blnr)

  | LogicalOr(astl, astr) ->
      let blnl = interpret_bool env astl in
      let blnr = interpret_bool env astr in
        BooleanConstant(blnl || blnr)

  | LogicalNot(astl) ->
      let blnl = interpret_bool env astl in
        BooleanConstant(not blnl)

  | FloatPlus(ast1, ast2) ->
      let flt1 = interpret_float env ast1 in
      let flt2 = interpret_float env ast2 in
        FloatConstant(flt1 +. flt2)

  | FloatMinus(ast1, ast2) ->
      let flt1 = interpret_float env ast1 in
      let flt2 = interpret_float env ast2 in
        FloatConstant(flt1 -. flt2)

  | LengthPlus(ast1, ast2) ->
      let len1 = interpret_length env ast1 in
      let len2 = interpret_length env ast2 in
        LengthConstant(HorzBox.(len1 +% len2))

  | LengthMinus(ast1, ast2) ->
      let len1 = interpret_length env ast1 in
      let len2 = interpret_length env ast2 in
        LengthConstant(HorzBox.(len1 -% len2))

  | LengthTimes(ast1, ast2) ->
      let len1 = interpret_length env ast1 in
      let flt2 = interpret_float env ast2 in
        LengthConstant(HorzBox.(len1 *% flt2))

  | LengthDivides(ast1, ast2) ->
      let len1 = interpret_length env ast1 in
      let len2 = interpret_length env ast2 in
        FloatConstant(HorzBox.(len1 /% len2))

  | LengthLessThan(ast1, ast2) ->
      let len1 = interpret_length env ast1 in
      let len2 = interpret_length env ast2 in
        BooleanConstant(HorzBox.(len1 <% len2))

  | LengthGreaterThan(ast1, ast2) ->
      let len1 = interpret_length env ast1 in
      let len2 = interpret_length env ast2 in
        BooleanConstant(HorzBox.(len2 <% len1))


and interpret_input_vert env valuectx (ivlst : input_vert_element list) : abstract_tree =
  let (valuectxfinal, imvblstacc) =
    ivlst |> List.fold_left (fun (valuectx, lstacc) iv ->
      match iv with
      | InputVertEmbedded(astcmd, astarglst) ->
          let valuecmd = interpret env astcmd in
          begin
            match valuecmd with
            | LambdaVertWithEnvironment(evid, astdef, envf) ->
                let valuedef = reduce_beta envf evid valuectx astdef in
                let valueret = reduce_beta_list env valuedef astarglst in
                begin
                  match valueret with
                  | Vert(imvblst) -> (valuectx, imvblst :: lstacc)
                  | _             -> report_bug_evaluator "interpret_input_vert; 1"
                end
(*
            | LambdaVertDetailedWithEnv(evid, astdef, envf) ->
                let valuedef = reduce_beta envf evid (Context(ctx)) astdef in
                let valueret = reduce_beta_list env valuedef astarglst in
                begin
                  match valueret with
                  | TupleCons(Context(ctxnext), TupleCons(Vert(imvblst), EndOfTuple)) -> (ctxnext, imvblst :: lstacc)
                  | _                                                                 -> report_bug_evaluator "interpret_input_vert; 2"
                end
*)
            | _ -> report_bug_evaluator "interpret_input_vert; other than LambdaVertWithEnvironment or LambdaVertDetailedWithEnv"
          end
    ) (valuectx, [])
  in
  let imvblst = imvblstacc |> List.rev |> List.concat in
    Vert(imvblst)


and interpret_input_horz (env : environment) (ctx : HorzBox.input_context) (ihlst : input_horz_element list) : abstract_tree =
  let rec eval_content env ihlst =
    ihlst |> List.fold_left (fun evihacc ih ->
      match ih with
      | InputHorzText(s) ->
          EvInputHorzText(s) :: evihacc

      | InputHorzEmbedded(astcmd, astlst) ->
          EvInputHorzEmbedded(astcmd, astlst) :: evihacc

      | InputHorzContent(ast0) ->
          let value0 = interpret env ast0 in
          begin
            match value0 with
            | InputHorzWithEnvironment(ihlstsub, envsub) ->
                let evihlst = eval_content envsub ihlstsub in
                  List.rev_append evihlst evihacc

            | _ ->
                Format.printf "eval_content; %a --->* %a" pp_abstract_tree ast0 pp_abstract_tree value0;
                assert false
          end
    ) [] |> List.rev

  in
  let normalize evihlst =
    evihlst |> List.fold_left (fun acc evih ->
      match evih with
      | EvInputHorzEmbedded(_, _) ->
          (evih :: acc)

      | EvInputHorzText(s2) ->
          begin
            match acc with
            | EvInputHorzText(s1) :: acctail -> (EvInputHorzText(s1 ^ s2) :: acctail)
            | _                              -> (evih :: acc)
          end
          
    ) [] |> List.rev
  in
  let evihlst = eval_content env ihlst in
  let evihlstnml = normalize evihlst in
  let hblstacc =
    evihlstnml |> List.fold_left (fun lstacc ih ->
      match ih with
      | EvInputHorzEmbedded(astcmd, astarglst) ->
          let valuecmd = interpret env astcmd in
          begin
            match valuecmd with
            | LambdaHorzWithEnvironment(evid, astdef, envf) ->
                let valuedef = reduce_beta envf evid (Context(ctx)) astdef in
                let valueret = reduce_beta_list env valuedef astarglst in
                let hblst = interpret_horz env valueret in
                  hblst :: lstacc

            | _ -> report_bug_evaluator "interpret_input_horz; other than LambdaHorzWithEnvironment(_, _, _)"
          end

      | EvInputHorzText(s) ->
          (lex_horz_text ctx s) :: lstacc

    ) []
  in
  let hblst = hblstacc |> List.rev |> List.concat in
  Horz(hblst)


and interpret_option env extractf ast =
  let value = interpret env ast in
    match value with
    | Constructor("None", UnitConstant) -> None
    | Constructor("Some", valuesub)     -> Some(extractf valuesub)
    | _                                 -> report_bug_evaluator "interpret_option"


and interpret_cell env ast : HorzBox.cell =
  let value = interpret env ast in
    match value with
    | Constructor("NormalCell", Horz(hblst)) -> HorzBox.NormalCell(hblst)
    | Constructor("EmptyCell", UnitConstant) -> HorzBox.EmptyCell
    | Constructor("MultiCell", TupleCons(IntegerConstant(nr),
                                 TupleCons(IntegerConstant(nc),
                                   TupleCons(Horz(hblst), EndOfTuple))))
      -> HorzBox.MultiCell(nr, nc, hblst)
    | _ -> report_bug_evaluator "interpret_cell"


and interpret_math_class env ast : HorzBox.math_kind =
  let value = interpret env ast in
    match value with
    | Constructor("MathOrd"   , UnitConstant) -> HorzBox.MathOrdinary
    | Constructor("MathBin"   , UnitConstant) -> HorzBox.MathBinary
    | Constructor("MathRel"   , UnitConstant) -> HorzBox.MathRelation
    | Constructor("MathOp"    , UnitConstant) -> HorzBox.MathOperator
    | Constructor("MathPunct" , UnitConstant) -> HorzBox.MathPunct
    | Constructor("MathOpen"  , UnitConstant) -> HorzBox.MathOpen
    | Constructor("MathClose" , UnitConstant) -> HorzBox.MathClose
    | Constructor("MathPrefix", UnitConstant) -> HorzBox.MathPrefix
    | _ ->
        report_bug_evaluator "interpret_math_class"


and interpret_math env ast : HorzBox.math list =
  let value = interpret env ast in
    match value with
    | MathValue(mlst) -> mlst
    | _               -> report_bug_evaluator ("interpret_math; " ^ (Display.string_of_ast value))


and interpret_math_char_class env ast : HorzBox.math_char_class =
  let value = interpret env ast in
    match value with
    | Constructor("MathItalic"    , UnitConstant) -> HorzBox.MathItalic
    | Constructor("MathRoman"     , UnitConstant) -> HorzBox.MathRoman
    | Constructor("MathBoldItalic", UnitConstant) -> HorzBox.MathBoldItalic
    | Constructor("MathBoldRoman" , UnitConstant) -> HorzBox.MathBoldRoman
    | _ ->
        report_bug_evaluator "interpret_math_char_class"


and interpret_script env ast : CharBasis.script =
  let value = interpret env ast in
    match value with
    | Constructor("HanIdeographic", UnitConstant) -> CharBasis.HanIdeographic
    | Constructor("Kana"          , UnitConstant) -> CharBasis.HiraganaOrKatakana
    | Constructor("Latin"         , UnitConstant) -> CharBasis.Latin
    | Constructor("Other"         , UnitConstant) -> CharBasis.OtherScript
    | _ ->
        report_bug_evaluator ("interpret_script: not a script value; "
                              ^ (Display.string_of_ast ast)
                              ^ " ->* " ^ (Display.string_of_ast value))


and make_script_value script =
  let label =
    match script with
    | CharBasis.HanIdeographic     -> "HanIdeographic"
    | CharBasis.HiraganaOrKatakana -> "Kana"
    | CharBasis.Latin              -> "Latin"
    | CharBasis.OtherScript        -> "OtherScript"
    | _                            -> report_bug_evaluator "make_script_value"
  in
    Constructor(label, UnitConstant)


and interpret_language_system env ast : CharBasis.language_system =
  let value = interpret env ast in
    match value with
    | Constructor("Japanese"        , UnitConstant) -> CharBasis.Japanese
    | Constructor("English"         , UnitConstant) -> CharBasis.English
    | Constructor("NoLanguageSystem", UnitConstant) -> CharBasis.NoLanguageSystem
    | _ ->
        report_bug_evaluator "interpret_language_system"


and make_language_system_value langsys =
  let label =
    match langsys with
    | CharBasis.Japanese         -> "Japanese"
    | CharBasis.English          -> "English"
    | CharBasis.NoLanguageSystem -> "NoLanguageSystem"
  in
    Constructor(label, UnitConstant)


and interpret_string (env : environment) (ast : abstract_tree) : string =
  let vs = interpret env ast in
    match vs with
    | StringEmpty       -> ""
    | StringConstant(s) -> s
    | _                 -> report_bug_evaluator ("interpret_string: not a StringEmpty nor a StringConstant; "
                                                 ^ (Display.string_of_ast ast)
                                                 ^ " ->* " ^ (Display.string_of_ast vs))


and interpret_path_value env ast : HorzBox.path list =
  let value = interpret env ast in
    match value with
    | PathValue(pathlst) -> pathlst
    | _                  -> report_bug_evaluator ("interpret_path_value: not a PathValue; "
                                                  ^ (Display.string_of_ast ast)
                                                  ^ " ->* " ^ (Display.string_of_ast value))


and interpret_context (env : environment) (ast : abstract_tree) : HorzBox.input_context =
  let value = interpret env ast in
    match value with
    | Context(ctx)         -> ctx
    | UninitializedContext -> raise (EvalError("uninitialized context"))
    | _                    -> report_bug_evaluator ("interpret_context: not a Context; "
                                                    ^ (Display.string_of_ast ast)
                                                    ^ " ->* " ^ (Display.string_of_ast value))

(*
and interpret_graphics_context (env : environment) (ast : abstract_tree) : HorzBox.graphics_state =
  let value = interpret env ast in
    match value with
    | GraphicsContext(gctx) -> gctx
    | _                     -> report_bug_evaluator ("interpret_graphics_context: not a GraphicsContext; "
                                                     ^ (Display.string_of_ast ast)
                                                     ^ " ->* " ^ (Display.string_of_ast value))
*)

and interpret_tuple3 env getf ast =
  let value = interpret env ast in
    match value with
    | TupleCons(v1, TupleCons(v2, TupleCons(v3, EndOfTuple))) ->
        let c1 = getf v1 in
        let c2 = getf v2 in
        let c3 = getf v3 in
          (c1, c2, c3)
    | _ -> report_bug_evaluator ("interpret_tuple3; " ^ (Display.string_of_ast value))


and make_color_value color =
  let open HorzBox in
    match color with
    | DeviceGray(gray) ->
        Constructor("Gray", FloatConstant(gray))

    | DeviceRGB(r, g, b) ->
        Constructor("RGB", TupleCons(FloatConstant(r),
                             TupleCons(FloatConstant(g),
                               TupleCons(FloatConstant(b), EndOfTuple))))

    | DeviceCMYK(c, m, y, k) ->
        Constructor("CMYK", TupleCons(FloatConstant(c),
                              TupleCons(FloatConstant(m),
                                TupleCons(FloatConstant(y),
                                  TupleCons(FloatConstant(k), EndOfTuple)))))


and interpret_color env ast : HorzBox.color =
  let value = interpret env ast in
    match value with
    | Constructor("Gray", FloatConstant(gray)) -> HorzBox.DeviceGray(gray)

    | Constructor("RGB", TupleCons(FloatConstant(fltR),
                           TupleCons(FloatConstant(fltG),
                             TupleCons(FloatConstant(fltB), EndOfTuple)))) ->
        HorzBox.DeviceRGB(fltR, fltG, fltB)

    | Constructor("CMYK", TupleCons(FloatConstant(fltC),
                            TupleCons(FloatConstant(fltM),
                              TupleCons(FloatConstant(fltY),
                                TupleCons(FloatConstant(fltK), EndOfTuple))))) ->
        HorzBox.DeviceCMYK(fltC, fltM, fltY, fltK)

    | _ -> report_bug_evaluator ("interpret_color; " ^ (Display.string_of_ast value))


and interpret_font (env : environment) (ast : abstract_tree) : HorzBox.font_with_ratio =
  let value = interpret env ast in
    match value with
    | FontDesignation(fontwr) -> fontwr
    | _                       -> report_bug_evaluator ("interpret_font: not a FontDesignation; "
                                                       ^ (Display.string_of_ast ast)
                                                       ^ " ->* " ^ (Display.string_of_ast value))


and interpret_bool (env : environment) (ast : abstract_tree) : bool =
  let vb = interpret env ast in
    match vb with
    | BooleanConstant(bc) -> bc
    | other               -> report_bug_evaluator ("interpret_bool: not a BooleanConstant; "
                                                   ^ (Display.string_of_ast ast)
                                                   ^ " ->* " ^ (Display.string_of_ast vb))


and interpret_int (env : environment) (ast : abstract_tree) : int =
  let vi = interpret env ast in
    match vi with
    | IntegerConstant(nc) -> nc
    | _                   -> report_bug_evaluator ("interpret_int: not a IntegerConstant; "
                                                   ^ (Display.string_of_ast ast)
                                                   ^ " ->* " ^ (Display.string_of_ast vi))


and interpret_float (env : environment) (ast : abstract_tree) : float =
  let vf = interpret env ast in
    match vf with
    | FloatConstant(nc) -> nc
    | _                 -> report_bug_evaluator ("interpret_float: not a FloatConstant; "
                                                 ^ (Display.string_of_ast ast)
                                                 ^ " ->* " ^ (Display.string_of_ast vf))


and interpret_length (env : environment) (ast : abstract_tree) : length =
  let vl = interpret env ast in
    match vl with
    | LengthConstant(lc) -> lc
    | _                  -> report_bug_evaluator ("interpret_float: not a FloatConstant; "
                                                  ^ (Display.string_of_ast ast)
                                                  ^ " ->* " ^ (Display.string_of_ast vl))

and interpret_page env ast : HorzBox.page_size =
  let vpage = interpret env ast in
    match vpage with
    | Constructor("A4Paper", UnitConstant) -> HorzBox.A4Paper

    | Constructor("UserDefinedPaper",
        TupleCons(LengthConstant(pgwid), TupleCons(LengthConstant(pghgt), EndOfTuple))) ->
          HorzBox.UserDefinedPaper(pgwid, pghgt)

    | _ -> report_bug_evaluator ("interpret_page; " ^ (Display.string_of_ast vpage))


and select_pattern (env : environment) (astobj : abstract_tree) (pmcons : pattern_match_cons) =
  match pmcons with
  | EndOfPatternMatch -> raise (EvalError("no matches"))

  | PatternMatchCons(pat, astto, tailcons) ->
      let envnew = copy_environment env in
      let b = check_pattern_matching envnew pat astobj in
        if b then interpret envnew astto else select_pattern env astobj tailcons

  | PatternMatchConsWhen(pat, astb, astto, tailcons) ->
      let envnew = copy_environment env in
      let b = check_pattern_matching envnew pat astobj in
      let bb = interpret_bool envnew astb in
        if b && bb then interpret envnew astto else select_pattern env astobj tailcons


and check_pattern_matching (env : environment) (pat : pattern_tree) (astobj : abstract_tree) =
  match (pat, astobj) with
  | (PIntegerConstant(pnc), IntegerConstant(nc)) -> pnc = nc
  | (PBooleanConstant(pbc), BooleanConstant(bc)) -> pbc = bc
  | (PStringConstant(ast1), ast2)                ->
      let str1 = interpret_string env ast1 in
      let str2 = interpret_string env ast2 in
        String.equal str1 str2

  | (PUnitConstant, UnitConstant)                -> true
  | (PWildCard, _)                               -> true
  | (PVariable(evid), _)                         ->
      begin
        add_to_environment env evid (ref astobj) ; true
      end
  | (PAsVariable(evid, psub), sub)              ->
      begin
        add_to_environment env evid (ref sub) ; check_pattern_matching env psub sub
      end

  | (PEndOfList, EndOfList)                      -> true
  | (PListCons(phd, ptl), ListCons(hd, tl))      ->
      (check_pattern_matching env phd hd) && (check_pattern_matching env ptl tl)

  | (PEndOfTuple, EndOfTuple)                    -> true
  | (PTupleCons(phd, ptl), TupleCons(hd, tl))    ->
      (check_pattern_matching env phd hd) && (check_pattern_matching env ptl tl)

  | (PConstructor(cnm1, psub), Constructor(cnm2, sub))
      when cnm1 = cnm2                           -> check_pattern_matching env psub sub

  | _                                            -> false


and add_mutuals_to_environment (env : environment) (mutletcons : mutual_let_cons) =
  let lst = add_mutuals_to_environment_sub [] env mutletcons in
  let () = PrintForDebug.evalE ("add_mutuals_to_environment") in  (* for debug *)
      add_zeroary_mutuals lst env


and add_mutuals_to_environment_sub (lst : (EvalVarID.t * abstract_tree) list) (env : environment) (mutletcons : mutual_let_cons) =
  match mutletcons with
  | EndOfMutualLet                         -> lst
  | MutualLetCons(evid, astcont, tailcons) ->
      begin
        try
          let valuecont = interpret env astcont in
            begin
              add_to_environment env evid (ref valuecont) ;
              add_mutuals_to_environment_sub lst env tailcons
            end
        with
        | EvalError(_) -> add_mutuals_to_environment_sub ((evid, astcont) :: lst) env tailcons
            (* 0-ary definition dependent of ``sibling'' functions *)
      end


and add_zeroary_mutuals (lst : (EvalVarID.t * abstract_tree) list) (env : environment) =
  let newlst = add_zeroary_mutuals_sub lst env [] in
    if List.length newlst = 0 then
      ()
    else if (List.length newlst) = (List.length lst) then
      let msg = lst |> List.fold_left (fun s (evid, _) -> s ^ (EvalVarID.show_direct evid) ^ " ") "" in
      raise (EvalError("meaningless 0-ary mutual recursion; " ^ msg))
    else
      add_zeroary_mutuals newlst env


and add_zeroary_mutuals_sub (lst : (EvalVarID.t * abstract_tree) list) (env : environment) (acc : (EvalVarID.t * abstract_tree) list) =
  match lst with
  | []                      -> acc
  | (evid, astcont) :: tail ->
      begin
        try
          let valuecont = interpret env astcont in
            begin
              add_to_environment env evid (ref valuecont) ;
              add_zeroary_mutuals_sub tail env acc
            end
        with
        | EvalError(_) -> add_zeroary_mutuals_sub tail env ((evid, astcont) :: acc)
      end
