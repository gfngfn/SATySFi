
open MyUtil
open LengthInterface
open Types

exception EvalError of string

type nom_input_horz_element =
  | NomInputHorzText     of string
  | NomInputHorzEmbedded of abstract_tree * abstract_tree list
  | NomInputHorzContent  of nom_input_horz_element list * environment


let report_bug_evaluator msg ast value =
  Format.printf "[Bug]@ %s:" msg;
  Format.printf "%a@ --->*@ %a" pp_abstract_tree ast pp_syntactic_value value;
  failwith ("bug: " ^ msg)


let report_bug_evaluator_ast msg ast =
  Format.printf "[Bug]@ %s:" msg;
  Format.printf "%a" pp_abstract_tree ast;
  failwith ("bug: " ^ msg)


let report_bug_evaluator_value msg value =
  Format.printf "[Bug]@ %s:" msg;
  Format.printf "%a" pp_syntactic_value value;
  failwith ("bug: " ^ msg)


let chop_space_indent s =
  let imax = String.length s in
  let rec aux i s =
    if i >= imax then
      (i, "")
    else
      match String.get s i with
      | ' ' -> aux (i + 1) s
      | _   -> (i, String.sub s i (imax - i))
  in
  try
    aux 0 s
  with
  | Invalid_argument(_) -> assert false


let lex_horz_text (ctx : HorzBox.context_main) (s_utf8 : string) : HorzBox.horz_box list =
  let uchlst = InternalText.to_uchar_list (InternalText.of_utf8 s_utf8) in
    HorzBox.([HorzPure(PHCInnerString(ctx, uchlst))])


let make_list (type a) (makef : a -> syntactic_value) (lst : a list) : syntactic_value =
  List.fold_right (fun x ast -> ListCons(makef x, ast)) lst EndOfList


let make_line_stack (hblstlst : (HorzBox.horz_box list) list) =
  let open HorzBox in
  let wid =
    hblstlst |> List.fold_left (fun widacc hblst ->
      let (wid, _, _) = LineBreak.get_natural_metrics hblst in
       Length.max wid widacc
    ) Length.zero
  in
  let trilst = hblstlst |> List.map (fun hblst -> LineBreak.fit hblst wid) in
  let imvblst =
    trilst |> List.fold_left (fun imvbacc (imhblst, hgt, dpt) ->
      Alist.extend imvbacc (VertLine(hgt, dpt, imhblst))
    ) Alist.empty |> Alist.to_list
  in
    (wid, imvblst)


let adjust_to_first_line (imvblst : HorzBox.intermediate_vert_box list) =
  let open HorzBox in
  let rec aux optinit totalhgtinit imvblst =
    imvblst |> List.fold_left (fun (opt, totalhgt) imvb ->
      match (imvb, opt) with
      | (ImVertLine(hgt, dpt, _), None)  -> (Some(totalhgt +% hgt), totalhgt +% hgt +% (Length.negate dpt))
      | (ImVertLine(hgt, dpt, _), _)     -> (opt, totalhgt +% hgt +% (Length.negate dpt))
      | (ImVertFixedEmpty(vskip), _)     -> (opt, totalhgt +% vskip)

      | (ImVertFrame(pads, _, _, imvblstsub), _) ->
          let totalhgtbefore = totalhgt +% pads.paddingT in
          let (optsub, totalhgtsub) = aux opt totalhgtbefore imvblstsub in
          let totalhgtafter = totalhgtsub +% pads.paddingB in
            (optsub, totalhgtafter)

    ) (optinit, totalhgtinit)
  in
    match aux None Length.zero imvblst with
    | (Some(hgt), totalhgt) -> (hgt, Length.negate (totalhgt -% hgt))
    | (None, totalhgt)      -> (Length.zero, Length.negate totalhgt)


let adjust_to_last_line (imvblst : HorzBox.intermediate_vert_box list) =
    let open HorzBox in
    let rec aux optinit totalhgtinit evvblst =
      let evvblstrev = List.rev evvblst in
        evvblstrev |> List.fold_left (fun (opt, totalhgt) imvblast ->
            match (imvblast, opt) with
            | (ImVertLine(hgt, dpt, _), None)  -> (Some((Length.negate totalhgt) +% dpt), totalhgt +% (Length.negate dpt) +% hgt)
            | (ImVertLine(hgt, dpt, _), _)     -> (opt, totalhgt +% (Length.negate dpt) +% hgt)
            | (ImVertFixedEmpty(vskip), _)     -> (opt, totalhgt +% vskip)

            | (ImVertFrame(pads, _, _, evvblstsub), _) ->
                let totalhgtbefore = totalhgt +% pads.paddingB in
                let (optsub, totalhgtsub) = aux opt totalhgtbefore evvblstsub in
                let totalhgtafter = totalhgtsub +% pads.paddingT in
                  (optsub, totalhgtafter)

        ) (optinit, totalhgtinit)
    in
      match aux None Length.zero imvblst with
      | (Some(dpt), totalhgt) -> (totalhgt +% dpt, dpt)
      | (None, totalhgt)      -> (totalhgt, Length.zero)


let get_list getf value =
  let rec aux acc value =
    match value with
    | ListCons(vhead, vtail) -> aux (Alist.extend acc (getf vhead)) vtail
    | EndOfList              -> Alist.to_list acc
    | _                      -> report_bug_evaluator_value "get_list" value
  in
    aux Alist.empty value


let interpret_list interpretf (env : environment) getf (ast : abstract_tree) =
  let value = interpretf env ast in
    get_list getf value


let interpret_option interpretf (env : environment) (getf : syntactic_value -> 'a) (ast : abstract_tree) : 'a option =
  let value = interpretf env ast in
    match value with
    | Constructor("None", UnitConstant) -> None
    | Constructor("Some", valuesub)     -> Some(getf valuesub)
    | _                                 -> report_bug_evaluator "interpret_option" ast value


let rec reduce_beta envf evid valuel astdef =
  let envnew = add_to_environment envf evid (ref valuel) in
    interpret envnew astdef


and reduce_beta_list env valuef astarglst =
  match astarglst with
  | [] ->
      valuef

  | astarg :: astargtail ->
      begin
        match valuef with
        | FuncWithEnvironment(patbrs, envf) ->
          (* -- left-to-right evaluation -- *)
            let valuearg = interpret env astarg in
            let valuefnew = select_pattern (Range.dummy "reduce_beta_list") envf valuearg patbrs in
              reduce_beta_list env valuefnew astargtail

        | _ -> report_bug_evaluator_value "reduce_beta_list" valuef
      end


and interpret_vert env ast =
  let value = interpret env ast in
    get_vert value

and get_vert value =
    match value with
    | Vert(imvblst) -> imvblst
    | _             -> report_bug_evaluator_value "get_vert" value


and interpret_horz env ast =
  let value = interpret env ast in
    get_horz value


and get_horz value =
  match value with
  | Horz(hblst) -> hblst
  | _           -> report_bug_evaluator_value "get_horz" value


and interpret_point env ast =
  let value = interpret env ast in
    get_point value


and get_point value =
    match value with
    | TupleCons(LengthConstant(lenx),
        TupleCons(LengthConstant(leny), EndOfTuple)) -> (lenx, leny)

    | _ -> report_bug_evaluator_value "get_point" value


and interpret_prepath env ast =
  let value = interpret env ast in
    match value with
    | PrePathValue(prepath) -> prepath
    | _ -> report_bug_evaluator "interpret_prepath" ast value


and interpret_paddings env ast =
  let value = interpret env ast in
    get_paddings value


and get_paddings value =
  match value with
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

  | _ -> report_bug_evaluator_value "interpret_paddings" value


and interpret_decoset env ast =
  let value = interpret env ast in
  match value with
  | TupleCons(valuedecoS,
      TupleCons(valuedecoH,
        TupleCons(valuedecoM,
          TupleCons(valuedecoT, EndOfTuple)))) ->
      (valuedecoS, valuedecoH, valuedecoM, valuedecoT)

  | _ -> report_bug_evaluator "interpret_decoset" ast value


and interpret_path env pathcomplst cycleopt =
  let pathelemlst =
    pathcomplst |> List.map (function
      | PathLineTo(astpt) ->
          let pt = interpret_point env astpt in
            GraphicData.LineTo(pt)

      | PathCubicBezierTo(astpt1, astpt2, astpt) ->
          let pt1 = interpret_point env astpt1 in
          let pt2 = interpret_point env astpt2 in
          let pt = interpret_point env astpt in
            GraphicData.CubicBezierTo(pt1, pt2, pt)
    )
  in
  let closingopt =
    match cycleopt with
    | None -> None

    | Some(PathLineTo(())) -> Some(GraphicData.LineTo(()))

    | Some(PathCubicBezierTo(astpt1, astpt2, ())) ->
        let pt1 = interpret_point env astpt1 in
        let pt2 = interpret_point env astpt2 in
          Some(GraphicData.CubicBezierTo(pt1, pt2, ()))
  in
    (pathelemlst, closingopt)


and graphics_of_list value : (HorzBox.intermediate_horz_box list) Graphics.t =
  let rec aux gracc value =
    match value with
    | EndOfList                             -> gracc
    | ListCons(GraphicsValue(grelem), tail) -> aux (Graphics.extend gracc grelem) tail
    | _                                     -> report_bug_evaluator_value "make_frame_deco" value
  in
    aux Graphics.empty value


and make_page_break_info pbinfo =
  let asc =
    Assoc.of_list [
      ("page-number", IntegerConstant(pbinfo.HorzBox.current_page_number));
    ]
  in
    RecordValue(asc)


and make_page_content_info pcinfo =
  make_page_break_info pcinfo  (* temporary *)
(*
  let asc =
    Assoc.of_list [
      ("page-number", IntegerConstant(pcinfo.HorzBox.page_number));
    ]
  in
    RecordValue(asc)
*)

and make_header_or_footer env (valuef : syntactic_value) : HorzBox.header_or_footer =
  (fun pbinfo ->
    let valuepbinfo = make_page_break_info pbinfo in
    let ast = Apply(Value(valuef), Value(valuepbinfo)) in
    let vblst = interpret_vert env ast in
      PageBreak.solidify vblst
  )


and make_page_content_scheme_func env astf : HorzBox.page_content_scheme_func =
  let valuef = interpret env astf in
    (fun pbinfo ->
      let valuepbinfo = make_page_break_info pbinfo in
      let ast = Apply(Value(valuef), Value(valuepbinfo)) in
      let valueret = interpret env ast in
        match valueret with
        | RecordValue(asc) ->
            begin
              match
                (Assoc.find_opt asc "text-origin",
                 Assoc.find_opt asc "text-height")
              with
              | (Some(vTO), Some(vTH)) ->
                  HorzBox.({
                    page_content_origin = get_point vTO;
                    page_content_height = get_length vTH;
                  })

              | _ -> report_bug_evaluator_value "make_page_scheme_func" valueret
            end

        | _ -> report_bug_evaluator "make_page_scheme_func" ast valueret
    )


and make_page_parts_scheme_func env astf : HorzBox.page_parts_scheme_func =
  let valuef = interpret env astf in
  (fun pcinfo ->
    let valuepcinfo = make_page_content_info pcinfo in
    let ast = Apply(Value(valuef), Value(valuepcinfo)) in
    let valueret = interpret env ast in
      match valueret with
      | RecordValue(asc) ->
          begin
            match
              (Assoc.find_opt asc "header-origin",
               Assoc.find_opt asc "header-content",
               Assoc.find_opt asc "footer-origin",
               Assoc.find_opt asc "footer-content")
            with
            | (Some(vHO), Some(vHC), Some(vFO), Some(vFC)) ->
                HorzBox.({
                  header_origin  = get_point vHO;
                  header_content = PageBreak.solidify (get_vert vHC);
                  footer_origin  = get_point vFO;
                  footer_content = PageBreak.solidify (get_vert vFC);
                })

            | _ -> report_bug_evaluator_value "make_page_parts_scheme_func" valueret
          end

      | _ -> report_bug_evaluator "make_page_parts_scheme_func" ast valueret
  )


and make_hook env (asthook : abstract_tree) : (HorzBox.page_break_info -> point -> unit) =
  let valuehook = interpret env asthook in
  (fun pbinfo (xpos, yposbaseline) ->
    let astpt = Value(TupleCons(LengthConstant(xpos), TupleCons(LengthConstant(yposbaseline), EndOfTuple))) in
    let valuepbinfo = make_page_break_info pbinfo in
    let ast = Apply(Apply(Value(valuehook), Value(valuepbinfo)), astpt) in
    let valueret = interpret env ast in
      match valueret with
      | UnitConstant -> ()
      | _            -> report_bug_evaluator "make_hook" ast valueret
  )


and make_frame_deco env valuedeco =
  (fun (xpos, ypos) wid hgt dpt ->
    let astpos = Value(TupleCons(LengthConstant(xpos), TupleCons(LengthConstant(ypos), EndOfTuple))) in
    let astwid = Value(LengthConstant(wid)) in
    let asthgt = Value(LengthConstant(hgt)) in
    let astdpt = Value(LengthConstant(Length.negate dpt)) in
      (* -- depth values for users are nonnegative -- *)
    let valueret = reduce_beta_list env valuedeco [astpos; astwid; asthgt; astdpt] in
      graphics_of_list valueret
  )


and make_paren env valueparenf : HorzBox.paren =
  (fun hgt dpt hgtaxis fontsize color ->
    let asthgt      = Value(LengthConstant(hgt)) in
    let astdpt      = Value(LengthConstant(Length.negate dpt)) in
      (* -- depth values for users are nonnegative -- *)
    let asthgtaxis  = Value(LengthConstant(hgtaxis)) in
    let astfontsize = Value(LengthConstant(fontsize)) in
    let astcolor    = Value(make_color_value color) in
    let valueret = reduce_beta_list env valueparenf [asthgt; astdpt; asthgtaxis; astfontsize; astcolor] in
      match valueret with
      | TupleCons(valuehorz, TupleCons(valuekernf, EndOfTuple)) ->
          let hblst = interpret_horz env (Value(valuehorz)) in
          let kernf = make_math_kern_func env valuekernf in
            (hblst, kernf)

      | _ ->
          report_bug_evaluator_value "make_paren" valueret
  )


and make_math_kern_func env valuekernf : HorzBox.math_kern_func =
  (fun ypos ->
    let astypos = Value(LengthConstant(ypos)) in
    let valueret = reduce_beta_list env valuekernf [astypos] in
      get_length valueret
  )


and make_math_char_kern_func env valuekernf : HorzBox.math_char_kern_func =
  (fun fontsize ypos ->
    let astfontsize = Value(LengthConstant(fontsize)) in
    let astypos     = Value(LengthConstant(ypos)) in
    let valueret = reduce_beta_list env valuekernf [astfontsize; astypos] in
      get_length valueret
  )

and make_inline_graphics env valueg =
  (fun (xpos, ypos) ->
    let astpos = Value(TupleCons(LengthConstant(xpos), TupleCons(LengthConstant(ypos), EndOfTuple))) in
    let valueret = reduce_beta_list env valueg [astpos] in
      graphics_of_list valueret
  )


and interpret_input_horz_content env (ihlst : input_horz_element list) =
  ihlst |> List.map (function
    | InputHorzText(s) ->
        ImInputHorzText(s)

    | InputHorzEmbedded(astcmd, astarglst) ->
        ImInputHorzEmbedded(astcmd, astarglst)

    | InputHorzEmbeddedMath(astmath) ->
        ImInputHorzEmbeddedMath(astmath)

    | InputHorzContent(ast) ->
        let value = interpret env ast in
        begin
          match value with
          | InputHorzWithEnvironment(imihlst, envsub) ->
              ImInputHorzContent(imihlst, envsub)

          | _ -> report_bug_evaluator "interpret_input_horz_content" ast value
        end
  )

and interpret_input_vert_content env (ivlst : input_vert_element list) =
  ivlst |> List.map (function
    | InputVertEmbedded(astcmd, astarglst) ->
        ImInputVertEmbedded(astcmd, astarglst)

    | InputVertContent(ast) ->
        let value = interpret env ast in
        begin
          match value with
          | InputVertWithEnvironment(imivlst, envsub) ->
              ImInputVertContent(imivlst, envsub)

          | _ -> report_bug_evaluator "interpret_input_vert_content" ast value
        end
  )


and interpret env ast =
  match ast with

(* ---- basic value ---- *)

  | Value(v) -> v

  | FinishHeaderFile -> EvaluatedEnvironment(env)

  | FinishStruct -> EvaluatedEnvironment(env)

  | InputHorz(ihlst) ->
      let imihlst = interpret_input_horz_content env ihlst in
        InputHorzWithEnvironment(imihlst, env)
          (* -- lazy evaluation; evaluates embedded variables only -- *)

  | InputVert(ivlst) ->
      let imivlst = interpret_input_vert_content env ivlst in
        InputVertWithEnvironment(imivlst, env)
          (* -- lazy evaluation; evaluates embedded variables only -- *)

  | LengthDescription(flt, unitnm) ->
      let len =
        match unitnm with  (* temporary; ad-hoc handling of unit names *)
        | "pt"   -> Length.of_pdf_point flt
        | "cm"   -> Length.of_centimeter flt
        | "mm"   -> Length.of_millimeter flt
        | "inch" -> Length.of_inch flt
        | _      -> report_bug_evaluator_ast "LengthDescription; unknown unit name" ast
      in
        LengthConstant(len)

  | Concat(ast1, ast2) ->
      let value1 = interpret env ast1 in
      let value2 = interpret env ast2 in
        begin
          match (value1, value2) with
          | (StringEmpty, _)                         -> value2
          | (_, StringEmpty)                         -> value1
          | (StringConstant(s1), StringConstant(s2)) -> StringConstant(s1 ^ s2)
          | _                                        -> report_bug_evaluator "Concat" ast1 value1
        end

(* ---- values for backend ---- *)

  | PrimitiveSetMathVariantToChar(asts, astmccls, astmathcls, aststr, astctx) ->
      let s = interpret_string env asts in
      let mccls = interpret_math_char_class env astmccls in
      let is_big = false in
      let mathcls = interpret_math_class env astmathcls in
      let uchlst = interpret_uchar_list env aststr in
      let (ctx, v) = interpret_context env astctx in
      let mvvalue = (mathcls, HorzBox.MathVariantToChar(is_big, uchlst)) in
      let mcclsmap = ctx.HorzBox.math_variant_char_map in
        Context(HorzBox.({ ctx with math_variant_char_map = mcclsmap |> MathVariantCharMap.add (s, mccls) mvvalue }), v)

  | PrimitiveSetMathCommand(astcmd, astctx) ->
      let valuecmd = interpret env astcmd in
      let (ctx, _) = interpret_context env astctx in
        Context(ctx, valuecmd)

  | BackendMathVariantCharDirect(astmathcls, astrcd) ->   (* TEMPORARY; should extend more *)
      let mathcls = interpret_math_class env astmathcls in
      let is_big = false in  (* temporary *)
      let valuercd = interpret env astrcd in
      let rcd =
        match valuercd with
        | RecordValue(rcd) -> rcd
        | _                -> report_bug_evaluator "BackendMathVariantCharDirect: not a record" astrcd valuercd
      in
      begin
        match
          ( Assoc.find_opt rcd "italic",
            Assoc.find_opt rcd "bold-italic",
            Assoc.find_opt rcd "roman",
            Assoc.find_opt rcd "bold-roman",
            Assoc.find_opt rcd "script",
            Assoc.find_opt rcd "bold-script",
            Assoc.find_opt rcd "fraktur",
            Assoc.find_opt rcd "bold-fraktur",
            Assoc.find_opt rcd "double-struck" )
        with
        | ( Some(vcpI),
            Some(vcpBI),
            Some(vcpR),
            Some(vcpBR),
            Some(vcpS),
            Some(vcpBS),
            Some(vcpF),
            Some(vcpBF),
            Some(vcpDS) ) ->
              let uchlstI  = get_uchar_list vcpI  in
              let uchlstBI = get_uchar_list vcpBI in
              let uchlstR  = get_uchar_list vcpR  in
              let uchlstBR = get_uchar_list vcpBR in
              let uchlstS  = get_uchar_list vcpS  in
              let uchlstBS = get_uchar_list vcpBS in
              let uchlstF  = get_uchar_list vcpF  in
              let uchlstBF = get_uchar_list vcpBF in
              let uchlstDS = get_uchar_list vcpDS in
              let mvsty =
                HorzBox.({
                  math_italic        = uchlstI ;
                  math_bold_italic   = uchlstBI;
                  math_roman         = uchlstR ;
                  math_bold_roman    = uchlstBR;
                  math_script        = uchlstS ;
                  math_bold_script   = uchlstBS;
                  math_fraktur       = uchlstF ;
                  math_bold_fraktur  = uchlstBF;
                  math_double_struck = uchlstDS;
                })
              in
                MathValue(HorzBox.([MathPure(MathVariantCharDirect(mathcls, is_big, mvsty))]))

        | _ -> report_bug_evaluator "BackendMathVariantCharDirect: missing some field" astrcd valuercd
      end

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
        MathValue([MathGroup(mathcls1, mathcls2, mlst)])

  | BackendMathSuperscript(astm1, astm2) ->
      let mlst1 = interpret_math env astm1 in
      let mlst2 = interpret_math env astm2 in
        MathValue([MathSuperscript(mlst1, mlst2)])

  | BackendMathSubscript(astm1, astm2) ->
      let mlst1 = interpret_math env astm1 in
      let mlst2 = interpret_math env astm2 in
        MathValue([MathSubscript(mlst1, mlst2)])

  | BackendMathFraction(astm1, astm2) ->
      let mlst1 = interpret_math env astm1 in
      let mlst2 = interpret_math env astm2 in
        MathValue([MathFraction(mlst1, mlst2)])

  | BackendMathRadical(astm1, astm2) ->
      let mlst1opt = interpret_option interpret env get_math astm1 in
      let mlst2 = interpret_math env astm2 in
      let radical = Primitives.default_radical in  (* temporary; should be variable *)
      begin
        match mlst1opt with
        | None        -> MathValue([MathRadical(radical, mlst2)])
        | Some(mlst1) -> MathValue([MathRadicalWithDegree(mlst1, mlst2)])
      end

  | BackendMathParen(astparenL, astparenR, astm1) ->
      let mlst1 = interpret_math env astm1 in
      let valueparenL = interpret env astparenL in
      let valueparenR = interpret env astparenR in
      let parenL = make_paren env valueparenL in
      let parenR = make_paren env valueparenR in
        MathValue([MathParen(parenL, parenR, mlst1)])

  | BackendMathUpperLimit(astm1, astm2) ->
      let mlst1 = interpret_math env astm1 in
      let mlst2 = interpret_math env astm2 in
        MathValue([MathUpperLimit(mlst1, mlst2)])

  | BackendMathLowerLimit(astm1, astm2) ->
      let mlst1 = interpret_math env astm1 in
      let mlst2 = interpret_math env astm2 in
        MathValue([MathLowerLimit(mlst1, mlst2)])

  | BackendMathChar(astmathcls, is_big, aststr) ->
      let mathcls = interpret_math_class env astmathcls in
      let s = interpret_string env aststr in
      let uchlst = (InternalText.to_uchar_list (InternalText.of_utf8 s)) in
      let mlst = [HorzBox.(MathPure(MathElement(mathcls, MathChar(is_big, uchlst))))] in
        MathValue(mlst)

  | BackendMathCharWithKern(astmathcls, is_big, aststr, astkernfL, astkernfR) ->
      let mathcls = interpret_math_class env astmathcls in
      let s = interpret_string env aststr in
      let valuekernfL = interpret env astkernfL in
      let valuekernfR = interpret env astkernfR in
      let uchlst = (InternalText.to_uchar_list (InternalText.of_utf8 s)) in
      let kernfL = make_math_char_kern_func env valuekernfL in
      let kernfR = make_math_char_kern_func env valuekernfR in
      let mlst = [HorzBox.(MathPure(MathElement(mathcls, MathCharWithKern(is_big, uchlst, kernfL, kernfR))))] in
        MathValue(mlst)

  | BackendMathText(astmathcls, astf) ->
      let mathcls = interpret_math_class env astmathcls in
      let valuef = interpret env astf in
      let hblstf ictx =
(*
          Format.printf "Evaluator> BackendMathText\n";
          Format.printf "%a\n" pp_syntactic_value valuef;
*)
          interpret_horz env (Apply(Value(valuef), Value(Context(ictx))))
      in
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
      let ictx = interpret_context env astctx in
      let mlst = interpret_math env astm in
      let mathctx = MathContext.make ictx in
      let hblst = Math.main mathctx mlst in
        Horz(hblst)

  | BackendTabular(asttabular, astrulesf) ->
      let get_row : syntactic_value -> HorzBox.cell list = get_list get_cell in
      let tabular : HorzBox.row list = interpret_list interpret env get_row asttabular in
      let (imtabular, widlst, lenlst, wid, hgt, dpt) = Tabular.main tabular in
      let valuerulesf = interpret env astrulesf in
      let rulesf xs ys =
        let valueret =
          reduce_beta_list env valuerulesf [Value(make_length_list xs); Value(make_length_list ys)]
        in
        graphics_of_list valueret
      in
(*
      (* begin: temporary *)
      let rulesf xs ys =
        let (xfirst, xlast) =
          match (xs, List.rev xs) with
          | (xfirst :: _, xlast :: _) -> (xfirst, xlast)
          | _                         -> assert false
        in
        let (yfirst, ylast) =
          match (ys, List.rev ys) with
          | (yfirst :: _, ylast :: _) -> (yfirst, ylast)
          | _                         -> assert false
        in
        let open GraphicData in
        let thickness = Length.of_pdf_point 0.5 in
        let color = DeviceRGB(0., 0., 0.) in
        let grxs : 'a Graphics.t =
          xs |> List.fold_left (fun gr x ->
            Graphics.extend gr
              (Graphics.make_stroke thickness color
                [GeneralPath((x, yfirst), [LineTo((x, ylast))], None)])
          ) Graphics.empty
        in
          ys |> List.fold_left (fun gr y ->
            Graphics.extend gr
              (Graphics.make_stroke thickness color
                [GeneralPath((xfirst, y), [LineTo((xlast, y))], None)])
          ) grxs
      in
      (* end: temporary *)
*)
        Horz(HorzBox.([HorzPure(PHGFixedTabular(wid, hgt, dpt, imtabular, widlst, lenlst, rulesf))]))

  | BackendRegisterPdfImage(aststr, astpageno) ->
      let srcpath = interpret_string env aststr in
      let pageno = interpret_int env astpageno in
      let imgkey = ImageInfo.add_pdf srcpath pageno in
        ImageKey(imgkey)

  | BackendRegisterOtherImage(aststr) ->
      let srcpath = interpret_string env aststr in
      let imgkey = ImageInfo.add_image srcpath in
        ImageKey(imgkey)

  | BackendUseImageByWidth(astimg, astwid) ->
      let valueimg = interpret env astimg in
      let wid = interpret_length env astwid in
      begin
        match valueimg with
        | ImageKey(imgkey) ->
            let hgt = ImageInfo.get_height_from_width imgkey wid in
              Horz(HorzBox.([HorzPure(PHGFixedImage(wid, hgt, imgkey))]))

        | _ -> report_bug_evaluator "BackendUseImage" astimg valueimg
      end

  | BackendHookPageBreak(asthook) ->
      let hookf : HorzBox.page_break_info -> point -> unit = make_hook env asthook in
        Horz(HorzBox.([HorzPure(PHGHookPageBreak(hookf))]))

  | Path(astpt0, pathcomplst, cycleopt) ->
      let pt0 = interpret_point env astpt0 in
      let (pathelemlst, closingopt) = interpret_path env pathcomplst cycleopt in
        PathValue([GraphicData.GeneralPath(pt0, pathelemlst, closingopt)])

  | PathUnite(astpath1, astpath2) ->
      let pathlst1 = interpret_path_value env astpath1 in
      let pathlst2 = interpret_path_value env astpath2 in
        PathValue(List.append pathlst1 pathlst2)

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

  | HorzConcat(ast1, ast2) ->
      let hblst1 = interpret_horz env ast1 in
      let hblst2 = interpret_horz env ast2 in
        Horz(List.append hblst1 hblst2)

  | VertConcat(ast1, ast2) ->
      let vblst1 = interpret_vert env ast1 in
      let vblst2 = interpret_vert env ast2 in
        Vert(List.append vblst1 vblst2)

  | LambdaVert(evid, astdef) -> LambdaVertWithEnvironment(evid, astdef, env)

  | LambdaHorz(evid, astdef) -> LambdaHorzWithEnvironment(evid, astdef, env)

  | HorzLex(astctx, ast1) ->
      let valuectx = interpret env astctx in
      let value1 = interpret env ast1 in
      begin
        match value1 with
        | InputHorzWithEnvironment(imihlst, envi) -> interpret_intermediate_input_horz envi valuectx imihlst
        | _                                       -> report_bug_evaluator "HorzLex" ast1 value1
      end

  | VertLex(astctx, ast1) ->
      let valuectx = interpret env astctx in
      let value1 = interpret env ast1 in
      begin
        match value1 with
        | InputVertWithEnvironment(imivlst, envi) -> interpret_intermediate_input_vert envi valuectx imivlst
        | _                                       -> report_bug_evaluator "VertLex" ast1 value1
      end

  | BackendFont(astabbrev, astszrat, astrsrat) ->
      let abbrev = interpret_string env astabbrev in
      let size_ratio = interpret_float env astszrat in
      let rising_ratio = interpret_float env astrsrat in
        make_font_value (abbrev, size_ratio, rising_ratio)

  | BackendLineBreaking(astb1, astb2, astctx, asthorz) ->
      let is_breakable_top = interpret_bool env astb1 in
      let is_breakable_bottom = interpret_bool env astb2 in
      let (ctx, _) = interpret_context env astctx in
      let hblst = interpret_horz env asthorz in
      let imvblst = HorzBox.(LineBreak.main is_breakable_top is_breakable_bottom ctx.paragraph_top ctx.paragraph_bottom ctx hblst) in
        Vert(imvblst)

  | BackendPageBreaking(astpagesize, astpagecontf, astpagepartsf, astvert) ->
      let pagesize = interpret_page_size env astpagesize in
      let pagecontf = make_page_content_scheme_func env astpagecontf in
      let pagepartsf = make_page_parts_scheme_func env astpagepartsf in
      let vblst = interpret_vert env astvert in
        DocumentValue(pagesize, pagecontf, pagepartsf, vblst)

  | BackendVertFrame(astctx, astpads, astdecoset, astk) ->
      let (ctx, valuecmd) = interpret_context env astctx in
      let valuek = interpret env astk in
      let pads = interpret_paddings env astpads in
      let (valuedecoS, valuedecoH, valuedecoM, valuedecoT) = interpret_decoset env astdecoset in
      let valuectxsub =
        Context(HorzBox.({ ctx with paragraph_width = HorzBox.(ctx.paragraph_width -% pads.paddingL -% pads.paddingR) }), valuecmd)
      in
      let vblst = interpret_vert env (Apply(Value(valuek), Value(valuectxsub))) in
        Vert(HorzBox.([
          VertTopMargin(true, ctx.paragraph_top);
          VertFrame(pads,
                      make_frame_deco env valuedecoS,
                      make_frame_deco env valuedecoH,
                      make_frame_deco env valuedecoM,
                      make_frame_deco env valuedecoT,
                      ctx.paragraph_width, vblst);
          VertBottomMargin(true, ctx.paragraph_bottom);
        ]))

  | BackendEmbeddedVertTop(astctx, astlen, astk) ->
      let (ctx, valuecmd) = interpret_context env astctx in
      let wid = interpret_length env astlen in
      let valuek = interpret env astk in
      let valuectxsub =
        Context(HorzBox.({ ctx with paragraph_width = wid; }), valuecmd)
      in
      let vblst = interpret_vert env (Apply(Value(valuek), Value(valuectxsub))) in
      let imvblst = PageBreak.solidify vblst in
      let (hgt, dpt) = adjust_to_first_line imvblst in
(*
      let () = PrintForDebug.embvertE (Format.sprintf "EmbeddedVert: height = %f, depth = %f" (Length.to_pdf_point hgt) (Length.to_pdf_point dpt)) in  (* for debug *)
*)
        Horz(HorzBox.([HorzPure(PHGEmbeddedVert(wid, hgt, dpt, imvblst))]))

  | BackendVertSkip(astlen) ->
      let len = interpret_length env astlen in
        Vert(HorzBox.([VertFixedBreakable(len)]))

  | BackendEmbeddedVertBottom(astctx, astlen, astk) ->
      let (ctx, valuecmd) = interpret_context env astctx in
      let wid = interpret_length env astlen in
      let valuek = interpret env astk in
      let valuectxsub =
        Context(HorzBox.({ ctx with paragraph_width = wid; }), valuecmd)
      in
      let vblst = interpret_vert env (Apply(Value(valuek), Value(valuectxsub))) in
      let imvblst = PageBreak.solidify vblst in
      let (hgt, dpt) = adjust_to_last_line imvblst in
(*
      let () = PrintForDebug.embvertE (Format.sprintf "EmbeddedVert: height = %f, depth = %f" (Length.to_pdf_point hgt) (Length.to_pdf_point dpt)) in  (* for debug *)
*)
        Horz(HorzBox.([HorzPure(PHGEmbeddedVert(wid, hgt, dpt, imvblst))]))

  | BackendLineStackTop(astlst) ->
      let hblstlst = interpret_list interpret env get_horz astlst in
      let (wid, vblst) = make_line_stack hblstlst in
      let imvblst = PageBreak.solidify vblst in
      let (hgt, dpt) = adjust_to_first_line imvblst in
        Horz(HorzBox.([HorzPure(PHGEmbeddedVert(wid, hgt, dpt, imvblst))]))

  | BackendLineStackBottom(astlst) ->
      let hblstlst = interpret_list interpret env get_horz astlst in
      let (wid, vblst) = make_line_stack hblstlst in
      let imvblst = PageBreak.solidify vblst in
      let (hgt, dpt) = adjust_to_last_line imvblst in
        Horz(HorzBox.([HorzPure(PHGEmbeddedVert(wid, hgt, dpt, imvblst))]))

  | PrimitiveGetInitialContext(astwid, astcmd) ->
      let txtwid = interpret_length env astwid in
      let valuecmd = interpret env astcmd in
        Context(Primitives.get_initial_context txtwid, valuecmd)

  | PrimitiveSetSpaceRatio(astratio, astctx) ->
      let ratio = interpret_float env astratio in
      let (ctx, valuecmd) = interpret_context env astctx in
        Context(HorzBox.({ ctx with space_natural = ratio; }), valuecmd)

  | PrimitiveSetParagraphMargin(asttop, astbottom, astctx) ->
      let lentop = interpret_length env asttop in
      let lenbottom = interpret_length env astbottom in
      let (ctx, valuecmd) = interpret_context env astctx in
        Context(HorzBox.({ ctx with
          paragraph_top    = lentop;
          paragraph_bottom = lenbottom;
        }), valuecmd)

  | PrimitiveSetFontSize(astsize, astctx) ->
      let size = interpret_length env astsize in
      let (ctx, valuecmd) = interpret_context env astctx in
        Context(HorzBox.({ ctx with font_size = size; }), valuecmd)

  | PrimitiveGetFontSize(astctx) ->
      let (ctx, _) = interpret_context env astctx in
        LengthConstant(ctx.HorzBox.font_size)

  | PrimitiveSetFont(astscript, astfont, astctx) ->
      let script = interpret_script env astscript in
      let font_info = interpret_font env astfont in
      let (ctx, valuecmd) = interpret_context env astctx in
      let font_scheme_new = HorzBox.(ctx.font_scheme |> CharBasis.ScriptSchemeMap.add script font_info) in
        Context(HorzBox.({ ctx with font_scheme = font_scheme_new; }), valuecmd)

  | PrimitiveGetFont(astscript, astctx) ->
      let script = interpret_script env astscript in
      let (ctx, _) = interpret_context env astctx in
      let fontwr = HorzBox.get_font_with_ratio ctx script in
        make_font_value fontwr

  | PrimitiveSetMathFont(aststr, astctx) ->
      let mfabbrev = interpret_string env aststr in
      let (ctx, valuecmd) = interpret_context env astctx in
        Context(HorzBox.({ ctx with math_font = mfabbrev; }), valuecmd)

  | PrimitiveSetDominantWideScript(astscript, astctx) ->
      let script = interpret_script env astscript in
      let (ctx, valuecmd) = interpret_context env astctx in
        Context(HorzBox.({ ctx with dominant_wide_script = script; }), valuecmd)

  | PrimitiveGetDominantWideScript(astctx) ->
      let (ctx, _) = interpret_context env astctx in
        make_script_value ctx.HorzBox.dominant_wide_script

  | PrimitiveSetDominantNarrowScript(astscript, astctx) ->
      let script = interpret_script env astscript in
      let (ctx, valuecmd) = interpret_context env astctx in
        Context(HorzBox.({ ctx with dominant_narrow_script = script; }), valuecmd)

  | PrimitiveGetDominantNarrowScript(astctx) ->
      let (ctx, _) = interpret_context env astctx in
        make_script_value ctx.HorzBox.dominant_narrow_script

  | PrimitiveSetLangSys(astscript, astlangsys, astctx) ->
      let script = interpret_script env astscript in
      let langsys = interpret_language_system env astlangsys in
      let (ctx, valuecmd) = interpret_context env astctx in
        Context(HorzBox.({ ctx with langsys_scheme = ctx.langsys_scheme |> CharBasis.ScriptSchemeMap.add script langsys}), valuecmd)

  | PrimitiveGetLangSys(astscript, astctx) ->
      let script = interpret_script env astscript in
      let (ctx, _) = interpret_context env astctx in
      let langsys = HorzBox.get_language_system ctx script in
        make_language_system_value langsys

  | PrimitiveSetTextColor(astcolor, astctx) ->
      let color = interpret_color env astcolor in
      let (ctx, valuecmd) = interpret_context env astctx in
        Context(HorzBox.({ ctx with text_color = color; }), valuecmd)

  | PrimitiveGetTextColor(astctx) ->
      let (ctx, _) = interpret_context env astctx in
      let color = ctx.HorzBox.text_color in
        make_color_value color

  | PrimitiveSetLeading(astlen, astctx) ->
      let len = interpret_length env astlen in
      let (ctx, valuecmd) = interpret_context env astctx in
        Context(HorzBox.({ ctx with leading = len; }), valuecmd)

  | PrimitiveGetTextWidth(astctx) ->
      let (ctx, _) = interpret_context env astctx in
        LengthConstant(ctx.HorzBox.paragraph_width)

  | PrimitiveSetManualRising(astrising, astctx) ->
      let rising = interpret_length env astrising in
      let (ctx, valuecmd) = interpret_context env astctx in
        Context(HorzBox.({ ctx with manual_rising = rising; }), valuecmd)

  | PrimitiveSetHyphenPenalty(astpnlty, astctx) ->
      let pnlty = interpret_int env astpnlty in
      let (ctx, valuecmd) = interpret_context env astctx in
        Context(HorzBox.({ ctx with hyphen_badness = pnlty; }), valuecmd)

  | PrimitiveEmbed(aststr) ->
      let str = interpret_string env aststr in
        InputHorzWithEnvironment([ImInputHorzText(str)], env)

  | PrimitiveGetAxisHeight(astctx) ->
      let (ctx, _) = interpret_context env astctx in
      let fontsize = ctx.HorzBox.font_size in
      let mfabbrev = ctx.HorzBox.math_font in
      let hgt = FontInfo.get_axis_height mfabbrev fontsize in
        LengthConstant(hgt)

  | BackendFixedEmpty(astwid) ->
      let wid = interpret_length env astwid in
        Horz([HorzBox.HorzPure(HorzBox.PHSFixedEmpty(wid))])

  | BackendOuterEmpty(astnat, astshrink, aststretch) ->
      let widnat = interpret_length env astnat in
      let widshrink = interpret_length env astshrink in
      let widstretch = interpret_length env aststretch in
        Horz([HorzBox.HorzPure(HorzBox.PHSOuterEmpty(widnat, widshrink, widstretch))])

  | BackendOuterFrame(astpads, astdeco, asth) ->
      let pads = interpret_paddings env astpads in
      let hblst = interpret_horz env asth in
      let valuedeco = interpret env astdeco in
        Horz([HorzBox.HorzPure(HorzBox.PHGOuterFrame(
          pads,
          make_frame_deco env valuedeco,
          hblst))])

  | BackendInnerFrame(astpads, astdeco, asth) ->
      let pads = interpret_paddings env astpads in
      let hblst = interpret_horz env asth in
      let valuedeco = interpret env astdeco in
        Horz([HorzBox.HorzPure(HorzBox.PHGInnerFrame(
          pads,
          make_frame_deco env valuedeco,
          hblst))])

  | BackendFixedFrame(astwid, astpads, astdeco, asth) ->
      let wid = interpret_length env astwid in
      let pads = interpret_paddings env astpads in
      let hblst = interpret_horz env asth in
      let valuedeco = interpret env astdeco in
        Horz([HorzBox.HorzPure(HorzBox.PHGFixedFrame(
          pads, wid,
          make_frame_deco env valuedeco,
          hblst))])

  | BackendOuterFrameBreakable(astpads, astdecoset, asth) ->
      let pads = interpret_paddings env astpads in
      let hblst = interpret_horz env asth in
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

  | BackendScriptGuard(astscript, asth) ->
      let script = interpret_script env astscript in
      let hblst = interpret_horz env asth in
        Horz(HorzBox.([HorzScriptGuard(script, hblst)]))

  | BackendDiscretionary(astpb, asth0, asth1, asth2) ->
      let pb = interpret_int env astpb in
      let hblst0 = interpret_horz env asth0 in
      let hblst1 = interpret_horz env asth1 in
      let hblst2 = interpret_horz env asth2 in
        Horz(HorzBox.([HorzDiscretionary(pb, hblst0, hblst1, hblst2)]))

  | BackendRegisterCrossReference(astk, astv) ->
      let k = interpret_string env astk in
      let v = interpret_string env astv in
      begin
        CrossRef.register k v;
        UnitConstant
      end

  | BackendGetCrossReference(astk) ->
      let k = interpret_string env astk in
      begin
        match CrossRef.get k with
        | None    -> Constructor("None", UnitConstant)
        | Some(v) -> Constructor("Some", StringConstant(v))
      end

  | PrimitiveGetNaturalWidth(asthorz) ->
      let hblst = interpret_horz env asthorz in
      let (wid, _, _) = LineBreak.get_natural_metrics hblst in
        LengthConstant(wid)

  | PrimitiveGetNaturalLength(astvert) ->
      let vblst = interpret_vert env astvert in
      let imvblst = PageBreak.solidify vblst in
      let (hgt, dpt) = adjust_to_first_line imvblst in
        LengthConstant(hgt +% (Length.negate dpt))

  | PrimitiveDisplayMessage(aststr) ->
      let str = interpret_string env aststr in
        print_endline str;
        UnitConstant

  | PrimitiveTupleCons(asthd, asttl) ->
      let valuehd = interpret env asthd in
      let valuetl = interpret env asttl in
        TupleCons(valuehd, valuetl)

  | PrimitiveListCons(asthd, asttl) ->
      let valuehd = interpret env asthd in
      let valuetl = interpret env asttl in
        ListCons(valuehd, valuetl)

(* -- fundamentals -- *)

  | ContentOf(rng, evid) ->
(*
      let () = PrintForDebug.evalE ("ContentOf(" ^ (EvalVarID.show_direct evid) ^ ")") in  (* for debug *)
*)
      begin
        match find_in_environment env evid with
        | Some(rfvalue) ->
          let value = !rfvalue in
(*
          let () = PrintForDebug.evalE ("  -> " ^ (show_syntactic_value value)) in  (* for debug *)
*)
(*
          Format.printf "Evaluator> ContentOf: %s ---> %s\n" (EvalVarID.show_direct evid) (show_syntactic_value value);
*)
            value

        | None ->
            report_bug_evaluator_ast ("ContentOf: variable '" ^ (EvalVarID.show_direct evid) ^ "' (at " ^ (Range.to_string rng) ^ ") not found") ast
      end

  | LetRecIn(recbinds, ast2) ->
      let envnew = add_letrec_bindings_to_environment env recbinds in
        interpret envnew ast2

  | LetNonRecIn(pat, ast1, ast2) ->
      let value1 = interpret env ast1 in
        select_pattern (Range.dummy "LetNonRecIn") env value1 [PatternBranch(pat, ast2)]

  | Function(patbrs) ->
      FuncWithEnvironment(patbrs, env)

  | Apply(ast1, ast2) ->
(*
      let () = PrintForDebug.evalE ("Apply(" ^ (show_abstract_tree ast1) ^ ", " ^ (show_abstract_tree ast2) ^ ")") in  (* for debug *)
*)
      let value1 = interpret env ast1 in
      begin
        match value1 with
        | FuncWithEnvironment(patbrs, env1) ->
            let value2 = interpret env ast2 in
              select_pattern (Range.dummy "Apply") env1 value2 patbrs

        | _ -> report_bug_evaluator "Apply: not a function" ast1 value1
      end

  | IfThenElse(astb, ast1, ast2) ->
      let b = interpret_bool env astb in
        if b then interpret env ast1 else interpret env ast2

(* ---- record ---- *)

  | Record(asc) ->
      RecordValue(Assoc.map_value (interpret env) asc)

  | AccessField(ast1, fldnm) ->
      let value1 = interpret env ast1 in
      begin
        match value1 with
        | RecordValue(asc1) ->
            begin
              match Assoc.find_opt asc1 fldnm with
              | None    -> report_bug_evaluator ("AccessField: field '" ^ fldnm ^ "' not found") ast1 value1
              | Some(v) -> v
            end

        | _ -> report_bug_evaluator "AccessField: not a Record" ast1 value1
      end

(* ---- imperatives ---- *)

  | LetMutableIn(evid, astini, astaft) ->
      let valueini = interpret env astini in
      let stid = register_location env valueini in
(*
      Format.printf "Evaluator> LetMutableIn; %s <- %s\n" (StoreID.show_direct stid) (show_syntactic_value valueini);  (* for debug *)
*)
      let envnew = add_to_environment env evid (ref (Location(stid))) in
        interpret envnew astaft

  | Sequential(ast1, ast2) ->
(*
      let () = PrintForDebug.evalE ("Sequential(" ^ (show_abstract_tree ast1) ^ ", " ^ (show_abstract_tree ast2) ^ ")") in  (* for debug *)
*)
      let value1 = interpret env ast1 in
(*
      let () = PrintForDebug.evalE ("value1 = " ^ (show_syntactic_value value1)) in  (* for debug *)
*)
      let value2 = interpret env ast2 in
(*
      let () = PrintForDebug.evalE ("value2 = " ^ (show_syntactic_value value2)) in  (* for debug *)
*)
        begin
          match value1 with
          | UnitConstant -> value2
          | _            -> report_bug_evaluator "Sequential: first operand value is not a UnitConstant" ast1 value1
        end

  | Overwrite(evid, astnew) ->
      begin
        match find_in_environment env evid with
        | Some(rfvalue) ->
            let value = !rfvalue in
            begin
              match value with
              | Location(stid) ->
                  let valuenew = interpret env astnew in
(*
                  Format.printf "Evaluator> Overwrite; %s <- %s\n" (StoreID.show_direct stid) (show_syntactic_value valuenew);  (* for debug *)
*)
                    begin
                      update_location env stid valuenew;
                      UnitConstant
                    end
              | _ -> report_bug_evaluator_value "Overwrite: value is not a Location" value
            end

        | None ->
            report_bug_evaluator_ast ("Overwrite: mutable value '" ^ (EvalVarID.show_direct evid) ^ "' not found") ast
      end

  | WhileDo(astb, astc) ->
      if interpret_bool env astb then
        let _ = interpret env astc in interpret env (WhileDo(astb, astc))
      else
        UnitConstant

  | Dereference(astcont) ->
      let valuecont = interpret env astcont in
      begin
        match valuecont with
        | Location(stid) ->
            begin
              match find_location_value env stid with
              | Some(value) -> value
              | None        -> report_bug_evaluator "Dereference; not found" astcont valuecont
            end

        | _ ->
            report_bug_evaluator "Dereference" astcont valuecont
      end

(* ---- others ---- *)

  | PatternMatch(rng, astobj, patbrs) ->
      let valueobj = interpret env astobj in
        select_pattern rng env valueobj patbrs

  | NonValueConstructor(constrnm, astcont) ->
      let valuecont = interpret env astcont in
        Constructor(constrnm, valuecont)

  | Module(astmdl, astaft) ->
      let value = interpret env astmdl in
      begin
        match value with
        | EvaluatedEnvironment(envfinal) -> interpret envfinal astaft
        | _                              -> report_bug_evaluator "Module" astmdl value
      end

(* -- primitive operation -- *)

  | PrimitiveSame(ast1, ast2) ->
      let str1 = interpret_string env ast1 in
      let str2 = interpret_string env ast2 in
        BooleanConstant(String.equal str1 str2)

  | PrimitiveStringCompare(ast1, ast2) ->
      let str1 = interpret_string env ast1 in
      let str2 = interpret_string env ast2 in
        IntegerConstant(String.compare str1 str2)

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
      let ilst = interpret_list interpret env get_int astil in
      let s =
        (List.map Uchar.of_int ilst) |> InternalText.of_uchar_list |> InternalText.to_utf8
      in
        StringConstant(s)

  | PrimitiveRegExpOfString(aststr) ->
      let str = interpret_string env aststr in
      let regexp =
        try Str.regexp str with
        | Failure(msg) -> raise (EvalError("regexp-of-string: " ^ msg))
      in
        RegExpConstant(regexp)

  | PrimitiveStringMatch(astpat, astr) ->
      let pat = interpret_regexp env astpat in
      let s   = interpret_string env astr in
      BooleanConstant(Str.string_match pat s 0)

  | PrimitiveSplitIntoLines(asts) ->
      let s = interpret_string env asts in
      let slst = String.split_on_char '\n' s in
      let pairlst = slst |> List.map chop_space_indent in
        pairlst |> make_list (fun (i, s) ->
          TupleCons(IntegerConstant(i), TupleCons(StringConstant(s), EndOfTuple)))

  | PrimitiveSplitOnRegExp(astre, aststr) ->
      let sep = interpret_regexp env astre in
      let str = interpret_string env aststr in
      let slst = Str.split sep str in
      let pairlst = slst |> List.map chop_space_indent in
        pairlst |> make_list (fun (i, s) ->
          TupleCons(IntegerConstant(i), TupleCons(StringConstant(s), EndOfTuple)))

  | PrimitiveArabic(astnum) ->
      let num = interpret_int env astnum in StringConstant(string_of_int num)

  | PrimitiveFloat(ast1) ->
      let ic1 = interpret_int env ast1 in FloatConstant(float_of_int ic1)

  | PrimitiveRound(ast1) ->
      let fc1 = interpret_float env ast1 in IntegerConstant(int_of_float fc1)

  | PrimitiveDrawText(astpt, asth) ->
      let pt = interpret_point env astpt in
      let hblst = interpret_horz env asth in
      let (imhblst, _, _) = LineBreak.natural hblst in
      let grelem = Graphics.make_text pt imhblst in
        GraphicsValue(grelem)

  | PrimitiveDrawStroke(astwid, astcolor, astpath) ->
      let wid = interpret_length env astwid in
      let color = interpret_color env astcolor in
      let pathlst = interpret_path_value env astpath in
      let grelem = Graphics.make_stroke wid color pathlst in
        GraphicsValue(grelem)

  | PrimitiveDrawFill(astcolor, astpath) ->
      let color = interpret_color env astcolor in
      let pathlst = interpret_path_value env astpath in
      let grelem = Graphics.make_fill color pathlst in
        GraphicsValue(grelem)

  | PrimitiveDrawDashedStroke(astwid, astdash, astcolor, astpath) ->
      let wid = interpret_length env astwid in
      let (len1, len2, len3) =
        astdash |> interpret_tuple3 env (fun value ->
          match value with
          | LengthConstant(len) -> len
          | _ -> report_bug_evaluator "PrimitiveDrawDashedStroke" ast value
        )
      in
      let color = interpret_color env astcolor in
      let pathlst = interpret_path_value env astpath in
      let grelem = Graphics.make_dashed_stroke wid (len1, len2, len3) color pathlst in
        GraphicsValue(grelem)

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

  | FloatTimes(ast1, ast2) ->
      let flt1 = interpret_float env ast1 in
      let flt2 = interpret_float env ast2 in
        FloatConstant(flt1 *. flt2)

  | FloatDivides(ast1, ast2) ->
      let flt1 = interpret_float env ast1 in
      let flt2 = interpret_float env ast2 in
        FloatConstant(flt1 /. flt2)

  | FloatSine(ast1) ->
      let flt1 = interpret_float env ast1 in
        FloatConstant(sin flt1)

  | FloatArcSine(ast1) ->
      let flt1 = interpret_float env ast1 in
        FloatConstant(asin flt1)

  | FloatCosine(ast1) ->
      let flt1 = interpret_float env ast1 in
        FloatConstant(cos flt1)

  | FloatArcCosine(ast1) ->
      let flt1 = interpret_float env ast1 in
        FloatConstant(acos flt1)

  | FloatTangent(ast1) ->
      let flt1 = interpret_float env ast1 in
        FloatConstant(tan flt1)

  | FloatArcTangent(ast1) ->
      let flt1 = interpret_float env ast1 in
        FloatConstant(atan flt1)

  | FloatArcTangent2(ast1, ast2) ->
      let flt1 = interpret_float env ast1 in
      let flt2 = interpret_float env ast2 in
        FloatConstant(atan2 flt1 flt2)

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


and interpret_intermediate_input_vert env (valuectx : syntactic_value) (imivlst : intermediate_input_vert_element list) : syntactic_value =
  let rec interpret_commands env (imivlst : intermediate_input_vert_element list) =
    imivlst |> List.map (fun imiv ->
      match imiv with
      | ImInputVertEmbedded(astcmd, astarglst) ->
          let valuecmd = interpret env astcmd in
          begin
            match valuecmd with
            | LambdaVertWithEnvironment(evid, astdef, envf) ->
                let valuedef = reduce_beta envf evid valuectx astdef in
                let valueret = reduce_beta_list env valuedef astarglst in
                  get_vert valueret

            | _ -> report_bug_evaluator "interpret_intermediate_input_vert:1" astcmd valuecmd
          end

      | ImInputVertContent(imivlstsub, envsub) ->
          interpret_commands envsub imivlstsub

    ) |> List.concat
  in
  let imvblst = interpret_commands env imivlst in
    Vert(imvblst)


and interpret_intermediate_input_horz (env : environment) (valuectx : syntactic_value) (imihlst : intermediate_input_horz_element list) : syntactic_value =

  let (ctx, valuemcmd) = get_context valuectx in

  let rec normalize (imihlst : intermediate_input_horz_element list) =
    imihlst |> List.fold_left (fun acc imih ->
      match imih with
      | ImInputHorzEmbedded(astcmd, astarglst) ->
          let nmih = NomInputHorzEmbedded(astcmd, astarglst) in
            Alist.extend acc nmih

      | ImInputHorzText(s2) ->
          begin
            match Alist.chop_last acc with
            | Some(accrest, NomInputHorzText(s1)) -> (Alist.extend accrest (NomInputHorzText(s1 ^ s2)))
            | _                                   -> (Alist.extend acc (NomInputHorzText(s2)))
          end

      | ImInputHorzEmbeddedMath(astmath) ->
          let nmih = NomInputHorzEmbedded(Value(valuemcmd), [astmath]) in
            Alist.extend acc nmih

      | ImInputHorzContent(imihlstsub, envsub) ->
          let nmihlstsub = normalize imihlstsub in
          let nmih = NomInputHorzContent(nmihlstsub, envsub) in
            Alist.extend acc nmih

    ) Alist.empty |> Alist.to_list
  in

  let rec interpret_commands env (nmihlst : nom_input_horz_element list) : HorzBox.horz_box list =
    nmihlst |> List.map (fun nmih ->
      match nmih with
      | NomInputHorzEmbedded(astcmd, astarglst) ->
          let valuecmd = interpret env astcmd in
          begin
            match valuecmd with
            | LambdaHorzWithEnvironment(evid, astdef, envf) ->
                let valuedef = reduce_beta envf evid valuectx astdef in
                let valueret = reduce_beta_list env valuedef astarglst in
                let hblst = get_horz valueret in
                  hblst

            | _ -> report_bug_evaluator "interpret_input_horz" astcmd valuecmd
          end

      | NomInputHorzText(s) ->
          lex_horz_text ctx s

      | NomInputHorzContent(nmihlstsub, envsub) ->
          interpret_commands envsub nmihlstsub

    ) |> List.concat
  in

  let nmihlst = normalize imihlst in
  let hblst = interpret_commands env nmihlst in
    Horz(hblst)


and interpret_cell env ast : HorzBox.cell =
  let value = interpret env ast in
    get_cell value


and get_cell value : HorzBox.cell =
    match value with
    | Constructor("NormalCell", TupleCons(valuepads,
                                  TupleCons(Horz(hblst), EndOfTuple))) ->
        let pads = get_paddings valuepads in
          HorzBox.NormalCell(pads, hblst)

    | Constructor("EmptyCell", UnitConstant) -> HorzBox.EmptyCell

    | Constructor("MultiCell", TupleCons(IntegerConstant(nr),
                                 TupleCons(IntegerConstant(nc),
                                   TupleCons(valuepads,
                                     TupleCons(Horz(hblst), EndOfTuple))))) ->
        let pads = get_paddings valuepads in
          HorzBox.MultiCell(nr, nc, pads, hblst)

    | _ -> report_bug_evaluator_value "get_cell" value


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
        report_bug_evaluator "interpret_math_class" ast value


and interpret_math env ast : math list =
  let value = interpret env ast in
    get_math value


and get_math value : math list =
    match value with
    | MathValue(mlst) -> mlst
    | _               -> report_bug_evaluator_value "get_math" value


and interpret_math_char_class env ast : HorzBox.math_char_class =
  let value = interpret env ast in
    match value with
    | Constructor("MathItalic"      , UnitConstant) -> HorzBox.MathItalic
    | Constructor("MathBoldItalic"  , UnitConstant) -> HorzBox.MathBoldItalic
    | Constructor("MathRoman"       , UnitConstant) -> HorzBox.MathRoman
    | Constructor("MathBoldRoman"   , UnitConstant) -> HorzBox.MathBoldRoman
    | Constructor("MathScript"      , UnitConstant) -> HorzBox.MathScript
    | Constructor("MathBoldScript"  , UnitConstant) -> HorzBox.MathBoldScript
    | Constructor("MathFraktur"     , UnitConstant) -> HorzBox.MathFraktur
    | Constructor("MathBoldFraktur" , UnitConstant) -> HorzBox.MathBoldFraktur
    | Constructor("MathDoubleStruck", UnitConstant) -> HorzBox.MathDoubleStruck
    | _ ->
        report_bug_evaluator "interpret_math_char_class" ast value


and interpret_script env ast : CharBasis.script =
  let value = interpret env ast in
    match value with
    | Constructor("HanIdeographic", UnitConstant) -> CharBasis.HanIdeographic
    | Constructor("Kana"          , UnitConstant) -> CharBasis.HiraganaOrKatakana
    | Constructor("Latin"         , UnitConstant) -> CharBasis.Latin
    | Constructor("Other"         , UnitConstant) -> CharBasis.OtherScript
    | _ ->
        report_bug_evaluator "interpret_script: not a script value" ast value


and make_script_value script =
  let label =
    match script with
    | CharBasis.HanIdeographic     -> "HanIdeographic"
    | CharBasis.HiraganaOrKatakana -> "Kana"
    | CharBasis.Latin              -> "Latin"
    | CharBasis.OtherScript        -> "OtherScript"
    | _                            -> report_bug_evaluator_value "make_script_value" UnitConstant
  in
    Constructor(label, UnitConstant)


and make_font_value (abbrev, sizer, risingr) =
  TupleCons(StringConstant(abbrev),
    TupleCons(FloatConstant(sizer),
      TupleCons(FloatConstant(risingr), EndOfTuple)))


and interpret_language_system env ast : CharBasis.language_system =
  let value = interpret env ast in
    match value with
    | Constructor("Japanese"        , UnitConstant) -> CharBasis.Japanese
    | Constructor("English"         , UnitConstant) -> CharBasis.English
    | Constructor("NoLanguageSystem", UnitConstant) -> CharBasis.NoLanguageSystem
    | _ ->
        report_bug_evaluator "interpret_language_system" ast value


and make_language_system_value langsys =
  let label =
    match langsys with
    | CharBasis.Japanese         -> "Japanese"
    | CharBasis.English          -> "English"
    | CharBasis.NoLanguageSystem -> "NoLanguageSystem"
  in
    Constructor(label, UnitConstant)


and interpret_string (env : environment) (ast : abstract_tree) : string =
  let value = interpret env ast in
    get_string value


and get_string (value : syntactic_value) : string =
    match value with
    | StringEmpty       -> ""
    | StringConstant(s) -> s
    | _                 -> report_bug_evaluator_value "get_string" value


and interpret_regexp (env : environment) (ast : abstract_tree) : Str.regexp =
  let value = interpret env ast in
    get_regexp value


and get_regexp (value : syntactic_value) : Str.regexp =
  match value with
  | RegExpConstant(regexp) -> regexp
  | _                      -> report_bug_evaluator_value "get_regexp" value


and interpret_uchar_list (env : environment) (ast : abstract_tree) : Uchar.t list =
  let value = interpret env ast in
    get_uchar_list value


and get_uchar_list (value : syntactic_value) : Uchar.t list =
  match value with
  | StringEmpty       -> []
  | StringConstant(s) -> InternalText.to_uchar_list (InternalText.of_utf8 s)
  | _                 -> report_bug_evaluator_value "get_uchar_list" value


and interpret_path_value env ast : GraphicData.path list =
  let value = interpret env ast in
    match value with
    | PathValue(pathlst) -> pathlst
    | _                  -> report_bug_evaluator "interpret_path_value" ast value


and interpret_context (env : environment) (ast : abstract_tree) : input_context =
  let value = interpret env ast in
    get_context value


and get_context (value : syntactic_value) : input_context =
    match value with
    | Context(ictx) -> ictx
    | _             -> report_bug_evaluator_value "get_context" value


and interpret_tuple3 env getf ast =
  let value = interpret env ast in
    match value with
    | TupleCons(v1, TupleCons(v2, TupleCons(v3, EndOfTuple))) ->
        let c1 = getf v1 in
        let c2 = getf v2 in
        let c3 = getf v3 in
          (c1, c2, c3)
    | _ -> report_bug_evaluator "interpret_tuple3" ast value


and make_length_list lenlst =
  List.fold_right (fun l acc ->
    ListCons(LengthConstant(l), acc)
  ) lenlst EndOfList


and make_color_value color =
  let open GraphicData in
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


and interpret_color env ast : GraphicData.color =
  let value = interpret env ast in
  let open GraphicData in
    match value with
    | Constructor("Gray", FloatConstant(gray)) -> DeviceGray(gray)

    | Constructor("RGB", TupleCons(FloatConstant(fltR),
                           TupleCons(FloatConstant(fltG),
                             TupleCons(FloatConstant(fltB), EndOfTuple)))) ->
        DeviceRGB(fltR, fltG, fltB)

    | Constructor("CMYK", TupleCons(FloatConstant(fltC),
                            TupleCons(FloatConstant(fltM),
                              TupleCons(FloatConstant(fltY),
                                TupleCons(FloatConstant(fltK), EndOfTuple))))) ->
        DeviceCMYK(fltC, fltM, fltY, fltK)

    | _ -> report_bug_evaluator "interpret_color" ast value


and interpret_font (env : environment) (ast : abstract_tree) : HorzBox.font_with_ratio =
  let value = interpret env ast in
    match value with
    | TupleCons(valueabbrev,
        TupleCons(valuesizer,
          TupleCons(valuerisingr, EndOfTuple))) ->
        let abbrev = get_string valueabbrev in
        let sizer = get_float valuesizer in
        let risingr = get_float valuerisingr in
          (abbrev, sizer, risingr)

    | _ -> report_bug_evaluator "interpret_font" ast value


and interpret_bool (env : environment) (ast : abstract_tree) : bool =
  let value = interpret env ast in
    match value with
    | BooleanConstant(bc) -> bc
    | other               -> report_bug_evaluator "interpret_bool" ast value


and interpret_int (env : environment) (ast : abstract_tree) : int =
  let value = interpret env ast in
    get_int value


and get_int (value : syntactic_value) : int =
  match value with
  | IntegerConstant(nc) -> nc
  | _                   -> report_bug_evaluator_value "get_int" value


and interpret_float (env : environment) (ast : abstract_tree) : float =
  let value = interpret env ast in
    get_float value


and get_float value : float =
  match value with
  | FloatConstant(nc) -> nc
  | _                 -> report_bug_evaluator_value "get_float" value


and interpret_length (env : environment) (ast : abstract_tree) : length =
  let value = interpret env ast in
    get_length value


and get_length (value : syntactic_value) : length =
    match value with
    | LengthConstant(lc) -> lc
    | _                  -> report_bug_evaluator_value "get_float" value


and interpret_page_size env ast : HorzBox.page_size =
  let value = interpret env ast in
    match value with
    | Constructor("A0Paper" , UnitConstant) -> HorzBox.A0Paper
    | Constructor("A1Paper" , UnitConstant) -> HorzBox.A1Paper
    | Constructor("A2Paper" , UnitConstant) -> HorzBox.A2Paper
    | Constructor("A3Paper" , UnitConstant) -> HorzBox.A3Paper
    | Constructor("A4Paper" , UnitConstant) -> HorzBox.A4Paper
    | Constructor("A5Paper" , UnitConstant) -> HorzBox.A5Paper
    | Constructor("USLetter", UnitConstant) -> HorzBox.USLetter
    | Constructor("USLegal" , UnitConstant) -> HorzBox.USLegal

    | Constructor("UserDefinedPaper",
        TupleCons(LengthConstant(pgwid), TupleCons(LengthConstant(pghgt), EndOfTuple))) ->
          HorzBox.UserDefinedPaper(pgwid, pghgt)

    | _ -> report_bug_evaluator "interpret_page" ast value


and select_pattern (rng : Range.t) (env : environment) (valueobj : syntactic_value) (patbrs : pattern_branch list) =
  let iter = select_pattern rng env valueobj in
  match patbrs with
  | [] ->
(*
      Format.printf "Evaluator> %a\n" pp_syntactic_value valueobj;
*)
      raise (EvalError("no matches (" ^ (Range.to_string rng) ^ ")"))

  | PatternBranch(pat, astto) :: tail ->
      let (b, envnew) = check_pattern_matching env pat valueobj in
        if b then
          interpret envnew astto
        else
          iter tail

  | PatternBranchWhen(pat, astcond, astto) :: tail ->
      let (b, envnew) = check_pattern_matching env pat valueobj in
      let cond = interpret_bool envnew astcond in
        if b && cond then
          interpret envnew astto
        else
          iter tail


and check_pattern_matching (env : environment) (pat : pattern_tree) (valueobj : syntactic_value) =
  let return b = (b, env) in
  match (pat, valueobj) with
  | (PIntegerConstant(pnc), IntegerConstant(nc)) -> return (pnc = nc)
  | (PBooleanConstant(pbc), BooleanConstant(bc)) -> return (pbc = bc)

  | (PStringConstant(ast1), value2) ->
      let str1 = interpret_string env ast1 in
      let str2 = get_string value2 in
        return (String.equal str1 str2)

  | (PUnitConstant, UnitConstant) -> return true
  | (PWildCard, _)                -> return true

  | (PVariable(evid), _) ->
      let envnew = add_to_environment env evid (ref valueobj) in
        (true, envnew)

  | (PAsVariable(evid, psub), sub) ->
      let envnew = add_to_environment env evid (ref sub) in
        check_pattern_matching envnew psub sub

  | (PEndOfList, EndOfList) -> return true

  | (PListCons(phd, ptl), ListCons(hd, tl)) ->
      let (bhd, envhd) = check_pattern_matching env phd hd in
      let (btl, envtl) = check_pattern_matching envhd ptl tl in
      if bhd && btl then
        (true, envtl)
      else
        return false

  | (PEndOfTuple, EndOfTuple) -> return true

  | (PTupleCons(phd, ptl), TupleCons(hd, tl)) ->
      let (bhd, envhd) = check_pattern_matching env phd hd in
      let (btl, envtl) = check_pattern_matching envhd ptl tl in
      if bhd && btl then
        (true, envtl)
      else
        return false

  | (PConstructor(cnm1, psub), Constructor(cnm2, sub))
      when cnm1 = cnm2 -> check_pattern_matching env psub sub

  | _ -> return false


and add_letrec_bindings_to_environment (env : environment) (recbinds : letrec_binding list) : environment =
  let trilst =
    recbinds |> List.map (function LetRecBinding(evid, patbrs) ->
      let loc = ref StringEmpty in
      (evid, loc, patbrs)
    )
  in
  let envnew =
    trilst @|> env @|> List.fold_left (fun envacc (evid, loc, _) ->
      add_to_environment envacc evid loc
    )
  in
  trilst |> List.iter (fun (evid, loc, patbrs) ->
(*
    Format.printf "Evaluator> letrec %s\n" (EvalVarID.show_direct evid);  (* for debug *)
*)
    loc := FuncWithEnvironment(patbrs, envnew)
  );

  (* begin: for debug *)
(*
  let () =
    let (valenv, _) = envnew in
    valenv |> EvalVarIDMap.iter (fun evid loc ->
      Format.printf "| %s =\n" (EvalVarID.show_direct evid);
    );
  in
*)
  (* end: for debug *)

  envnew
