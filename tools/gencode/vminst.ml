(**
   {1 SATySFi virtual machine instruction definitions}

   This file is a part of [gencode.exe].
   [gencode.exe] generates OCaml source code and
   type declarations included from

   - [types_.cppo.ml],
   - [ir_.cppo.ml],
   - [evaluator_.cppo.ml], and
   - [vm_.cppo.ml].

   To add a new primitive,

   + Add a new instruction definition to this file.
      ([is_pdf_mode_primitive] or [is_text_mode_primitive] should be [true].)
   + Add a new entry to [primitives.ml].
*)

let def =
  Instruction.(
    [ inst "Concat"
        ~name:"^"
        ~type_:{|
~% (tS @-> tS @-> tS)
|}
        ~fields:[
        ]
        ~params:[
          param "s1" ~type_:"string";
          param "s2" ~type_:"string";
        ]
        ~is_pdf_mode_primitive:true
        ~is_text_mode_primitive:true
        ~code:{|
make_string (s1 ^ s2)
|}
    ; inst "PrimitiveSetMathVariantToChar"
        ~name:"set-math-variant-char"
        ~type_:{|
~% (tMCCLS @-> tI @-> tI @-> tCTX @-> tCTX)
|}
        ~fields:[
        ]
        ~params:[
          param "mccls" ~type_:"math_char_class";
          param "cpfrom" ~type_:"int";
          param "cpto" ~type_:"int";
          param "(ctx, ctxsub)" ~type_:"context";
        ]
        ~is_pdf_mode_primitive:true
        ~is_text_mode_primitive:true
        ~code:{|
let uchfrom = Uchar.of_int cpfrom in
let uchto = Uchar.of_int cpto in
let mcclsmap = ctx.HorzBox.math_variant_char_map in
Context(HorzBox.({ ctx with
  math_variant_char_map = mcclsmap |> MathVariantCharMap.add (uchfrom, mccls) (uchto, MathOrdinary);
}), ctxsub)
|}
    ; inst "PrimitiveSetMathChar"
        ~name:"set-math-char"
        ~type_:{|
~% (tI @-> tI @-> tMATHCLS @-> tCTX @-> tCTX)
|}
        ~fields:[
        ]
        ~params:[
          param "cp_from" ~type_:"int";
          param "cp_to" ~type_:"int";
          param "mk" ~type_:"math_class";
          param "(ctx, ctxsub)" ~type_:"context";
        ]
        ~is_pdf_mode_primitive:true
        ~is_text_mode_primitive:true
        ~code:{|
let uch_from = Uchar.of_int cp_from in
let uch_to = Uchar.of_int cp_to in
let mkmap = ctx.HorzBox.math_class_map in
Context(HorzBox.({ ctx with
  math_class_map = mkmap |> MathClassMap.add uch_from (uch_to, mk);
}), ctxsub)
|}
    ; inst "PrimitiveConvertStringForMath"
        ~name:"convert-string-for-math"
        ~type_:{|
~% (tCTX @-> tMCCLS @-> tS @-> tS)
|}
        ~fields:[
        ]
        ~params:[
          param "(ctx, ctxsub)" ~type_:"context";
          param "mccls" ~type_:"math_char_class";
          param "s" ~type_:"string";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
let ctx = HorzBox.({ ctx with math_char_class = mccls; }) in let uchs = let uchs = InternalText.to_uchar_list (InternalText.of_utf8 s) in uchs |> List.map (fun uch_from -> let (_, uch_to) = MathContext.convert_math_variant_char (ctx, ctxsub) uch_from in uch_to ) in make_string (InternalText.to_utf8 (InternalText.of_uchar_list uchs))
|}
    ; inst "PrimitiveSetMathCommand"
        ~name:"set-math-command"
        ~type_:{|
~% (tICMD tMATH @-> tCTX @-> tCTX)
|}
        ~fields:[
        ]
        ~params:[
          param "valuecmd";
          param "(ctx, ctxsub)" ~type_:"context";
        ]
        ~is_pdf_mode_primitive:true
        ~needs_reducef:true
        ~code:{|
let mcmd = get_math_command_func reducef valuecmd in
Context(ctx, { ctxsub with math_command = mcmd; })
|}
    ; inst "PrimitiveSetCodeTextCommand"
        ~name:"set-code-text-command"
        ~type_:{|
~% (tICMD tS @-> tCTX @-> tCTX)
|}
        ~fields:[
        ]
        ~params:[
          param "valuecmd";
          param "(ctx, ctxsub)" ~type_:"context";
        ]
        ~is_pdf_mode_primitive:true
        ~needs_reducef:true
        ~code:{|
let ctcmd = get_code_text_command_func reducef valuecmd in
Context(ctx, { ctxsub with code_text_command = ctcmd; })
|}
    ; inst "BackendMathVariantCharDirect"
        ~name:"math-variant-char"
        ~type_:{|
~% (tMATHCLS @-> tMCSTY @-> tMATH)
|}
        ~fields:[
        ]
        ~params:[
          param "mathcls" ~type_:"math_class";
          param "valuercd";
        ]
        ~is_pdf_mode_primitive:true
        ~is_text_mode_primitive:true
        ~code:{|
let is_big = false in  (* temporary *)
let mvsty = get_math_variant_style valuercd in
MathValue(HorzBox.([MathPure(MathVariantCharDirect(mathcls, is_big, mvsty))]))
|}
    ; inst "BackendGetLeftMathClass"
        ~name:"get-left-math-class"
        ~type_:{|
~% (tCTX @-> tMATH @-> tOPT tMATHCLS)
|}
        ~fields:[
        ]
        ~params:[
          param "ictx" ~type_:"context";
          param "mathlst" ~type_:"math";
        ]
        ~is_pdf_mode_primitive:true
        ~is_text_mode_primitive:true
        ~code:{|
match mathlst with
| [] ->
    Constructor("None", const_unit)

| math :: _ ->
    let mathcls = Math.get_left_math_kind ictx math in
    make_math_class_option_value mathcls
|}
    ; inst "BackendGetRightMathClass"
        ~name:"get-right-math-class"
        ~type_:{|
~% (tCTX @-> tMATH @-> tOPT tMATHCLS)
|}
        ~fields:[
        ]
        ~params:[
          param "ictx" ~type_:"context";
          param "mathlst" ~type_:"math";
        ]
        ~is_pdf_mode_primitive:true
        ~is_text_mode_primitive:true
        ~code:{|
match List.rev mathlst with
| [] ->
    Constructor("None", const_unit)

| math :: _ ->
    let mathcls = Math.get_right_math_kind ictx math in
    make_math_class_option_value mathcls
|}
    ; inst "BackendSpaceBetweenMaths"
        ~name:"space-between-maths"
        ~type_:{|
~% (tCTX @-> tMATH @-> tMATH @-> tOPT tIB)
|}
        ~fields:[
        ]
        ~params:[
          param "ictx" ~type_:"context";
          param "mathlst1" ~type_:"math";
          param "mathlst2" ~type_:"math";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
let mathctx = MathContext.make ictx in
let hbspaceopt = Math.space_between_maths mathctx mathlst1 mathlst2 in
match hbspaceopt with
| None          -> Constructor("None", const_unit)
| Some(hbspace) -> Constructor("Some", make_horz [hbspace])
|}
    ; inst "BackendMathConcat"
        ~name:"math-concat"
        ~type_:{|
~% (tMATH @-> tMATH @-> tMATH)
|}
        ~fields:[
        ]
        ~params:[
          param "mlst1" ~type_:"math";
          param "mlst2" ~type_:"math";
        ]
        ~is_pdf_mode_primitive:true
        ~is_text_mode_primitive:true
        ~code:{|
MathValue(List.append mlst1 mlst2)
|}
    ; inst "BackendMathGroup"
        ~name:"math-group"
        ~type_:{|
~% (tMATHCLS @-> tMATHCLS @-> tMATH @-> tMATH)
|}
        ~fields:[
        ]
        ~params:[
          param "mathcls1" ~type_:"math_class";
          param "mathcls2" ~type_:"math_class";
          param "mlst" ~type_:"math";
        ]
        ~is_pdf_mode_primitive:true
        ~is_text_mode_primitive:true
        ~code:{|
MathValue([MathGroup(mathcls1, mathcls2, mlst)])
|}
    ; inst "BackendMathSuperscript"
        ~name:"math-sup"
        ~type_:{|
~% (tMATH @-> tMATH @-> tMATH)
|}
        ~fields:[
        ]
        ~params:[
          param "mlst1" ~type_:"math";
          param "mlst2" ~type_:"math";
        ]
        ~is_pdf_mode_primitive:true
        ~is_text_mode_primitive:true
        ~code:{|
MathValue([MathSuperscript(mlst1, mlst2)])
|}
    ; inst "BackendMathSubscript"
        ~name:"math-sub"
        ~type_:{|
~% (tMATH @-> tMATH @-> tMATH)
|}
        ~fields:[
        ]
        ~params:[
          param "mlst1" ~type_:"math";
          param "mlst2" ~type_:"math";
        ]
        ~is_pdf_mode_primitive:true
        ~is_text_mode_primitive:true
        ~code:{|
MathValue([MathSubscript(mlst1, mlst2)])
|}
    ; inst "BackendMathFraction"
        ~name:"math-frac"
        ~type_:{|
~% (tMATH @-> tMATH @-> tMATH)
|}
        ~fields:[
        ]
        ~params:[
          param "mlst1" ~type_:"math";
          param "mlst2" ~type_:"math";
        ]
        ~is_pdf_mode_primitive:true
        ~is_text_mode_primitive:true
        ~code:{|
MathValue([MathFraction(mlst1, mlst2)])
|}
    ; inst "BackendMathRadical"
        ~name:"math-radical"
        ~type_:{|
~% (tOPT tMATH @-> tMATH @-> tMATH)
|}
        ~fields:[
        ]
        ~params:[
          param "value1mopt";
          param "mlst2" ~type_:"math";
        ]
        ~is_pdf_mode_primitive:true
        ~is_text_mode_primitive:true
        ~code:{|
let mlst1opt = get_option get_math value1mopt in
let radical = Primitives.default_radical in  (* temporary; should be variable *)
match mlst1opt with
| None        -> MathValue([MathRadical(radical, mlst2)])
| Some(mlst1) -> MathValue([MathRadicalWithDegree(mlst1, mlst2)])
|}
    ; inst "BackendMathParen"
        ~name:"math-paren"
        ~type_:{|
~% (tPAREN @-> tPAREN @-> tMATH @-> tMATH)
|}
        ~fields:[
        ]
        ~params:[
          param "valueparenL";
          param "valueparenR";
          param "mlst1" ~type_:"math";
        ]
        ~is_pdf_mode_primitive:true
        ~is_text_mode_primitive:true
        ~needs_reducef:true
        ~code:{|
let parenL = make_paren reducef valueparenL in
let parenR = make_paren reducef valueparenR in
MathValue([MathParen(parenL, parenR, mlst1)])
|}
    ; inst "BackendMathParenWithMiddle"
        ~name:"math-paren-with-middle"
        ~type_:{|
~% (tPAREN @-> tPAREN @-> tPAREN @-> tL tMATH @-> tMATH)
|}
        ~fields:[
        ]
        ~params:[
          param "valueparenL";
          param "valueparenR";
          param "valuemiddle";
          param "mlstlst" ~type_:"math_list";
        ]
        ~is_pdf_mode_primitive:true
        ~is_text_mode_primitive:true
        ~needs_reducef:true
        ~code:{|
let parenL = make_paren reducef valueparenL in
let parenR = make_paren reducef valueparenR in
let middle = make_paren reducef valuemiddle in
MathValue([MathParenWithMiddle(parenL, parenR, middle, mlstlst)])
|}
    ; inst "BackendMathUpperLimit"
        ~name:"math-upper"
        ~type_:{|
~% (tMATH @-> tMATH @-> tMATH)
|}
        ~fields:[
        ]
        ~params:[
          param "mlst1" ~type_:"math";
          param "mlst2" ~type_:"math";
        ]
        ~is_pdf_mode_primitive:true
        ~is_text_mode_primitive:true
        ~code:{|
MathValue([MathUpperLimit(mlst1, mlst2)])
|}
    ; inst "BackendMathLowerLimit"
        ~name:"math-lower"
        ~type_:{|
~% (tMATH @-> tMATH @-> tMATH)
|}
        ~fields:[
        ]
        ~params:[
          param "mlst1" ~type_:"math";
          param "mlst2" ~type_:"math";
        ]
        ~is_pdf_mode_primitive:true
        ~is_text_mode_primitive:true
        ~code:{|
MathValue([MathLowerLimit(mlst1, mlst2)])
|}
    ; inst "BackendMathPullInScripts"
        ~name:"math-pull-in-scripts"
        ~type_:{|
~% (tMATHCLS @-> tMATHCLS @-> (tOPT tMATH @-> tOPT tMATH @-> tMATH) @-> tMATH)
|}
        ~fields:[
        ]
        ~params:[
          param "mathcls1" ~type_:"math_class";
          param "mathcls2" ~type_:"math_class";
          param "valuef";
        ]
        ~is_pdf_mode_primitive:true
        ~is_text_mode_primitive:true
        ~needs_reducef:true
        ~code:{|
let mlstf = make_pull_in_scripts reducef valuef in
let mlst = [HorzBox.(MathPullInScripts(mathcls1, mathcls2, mlstf))] in
MathValue(mlst)
|}
    ; inst "BackendMathChar"
        ~name:"math-char"
        ~type_:{|
~% (tMATHCLS @-> tS @-> tMATH)
|}
        ~fields:[
        ]
        ~params:[
          param "mathcls" ~type_:"math_class";
          param "uchlst" ~type_:"uchar_list";
        ]
        ~is_pdf_mode_primitive:true
        ~is_text_mode_primitive:true
        ~code:{|
let mlst = [HorzBox.(MathPure(MathElement(mathcls, MathChar(false, uchlst))))] in
MathValue(mlst)
|}
    ; inst "BackendMathBigChar"
        ~name:"math-big-char"
        ~type_:{|
~% (tMATHCLS @-> tS @-> tMATH)
|}
        ~fields:[
        ]
        ~params:[
          param "mathcls" ~type_:"math_class";
          param "uchlst" ~type_:"uchar_list";
        ]
        ~is_pdf_mode_primitive:true
        ~is_text_mode_primitive:true
        ~code:{|
let mlst = [HorzBox.(MathPure(MathElement(mathcls, MathChar(true, uchlst))))] in
MathValue(mlst)
|}
    ; inst "BackendMathCharWithKern"
        ~name:"math-char-with-kern"
        ~type_:{|
let mckf = tLN @-> tLN @-> tLN in
  ~% (tMATHCLS @-> tS @-> mckf @-> mckf @-> tMATH)
|}
        ~fields:[
        ]
        ~params:[
          param "mathcls" ~type_:"math_class";
          param "uchlst" ~type_:"uchar_list";
          param "valuekernfL";
          param "valuekernfR";
        ]
        ~is_pdf_mode_primitive:true
        ~is_text_mode_primitive:true
        ~needs_reducef:true
        ~code:{|
let kernfL = make_math_char_kern_func reducef valuekernfL in
let kernfR = make_math_char_kern_func reducef valuekernfR in
let mlst = [HorzBox.(MathPure(MathElement(mathcls, MathCharWithKern(false, uchlst, kernfL, kernfR))))] in
MathValue(mlst)
|}
    ; inst "BackendMathBigCharWithKern"
        ~name:"math-big-char-with-kern"
        ~type_:{|
let mckf = tLN @-> tLN @-> tLN in
  ~% (tMATHCLS @-> tS @-> mckf @-> mckf @-> tMATH)
|}
        ~fields:[
        ]
        ~params:[
          param "mathcls" ~type_:"math_class";
          param "uchlst" ~type_:"uchar_list";
          param "valuekernfL";
          param "valuekernfR";
        ]
        ~is_pdf_mode_primitive:true
        ~is_text_mode_primitive:true
        ~needs_reducef:true
        ~code:{|
let kernfL = make_math_char_kern_func reducef valuekernfL in
let kernfR = make_math_char_kern_func reducef valuekernfR in
let mlst = [HorzBox.(MathPure(MathElement(mathcls, MathCharWithKern(true, uchlst, kernfL, kernfR))))] in
MathValue(mlst)
|}
    ; inst "BackendMathText"
        ~name:"text-in-math"
        ~type_:{|
~% (tMATHCLS @-> (tCTX @-> tIB) @-> tMATH)
|}
        ~fields:[
        ]
        ~params:[
          param "mathcls" ~type_:"math_class";
          param "valuef";
        ]
        ~is_pdf_mode_primitive:true
        ~needs_reducef:true
        ~code:{|
let hblstf ictx =
  let valueh = reducef valuef [Context(ictx)] in
  get_horz valueh
in
MathValue(HorzBox.([MathPure(MathElement(mathcls, MathEmbeddedText(hblstf)))]))
|}
    ; inst "BackendMathColor"
        ~name:"math-color"
        ~type_:{|
~% (tCLR @-> tMATH @-> tMATH)
|}
        ~fields:[
        ]
        ~params:[
          param "color" ~type_:"color";
          param "mlst" ~type_:"math";
        ]
        ~is_pdf_mode_primitive:true
        ~is_text_mode_primitive:true
        ~code:{|
MathValue(HorzBox.([MathChangeContext(MathChangeColor(color), mlst)]))
|}
    ; inst "BackendMathCharClass"
        ~name:"math-char-class"
        ~type_:{|
~% (tMCCLS @-> tMATH @-> tMATH)
|}
        ~fields:[
        ]
        ~params:[
          param "mccls" ~type_:"math_char_class";
          param "mlst" ~type_:"math";
        ]
        ~is_pdf_mode_primitive:true
        ~is_text_mode_primitive:true
        ~code:{|
MathValue(HorzBox.([MathChangeContext(MathChangeMathCharClass(mccls), mlst)]))
|}
    ; inst "BackendEmbeddedMath"
        ~name:"embed-math"
        ~type_:{|
~% (tCTX @-> tMATH @-> tIB)
|}
        ~fields:[
        ]
        ~params:[
          param "ictx" ~type_:"context";
          param "mlst" ~type_:"math";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
let mathctx = MathContext.make ictx in
let hblst = Math.main mathctx mlst in
make_horz hblst
|}
    ; inst "BackendTabular"
        ~name:"tabular"
        ~type_:{|
~% ((tL (tL tCELL)) @-> tRULESF @-> tIB)
|}
        ~fields:[
        ]
        ~params:[
          param "valuetabular";
          param "valuerulesf";
        ]
        ~is_pdf_mode_primitive:true
        ~needs_reducef:true
        ~code:{|
let tabular = get_list (get_list get_cell) valuetabular in
let (imtabular, widlst, lenlst, wid, hgt, dpt) = Tabular.main tabular in
let rulesf xs ys =
  let valuexs = make_length_list xs in
  let valueys = make_length_list ys in
  let valueret = reducef valuerulesf [valuexs; valueys] in
  graphics_of_list valueret
in
make_horz (HorzBox.([HorzPure(PHGFixedTabular(wid, hgt, dpt, imtabular, widlst, lenlst, rulesf))]))
|}
    ; inst "BackendRegisterPdfImage"
        ~name:"load-pdf-image"
        ~type_:{|
~% (tS @-> tI @-> tIMG)
|}
        ~fields:[
        ]
        ~params:[
          param "relpathstr" ~type_:"string";
          param "pageno" ~type_:"int";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
let abspath = MyUtil.make_abs_path (Filename.concat (OptionState.job_directory ()) relpathstr) in
let imgkey = ImageInfo.add_pdf abspath pageno in
make_image_key imgkey
|}
    ; inst "BackendRegisterOtherImage"
        ~name:"load-image"
        ~type_:{|
~% (tS @-> tIMG)
|}
        ~fields:[
        ]
        ~params:[
          param "relpath" ~type_:"string";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
let abspath = MyUtil.make_abs_path (Filename.concat (OptionState.job_directory ()) relpath) in
let imgkey = ImageInfo.add_image abspath in
make_image_key imgkey
|}
    ; inst "BackendUseImageByNaturalSize"
        ~name:"use-image"
        ~type_:{|
~% (tIMG @-> tIB)
|}
        ~fields:[
        ]
        ~params:[
          param "valueimg";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
match valueimg with
| BaseConstant(BCImageKey(imgkey)) ->
    let (wid, hgt) = ImageInfo.get_size imgkey in
    make_horz (HorzBox.([HorzPure(PHGFixedImage(wid, hgt, imgkey))]))

| _ ->
    report_bug_vm "BackendUseImageByNaturalSize"
|}
    ; inst "BackendHookPageBreak"
        ~name:"hook-page-break"
        ~type_:{|
~% ((tPBINFO @-> tPT @-> tU) @-> tIB)
|}
        ~fields:[
        ]
        ~params:[
          param "hookf";
        ]
        ~is_pdf_mode_primitive:true
        ~needs_reducef:true
        ~code:{|
let hookf = make_hook reducef hookf in
make_horz (HorzBox.([HorzPure(PHGHookPageBreak(hookf))]))
|}
    ; inst "BackendHookPageBreakBlock"
        ~name:"hook-page-break-block"
        ~type_:{|
~% ((tPBINFO @-> tPT @-> tU) @-> tBB)
|}
        ~fields:[
        ]
        ~params:[
          param "hookf";
        ]
        ~is_pdf_mode_primitive:true
        ~needs_reducef:true
        ~code:{|
let hookf = make_hook reducef hookf in
make_vert (HorzBox.([VertHookPageBreak(hookf)]))
|}
    ; inst "PathUnite"
        ~name:"unite-path"
        ~type_:{|
~% (tPATH @-> tPATH @-> tPATH)
|}
        ~fields:[
        ]
        ~params:[
          param "pathlst1" ~type_:"path_value";
          param "pathlst2" ~type_:"path_value";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
make_path (List.append pathlst1 pathlst2)
|}
    ; inst "PathShift"
        ~name:"shift-path"
        ~type_:{|
~% (tPT @-> tPATH @-> tPATH)
|}
        ~fields:[
        ]
        ~params:[
          param "ptshift" ~type_:"point";
          param "pathlst" ~type_:"path_value";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
make_path (List.map (shift_path ptshift) pathlst)
|}
    ; inst "PathLinearTransform"
        ~name:"linear-transform-path"
        ~type_:{|
~% (tFL @-> tFL @-> tFL @-> tFL @-> tPATH @-> tPATH)
|}
        ~fields:[
        ]
        ~params:[
          param "a" ~type_:"float";
          param "b" ~type_:"float";
          param "c" ~type_:"float";
          param "d" ~type_:"float";
          param "pathlst" ~type_:"path_value";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
make_path (List.map (linear_transform_path ((a, b), (c, d))) pathlst)
|}
    ; inst "PathGetBoundingBox"
        ~name:"get-path-bbox"
        ~type_:{|
~% (tPATH @-> tPROD [tPT; tPT])
|}
        ~fields:[
        ]
        ~params:[
          param "pathlst" ~type_:"path_value";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
let (ptmin, ptmax) = get_path_list_bbox pathlst in
let value1 = make_point_value ptmin in
let value2 = make_point_value ptmax in
Tuple([value1; value2])
|}
    ; inst "PrePathBeginning"
        ~name:"start-path"
        ~type_:{|
~% (tPT @-> tPRP)
|}
        ~fields:[
        ]
        ~params:[
          param "pt0" ~type_:"point";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
make_prepath (PrePath.start pt0)
|}
    ; inst "PrePathLineTo"
        ~name:"line-to"
        ~type_:{|
~% (tPT @-> tPRP @-> tPRP)
|}
        ~fields:[
        ]
        ~params:[
          param "pt1" ~type_:"point";
          param "prepath" ~type_:"prepath";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
make_prepath (prepath |> PrePath.line_to pt1)
|}
    ; inst "PrePathCubicBezierTo"
        ~name:"bezier-to"
        ~type_:{|
~% (tPT @-> tPT @-> tPT @-> tPRP @-> tPRP)
|}
        ~fields:[
        ]
        ~params:[
          param "ptS" ~type_:"point";
          param "ptT" ~type_:"point";
          param "pt1" ~type_:"point";
          param "prepath" ~type_:"prepath";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
make_prepath (prepath |> PrePath.bezier_to ptS ptT pt1)
|}
    ; inst "PrePathTerminate"
        ~name:"terminate-path"
        ~type_:{|
~% (tPRP @-> tPATH)
|}
        ~fields:[
        ]
        ~params:[
          param "prepath" ~type_:"prepath";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
make_path ([prepath |> PrePath.terminate])
|}
    ; inst "PrePathCloseWithLine"
        ~name:"close-with-line"
        ~type_:{|
~% (tPRP @-> tPATH)
|}
        ~fields:[
        ]
        ~params:[
          param "prepath" ~type_:"prepath";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
make_path ([prepath |> PrePath.close_with_line])
|}
    ; inst "PrePathCloseWithCubicBezier"
        ~name:"close-with-bezier"
        ~type_:{|
~% (tPT @-> tPT @-> tPRP @-> tPATH)
|}
        ~fields:[
        ]
        ~params:[
          param "ptS" ~type_:"point";
          param "ptT" ~type_:"point";
          param "prepath" ~type_:"prepath";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
make_path ([prepath |> PrePath.close_with_bezier ptS ptT])
|}
    ; inst "HorzConcat"
        ~name:"++"
        ~type_:{|
~% (tIB @-> tIB @-> tIB)
|}
        ~fields:[
        ]
        ~params:[
          param "hblst1" ~type_:"horz";
          param "hblst2" ~type_:"horz";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
make_horz (List.append hblst1 hblst2)
|}
    ; inst "VertConcat"
        ~name:"+++"
        ~type_:{|
~% (tBB @-> tBB @-> tBB)
|}
        ~fields:[
        ]
        ~params:[
          param "vblst1" ~type_:"vert";
          param "vblst2" ~type_:"vert";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
make_vert (List.append vblst1 vblst2)
|}
    ; inst "HorzLex"
        ~name:"read-inline"
        ~type_:{|
~% (tCTX @-> tIT @-> tIB)
|}
        ~fields:[
        ]
        ~params:[
          param "valuectx";
          param "value1";
        ]
        ~is_pdf_mode_primitive:true
        ~code_interp:{|
match value1 with
| InputHorzClosure(imihlst, envi) -> interpret_pdf_mode_intermediate_input_horz envi valuectx imihlst
| _                               -> report_bug_value "HorzLex" value1
|}
        ~code:{|
match value1 with
| CompiledInputHorzClosure(imihlst, envi) -> exec_pdf_mode_intermediate_input_horz envi valuectx imihlst
| _                                       -> report_bug_vm "HorzLex"
|}
    ; inst "VertLex"
        ~name:"read-block"
        ~type_:{|
~% (tCTX @-> tBT @-> tBB)
|}
        ~fields:[
        ]
        ~params:[
          param "valuectx";
          param "value1";
        ]
        ~is_pdf_mode_primitive:true
        ~code_interp:{|
match value1 with
| InputVertClosure(imivlst, envi) -> interpret_pdf_mode_intermediate_input_vert envi valuectx imivlst
| _                               -> report_bug_value "VertLex" value1
|}
        ~code:{|
match value1 with
| CompiledInputVertClosure(imivlst, envi) -> exec_pdf_mode_intermediate_input_vert envi valuectx imivlst
| _                                       -> report_bug_vm "VertLex"
|}
    ; inst "TextHorzLex"
        ~name:"stringify-inline"
        ~type_:{|
~% (tTCTX @-> tIT @-> tS)
|}
        ~fields:[
        ]
        ~params:[
          param "valuetctx";
          param "value1";
        ]
        ~is_text_mode_primitive:true
        ~code_interp:{|
match value1 with
| InputHorzClosure(imihlst, envi) -> interpret_text_mode_intermediate_input_horz envi valuetctx imihlst
| _                               -> report_bug_value "TextHorzLex" value1
|}
        ~code:{|
match value1 with
| CompiledInputHorzClosure(imihlst, envi) -> exec_text_mode_intermediate_input_horz envi valuetctx imihlst
| _                                       -> report_bug_vm "TextHorzLex"
|}
    ; inst "TextVertLex"
        ~name:"stringify-block"
        ~type_:{|
~% (tTCTX @-> tBT @-> tS)
|}
        ~fields:[
        ]
        ~params:[
          param "valuetctx";
          param "value1";
        ]
        ~is_text_mode_primitive:true
        ~code_interp:{|
match value1 with
| InputVertClosure(imivlst, envi) -> interpret_text_mode_intermediate_input_vert envi valuetctx imivlst
| _                               -> report_bug_value "TextVertLex" value1
|}
        ~code:{|
match value1 with
| CompiledInputVertClosure(imivlst, envi) -> exec_text_mode_intermediate_input_vert envi valuetctx imivlst
| _                                       -> report_bug_vm "TextVertLex"
|}
    ; inst "TextDeepenIndent"
        ~name:"deepen-indent"
        ~type_:{|
~% (tI @-> tTCTX @-> tTCTX)
|}
        ~fields:[
        ]
        ~params:[
          param "i" ~type_:"int";
          param "tctx" ~type_:"text_mode_context";
        ]
        ~is_text_mode_primitive:true
        ~code:{|
let tctx = tctx |> TextBackend.deepen_indent i in
BaseConstant(BCTextModeContext(tctx))
|}
    ; inst "TextBreak"
        ~name:"break"
        ~type_:{|
~% (tTCTX @-> tS)
|}
        ~fields:[
        ]
        ~params:[
          param "tctx" ~type_:"text_mode_context";
        ]
        ~is_text_mode_primitive:true
        ~code:{|
let i = TextBackend.get_indent tctx in
let s = "\n" ^ (String.make i ' ') in
make_string s
|}
    ; inst "TextGetInitialTextModeContext"
        ~name:"get-initial-text-info"
        ~type_:{|
~% (tU @-> tTCTX)
|}
        ~fields:[
        ]
        ~params:[
          param "value1";
        ]
        ~is_text_mode_primitive:true
        ~code:{|
match value1 with
| BaseConstant(BCUnit) ->
    let tctx = TextBackend.get_initial_text_mode_context () in
    BaseConstant(BCTextModeContext(tctx))

| _ ->
    report_bug_value "TextGetInitialTextModeContext" value1
|}
    ; inst "PrimitiveEmbeddedVertBreakable"
        ~name:"embed-block-breakable"
        ~type_:{|
~% (tCTX @-> tBB @-> tIB)
|}
        ~fields:[
        ]
        ~params:[
          param "(ctx, _)" ~type_:"context";
          param "vblst" ~type_:"vert";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
let wid = ctx.HorzBox.paragraph_width in
make_horz [HorzEmbeddedVertBreakable(wid, vblst)]
|}
    ; inst "BackendFont"
        ~fields:[
        ]
        ~params:[
          param "abbrev" ~type_:"string";
          param "size_ratio" ~type_:"float";
          param "rising_ratio" ~type_:"float";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
make_font_value (abbrev, size_ratio, rising_ratio)
|}
    ; inst "BackendLineBreaking"
        ~name:"line-break"
        ~type_:{|
~% (tB @-> tB @-> tCTX @-> tIB @-> tBB)
|}
        ~fields:[
        ]
        ~params:[
          param "is_breakable_top" ~type_:"bool";
          param "is_breakable_bottom" ~type_:"bool";
          param "(ctx,  _)" ~type_:"context";
          param "hblst" ~type_:"horz";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
let open HorzBox in
let br_top    = if is_breakable_top then Breakable else Unbreakable in
let br_bottom = if is_breakable_bottom then Breakable else Unbreakable in
let imvblst = LineBreak.main (br_top, ctx.paragraph_top) (br_bottom, ctx.paragraph_bottom) ctx hblst in
make_vert imvblst
|}
    ; inst "BackendPageBreaking"
        ~name:"page-break"
        ~type_:{|
~% (tPG @-> tPAGECONTF @-> tPAGEPARTSF @-> tBB @-> tDOC)
|}
        ~fields:[
        ]
        ~params:[
          param "pagesize" ~type_:"page_size";
          param "valuepagecontf";
          param "valuepagepartsf";
          param "vblst" ~type_:"vert";
        ]
        ~is_pdf_mode_primitive:true
        ~needs_reducef:true
        ~code:{|
let pagecontf = make_page_content_scheme_func reducef valuepagecontf in
let pagepartsf = make_page_parts_scheme_func reducef valuepagepartsf in
BaseConstant(BCDocument(pagesize, SingleColumn, (fun () -> []), (fun () -> []), pagecontf, pagepartsf, vblst))
|}
    ; inst "BackendPageBreakingTwoColumn"
        ~name:"page-break-two-column"
        ~type_:{|
~% (tPG @-> tLN @-> (tU @-> tBB) @-> tPAGECONTF @-> tPAGEPARTSF @-> tBB @-> tDOC)
|}
        ~fields:[
        ]
        ~params:[
          param "pagesize" ~type_:"page_size";
          param "origin_shift" ~type_:"length";
          param "valuecolumnhookf";
          param "valuepagecontf";
          param "valuepagepartsf";
          param "vblst" ~type_:"vert";
        ]
        ~is_pdf_mode_primitive:true
        ~needs_reducef:true
        ~code:{|
let columnhookf = make_column_hook_func reducef valuecolumnhookf in
let pagecontf = make_page_content_scheme_func reducef valuepagecontf in
let pagepartsf = make_page_parts_scheme_func reducef valuepagepartsf in
BaseConstant(BCDocument(pagesize, MultiColumn([origin_shift]), columnhookf, (fun () -> []), pagecontf, pagepartsf, vblst))
|}
    ; inst "BackendPageBreakingMultiColumn"
        ~name:"page-break-multicolumn"
        ~type_:{|
~% (tPG @-> tL tLN @-> (tU @-> tBB) @-> (tU @-> tBB) @-> tPAGECONTF @-> tPAGEPARTSF @-> tBB @-> tDOC)
|}
        ~fields:[
        ]
        ~params:[
          param "pagesize" ~type_:"page_size";
          param "origin_shifts" ~type_:"length_list";
          param "valuecolumnhookf";
          param "valuecolumnendhookf";
          param "valuepagecontf";
          param "valuepagepartsf";
          param "vbs" ~type_:"vert";
        ]
        ~is_pdf_mode_primitive:true
        ~needs_reducef:true
        ~code:{|
let columnhookf = make_column_hook_func reducef valuecolumnhookf in
let columnendhookf = make_column_hook_func reducef valuecolumnendhookf in
let pagecontf = make_page_content_scheme_func reducef valuepagecontf in
let pagepartsf = make_page_parts_scheme_func reducef valuepagepartsf in
BaseConstant(BCDocument(pagesize, MultiColumn(origin_shifts), columnhookf, columnendhookf, pagecontf, pagepartsf, vbs))
|}
    ; inst "BackendVertFrame"
        ~name:"block-frame-breakable"
        ~type_:{|
~% (tCTX @-> tPADS @-> tDECOSET @-> (tCTX @-> tBB) @-> tBB)
|}
        ~fields:[
        ]
        ~params:[
          param "(ctx, ctxsub)" ~type_:"context";
          param "pads" ~type_:"paddings";
          param "(valuedecoS, valuedecoH, valuedecoM, valuedecoT)" ~type_:"decoset";
          param "valuek";
        ]
        ~is_pdf_mode_primitive:true
        ~needs_reducef:true
        ~code:{|
let valuectxsub =
  Context(HorzBox.({ ctx with
    paragraph_width = HorzBox.(ctx.paragraph_width -% pads.paddingL -% pads.paddingR);
  }), ctxsub)
in
let vblst =
  let valuev = reducef valuek [valuectxsub] in
  get_vert valuev
in
let margins =
  HorzBox.{
    margin_top    = Some((Breakable, ctx.paragraph_top));
    margin_bottom = Some((Breakable, ctx.paragraph_bottom));
  }
in
make_vert (HorzBox.([
  VertFrame(margins, pads,
    make_frame_deco reducef valuedecoS,
    make_frame_deco reducef valuedecoH,
    make_frame_deco reducef valuedecoM,
    make_frame_deco reducef valuedecoT,
    ctx.paragraph_width, vblst);
]))
|}
    ; inst "BackendAddFootnote"
        ~name:"add-footnote"
        ~type_:{|
~% (tBB @-> tIB)
|}
        ~fields:[
        ]
        ~params:[
          param "vblst" ~type_:"vert";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
let imvblst = PageBreak.solidify vblst in
make_horz (HorzBox.([HorzPure(PHGFootnote(imvblst))]))
|}
    ; inst "BackendEmbeddedVertTop"
        ~name:"embed-block-top"
        ~type_:{|
~% (tCTX @-> tLN @-> (tCTX @-> tBB) @-> tIB)
|}
        ~fields:[
        ]
        ~params:[
          param "(ctx, ctxsub)" ~type_:"context";
          param "wid" ~type_:"length";
          param "valuek";
        ]
        ~is_pdf_mode_primitive:true
        ~needs_reducef:true
        ~code:{|
let valuectxsub =
  Context(HorzBox.({ ctx with paragraph_width = wid; }), ctxsub)
in
let vblst =
  let valuev = reducef valuek [valuectxsub] in
  get_vert valuev
in
let imvblst = PageBreak.solidify vblst in
let (hgt, dpt) = PageBreak.adjust_to_first_line imvblst in
make_horz (HorzBox.([HorzPure(PHGEmbeddedVert(wid, hgt, dpt, imvblst))]))
|}
    ; inst "BackendVertSkip"
        ~name:"block-skip"
        ~type_:{|
~% (tLN @-> tBB)
|}
        ~fields:[
        ]
        ~params:[
          param "len" ~type_:"length";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
make_vert (HorzBox.([VertFixedBreakable(len)]))
|}
    ; inst "BackendEmbeddedVertBottom"
        ~name:"embed-block-bottom"
        ~type_:{|
~% (tCTX @-> tLN @-> (tCTX @-> tBB) @-> tIB)
|}
        ~fields:[
        ]
        ~params:[
          param "(ctx, ctxsub)" ~type_:"context";
          param "wid" ~type_:"length";
          param "valuek";
        ]
        ~is_pdf_mode_primitive:true
        ~needs_reducef:true
        ~code:{|
let valuectxsub =
  Context(HorzBox.({ ctx with paragraph_width = wid; }), ctxsub)
in
let vblst =
  let valuev = reducef valuek [valuectxsub] in
    get_vert valuev
in
let imvblst = PageBreak.solidify vblst in
let (hgt, dpt) = PageBreak.adjust_to_last_line imvblst in
make_horz (HorzBox.([HorzPure(PHGEmbeddedVert(wid, hgt, dpt, imvblst))]))
|}
    ; inst "BackendLineStackTop"
        ~name:"line-stack-top"
        ~type_:{|
~% ((tL tIB) @-> tIB)
|}
        ~fields:[
        ]
        ~params:[
          param "valuehblstlst";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
let hblstlst = get_list get_horz valuehblstlst in
let (wid, vblst) = make_line_stack hblstlst in
let imvblst = PageBreak.solidify vblst in
let (hgt, dpt) = PageBreak.adjust_to_first_line imvblst in
make_horz (HorzBox.([HorzPure(PHGEmbeddedVert(wid, hgt, dpt, imvblst))]))
|}
    ; inst "BackendLineStackBottom"
        ~name:"line-stack-bottom"
        ~type_:{|
~% ((tL tIB) @-> tIB)
|}
        ~fields:[
        ]
        ~params:[
          param "valuehblstlst";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
let hblstlst = get_list get_horz valuehblstlst in
let (wid, vblst) = make_line_stack hblstlst in
let imvblst = PageBreak.solidify vblst in
let (hgt, dpt) = PageBreak.adjust_to_last_line imvblst in
make_horz (HorzBox.([HorzPure(PHGEmbeddedVert(wid, hgt, dpt, imvblst))]))
|}
    ; inst "PrimitiveGetInitialContext"
        ~name:"get-initial-context"
        ~type_:{|
~% (tLN @-> tICMD tMATH @-> tCTX)
|}
        ~fields:[
        ]
        ~params:[
          param "txtwid" ~type_:"length";
          param "valuecmd";
        ]
        ~is_pdf_mode_primitive:true
        ~needs_reducef:true
        ~code:{|
let ctx = Primitives.get_pdf_mode_initial_context txtwid in
let mcmd = get_math_command_func reducef valuecmd in
let ctcmd = DefaultCodeTextCommand in
let ctxsub =
  {
    math_command = mcmd;
    code_text_command = ctcmd;
    dummy = ();
  }
in
Context(ctx, ctxsub)
|}
    ; inst "PrimitiveSetHyphenMin"
        ~name:"set-hyphen-min"
        ~type_:{|
~% (tI @-> tI @-> tCTX @-> tCTX)
|}
        ~fields:[
        ]
        ~params:[
          param "lmin" ~type_:"int";
          param "rmin" ~type_:"int";
          param "(ctx, ctxsub)" ~type_:"context";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
Context(HorzBox.({ ctx with
  left_hyphen_min = max 0 lmin;
  right_hyphen_min = max 0 rmin;
}), ctxsub)
|}
    ; inst "PrimitiveSetMinGapOfLines"
        ~name:"set-min-gap-of-lines"
        ~type_:{|
~% (tLN @-> tCTX @-> tCTX)
|}
        ~fields:[
        ]
        ~params:[
          param "lengap" ~type_:"length";
          param "(ctx, ctxsub)" ~type_:"context";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
Context(HorzBox.({ ctx with
  min_gap_of_lines = Length.max Length.zero lengap;
}), ctxsub)
|}
    ; inst "PrimitiveSetSpaceRatio"
        ~name:"set-space-ratio"
        ~type_:{|
~% (tFL @-> tFL @-> tFL @-> tCTX @-> tCTX)
|}
        ~fields:[
        ]
        ~params:[
          param "ratio_natural" ~type_:"float";
          param "ratio_shrink" ~type_:"float";
          param "ratio_stretch" ~type_:"float";
          param "(ctx, ctxsub)" ~type_:"context";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
Context(HorzBox.({ ctx with
  space_natural = max 0. ratio_natural;
  space_shrink  = max 0. ratio_shrink;
  space_stretch = max 0. ratio_stretch;
}), ctxsub)
|}
    ; inst "PrimitiveSetAdjacentStretchRatio"
        ~name:"set-adjacent-stretch-ratio"
        ~type_:{|
~% (tFL @-> tCTX @-> tCTX)
|}
        ~fields:[
        ]
        ~params:[
          param "ratio" ~type_:"float";
          param "(ctx, ctxsub)" ~type_:"context";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
Context(HorzBox.({ ctx with
  adjacent_stretch = max 0. ratio;
}), ctxsub)
|}
    ; inst "PrimitiveSetSpaceRatioBetweenScripts"
        ~name:"set-space-ratio-between-scripts"
        ~type_:{|
~% (tFL @-> tFL @-> tFL @-> tSCR @-> tSCR @-> tCTX @-> tCTX)
|}
        ~fields:[
        ]
        ~params:[
          param "ratio_natural" ~type_:"float";
          param "ratio_shrink" ~type_:"float";
          param "ratio_stretch" ~type_:"float";
          param "script1" ~type_:"script";
          param "script2" ~type_:"script";
          param "(ctx, ctxsub)" ~type_:"context";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
Context(HorzBox.({ ctx with
  script_space_map =
    ctx.script_space_map |> CharBasis.ScriptSpaceMap.add
      (script1, script2)
      (max 0. ratio_natural, max 0. ratio_shrink, max 0. ratio_stretch)
}), ctxsub)
|}
    ; inst "PrimitiveGetSpaceRatioBetweenScripts"
        ~name:"get-space-ratio-between-scripts"
        ~type_:{|
~% (tCTX @-> tSCR @-> tSCR @-> tOPT (tPROD [tFL; tFL; tFL]))
|}
        ~fields:[
        ]
        ~params:[
          param "(ctx, _)" ~type_:"context";
          param "script1" ~type_:"script";
          param "script2" ~type_:"script";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
match ctx.script_space_map |> CharBasis.ScriptSpaceMap.find_opt (script1, script2) with
| None ->
    Constructor("None", const_unit)

| Some((r0, r1, r2)) ->
    Constructor("Some", Tuple([
      make_float r0;
      make_float r1;
      make_float r2;
    ]))
|}
    ; inst "PrimitiveSetParagraphMargin"
        ~name:"set-paragraph-margin"
        ~type_:{|
~% (tLN @-> tLN @-> tCTX @-> tCTX)
|}
        ~fields:[
        ]
        ~params:[
          param "lentop" ~type_:"length";
          param "lenbottom" ~type_:"length";
          param "(ctx, ctxsub)" ~type_:"context";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
Context(HorzBox.({ ctx with
  paragraph_top    = lentop;
  paragraph_bottom = lenbottom;
}), ctxsub)
|}
    ; inst "PrimitiveSetParagraphMinAscenderAndDescender"
        ~name:"set-min-paragraph-ascender-and-descender"
        ~type_:{|
~% (tLN @-> tLN @-> tCTX @-> tCTX)
|}
        ~fields:[
        ]
        ~params:[
          param "lenminasc" ~type_:"length";
          param "lenmindesc" ~type_:"length";
          param "(ctx, ctxsub)" ~type_:"context";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
Context(HorzBox.({ ctx with
  min_first_line_ascender = lenminasc;
  min_last_line_descender = lenmindesc;
}), ctxsub)
|}
    ; inst "PrimitiveSetFontSize"
        ~name:"set-font-size"
        ~type_:{|
~% (tLN @-> tCTX @-> tCTX)
|}
        ~fields:[
        ]
        ~params:[
          param "size" ~type_:"length";
          param "(ctx, ctxsub)" ~type_:"context";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
Context(HorzBox.({ ctx with font_size = size; }), ctxsub)
|}
    ; inst "PrimitiveGetFontSize"
        ~name:"get-font-size"
        ~type_:{|
~% (tCTX @-> tLN)
|}
        ~fields:[
        ]
        ~params:[
          param "(ctx, _)" ~type_:"context";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
make_length (ctx.HorzBox.font_size)
|}
    ; inst "PrimitiveSetFont"
        ~name:"set-font"
        ~type_:{|
~% (tSCR @-> tFONT @-> tCTX @-> tCTX)
|}
        ~fields:[
        ]
        ~params:[
          param "script" ~type_:"script";
          param "font_info" ~type_:"font";
          param "(ctx, ctxsub)" ~type_:"context";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
let font_scheme_new = HorzBox.(ctx.font_scheme |> CharBasis.ScriptSchemeMap.add script font_info) in
Context(HorzBox.({ ctx with font_scheme = font_scheme_new; }), ctxsub)
|}
    ; inst "PrimitiveGetFont"
        ~name:"get-font"
        ~type_:{|
~% (tSCR @-> tCTX @-> tFONT)
|}
        ~fields:[
        ]
        ~params:[
          param "script" ~type_:"script";
          param "(ctx, _)" ~type_:"context";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
let fontwr = HorzBox.get_font_with_ratio ctx script in
make_font_value fontwr
|}
    ; inst "PrimitiveSetMathFont"
        ~name:"set-math-font"
        ~type_:{|
~% (tS @-> tCTX @-> tCTX)
|}
        ~fields:[
        ]
        ~params:[
          param "mfabbrev" ~type_:"string";
          param "(ctx, ctxsub)" ~type_:"context";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
Context(HorzBox.({ ctx with math_font = mfabbrev; }), ctxsub)
|}
    ; inst "PrimitiveSetDominantWideScript"
        ~name:"set-dominant-wide-script"
        ~type_:{|
~% (tSCR @-> tCTX @-> tCTX)
|}
        ~fields:[
        ]
        ~params:[
          param "script" ~type_:"script";
          param "(ctx, ctxsub)" ~type_:"context";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
Context(HorzBox.({ ctx with dominant_wide_script = script; }), ctxsub)
|}
    ; inst "PrimitiveGetDominantWideScript"
        ~name:"get-dominant-wide-script"
        ~type_:{|
~% (tCTX @-> tSCR)
|}
        ~fields:[
        ]
        ~params:[
          param "(ctx, _)" ~type_:"context";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
make_script_value ctx.HorzBox.dominant_wide_script
|}
    ; inst "PrimitiveSetDominantNarrowScript"
        ~name:"set-dominant-narrow-script"
        ~type_:{|
~% (tSCR @-> tCTX @-> tCTX)
|}
        ~fields:[
        ]
        ~params:[
          param "script" ~type_:"script";
          param "(ctx, ctxsub)" ~type_:"context";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
Context(HorzBox.({ ctx with dominant_narrow_script = script; }), ctxsub)
|}
    ; inst "PrimitiveGetDominantNarrowScript"
        ~name:"get-dominant-narrow-script"
        ~type_:{|
~% (tCTX @-> tSCR)
|}
        ~fields:[
        ]
        ~params:[
          param "(ctx, _)" ~type_:"context";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
make_script_value ctx.HorzBox.dominant_narrow_script
|}
    ; inst "PrimitiveSetLangSys"
        ~name:"set-language"
        ~type_:{|
~% (tSCR @-> tLANG @-> tCTX @-> tCTX)
|}
        ~fields:[
        ]
        ~params:[
          param "script" ~type_:"script";
          param "langsys" ~type_:"language_system";
          param "(ctx, ctxsub)" ~type_:"context";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
Context(HorzBox.({ ctx with
  langsys_scheme = ctx.langsys_scheme |> CharBasis.ScriptSchemeMap.add script langsys;
}), ctxsub)
|}
    ; inst "PrimitiveGetLangSys"
        ~name:"get-language"
        ~type_:{|
~% (tSCR @-> tCTX @-> tLANG)
|}
        ~fields:[
        ]
        ~params:[
          param "script" ~type_:"script";
          param "(ctx, _)" ~type_:"context";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
let langsys = HorzBox.get_language_system ctx script in
make_language_system_value langsys
|}
    ; inst "PrimitiveSetTextColor"
        ~name:"set-text-color"
        ~type_:{|
~% (tCLR @-> tCTX @-> tCTX)
|}
        ~fields:[
        ]
        ~params:[
          param "color" ~type_:"color";
          param "(ctx, ctxsub)" ~type_:"context";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
Context(HorzBox.({ ctx with text_color = color; }), ctxsub)
|}
    ; inst "PrimitiveGetTextColor"
        ~name:"get-text-color"
        ~type_:{|
~% (tCTX @-> tCLR)
|}
        ~fields:[
        ]
        ~params:[
          param "(ctx, _)" ~type_:"context";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
let color = ctx.HorzBox.text_color in
make_color_value color
|}
    ; inst "PrimitiveSetLeading"
        ~name:"set-leading"
        ~type_:{|
~% (tLN @-> tCTX @-> tCTX)
|}
        ~fields:[
        ]
        ~params:[
          param "len" ~type_:"length";
          param "(ctx, ctxsub)" ~type_:"context";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
Context(HorzBox.({ ctx with leading = len; }), ctxsub)
|}
    ; inst "PrimitiveGetTextWidth"
        ~name:"get-text-width"
        ~type_:{|
~% (tCTX @-> tLN)
|}
        ~fields:[
        ]
        ~params:[
          param "(ctx, _)" ~type_:"context";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
make_length (ctx.HorzBox.paragraph_width)
|}
    ; inst "PrimitiveSetManualRising"
        ~name:"set-manual-rising"
        ~type_:{|
~% (tLN @-> tCTX @-> tCTX)
|}
        ~fields:[
        ]
        ~params:[
          param "rising" ~type_:"length";
          param "(ctx, ctxsub)" ~type_:"context";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
Context(HorzBox.({ ctx with manual_rising = rising; }), ctxsub)
|}
    ; inst "PrimitiveRaise"
        ~name:"raise-inline"
        ~type_:{|
~% (tLN @-> tIB @-> tIB)
|}
        ~fields:[
        ]
        ~params:[
          param "rising" ~type_:"length";
          param "hblst" ~type_:"horz";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
make_horz (HorzBox.([HorzPure(PHGRising(rising, hblst))]))
|}
    ; inst "PrimitiveSetHyphenPenalty"
        ~name:"set-hyphen-penalty"
        ~type_:{|
~% (tI @-> tCTX @-> tCTX)
|}
        ~fields:[
        ]
        ~params:[
          param "pnlty" ~type_:"int";
          param "(ctx, ctxsub)" ~type_:"context";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
Context(HorzBox.({ ctx with hyphen_badness = pnlty; }), ctxsub)
|}
    ; inst "PrimitiveEmbed"
        ~name:"embed-string"
        ~type_:{|
~% (tS @-> tIT)
|}
        ~fields:[
        ]
        ~params:[
          param "str" ~type_:"string";
        ]
        ~is_pdf_mode_primitive:true
        ~is_text_mode_primitive:true
        ~code_interp:{|
InputHorzClosure([ImInputHorzText(str)], env)
|}
        ~code:{|
CompiledInputHorzClosure([CompiledImInputHorzText(str)], env)
|}
    ; inst "PrimitiveExtract"
        ~name:"extract-string"
        ~type_:{|
~% (tIB @-> tS)
|}
        ~fields:[
        ]
        ~params:[
          param "hblst" ~type_:"horz";
        ]
        ~is_pdf_mode_primitive:true
        ~is_text_mode_primitive:true
        ~code:{|
make_string (HorzBox.extract_string hblst)
|}
    ; inst "PrimitiveGetAxisHeight"
        ~name:"get-axis-height"
        ~type_:{|
~% (tCTX @-> tLN)
|}
        ~fields:[
        ]
        ~params:[
          param "(ctx, _)" ~type_:"context";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
let fontsize = ctx.HorzBox.font_size in
let mfabbrev = ctx.HorzBox.math_font in
let hgt = FontInfo.get_axis_height mfabbrev fontsize in
  make_length (hgt)
|}
    ; inst "BackendFixedEmpty"
        ~name:"inline-skip"
        ~type_:{|
~% (tLN @-> tIB)
|}
        ~fields:[
        ]
        ~params:[
          param "wid" ~type_:"length";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
make_horz [HorzBox.HorzPure(HorzBox.PHSFixedEmpty(wid))]
|}
    ; inst "BackendOuterEmpty"
        ~name:"inline-glue"
        ~type_:{|
~% (tLN @-> tLN @-> tLN @-> tIB)
|}
        ~fields:[
        ]
        ~params:[
          param "widnat" ~type_:"length";
          param "widshrink" ~type_:"length";
          param "widstretch" ~type_:"length";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
make_horz [HorzBox.HorzPure(HorzBox.PHSOuterEmpty(widnat, widshrink, widstretch))]
|}
    ; inst "BackendOuterFrame"
        ~name:"inline-frame-outer"
        ~type_:{|
~% (tPADS @-> tDECO @-> tIB @-> tIB)
|}
        ~fields:[
        ]
        ~params:[
          param "pads" ~type_:"paddings";
          param "valuedeco";
          param "hblst" ~type_:"horz";
        ]
        ~is_pdf_mode_primitive:true
        ~needs_reducef:true
        ~code:{|
make_horz ([HorzBox.HorzPure(HorzBox.PHGOuterFrame(
  pads,
  make_frame_deco reducef valuedeco,
  hblst))])
|}
    ; inst "BackendInnerFrame"
        ~name:"inline-frame-inner"
        ~type_:{|
~% (tPADS @-> tDECO @-> tIB @-> tIB)
|}
        ~fields:[
        ]
        ~params:[
          param "pads" ~type_:"paddings";
          param "valuedeco";
          param "hblst" ~type_:"horz";
        ]
        ~is_pdf_mode_primitive:true
        ~needs_reducef:true
        ~code:{|
make_horz ([HorzBox.HorzPure(HorzBox.PHGInnerFrame(
  pads,
  make_frame_deco reducef valuedeco,
  hblst))])
|}
    ; inst "BackendFixedFrame"
        ~name:"inline-frame-fixed"
        ~type_:{|
~% (tLN @-> tPADS @-> tDECO @-> tIB @-> tIB)
|}
        ~fields:[
        ]
        ~params:[
          param "wid" ~type_:"length";
          param "pads" ~type_:"paddings";
          param "valuedeco";
          param "hblst" ~type_:"horz";
        ]
        ~is_pdf_mode_primitive:true
        ~needs_reducef:true
        ~code:{|
make_horz ([HorzBox.HorzPure(HorzBox.PHGFixedFrame(
  pads, wid,
  make_frame_deco reducef valuedeco,
  hblst))])
|}
    ; inst "BackendOuterFrameBreakable"
        ~name:"inline-frame-breakable"
        ~type_:{|
~% (tPADS @-> tDECOSET @-> tIB @-> tIB)
|}
        ~fields:[
        ]
        ~params:[
          param "pads" ~type_:"paddings";
          param "(valuedecoS, valuedecoH, valuedecoM, valuedecoT)" ~type_:"decoset";
          param "hblst" ~type_:"horz";
        ]
        ~is_pdf_mode_primitive:true
        ~needs_reducef:true
        ~code:{|
make_horz ([HorzBox.HorzFrameBreakable(
  pads, Length.zero, Length.zero,
  make_frame_deco reducef valuedecoS,
  make_frame_deco reducef valuedecoH,
  make_frame_deco reducef valuedecoM,
  make_frame_deco reducef valuedecoT,
  hblst
)])
|}
    ; inst "BackendInlineGraphics"
        ~name:"inline-graphics"
        ~type_:{|
~% (tLN @-> tLN @-> tLN @-> tIGR @-> tIB)
|}
        ~fields:[
        ]
        ~params:[
          param "wid" ~type_:"length";
          param "hgt" ~type_:"length";
          param "dpt" ~type_:"length";
          param "valueg";
        ]
        ~is_pdf_mode_primitive:true
        ~needs_reducef:true
        ~code:{|
let graphics = make_inline_graphics reducef valueg in
make_horz (HorzBox.([HorzPure(PHGFixedGraphics(wid, hgt, Length.negate dpt, graphics))]))
|}
    ; inst "BackendInlineGraphicsOuter"
        ~name:"inline-graphics-outer"
        ~type_:{|
~% (tLN @-> tLN @-> tIGRO @-> tIB)
|}
        ~fields:[
        ]
        ~params:[
          param "hgt" ~type_:"length";
          param "dpt" ~type_:"length";
          param "valueg";
        ]
        ~is_pdf_mode_primitive:true
        ~needs_reducef:true
        ~code:{|
let graphics = make_inline_graphics_outer reducef valueg in
make_horz (HorzBox.([HorzPure(PHGOuterFilGraphics(hgt, Length.negate dpt, graphics))]))
|}
    ; inst "BackendScriptGuard"
        ~name:"script-guard"
        ~type_:{|
~% (tSCR @-> tIB @-> tIB)
|}
        ~fields:[
        ]
        ~params:[
          param "script" ~type_:"script";
          param "hblst" ~type_:"horz";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
make_horz (HorzBox.([HorzScriptGuard(script, script, hblst)]))
|}
    ; inst "BackendScriptGuardBoth"
        ~name:"script-guard-both"
        ~type_:{|
~% (tSCR @-> tSCR @-> tIB @-> tIB)
|}
        ~fields:[
        ]
        ~params:[
          param "scriptL" ~type_:"script";
          param "scriptR" ~type_:"script";
          param "hblst" ~type_:"horz";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
make_horz (HorzBox.([HorzScriptGuard(scriptL, scriptR, hblst)]))
|}
    ; inst "BackendGetLeftmostScript"
        ~name:"get-leftmost-script"
        ~type_:{|
~% (tIB @-> tOPT tSCR)
|}
        ~fields:[
        ]
        ~params:[
          param "hblst" ~type_:"horz";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
let scriptopt = LineBreak.get_leftmost_script hblst in
make_option make_script_value scriptopt
|}
    ; inst "BackendGetRightmostScript"
        ~name:"get-rightmost-script"
        ~type_:{|
~% (tIB @-> tOPT tSCR)
|}
        ~fields:[
        ]
        ~params:[
          param "hblst" ~type_:"horz";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
let scriptopt = LineBreak.get_rightmost_script hblst in
make_option make_script_value scriptopt
|}
    ; inst "BackendDiscretionary"
        ~name:"discretionary"
        ~type_:{|
~% (tI @-> tIB @-> tIB @-> tIB @-> tIB)
|}
        ~fields:[
        ]
        ~params:[
          param "pb" ~type_:"int";
          param "hblst0" ~type_:"horz";
          param "hblst1" ~type_:"horz";
          param "hblst2" ~type_:"horz";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
make_horz (HorzBox.([HorzDiscretionary(pb, hblst0, hblst1, hblst2)]))
|}
    ; inst "BackendRegisterCrossReference"
        ~name:"register-cross-reference"
        ~type_:{|
~% (tS @-> tS @-> tU)
|}
        ~fields:[
        ]
        ~params:[
          param "k" ~type_:"string";
          param "v" ~type_:"string";
        ]
        ~is_pdf_mode_primitive:true
        ~is_text_mode_primitive:true
        ~code:{|
CrossRef.register k v;
const_unit
|}
    ; inst "BackendGetCrossReference"
        ~name:"get-cross-reference"
        ~type_:{|
~% (tS @-> (tOPT tS))
|}
        ~fields:[
        ]
        ~params:[
          param "k" ~type_:"string";
        ]
        ~is_pdf_mode_primitive:true
        ~is_text_mode_primitive:true
        ~code:{|
match CrossRef.get k with
| None    -> Constructor("None", const_unit)
| Some(v) -> Constructor("Some", make_string v)
|}
    ; inst "PrimitiveGetNaturalMetrics"
        ~name:"get-natural-metrics"
        ~type_:{|
~% (tIB @-> tPROD [tLN; tLN; tLN])
|}
        ~fields:[
        ]
        ~params:[
          param "hblst" ~type_:"horz";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
let (wid, hgt, dpt) = LineBreak.get_natural_metrics hblst in
Tuple([
  make_length (wid);
  make_length (hgt);
  make_length (Length.negate dpt);
])
|}
    ; inst "PrimitiveGetNaturalLength"
        ~name:"get-natural-length"
        ~type_:{|
~% (tBB @-> tLN)
|}
        ~fields:[
        ]
        ~params:[
          param "vblst" ~type_:"vert";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
let imvblst = PageBreak.solidify vblst in
let (hgt, dpt) = PageBreak.adjust_to_first_line imvblst in
make_length (hgt +% (Length.negate dpt))
|}
    ; inst "PrimitiveDisplayMessage"
        ~name:"display-message"
        ~type_:{|
~% (tS @-> tU)
|}
        ~fields:[
        ]
        ~params:[
          param "str" ~type_:"string";
        ]
        ~is_pdf_mode_primitive:true
        ~is_text_mode_primitive:true
        ~code:{|
print_endline str;
const_unit
|}
    ; inst "PrimitiveListCons"
        ~fields:[
        ]
        ~params:[
          param "valuehd";
          param "valuetl";
        ]
        ~is_pdf_mode_primitive:true
        ~is_text_mode_primitive:true
        ~code:{|
match valuetl with
| List(vlst) -> List(valuehd :: vlst)
| _          -> report_bug_value "PrimitiveListCons" valuetl
|}
    ; inst "PrimitiveSame"
        ~name:"string-same"
        ~type_:{|
~% (tS @-> tS @-> tB)
|}
        ~fields:[
        ]
        ~params:[
          param "str1" ~type_:"string";
          param "str2" ~type_:"string";
        ]
        ~is_pdf_mode_primitive:true
        ~is_text_mode_primitive:true
        ~code:{|
make_bool (String.equal str1 str2)
|}
    ; inst "PrimitiveStringSub"
        ~name:"string-sub"
        ~type_:{|
~% (tS @-> tI @-> tI @-> tS)
|}
        ~fields:[
        ]
        ~params:[
          param "str" ~type_:"string";
          param "pos" ~type_:"int";
          param "wid" ~type_:"int";
        ]
        ~is_pdf_mode_primitive:true
        ~is_text_mode_primitive:true
        ~code:{|
let resstr =
  try BatUTF8.sub str pos wid with
  | Invalid_argument(s) -> report_dynamic_error "illegal index for string-sub"
in
make_string resstr
|}
    ; inst "PrimitiveStringSubBytes"
        ~name:"string-sub-bytes"
        ~type_:{|
~% (tS @-> tI @-> tI @-> tS)
|}
        ~fields:[
        ]
        ~params:[
          param "str" ~type_:"string";
          param "pos" ~type_:"int";
          param "wid" ~type_:"int";
        ]
        ~is_pdf_mode_primitive:true
        ~is_text_mode_primitive:true
        ~code:{|
let resstr =
  try String.sub str pos wid with
  | Invalid_argument(s) -> report_dynamic_error "illegal index for string-sub-bytes"
in
make_string resstr
|}
    ; inst "PrimitiveStringLength"
        ~name:"string-length"
        ~type_:{|
~% (tS @-> tI)
|}
        ~fields:[
        ]
        ~params:[
          param "str" ~type_:"string";
        ]
        ~is_pdf_mode_primitive:true
        ~is_text_mode_primitive:true
        ~code:{|
make_int (BatUTF8.length str)
|}
    ; inst "PrimitiveStringByteLength"
        ~name:"string-byte-length"
        ~type_:{|
~% (tS @-> tI)
|}
        ~fields:[
        ]
        ~params:[
          param "str" ~type_:"string";
        ]
        ~is_pdf_mode_primitive:true
        ~is_text_mode_primitive:true
        ~code:{|
make_int (String.length str)
|}
    ; inst "PrimitiveStringScan"
        ~name:"string-scan"
        ~type_:{|
~% (tRE @-> tS @-> tOPT (tPROD [tS; tS]))
|}
        ~fields:[
        ]
        ~params:[
          param "pat" ~type_:"regexp";
          param "str" ~type_:"string";
        ]
        ~is_pdf_mode_primitive:true
        ~is_text_mode_primitive:true
        ~code:{|
if Str.string_match pat str 0 then
  let matched = Str.matched_string str in
  let start   = String.length matched in
  let rest    = String.sub str start (String.length str - start) in
  Constructor("Some", Tuple([make_string matched; make_string rest]))
else
  Constructor("None", const_unit)
|}
    ; inst "PrimitiveStringUnexplode"
        ~name:"string-unexplode"
        ~type_:{|
~% ((tL tI) @-> tS)
|}
        ~fields:[
        ]
        ~params:[
          param "valueilst";
        ]
        ~is_pdf_mode_primitive:true
        ~is_text_mode_primitive:true
        ~code:{|
let ilst = get_list get_int valueilst in
let s = (List.map Uchar.of_int ilst) |> InternalText.of_uchar_list |> InternalText.to_utf8 in
make_string s
|}
    ; inst "PrimitiveStringExplode"
        ~name:"string-explode"
        ~type_:{|
~% (tS @-> (tL tI))
|}
        ~fields:[
        ]
        ~params:[
          param "str" ~type_:"string";
        ]
        ~is_pdf_mode_primitive:true
        ~is_text_mode_primitive:true
        ~code:{|
let ilst =
  str
  |> InternalText.of_utf8
  |> InternalText.to_uchar_list
  |> List.map Uchar.to_int
in
make_list make_int ilst
|}
    ; inst "PrimitiveRegExpOfString"
        ~name:"regexp-of-string"
        ~type_:{|
~% (tS @-> tRE)
|}
        ~fields:[
        ]
        ~params:[
          param "str" ~type_:"string";
        ]
        ~is_pdf_mode_primitive:true
        ~is_text_mode_primitive:true
        ~code:{|
let regexp =
  try Str.regexp str with
  | Failure(msg) -> report_dynamic_error ("regexp-of-string: " ^ msg)
in
make_regexp regexp
|}
    ; inst "PrimitiveStringMatch"
        ~name:"string-match"
        ~type_:{|
~% (tRE @-> tS @-> tB)
|}
        ~fields:[
        ]
        ~params:[
          param "pat" ~type_:"regexp";
          param "s" ~type_:"string";
        ]
        ~is_pdf_mode_primitive:true
        ~is_text_mode_primitive:true
        ~code:{|
make_bool (Str.string_match pat s 0)
|}
    ; inst "PrimitiveSplitIntoLines"
        ~name:"split-into-lines"
        ~type_:{|
~% (tS @-> (tL (tPROD [tI; tS])))
|}
        ~fields:[
        ]
        ~params:[
          param "s" ~type_:"string";
        ]
        ~is_pdf_mode_primitive:true
        ~is_text_mode_primitive:true
        ~code:{|
let slst = String.split_on_char '\n' s in
let pairlst = slst |> List.map chop_space_indent in
pairlst |> make_list (fun (i, s) -> Tuple([make_int i; make_string s]))
|}
    ; inst "PrimitiveSplitOnRegExp"
        ~name:"split-on-regexp"
        ~type_:{|
~% (tRE @-> tS @-> (tL (tPROD [tI; tS])))
|}
        ~fields:[
        ]
        ~params:[
          param "sep" ~type_:"regexp";
          param "str" ~type_:"string";
        ]
        ~is_pdf_mode_primitive:true
        ~is_text_mode_primitive:true
        ~code:{|
let slst = Str.split sep str in
let pairlst = slst |> List.map chop_space_indent in
pairlst |> make_list (fun (i, s) -> Tuple([make_int i; make_string s]))
|}
    ; inst "PrimitiveArabic"
        ~name:"arabic"
        ~type_:{|
~% (tI @-> tS)
|}
        ~fields:[
        ]
        ~params:[
          param "num" ~type_:"int";
        ]
        ~is_pdf_mode_primitive:true
        ~is_text_mode_primitive:true
        ~code:{|
make_string (string_of_int num)
|}
    ; inst "PrimitiveShowFloat"
        ~name:"show-float"
        ~type_:{|
~% (tFL @-> tS)
|}
        ~fields:[
        ]
        ~params:[
          param "fl" ~type_:"float";
        ]
        ~is_pdf_mode_primitive:true
        ~is_text_mode_primitive:true
        ~code:{|
make_string (string_of_float fl)
|}
    ; inst "PrimitiveFloat"
        ~name:"float"
        ~type_:{|
~% (tI @-> tFL)
|}
        ~fields:[
        ]
        ~params:[
          param "ic1" ~type_:"int";
        ]
        ~is_pdf_mode_primitive:true
        ~is_text_mode_primitive:true
        ~code:{|
make_float (float_of_int ic1)
|}
    ; inst "PrimitiveRound"
        ~name:"round"
        ~type_:{|
~% (tFL @-> tI)
|}
        ~fields:[
        ]
        ~params:[
          param "fc1" ~type_:"float";
        ]
        ~is_pdf_mode_primitive:true
        ~is_text_mode_primitive:true
        ~code:{|
make_int (int_of_float fc1)
|}
    ; inst "PrimitiveDrawText"
        ~name:"draw-text"
        ~type_:{|
~% (tPT @-> tIB @-> tGR)
|}
        ~fields:[
        ]
        ~params:[
          param "pt" ~type_:"point";
          param "hblst" ~type_:"horz";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
let (imhblst, _, _) = LineBreak.natural hblst in
let grelem = GraphicD.make_text pt imhblst in
make_graphics grelem
|}
    ; inst "PrimitiveDrawStroke"
        ~name:"stroke"
        ~type_:{|
~% (tLN @-> tCLR @-> tPATH @-> tGR)
|}
        ~fields:[
        ]
        ~params:[
          param "wid" ~type_:"length";
          param "color" ~type_:"color";
          param "pathlst" ~type_:"path_value";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
let grelem = GraphicD.make_stroke wid color pathlst in
make_graphics grelem
|}
    ; inst "PrimitiveDrawFill"
        ~name:"fill"
        ~type_:{|
~% (tCLR @-> tPATH @-> tGR)
|}
        ~fields:[
        ]
        ~params:[
          param "color" ~type_:"color";
          param "pathlst" ~type_:"path_value";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
let grelem = GraphicD.make_fill color pathlst in
make_graphics grelem
|}
    ; inst "PrimitiveDrawDashedStroke"
        ~name:"dashed-stroke"
        ~type_:{|
~% (tLN @-> tDASH @-> tCLR @-> tPATH @-> tGR)
|}
        ~fields:[
        ]
        ~params:[
          param "wid" ~type_:"length";
          param "valuetup3";
          param "color" ~type_:"color";
          param "pathlst" ~type_:"path_value";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
let (len1, len2, len3) = get_tuple3 get_length valuetup3 in
let grelem = GraphicD.make_dashed_stroke wid (len1, len2, len3) color pathlst in
make_graphics grelem
|}
    ; inst "PrimitiveLinearTransformGraphics"
        ~name:"linear-transform-graphics"
        ~type_:{|
~% (tFL @-> tFL @-> tFL @-> tFL @-> tGR @-> tGR)
|}
        ~fields:[
        ]
        ~params:[
          param "a" ~type_:"float";
          param "b" ~type_:"float";
          param "c" ~type_:"float";
          param "d" ~type_:"float";
          param "grelem" ~type_:"graphics_element";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
make_graphics (GraphicD.make_linear_trans (a, b, c, d) grelem)
|}
    ; inst "PrimitiveShiftGraphics"
        ~name:"shift-graphics"
        ~type_:{|
~% (tPT @-> tGR @-> tGR)
|}
        ~fields:[
        ]
        ~params:[
          param "vec" ~type_:"point";
          param "grelem" ~type_:"graphics_element";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
make_graphics (GraphicD.shift_element vec grelem)
|}
    ; inst "PrimtiveGetGraphicsBBox"
        ~name:"get-graphics-bbox"
        ~type_:{|
~% (tGR @-> tPROD [tPT; tPT])
|}
        ~fields:[
        ]
        ~params:[
          param "grelem" ~type_:"graphics_element";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
let (ptmin, ptmax) =
  GraphicD.get_element_bbox (fun (x, y) imhblst ->
    let (wid, hgt, dpt) = HorzBox.get_metrics_of_intermediate_horz_box_list imhblst in
      ((x, y +% dpt), (x +% wid, y +% hgt))
  ) grelem
in
let value1 = make_point_value ptmin in
let value2 = make_point_value ptmax in
Tuple([value1; value2])
|}
    ; inst "Times"
        ~name:"*"
        ~type_:{|
~% (tI @-> tI @-> tI)
|}
        ~fields:[
        ]
        ~params:[
          param "numl" ~type_:"int";
          param "numr" ~type_:"int";
        ]
        ~is_pdf_mode_primitive:true
        ~is_text_mode_primitive:true
        ~code:{|
make_int (numl * numr)
|}
    ; inst "Divides"
        ~name:"/"
        ~type_:{|
~% (tI @-> tI @-> tI)
|}
        ~fields:[
        ]
        ~params:[
          param "numl" ~type_:"int";
          param "numr" ~type_:"int";
        ]
        ~is_pdf_mode_primitive:true
        ~is_text_mode_primitive:true
        ~code:{|
try make_int (numl / numr) with
| Division_by_zero -> report_dynamic_error "division by zero"
|}
    ; inst "Mod"
        ~name:"mod"
        ~type_:{|
~% (tI @-> tI @-> tI)
|}
        ~fields:[
        ]
        ~params:[
          param "numl" ~type_:"int";
          param "numr" ~type_:"int";
        ]
        ~is_pdf_mode_primitive:true
        ~is_text_mode_primitive:true
        ~code:{|
try make_int (numl mod numr) with
| Division_by_zero -> report_dynamic_error "division by zero"
|}
    ; inst "Plus"
        ~name:"+"
        ~type_:{|
~% (tI @-> tI @-> tI)
|}
        ~fields:[
        ]
        ~params:[
          param "numl" ~type_:"int";
          param "numr" ~type_:"int";
        ]
        ~is_pdf_mode_primitive:true
        ~is_text_mode_primitive:true
        ~code:{|
make_int (numl + numr)
|}
    ; inst "Minus"
        ~name:"-"
        ~type_:{|
~% (tI @-> tI @-> tI)
|}
        ~fields:[
        ]
        ~params:[
          param "numl" ~type_:"int";
          param "numr" ~type_:"int";
        ]
        ~is_pdf_mode_primitive:true
        ~is_text_mode_primitive:true
        ~code:{|
make_int (numl - numr)
|}
    ; inst "EqualTo"
        ~name:"=="
        ~type_:{|
~% (tI @-> tI @-> tB)
|}
        ~fields:[
        ]
        ~params:[
          param "numl" ~type_:"int";
          param "numr" ~type_:"int";
        ]
        ~is_pdf_mode_primitive:true
        ~is_text_mode_primitive:true
        ~code:{|
make_bool (numl = numr)
|}
    ; inst "GreaterThan"
        ~name:">"
        ~type_:{|
~% (tI @-> tI @-> tB)
|}
        ~fields:[
        ]
        ~params:[
          param "numl" ~type_:"int";
          param "numr" ~type_:"int";
        ]
        ~is_pdf_mode_primitive:true
        ~is_text_mode_primitive:true
        ~code:{|
make_bool (numl > numr)
|}
    ; inst "LessThan"
        ~name:"<"
        ~type_:{|
~% (tI @-> tI @-> tB)
|}
        ~fields:[
        ]
        ~params:[
          param "numl" ~type_:"int";
          param "numr" ~type_:"int";
        ]
        ~is_pdf_mode_primitive:true
        ~is_text_mode_primitive:true
        ~code:{|
make_bool (numl < numr)
|}
    ; inst "LogicalAnd"
        ~name:"&&"
        ~type_:{|
~% (tB @-> tB @-> tB)
|}
        ~fields:[
        ]
        ~params:[
          param "binl" ~type_:"bool";
          param "binr" ~type_:"bool";
        ]
        ~is_pdf_mode_primitive:true
        ~is_text_mode_primitive:true
        ~code:{|
make_bool (binl && binr)
|}
    ; inst "LogicalOr"
        ~name:"||"
        ~type_:{|
~% (tB @-> tB @-> tB)
|}
        ~fields:[
        ]
        ~params:[
          param "binl" ~type_:"bool";
          param "binr" ~type_:"bool";
        ]
        ~is_pdf_mode_primitive:true
        ~is_text_mode_primitive:true
        ~code:{|
make_bool (binl || binr)
|}
    ; inst "LogicalNot"
        ~name:"not"
        ~type_:{|
~% (tB @-> tB)
|}
        ~fields:[
        ]
        ~params:[
          param "binl" ~type_:"bool";
        ]
        ~is_pdf_mode_primitive:true
        ~is_text_mode_primitive:true
        ~code:{|
make_bool (not binl)
|}
    ; inst "BitShiftRight"
        ~name:">>"
        ~type_:{|
~% (tI @-> tI @-> tI)
|}
        ~fields:[
        ]
        ~params:[
          param "numl" ~type_:"int";
          param "numr" ~type_:"int";
        ]
        ~is_pdf_mode_primitive:true
        ~is_text_mode_primitive:true
        ~code:{|
let bits =
  try numl lsr numr with
  | Invalid_argument(s) -> report_dynamic_error "Bit offset out of bounds for '>>'"
in
make_int bits
|}
    ; inst "BitShiftLeft"
        ~name:"<<"
        ~type_:{|
~% (tI @-> tI @-> tI)
|}
        ~fields:[
        ]
        ~params:[
          param "numl" ~type_:"int";
          param "numr" ~type_:"int";
        ]
        ~is_pdf_mode_primitive:true
        ~is_text_mode_primitive:true
        ~code:{|
let bits =
  try numl lsl numr with
  | Invalid_argument(s) -> report_dynamic_error "Bit offset out of bounds for '<<'"
in
make_int bits
|}
    ; inst "BitXor"
        ~name:"bxor"
        ~type_:{|
~% (tI @-> tI @-> tI)
|}
        ~fields:[
        ]
        ~params:[
          param "numl" ~type_:"int";
          param "numr" ~type_:"int";
        ]
        ~is_pdf_mode_primitive:true
        ~is_text_mode_primitive:true
        ~code:{|
make_int (numl lxor numr)
|}
    ; inst "BitAnd"
        ~name:"band"
        ~type_:{|
~% (tI @-> tI @-> tI)
|}
        ~fields:[
        ]
        ~params:[
          param "numl" ~type_:"int";
          param "numr" ~type_:"int";
        ]
        ~is_pdf_mode_primitive:true
        ~is_text_mode_primitive:true
        ~code:{|
make_int (numl land numr)
|}
    ; inst "BitOr"
        ~name:"bor"
        ~type_:{|
~% (tI @-> tI @-> tI)
|}
        ~fields:[
        ]
        ~params:[
          param "numl" ~type_:"int";
          param "numr" ~type_:"int";
        ]
        ~is_pdf_mode_primitive:true
        ~is_text_mode_primitive:true
        ~code:{|
make_int (numl lor numr)
|}
    ; inst "BitNot"
        ~name:"bnot"
        ~type_:{|
~% (tI @-> tI)
|}
        ~fields:[
        ]
        ~params:[
          param "num" ~type_:"int";
        ]
        ~is_pdf_mode_primitive:true
        ~is_text_mode_primitive:true
        ~code:{|
make_int (lnot num)
|}
    ; inst "FloatPlus"
        ~name:"+."
        ~type_:{|
~% (tFL @-> tFL @-> tFL)
|}
        ~fields:[
        ]
        ~params:[
          param "flt1" ~type_:"float";
          param "flt2" ~type_:"float";
        ]
        ~is_pdf_mode_primitive:true
        ~is_text_mode_primitive:true
        ~code:{|
make_float (flt1 +. flt2)
|}
    ; inst "FloatMinus"
        ~name:"-."
        ~type_:{|
~% (tFL @-> tFL @-> tFL)
|}
        ~fields:[
        ]
        ~params:[
          param "flt1" ~type_:"float";
          param "flt2" ~type_:"float";
        ]
        ~is_pdf_mode_primitive:true
        ~is_text_mode_primitive:true
        ~code:{|
make_float (flt1 -. flt2)
|}
    ; inst "FloatTimes"
        ~name:"*."
        ~type_:{|
~% (tFL @-> tFL @-> tFL)
|}
        ~fields:[
        ]
        ~params:[
          param "flt1" ~type_:"float";
          param "flt2" ~type_:"float";
        ]
        ~is_pdf_mode_primitive:true
        ~is_text_mode_primitive:true
        ~code:{|
make_float (flt1 *. flt2)
|}
    ; inst "FloatDivides"
        ~name:"/."
        ~type_:{|
~% (tFL @-> tFL @-> tFL)
|}
        ~fields:[
        ]
        ~params:[
          param "flt1" ~type_:"float";
          param "flt2" ~type_:"float";
        ]
        ~is_pdf_mode_primitive:true
        ~is_text_mode_primitive:true
        ~code:{|
make_float (flt1 /. flt2)
|}
    ; inst "FloatSine"
        ~name:"sin"
        ~type_:{|
~% (tFL @-> tFL)
|}
        ~fields:[
        ]
        ~params:[
          param "flt1" ~type_:"float";
        ]
        ~is_pdf_mode_primitive:true
        ~is_text_mode_primitive:true
        ~code:{|
make_float (sin flt1)
|}
    ; inst "FloatArcSine"
        ~name:"asin"
        ~type_:{|
~% (tFL @-> tFL)
|}
        ~fields:[
        ]
        ~params:[
          param "flt1" ~type_:"float";
        ]
        ~is_pdf_mode_primitive:true
        ~is_text_mode_primitive:true
        ~code:{|
make_float (asin flt1)
|}
    ; inst "FloatCosine"
        ~name:"cos"
        ~type_:{|
~% (tFL @-> tFL)
|}
        ~fields:[
        ]
        ~params:[
          param "flt1" ~type_:"float";
        ]
        ~is_pdf_mode_primitive:true
        ~is_text_mode_primitive:true
        ~code:{|
make_float (cos flt1)
|}
    ; inst "FloatArcCosine"
        ~name:"acos"
        ~type_:{|
~% (tFL @-> tFL)
|}
        ~fields:[
        ]
        ~params:[
          param "flt1" ~type_:"float";
        ]
        ~is_pdf_mode_primitive:true
        ~is_text_mode_primitive:true
        ~code:{|
make_float (acos flt1)
|}
    ; inst "FloatTangent"
        ~name:"tan"
        ~type_:{|
~% (tFL @-> tFL)
|}
        ~fields:[
        ]
        ~params:[
          param "flt1" ~type_:"float";
        ]
        ~is_pdf_mode_primitive:true
        ~is_text_mode_primitive:true
        ~code:{|
make_float (tan flt1)
|}
    ; inst "FloatArcTangent"
        ~name:"atan"
        ~type_:{|
~% (tFL @-> tFL)
|}
        ~fields:[
        ]
        ~params:[
          param "flt1" ~type_:"float";
        ]
        ~is_pdf_mode_primitive:true
        ~is_text_mode_primitive:true
        ~code:{|
make_float (atan flt1)
|}
    ; inst "FloatArcTangent2"
        ~name:"atan2"
        ~type_:{|
~% (tFL @-> tFL @-> tFL)
|}
        ~fields:[
        ]
        ~params:[
          param "flt1" ~type_:"float";
          param "flt2" ~type_:"float";
        ]
        ~is_pdf_mode_primitive:true
        ~is_text_mode_primitive:true
        ~code:{|
make_float (atan2 flt1 flt2)
|}
    ; inst "FloatLogarithm"
        ~name:"log"
        ~type_:{|
~% (tFL @-> tFL)
|}
        ~fields:[
        ]
        ~params:[
          param "flt" ~type_:"float";
        ]
        ~is_pdf_mode_primitive:true
        ~is_text_mode_primitive:true
        ~code:{|
make_float (log flt)
|}
    ; inst "FloatExponential"
        ~name:"exp"
        ~type_:{|
~% (tFL @-> tFL)
|}
        ~fields:[
        ]
        ~params:[
          param "flt" ~type_:"float";
        ]
        ~is_pdf_mode_primitive:true
        ~is_text_mode_primitive:true
        ~code:{|
make_float (exp flt)
|}
    ; inst "PrimitiveCeil"
        ~name:"ceil"
        ~type_:{|
~% (tFL @-> tFL)
|}
        ~fields:[
        ]
        ~params:[
          param "fc1" ~type_:"float";
        ]
        ~is_pdf_mode_primitive:true
        ~is_text_mode_primitive:true
        ~code:{|
make_float (ceil fc1)
|}
    ; inst "PrimitiveFloor"
        ~name:"floor"
        ~type_:{|
~% (tFL @-> tFL)
|}
        ~fields:[
        ]
        ~params:[
          param "fc1" ~type_:"float";
        ]
        ~is_pdf_mode_primitive:true
        ~is_text_mode_primitive:true
        ~code:{|
make_float (floor fc1)
|}
    ; inst "LengthPlus"
        ~name:"+'"
        ~type_:{|
~% (tLN @-> tLN @-> tLN)
|}
        ~fields:[
        ]
        ~params:[
          param "len1" ~type_:"length";
          param "len2" ~type_:"length";
        ]
        ~is_pdf_mode_primitive:true
        ~is_text_mode_primitive:true
        ~code:{|
make_length (HorzBox.(len1 +% len2))
|}
    ; inst "LengthMinus"
        ~name:"-'"
        ~type_:{|
~% (tLN @-> tLN @-> tLN)
|}
        ~fields:[
        ]
        ~params:[
          param "len1" ~type_:"length";
          param "len2" ~type_:"length";
        ]
        ~is_pdf_mode_primitive:true
        ~is_text_mode_primitive:true
        ~code:{|
make_length (HorzBox.(len1 -% len2))
|}
    ; inst "LengthTimes"
        ~name:"*'"
        ~type_:{|
~% (tLN @-> tFL @-> tLN)
|}
        ~fields:[
        ]
        ~params:[
          param "len1" ~type_:"length";
          param "flt2" ~type_:"float";
        ]
        ~is_pdf_mode_primitive:true
        ~is_text_mode_primitive:true
        ~code:{|
make_length (HorzBox.(len1 *% flt2))
|}
    ; inst "LengthDivides"
        ~name:"/'"
        ~type_:{|
~% (tLN @-> tLN @-> tFL)
|}
        ~fields:[
        ]
        ~params:[
          param "len1" ~type_:"length";
          param "len2" ~type_:"length";
        ]
        ~is_pdf_mode_primitive:true
        ~is_text_mode_primitive:true
        ~code:{|
make_float (HorzBox.(len1 /% len2))
|}
    ; inst "LengthLessThan"
        ~name:"<'"
        ~type_:{|
~% (tLN @-> tLN @-> tB)
|}
        ~fields:[
        ]
        ~params:[
          param "len1" ~type_:"length";
          param "len2" ~type_:"length";
        ]
        ~is_pdf_mode_primitive:true
        ~is_text_mode_primitive:true
        ~code:{|
make_bool (HorzBox.(len1 <% len2))
|}
    ; inst "LengthGreaterThan"
        ~name:">'"
        ~type_:{|
~% (tLN @-> tLN @-> tB)
|}
        ~fields:[
        ]
        ~params:[
          param "len1" ~type_:"length";
          param "len2" ~type_:"length";
        ]
        ~is_pdf_mode_primitive:true
        ~is_text_mode_primitive:true
        ~code:{|
make_bool (HorzBox.(len2 <% len1))
|}
    ; inst "PrimitiveSetWordBreakPenalty"
        ~name:"set-word-break-penalty"
        ~type_:{|
~% (tI @-> tCTX @-> tCTX)
|}
        ~fields:[
        ]
        ~params:[
          param "pnlty" ~type_:"int";
          param "(ctx, ctxsub)" ~type_:"context";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
Context(HorzBox.{ ctx with
  space_badness = pnlty;
}, ctxsub)
|}
    ; inst "PrimitiveSetEveryWordBreak"
        ~name:"set-every-word-break"
        ~type_:{|
~% (tIB @-> tIB @-> tCTX @-> tCTX)
|}
        ~fields:[
        ]
        ~params:[
          param "hblst1" ~type_:"horz";
          param "hblst2" ~type_:"horz";
          param "(ctx, ctxsub)" ~type_:"context";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
Context(HorzBox.({ ctx with
  before_word_break = hblst1;
  after_word_break = hblst2;
}), ctxsub)
|}
    ; inst "PrimitiveGetEveryWordBreak"
        ~name:"get-every-word-break"
        ~type_:{|
~% (tCTX @-> tPROD [tIB; tIB])
|}
        ~fields:[
        ]
        ~params:[
          param "(ctx, _)" ~type_:"context";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
let hblst1 = ctx.HorzBox.before_word_break in
let hblst2 = ctx.HorzBox.after_word_break in
Tuple([make_horz hblst1; make_horz hblst2])
|}
    ; inst "BackendProbeCrossReference"
        ~name:"probe-cross-reference"
        ~type_:{|
~% (tS @-> (tOPT tS))
|}
        ~fields:[
        ]
        ~params:[
          param "k" ~type_:"string";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
match CrossRef.probe k with
| None    -> Constructor("None", const_unit)
| Some(v) -> Constructor("Some", make_string v)
|}
    ; inst "BackendRegisterDestination"
        ~name:"register-destination"
        ~type_:{|
~% (tS @-> tPT @-> tU)
|}
        ~fields:[
        ]
        ~params:[
          param "name" ~type_:"string";
          param "p" ~type_:"point";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
NamedDest.register name p;
const_unit
|}
    ; inst "BackendRegisterLinkToUri"
        ~name:"register-link-to-uri"
        ~type_:{|
~% (tS @-> tPT @-> tLN @-> tLN @-> tLN @-> (tOPT (tPROD [tLN; tCLR])) @-> tU)
|}
        ~fields:[
        ]
        ~params:[
          param "uri" ~type_:"string";
          param "pt" ~type_:"point";
          param "wid" ~type_:"length";
          param "hgt" ~type_:"length";
          param "dpt" ~type_:"length";
          param "vborderopt";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
let borderopt = get_option (get_pair get_length get_color) vborderopt in
Annotation.register (Annotation.Link(Pdfaction.Uri(uri))) (pt, wid, hgt, dpt) borderopt;
const_unit
|}
    ; inst "BackendRegisterLinkToLocation"
        ~name:"register-link-to-location"
        ~type_:{|
~% (tS @-> tPT @-> tLN @-> tLN @-> tLN @-> (tOPT (tPROD [tLN; tCLR])) @-> tU)
|}
        ~fields:[
        ]
        ~params:[
          param "name" ~type_:"string";
          param "pt" ~type_:"point";
          param "wid" ~type_:"length";
          param "hgt" ~type_:"length";
          param "dpt" ~type_:"length";
          param "vborderopt";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
let borderopt = get_option (get_pair get_length get_color) vborderopt in
let destname = NamedDest.get name in
Annotation.register (Annotation.Link(Pdfaction.GotoName(destname))) (pt, wid, hgt, dpt) borderopt;
const_unit
|}
    ; inst "BackendRegisterOutline"
        ~name:"register-outline"
        ~type_:{|
~% ((tL(tPROD [tI; tS; tS; tB])) @-> tU)
|}
        ~fields:[
        ]
        ~params:[
          param "ol";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
Outline.register (get_list get_outline ol);
const_unit
|}
    ; inst "RegisterDocumentInformationDictionary"
        ~name:"register-document-information"
        ~type_:{|
~% (tDOCINFODIC @-> tU)
|}
        ~fields:[
        ]
        ~params:[
          param "valuedocinfodic";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
let docinfodic = make_doc_info_dictionary valuedocinfodic in
let () = DocumentInformationDictionary.register docinfodic in
const_unit
|}
    ; inst "AbortWithMessage"
        ~name:"abort-with-message"
        ~type_:{|
let tv = (let bid = BoundID.fresh UniversalKind () in PolyBound(bid)) in
~% (tS @-> (~@ tv))
|}
        ~fields:[
        ]
        ~params:[
          param "msg" ~type_:"string";
        ]
        ~is_pdf_mode_primitive:true
        ~is_text_mode_primitive:true
        ~code:{|
raise (report_dynamic_error msg)
|}
    ; inst "LiftString"
        ~name:"lift-string"
        ~type_:{|
~% (tS @-> tCODE tS)
|}
        ~fields:[
        ]
        ~params:[
          param "s" ~type_:"string";
        ]
        ~is_pdf_mode_primitive:true
        ~is_text_mode_primitive:true
        ~code:{|
lift_string_to_code_value s
|}
    ; inst "LiftInt"
        ~name:"lift-int"
        ~type_:{|
~% (tI @-> tCODE tI)
|}
        ~fields:[
        ]
        ~params:[
          param "n" ~type_:"int";
        ]
        ~is_pdf_mode_primitive:true
        ~is_text_mode_primitive:true
        ~code:{|
lift_integer_to_code_value n
|}
    ; inst "LiftFloat"
        ~name:"lift-float"
        ~type_:{|
~% (tFL @-> tCODE tFL)
|}
        ~fields:[
        ]
        ~params:[
          param "r" ~type_:"float";
        ]
        ~is_pdf_mode_primitive:true
        ~is_text_mode_primitive:true
        ~code:{|
lift_float_to_code_value r
|}
    ; inst "LiftLength"
        ~name:"lift-length"
        ~type_:{|
~% (tLN @-> tCODE tLN)
|}
        ~fields:[
        ]
        ~params:[
          param "len" ~type_:"length";
        ]
        ~is_pdf_mode_primitive:true
        ~is_text_mode_primitive:true
        ~code:{|
lift_length_to_code_value len
|}
    ; inst "PrimitiveGetInputPosition"
        ~name:"get-input-position"
        ~type_:{|
~% (tIPOS @-> tPROD [tS; tI; tI])
|}
        ~fields:[
        ]
        ~params:[
          param "ipos" ~type_:"input_position";
        ]
        ~is_pdf_mode_primitive:true
        ~is_text_mode_primitive:true
        ~code:{|
let v1 = make_string ipos.input_file_name in
let v2 = make_int ipos.input_line in
let v3 = make_int ipos.input_column in
Tuple([v1; v2; v3])
|}
    ; inst "ReadFile"
        ~name:"read-file"
        ~type_:{|
~% (tS @-> tL tS)
|}
        ~fields:[
        ]
        ~params:[
          param "relpath" ~type_:"string";
        ]
        ~is_pdf_mode_primitive:true
        ~is_text_mode_primitive:true
        ~code:{|
let parts = Core_kernel.Filename.parts relpath in
begin
  if parts |> List.exists (String.equal "..") then
    report_dynamic_error "cannot access files by using '..'"
  else
    ()
end;
let abspath =
  let jobdir = OptionState.job_directory () in
  Filename.concat jobdir relpath
in
let inc = open_in abspath in
let rec aux lineacc =
  match input_line inc with
  | exception End_of_file -> lineacc |> Alist.to_list
  | line                  -> aux (Alist.extend lineacc line)
in
let lines = aux Alist.empty in
close_in inc;
make_list make_string lines
|}
    ; inst "ClipGraphicsByPath"
        ~name:"clip-graphics-by-path"
        ~type_:{|
~% (tPATH @-> tGR @-> tGR)
|}
        ~fields:[
        ]
        ~params:[
          param "pathlst" ~type_:"path_value";
          param "grelem" ~type_:"graphics_element";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
let grelem = GraphicD.clip_graphics grelem pathlst in
make_graphics grelem
(* Does it work correctly when len(pathlst) > 1 ?? *)
|}
    ])
