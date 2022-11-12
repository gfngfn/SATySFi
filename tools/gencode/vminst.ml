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
    [ inst "PrimitiveConcatString"
        ~name:"^"
        ~type_:Type.(tS @-> tS @-> tS)
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
        ~type_:Type.(tI @-> (tMCCLS @-> tI) @-> tCTX @-> tCTX)
        ~fields:[
        ]
        ~params:[
          param "cp_from" ~type_:"int";
          param "value_selector";
          param "(ctx, ctxsub)" ~type_:"context";
        ]
        ~is_pdf_mode_primitive:true
        ~is_text_mode_primitive:true
        ~needs_reducef:true
        ~code:{|
let uch_from = Uchar.of_int cp_from in
let selector = make_math_variant_char_selector (reducef ~msg:"set-math-variant-char") value_selector in
let ctx =
  HorzBox.({ ctx with
    math_variant_char_map = ctx.math_variant_char_map |> MathVariantCharMap.add uch_from selector;
  })
in
make_context (ctx, ctxsub)
|}
    ; inst "PrimitiveSetMathChar"
        ~name:"set-math-char"
        ~type_:Type.(tI @-> tI @-> tMATHCLS @-> tCTX @-> tCTX)
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
        ~type_:Type.(tCTX @-> tMCCLS @-> tS @-> tS)
        ~fields:[
        ]
        ~params:[
          param "(ctx, ctxsub)" ~type_:"context";
          param "mccls" ~type_:"math_char_class";
          param "s" ~type_:"string";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
let ctx = HorzBox.({ ctx with math_char_class = mccls; }) in
let uchs =
  let uchs = InternalText.to_uchar_list (InternalText.of_utf8 s) in
  uchs |> List.map (fun uch_from ->
    let (_, uch_to) = Context.convert_math_variant_char (ctx, ctxsub) uch_from in
    uch_to
  )
in
make_string (InternalText.to_utf8 (InternalText.of_uchar_list uchs))
|}
    ; inst "PrimitiveSetMathCommand"
        ~name:"set-math-command"
        ~type_:Type.(tICMD tMT @-> tCTX @-> tCTX)
        ~fields:[
        ]
        ~params:[
          param "mcmd" ~type_:"math_command_func";
          param "(ctx, ctxsub)" ~type_:"context";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
Context(ctx, { ctxsub with math_command = mcmd; })
|}
    ; inst "PrimitiveSetCodeTextCommand"
        ~name:"set-code-text-command"
        ~type_:Type.(tICMD tS @-> tCTX @-> tCTX)
        ~fields:[
        ]
        ~params:[
          param "ctcmd" ~type_:"code_text_command_func";
          param "(ctx, ctxsub)" ~type_:"context";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
Context(ctx, { ctxsub with code_text_command = ctcmd; })
|}
    ; inst "PrimitiveGetLeftMathClass"
        ~name:"get-left-math-class"
        ~type_:Type.(tMB @-> tOPT tMATHCLS)
        ~fields:[
        ]
        ~params:[
          param "maths" ~type_:"math_boxes";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
match maths with
| [] ->
    Constructor("None", const_unit)

| math :: _ ->
    let mathcls = Math.get_left_math_kind math in
    make_math_class_option_value mathcls
|}
    ; inst "PrimitiveGetRightMathClass"
        ~name:"get-right-math-class"
        ~type_:Type.(tMB @-> tOPT tMATHCLS)
        ~fields:[
        ]
        ~params:[
          param "maths" ~type_:"math_boxes";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
match List.rev maths with
| [] ->
    Constructor("None", const_unit)

| math :: _ ->
    let mathcls = Math.get_right_math_kind math in
    make_math_class_option_value mathcls
|}
    ; inst "PrimitiveSpaceBetweenMaths"
        ~name:"space-between-maths"
        ~type_:Type.(tCTX @-> tMB @-> tMB @-> tOPT tIB)
        ~fields:[
        ]
        ~params:[
          param "ictx" ~type_:"context";
          param "ms1" ~type_:"math_boxes";
          param "ms2" ~type_:"math_boxes";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
let hb_space_opt = Math.space_between_maths ictx ms1 ms2 in
match hb_space_opt with
| None           -> Constructor("None", const_unit)
| Some(hb_space) -> Constructor("Some", make_inline_boxes [ hb_space ])
|}
    ; inst "PrimitiveMathConcat"
        ~name:"math-concat"
        ~type_:Type.(tMB @-> tMB @-> tMB)
        ~fields:[
        ]
        ~params:[
          param "ms1" ~type_:"math_boxes";
          param "ms2" ~type_:"math_boxes";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
make_math_boxes (List.append ms1 ms2)
|}
    ; inst "PrimitiveMathGroup"
        ~name:"math-group"
        ~type_:Type.(tMATHCLS @-> tMATHCLS @-> tMB @-> tMB)
        ~fields:[
        ]
        ~params:[
          param "left" ~type_:"math_class";
          param "right" ~type_:"math_class";
          param "inner" ~type_:"math_boxes";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
make_math_boxes [ MathBoxGroup{ left; right; inner } ]
|}
    ; inst "PrimitiveMathSup"
        ~name:"math-sup"
        ~type_:Type.(tCTX @-> tMB @-> (tCTX @-> tMB) @-> tMB)
        ~fields:[
        ]
        ~params:[
          param "ictx" ~type_:"context";
          param "base" ~type_:"math_boxes";
          param "value_sup_f";
        ]
        ~is_pdf_mode_primitive:true
        ~needs_reducef:true
        ~code:{|
let value_sup =
  let ictx = Context.(ictx |> enter_script) in
  reducef ~msg:"math-sup" value_sup_f [ Context(ictx) ]
in
let sup = get_math_boxes value_sup in
make_math_boxes [ MathBoxSuperscript{ context = ictx; base; sup } ]
|}
    ; inst "PrimitiveMathSub"
        ~name:"math-sub"
        ~type_:Type.(tCTX @-> tMB @-> (tCTX @-> tMB) @-> tMB)
        ~fields:[
        ]
        ~params:[
          param "ictx" ~type_:"context";
          param "base" ~type_:"math_boxes";
          param "value_sub_f";
        ]
        ~is_pdf_mode_primitive:true
        ~needs_reducef:true
        ~code:{|
let value_sub =
  let ictx = Context.(ictx |> enter_script) in
  reducef ~msg:"math-sup" value_sub_f [ Context(ictx) ]
in
let sub = get_math_boxes value_sub in
make_math_boxes [ MathBoxSubscript{ context = ictx; base; sub } ]
|}
    ; inst "PrimitiveMathFrac"
        ~name:"math-frac"
        ~type_:Type.(tCTX @-> tMB @-> tMB @-> tMB)
        ~fields:[
        ]
        ~params:[
          param "ictx" ~type_:"context";
          param "ms1" ~type_:"math_boxes";
          param "ms2" ~type_:"math_boxes";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
make_math_boxes [ MathBoxFraction{ context = ictx; numerator = ms1; denominator = ms2 } ]
|}
    ; inst "PrimitiveMathRadical"
        ~name:"math-radical"
        ~type_:Type.(tCTX @-> tOPT tMB @-> tMB @-> tMB)
        ~fields:[
        ]
        ~params:[
          param "context" ~type_:"context";
          param "value1mopt";
          param "inner" ~type_:"math_boxes";
        ]
        ~is_pdf_mode_primitive:true
        ~is_text_mode_primitive:true
        ~code:{|
let degree = get_option get_math_boxes value1mopt in
let radical = Primitives.default_radical in  (* TODO: make this changeable *)
make_math_boxes [ MathBoxRadical{ context; radical; degree; inner } ]
|}
    ; inst "PrimitiveMathParen"
        ~name:"math-paren"
        ~type_:Type.(tCTX @-> tPAREN @-> tPAREN @-> tMB @-> tMB)
        ~fields:[
        ]
        ~params:[
          param "context" ~type_:"context";
          param "value_left";
          param "value_right";
          param "inner" ~type_:"math_boxes";
        ]
        ~is_pdf_mode_primitive:true
        ~needs_reducef:true
        ~code:{|
let left = make_paren (reducef ~msg:"math-paren 1") value_left in
let right = make_paren (reducef ~msg:"math-paren 2") value_right in
make_math_boxes [ MathBoxParen{ context; left; right; inner } ]
|}
    ; inst "PrimitiveMathParenWithMiddle"
        ~name:"math-paren-with-middle"
        ~type_:Type.(tCTX @-> tPAREN @-> tPAREN @-> tPAREN @-> tL tMB @-> tMB)
        ~fields:[
        ]
        ~params:[
          param "context" ~type_:"context";
          param "value_parenL";
          param "value_parenR";
          param "value_middle";
          param "value_mss";
        ]
        ~is_pdf_mode_primitive:true
        ~needs_reducef:true
        ~code:{|
let left = make_paren (reducef ~msg:"math-paren-with-middle 1") value_parenL in
let right = make_paren (reducef ~msg:"math-paren-with-middle 2") value_parenR in
let middle = make_paren (reducef ~msg:"math-paren-with-middle 3") value_middle in
let inner = get_list get_math_boxes value_mss in
make_math_boxes [ MathBoxParenWithMiddle{ context; left; right; middle; inner } ]
|}
    ; inst "PrimitiveMathUpper"
        ~name:"math-upper"
        ~type_:Type.(tCTX @-> tMB @-> (tCTX @-> tMB) @-> tMB)
        ~fields:[
        ]
        ~params:[
          param "ictx" ~type_:"context";
          param "base" ~type_:"math_boxes";
          param "value_upper_f";
        ]
        ~is_pdf_mode_primitive:true
        ~needs_reducef:true
        ~code:{|
let value_upper =
  let ictx = Context.(ictx |> enter_script) in
  reducef ~msg:"math-sup" value_upper_f [ Context(ictx) ]
in
let upper = get_math_boxes value_upper in
make_math_boxes [ MathBoxUpperLimit{ context = ictx; base; upper } ]
|}
    ; inst "PrimitiveMathLower"
        ~name:"math-lower"
        ~type_:Type.(tCTX @-> tMB @-> (tCTX @-> tMB) @-> tMB)
        ~fields:[
        ]
        ~params:[
          param "ictx" ~type_:"context";
          param "base" ~type_:"math_boxes";
          param "value_lower_f";
        ]
        ~is_pdf_mode_primitive:true
        ~needs_reducef:true
        ~code:{|
let value_lower =
  let ictx = Context.(ictx |> enter_script) in
  reducef ~msg:"math-sup" value_lower_f [ Context(ictx) ]
in
let lower = get_math_boxes value_lower in
make_math_boxes [ MathBoxLowerLimit{ context = ictx; base; lower } ]
|}
    ; inst "PrimitiveMathChar"
        ~name:"math-char"
        ~type_:Type.(tCTX @-> tMATHCLS @-> tS @-> tMB)
        ~fields:[
        ]
        ~params:[
          param "ictx" ~type_:"context";
          param "mathcls" ~type_:"math_class";
          param "uchs" ~type_:"uchar_list";
        ]
        ~is_pdf_mode_primitive:true
        ~is_text_mode_primitive:true
        ~code:{|
let ma = MathChar{ context = ictx; is_big = false; chars = uchs } in
make_math_boxes [ MathBoxAtom{ kind = mathcls; main = ma } ]
|}
    ; inst "PrimitiveMathBigChar"
        ~name:"math-big-char"
        ~type_:Type.(tCTX @-> tMATHCLS @-> tS @-> tMB)
        ~fields:[
        ]
        ~params:[
          param "ictx" ~type_:"context";
          param "mathcls" ~type_:"math_class";
          param "uchs" ~type_:"uchar_list";
        ]
        ~is_pdf_mode_primitive:true
        ~is_text_mode_primitive:true
        ~code:{|
let ma = MathChar{ context = ictx; is_big = true; chars = uchs } in
make_math_boxes [ MathBoxAtom{ kind = mathcls; main = ma } ]
|}
    ; inst "PrimitiveMathCharWithKern"
        ~name:"math-char-with-kern"
        ~type_:Type.(tCTX @-> tMATHCLS @-> tS @-> mckf @-> mckf @-> tMB)
        ~fields:[
        ]
        ~params:[
          param "ictx" ~type_:"context";
          param "mathcls" ~type_:"math_class";
          param "uchs" ~type_:"uchar_list";
          param "value_left_kern";
          param "value_right_kern";
        ]
        ~is_pdf_mode_primitive:true
        ~is_text_mode_primitive:true
        ~needs_reducef:true
        ~code:{|
let left_kern = make_math_char_kern_func (reducef ~msg:"math-char-with-kern 1") value_left_kern in
let right_kern = make_math_char_kern_func (reducef ~msg:"math-char-with-kern 2") value_right_kern in
let ma = MathCharWithKern{ context = ictx; is_big = false; chars = uchs; left_kern; right_kern } in
make_math_boxes [ MathBoxAtom{ kind = mathcls; main = ma } ]
|}
    ; inst "PrimitiveMathBigCharWithKern"
        ~name:"math-big-char-with-kern"
        ~type_:Type.(tCTX @-> tMATHCLS @-> tS @-> mckf @-> mckf @-> tMB)
        ~fields:[
        ]
        ~params:[
          param "ictx" ~type_:"context";
          param "mathcls" ~type_:"math_class";
          param "uchs" ~type_:"uchar_list";
          param "value_left_kern";
          param "value_right_kern";
        ]
        ~is_pdf_mode_primitive:true
        ~is_text_mode_primitive:true
        ~needs_reducef:true
        ~code:{|
let left_kern = make_math_char_kern_func (reducef ~msg:"math-big-char-with-kern 1") value_left_kern in
let right_kern = make_math_char_kern_func (reducef ~msg:"math-big-char-with-kern 2") value_right_kern in
let ma = MathCharWithKern{ context = ictx; is_big = true; chars = uchs; left_kern; right_kern } in
make_math_boxes [ MathBoxAtom{ kind = mathcls; main = ma } ]
|}
    ; inst "PrimitiveEmbedInlineToMath"
        ~name:"embed-inline-to-math"
        ~type_:Type.(tMATHCLS @-> tIB @-> tMB)
        ~fields:[
        ]
        ~params:[
          param "mathcls" ~type_:"math_class";
          param "ibs" ~type_:"inline_boxes";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
make_math_boxes [ MathBoxAtom{ kind = mathcls; main = MathEmbeddedInline(ibs) } ]
|}
    ; inst "PrimitiveSetMathCharClass"
        ~name:"set-math-char-class"
        ~type_:Type.(tMCCLS @-> tCTX @-> tCTX)
        ~fields:[
        ]
        ~params:[
          param "mccls" ~type_:"math_char_class";
          param "ictx" ~type_:"context";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
let ictx = Context.(ictx |> set_math_char_class mccls) in
Context(ictx)
|}
    ; inst "PrimitiveGetMathCharClass"
        ~name:"get-math-char-class"
        ~type_:Type.(tCTX @-> tMCCLS)
        ~fields:[
        ]
        ~params:[
          param "ictx" ~type_:"context";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
let mccls = Context.(ictx |> math_char_class) in
make_math_char_class mccls
|}
    ; inst "PrimitiveEmbedMath"
        ~name:"embed-math"
        ~type_:Type.(tCTX @-> tMB @-> tIB)
        ~fields:[
        ]
        ~params:[
          param "ictx" ~type_:"context";
          param "mlst" ~type_:"math_boxes";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
let ibs = Math.main ictx mlst in
make_inline_boxes ibs
|}
    ; inst "PrimitiveTabular"
        ~name:"tabular"
        ~type_:Type.((tL (tL tCELL)) @-> ((tL tLN) @-> (tL tLN) @-> tGR) @-> tIB)
        ~fields:[
        ]
        ~params:[
          param "value_tabular";
          param "value_rulesf";
        ]
        ~is_pdf_mode_primitive:true
        ~needs_reducef:true
        ~code:{|
let tabular = get_list (get_list get_cell) value_tabular in
let (imtabular, wids, lens, width, height, depth) = Tabular.main tabular in
let rulesf xs ys =
  let value_xs = make_list make_length xs in
  let value_ys = make_list make_length ys in
  let value = reducef ~msg:"tabular" value_rulesf [ value_xs; value_ys ] in
  get_graphics value
in
make_inline_boxes HorzBox.([ HorzPure(PHGFixedTabular{
  width;
  height;
  depth;
  rows          = imtabular;
  column_widths = wids;
  row_heights   = lens;
  rule_graphics = rulesf;
})])
|}
    ; inst "PrimitiveLoadPdfImage"
        ~name:"load-pdf-image"
        ~type_:Type.(tS @-> tI @-> tIMG)
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
    ; inst "PrimitiveLoadImage"
        ~name:"load-image"
        ~type_:Type.(tS @-> tIMG)
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
    ; inst "PrimitiveUseImageByWidth"
        ~name:"use-image-by-width"
        ~type_:Type.(tIMG @-> tLN @-> tIB)
        ~fields:[
        ]
        ~params:[
          param "key" ~type_:"image";
          param "width" ~type_:"length";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
let height = ImageInfo.get_height_from_width key width in
make_inline_boxes HorzBox.([ HorzPure(PHGFixedImage{ width; height; key }) ])
|}
    ; inst "PrimitiveHookPageBreak"
        ~name:"hook-page-break"
        ~type_:Type.((tPBINFO @-> tPT @-> tU) @-> tIB)
        ~fields:[
        ]
        ~params:[
          param "hookf";
        ]
        ~is_pdf_mode_primitive:true
        ~needs_reducef:true
        ~code:{|
let hookf = make_hook (reducef ~msg:"hook-page-break") hookf in
make_inline_boxes HorzBox.([HorzPure(PHGHookPageBreak(hookf))])
|}
    ; inst "PrimitiveHookPageBreakBlock"
        ~name:"hook-page-break-block"
        ~type_:Type.((tPBINFO @-> tPT @-> tU) @-> tBB)
        ~fields:[
        ]
        ~params:[
          param "hookf";
        ]
        ~is_pdf_mode_primitive:true
        ~needs_reducef:true
        ~code:{|
let hookf = make_hook (reducef ~msg:"hook-page-break-block") hookf in
make_block_boxes HorzBox.([VertHookPageBreak(hookf)])
|}
    ; inst "PrimitiveUnitePath"
        ~name:"unite-path"
        ~type_:Type.(tPATH @-> tPATH @-> tPATH)
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
    ; inst "PrimitiveShiftPath"
        ~name:"shift-path"
        ~type_:Type.(tPT @-> tPATH @-> tPATH)
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
    ; inst "PrimitiveLinearTransformPath"
        ~name:"linear-transform-path"
        ~type_:Type.(tFL @-> tFL @-> tFL @-> tFL @-> tPATH @-> tPATH)
        ~fields:[
        ]
        ~params:[
          param "a" ~type_:"float";
          param "b" ~type_:"float";
          param "c" ~type_:"float";
          param "d" ~type_:"float";
          param "paths" ~type_:"path_value";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
make_path (List.map (linear_transform_path (a, b, c, d)) paths)
|}
    ; inst "PrimitiveGetPathBbox"
        ~name:"get-path-bbox"
        ~type_:Type.(tPATH @-> tPROD [tPT; tPT])
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
    ; inst "PrimitiveStartPath"
        ~name:"start-path"
        ~type_:Type.(tPT @-> tPRP)
        ~fields:[
        ]
        ~params:[
          param "pt0" ~type_:"point";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
make_prepath (PrePath.start pt0)
|}
    ; inst "PrimitiveLineTo"
        ~name:"line-to"
        ~type_:Type.(tPT @-> tPRP @-> tPRP)
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
    ; inst "PrimitiveBezierTo"
        ~name:"bezier-to"
        ~type_:Type.(tPT @-> tPT @-> tPT @-> tPRP @-> tPRP)
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
    ; inst "PrimitiveTerminatePath"
        ~name:"terminate-path"
        ~type_:Type.(tPRP @-> tPATH)
        ~fields:[
        ]
        ~params:[
          param "prepath" ~type_:"prepath";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
make_path ([prepath |> PrePath.terminate])
|}
    ; inst "PrimitiveCloseWithLine"
        ~name:"close-with-line"
        ~type_:Type.(tPRP @-> tPATH)
        ~fields:[
        ]
        ~params:[
          param "prepath" ~type_:"prepath";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
make_path ([prepath |> PrePath.close_with_line])
|}
    ; inst "PrimitiveCloseWithBezier"
        ~name:"close-with-bezier"
        ~type_:Type.(tPT @-> tPT @-> tPRP @-> tPATH)
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
    ; inst "PrimitiveConcatInline"
        ~name:"++"
        ~type_:Type.(tIB @-> tIB @-> tIB)
        ~fields:[
        ]
        ~params:[
          param "ibs1" ~type_:"inline_boxes";
          param "ibs2" ~type_:"inline_boxes";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
make_inline_boxes (List.append ibs1 ibs2)
|}
    ; inst "PrimitiveConcatBlock"
        ~name:"+++"
        ~type_:Type.(tBB @-> tBB @-> tBB)
        ~fields:[
        ]
        ~params:[
          param "bbs1" ~type_:"block_boxes";
          param "bbs2" ~type_:"block_boxes";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
make_block_boxes (List.append bbs1 bbs2)
|}
    ; inst "PrimitiveReadInline"
        ~name:"read-inline"
        ~type_:Type.(tCTX @-> tIT @-> tIB)
        ~fields:[
        ]
        ~params:[
          param "ictx" ~type_:"context";
          param "value1";
        ]
        ~is_pdf_mode_primitive:true
        ~code_interp:{|
let ihvs = get_inline_text value1 in
read_pdf_mode_inline_text ictx ihvs
|}
        ~code:{|
let _ = ictx in
let _ = value1 in
failwith "TODO: PrimitiveReadInline" (*
match value1 with
| CompiledInlineTextClosure(imihlst, envi) -> exec_pdf_mode_intermediate_inline_text envi ictx imihlst
| _                                        -> report_bug_vm "PrimitiveReadInline"
*)
|}
    ; inst "PrimitiveReadBlock"
        ~name:"read-block"
        ~type_:Type.(tCTX @-> tBT @-> tBB)
        ~fields:[
        ]
        ~params:[
          param "valuectx";
          param "value1";
        ]
        ~is_pdf_mode_primitive:true
        ~code_interp:{|
let bts = get_block_text value1 in
read_pdf_mode_block_text valuectx bts
|}
        ~code:{|
match value1 with
| CompiledBlockTextClosure(imivlst, envi) -> exec_pdf_mode_intermediate_block_text envi valuectx imivlst
| _                                       -> report_bug_vm "PrimitiveReadBlock"
|}
    ; inst "PrimitiveReadMath"
        ~name:"read-math"
        ~type_:Type.(tCTX @-> tMT @-> tMB)
        ~fields:[
        ]
        ~params:[
          param "ictx" ~type_:"context";
          param "value1";
        ]
        ~is_pdf_mode_primitive:true
        ~code_interp:{|
let imvs = get_math_text ~msg:"PrimitiveReadMath" value1 in
let mbs = read_pdf_mode_math_text ictx imvs in
make_math_boxes mbs
|}
        ~code:{|
let _ = ictx in
let _ = value1 in
failwith "TODO: PrimitiveReadMath" (*
match value1 with
| CompiledInputMathClosure(imims, envi) -> exec_pdf_mode_intermediate_math_text envi ictx imims
| _                                     -> report_bug_vm "PrimitiveReadMath"
*)
|}
    ; inst "PrimitiveStringifyInline"
        ~name:"stringify-inline"
        ~type_:Type.(tTCTX @-> tIT @-> tS)
        ~fields:[
        ]
        ~params:[
          param "value_tctx";
          param "its" ~type_:"inline_text";
        ]
        ~is_text_mode_primitive:true
        ~code_interp:{|
read_text_mode_inline_text value_tctx its
|}
        ~code:{|
let _ = value_tctx in
let _ = its in
failwith "TODO: PrimitiveStringifyInline" (*
match value1 with
| CompiledInlineTextClosure(imihlst, envi) -> exec_text_mode_intermediate_inline_text envi valuetctx imihlst
| _                                        -> report_bug_vm "PrimitiveStringifyInline"
*)
|}
    ; inst "PrimitiveStringifyBlock"
        ~name:"stringify-block"
        ~type_:Type.(tTCTX @-> tBT @-> tS)
        ~fields:[
        ]
        ~params:[
          param "value_tctx";
          param "bts" ~type_:"block_text";
        ]
        ~is_text_mode_primitive:true
        ~code_interp:{|
read_text_mode_block_text value_tctx bts
|}
        ~code:{|
let _ = value_tctx in
let _ = bts in
failwith "TODO: PrimitiveStringifyBlock" (*
match value1 with
| CompiledBlockTextClosure(imivlst, envi) -> exec_text_mode_intermediate_block_text envi valuetctx imivlst
| _                                       -> report_bug_vm "PrimitiveStringifyBlock"
*)
|}
    ; inst "PrimitiveStringifyMath"
        ~name:"stringify-math"
        ~type_:Type.(tTCTX @-> tMT @-> tS)
        ~fields:[
        ]
        ~params:[
          param "value_tctx";
          param "imvs" ~type_:"math_text ~msg:\"PrimitiveStringifyMath\"";
        ]
        ~is_text_mode_primitive:true
        ~code_interp:{|
read_text_mode_math_text value_tctx imvs
|}
        ~code:{|
let _ = value_tctx in
let _ = imvs in
failwith "TODO: PrimitiveStringifyMath"
|}
    ; inst "PrimitiveDeepenIndent"
        ~name:"deepen-indent"
        ~type_:Type.(tI @-> tTCTX @-> tTCTX)
        ~fields:[
        ]
        ~params:[
          param "i" ~type_:"int";
          param "(tctx, ctxsub)" ~type_:"text_mode_context";
        ]
        ~is_text_mode_primitive:true
        ~code:{|
let tctx = tctx |> TextBackend.deepen_indent i in
make_text_mode_context (tctx, ctxsub)
|}
    ; inst "PrimitiveBreak"
        ~name:"break"
        ~type_:Type.(tTCTX @-> tS)
        ~fields:[
        ]
        ~params:[
          param "(tctx, _)" ~type_:"text_mode_context";
        ]
        ~is_text_mode_primitive:true
        ~code:{|
let i = TextBackend.get_indent tctx in
let s = "\n" ^ (String.make i ' ') in
make_string s
|}
    ; inst "PrimitiveGetInitialTextModeContext"
        ~name:"get-initial-text-info"
        ~type_:Type.(tICMD tMT @-> (tS @-> tOPT tS @-> tOPT tS @-> tS) @-> tTCTX)
        ~fields:[
        ]
        ~params:[
          param "mcmd" ~type_:"math_command_func";
          param "mscriptsf" ~type_:"math_scripts_func";
        ]
        ~is_text_mode_primitive:true
        ~code:{|
let tctx = TextBackend.get_initial_text_mode_context () in
let ctcmd = DefaultCodeTextCommand in
let tctxsub =
  {
    text_mode_math_command      = mcmd;
    text_mode_code_text_command = ctcmd;
    text_mode_math_scripts_func = mscriptsf;
  }
in
make_text_mode_context (tctx, tctxsub)
|}
    ; inst "PrimitiveEmbedBlockBreakable"
        ~name:"embed-block-breakable"
        ~type_:Type.(tCTX @-> tBB @-> tIB)
        ~fields:[
        ]
        ~params:[
          param "(ctx, _)" ~type_:"context";
          param "contents" ~type_:"block_boxes";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
let width = ctx.HorzBox.paragraph_width in
make_inline_boxes [ HorzEmbeddedVertBreakable{ width; contents } ]
|}
    ; inst "PrimitiveLineBreak"
        ~name:"line-break"
        ~type_:Type.(tB @-> tB @-> tCTX @-> tIB @-> tBB)
        ~fields:[
        ]
        ~params:[
          param "is_breakable_top" ~type_:"bool";
          param "is_breakable_bottom" ~type_:"bool";
          param "(ctx,  _)" ~type_:"context";
          param "ibs" ~type_:"inline_boxes";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
let open HorzBox in
let br_top = if is_breakable_top then Breakable else Unbreakable in
let br_bottom = if is_breakable_bottom then Breakable else Unbreakable in
let imbbs = LineBreak.main (br_top, ctx.paragraph_top) (br_bottom, ctx.paragraph_bottom) ctx ibs in
make_block_boxes imbbs
|}
    ; inst "PrimitivePageBreak"
        ~name:"page-break"
        ~type_:Type.(tPROD [tLN; tLN] @-> tPAGECONTF @-> tPAGEPARTSF @-> tBB @-> tDOC)
        ~fields:[
        ]
        ~params:[
          param "pagesize" ~type_:"page_size";
          param "valuepagecontf";
          param "valuepagepartsf";
          param "bbs" ~type_:"block_boxes";
        ]
        ~is_pdf_mode_primitive:true
        ~needs_reducef:true
        ~code:{|
let pagecontf = make_page_content_scheme_func (reducef ~msg:"page-break 1") valuepagecontf in
let pagepartsf = make_page_parts_scheme_func (reducef ~msg:"page-break 2") valuepagepartsf in
BaseConstant(BCDocument(pagesize, SingleColumn, (fun () -> []), (fun () -> []), pagecontf, pagepartsf, bbs))
|}
    ; inst "PrimitivePageBreakTwoColumn"
        ~name:"page-break-two-column"
        ~type_:Type.(tPROD [tLN; tLN] @-> tLN @-> (tU @-> tBB) @-> tPAGECONTF @-> tPAGEPARTSF @-> tBB @-> tDOC)
        ~fields:[
        ]
        ~params:[
          param "pagesize" ~type_:"page_size";
          param "origin_shift" ~type_:"length";
          param "valuecolumnhookf";
          param "valuepagecontf";
          param "valuepagepartsf";
          param "bbs" ~type_:"block_boxes";
        ]
        ~is_pdf_mode_primitive:true
        ~needs_reducef:true
        ~code:{|
let columnhookf = make_column_hook_func (reducef ~msg:"page-break-two-column 1") valuecolumnhookf in
let pagecontf = make_page_content_scheme_func (reducef ~msg:"page-break-two-column 2") valuepagecontf in
let pagepartsf = make_page_parts_scheme_func (reducef ~msg:"page-break-two-column 3") valuepagepartsf in
BaseConstant(BCDocument(pagesize, MultiColumn([origin_shift]), columnhookf, (fun () -> []), pagecontf, pagepartsf, bbs))
|}
    ; inst "PrimitivePageBreakMulticolumn"
        ~name:"page-break-multicolumn"
        ~type_:Type.(tPROD [tLN; tLN] @-> tL tLN @-> (tU @-> tBB) @-> (tU @-> tBB) @-> tPAGECONTF @-> tPAGEPARTSF @-> tBB @-> tDOC)
        ~fields:[
        ]
        ~params:[
          param "pagesize" ~type_:"page_size";
          param "value_origin_shifts";
          param "valuecolumnhookf";
          param "valuecolumnendhookf";
          param "valuepagecontf";
          param "valuepagepartsf";
          param "bbs" ~type_:"block_boxes";
        ]
        ~is_pdf_mode_primitive:true
        ~needs_reducef:true
        ~code:{|
let origin_shifts = get_list get_length value_origin_shifts in
let columnhookf = make_column_hook_func (reducef ~msg:"page-break-multicolumn 1") valuecolumnhookf in
let columnendhookf = make_column_hook_func (reducef ~msg:"page-break-multicolumn 2") valuecolumnendhookf in
let pagecontf = make_page_content_scheme_func (reducef ~msg:"page-break-multicolumn 3") valuepagecontf in
let pagepartsf = make_page_parts_scheme_func (reducef ~msg:"page-break-multicolumn 4") valuepagepartsf in
BaseConstant(BCDocument(pagesize, MultiColumn(origin_shifts), columnhookf, columnendhookf, pagecontf, pagepartsf, bbs))
|}
    ; inst "PrimitiveBlockFrameBreakable"
        ~name:"block-frame-breakable"
        ~type_:Type.(tCTX @-> tPADS @-> tDECOSET @-> (tCTX @-> tBB) @-> tBB)
        ~fields:[
        ]
        ~params:[
          param "(ctx, ctxsub)" ~type_:"context";
          param "pads" ~type_:"paddings";
          param "(value_decoS, value_decoH, value_decoM, value_decoT)" ~type_:"decoset";
          param "value_f";
        ]
        ~is_pdf_mode_primitive:true
        ~needs_reducef:true
        ~code:{|
let ictx =
  (HorzBox.({ ctx with
    paragraph_width = HorzBox.(ctx.paragraph_width -% pads.paddingL -% pads.paddingR);
  }), ctxsub)
in
let bbs =
  let value = reducef ~msg:"block-frame-breakable 0" value_f [ make_context ictx ] in
  get_block_boxes value
in
let margins =
  HorzBox.{
    margin_top    = Some((Breakable, ctx.paragraph_top));
    margin_bottom = Some((Breakable, ctx.paragraph_bottom));
  }
in
make_block_boxes HorzBox.([
  VertFrame(margins, pads,
    make_frame_deco (reducef ~msg:"block-frame-breakable 1") value_decoS,
    make_frame_deco (reducef ~msg:"block-frame-breakable 2") value_decoH,
    make_frame_deco (reducef ~msg:"block-frame-breakable 3") value_decoM,
    make_frame_deco (reducef ~msg:"block-frame-breakable 4") value_decoT,
    ctx.paragraph_width, bbs);
])
|}
    ; inst "PrimitiveAddFootnote"
        ~name:"add-footnote"
        ~type_:Type.(tBB @-> tIB)
        ~fields:[
        ]
        ~params:[
          param "bbs" ~type_:"block_boxes";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
let imbbs = PageBreak.solidify bbs in
make_inline_boxes HorzBox.([ HorzPure(PHGFootnote(imbbs)) ])
|}
    ; inst "PrimitiveEmbeddBlockTop"
        ~name:"embed-block-top"
        ~type_:Type.(tCTX @-> tLN @-> (tCTX @-> tBB) @-> tIB)
        ~fields:[
        ]
        ~params:[
          param "(ctx, ctxsub)" ~type_:"context";
          param "width" ~type_:"length";
          param "value_f";
        ]
        ~is_pdf_mode_primitive:true
        ~needs_reducef:true
        ~code:{|
let ictx = (HorzBox.({ ctx with paragraph_width = width }), ctxsub) in
let bbs =
  let value = reducef ~msg:"embed-block-top" value_f [ make_context ictx ] in
  get_block_boxes value
in
let contents = PageBreak.solidify bbs in
let (height, depth) = PageBreak.adjust_to_first_line contents in
make_inline_boxes HorzBox.([ HorzPure(PHGEmbeddedVert{ width; height; depth; contents }) ])
|}
    ; inst "PrimitiveBlockSkip"
        ~name:"block-skip"
        ~type_:Type.(tLN @-> tBB)
        ~fields:[
        ]
        ~params:[
          param "len" ~type_:"length";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
make_block_boxes HorzBox.([VertFixedBreakable(len)])
|}
    ; inst "PrimitiveEmbedBlockBottom"
        ~name:"embed-block-bottom"
        ~type_:Type.(tCTX @-> tLN @-> (tCTX @-> tBB) @-> tIB)
        ~fields:[
        ]
        ~params:[
          param "(ctx, ctxsub)" ~type_:"context";
          param "width" ~type_:"length";
          param "value_f";
        ]
        ~is_pdf_mode_primitive:true
        ~needs_reducef:true
        ~code:{|
let ictx = (HorzBox.({ ctx with paragraph_width = width }), ctxsub) in
let bbs =
  let value = reducef ~msg:"embed-block-bottom" value_f [ make_context ictx ] in
  get_block_boxes value
in
let contents = PageBreak.solidify bbs in
let (height, depth) = PageBreak.adjust_to_last_line contents in
make_inline_boxes HorzBox.([ HorzPure(PHGEmbeddedVert{ width; height; depth; contents }) ])
|}
    ; inst "PrimitiveLineStackTop"
        ~name:"line-stack-top"
        ~type_:Type.((tL tIB) @-> tIB)
        ~fields:[
        ]
        ~params:[
          param "value_ibss";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
let ibss = get_list get_inline_boxes value_ibss in
let (width, bbs) = make_line_stack ibss in
let contents = PageBreak.solidify bbs in
let (height, depth) = PageBreak.adjust_to_first_line contents in
make_inline_boxes HorzBox.([ HorzPure(PHGEmbeddedVert{ width; height; depth; contents }) ])
|}
    ; inst "PrimitiveLineStackBottom"
        ~name:"line-stack-bottom"
        ~type_:Type.((tL tIB) @-> tIB)
        ~fields:[
        ]
        ~params:[
          param "value_ibss";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
let ibss = get_list get_inline_boxes value_ibss in
let (width, bbs) = make_line_stack ibss in
let contents = PageBreak.solidify bbs in
let (height, depth) = PageBreak.adjust_to_last_line contents in
make_inline_boxes HorzBox.([ HorzPure(PHGEmbeddedVert{ width; height; depth; contents }) ])
|}
    ; inst "PrimitiveGetInitialContext"
        ~name:"get-initial-context"
        ~type_:Type.(tLN @-> tICMD tMT @-> tCTX)
        ~fields:[
        ]
        ~params:[
          param "txtwid" ~type_:"length";
          param "mcmd" ~type_:"math_command_func";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
let ctx = Primitives.get_pdf_mode_initial_context txtwid in
let ctcmd = DefaultCodeTextCommand in
let ctxsub =
  {
    math_command      = mcmd;
    code_text_command = ctcmd;
  }
in
Context(ctx, ctxsub)
|}
    ; inst "PrimitiveSetHyphenMin"
        ~name:"set-hyphen-min"
        ~type_:Type.(tI @-> tI @-> tCTX @-> tCTX)
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
        ~type_:Type.(tLN @-> tCTX @-> tCTX)
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
        ~type_:Type.(tFL @-> tFL @-> tFL @-> tCTX @-> tCTX)
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
        ~type_:Type.(tFL @-> tCTX @-> tCTX)
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
        ~type_:Type.(tFL @-> tFL @-> tFL @-> tSCR @-> tSCR @-> tCTX @-> tCTX)
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
        ~type_:Type.(tCTX @-> tSCR @-> tSCR @-> tOPT (tPROD [tFL; tFL; tFL]))
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
    ; inst "PrimitiveGetMathAxisHeightRatio"
        ~name:"get-math-axis-height-ratio"
        ~type_:Type.(tCTX @-> tFL)
        ~fields:[
        ]
        ~params:[
          param "ictx" ~type_:"context";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
let mathkey = Context.math_font_key_exn ictx in
let mc = FontInfo.get_math_constants mathkey in
make_float (mc.FontFormat.axis_height)
|}
    ; inst "PrimitiveSetParagraphMargin"
        ~name:"set-paragraph-margin"
        ~type_:Type.(tLN @-> tLN @-> tCTX @-> tCTX)
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
        ~type_:Type.(tLN @-> tLN @-> tCTX @-> tCTX)
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
        ~type_:Type.(tLN @-> tCTX @-> tCTX)
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
        ~type_:Type.(tCTX @-> tLN)
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
        ~type_:Type.(tSCR @-> tFONTWR @-> tCTX @-> tCTX)
        ~fields:[
        ]
        ~params:[
          param "script" ~type_:"script";
          param "font_info" ~type_:"font_with_ratio";
          param "(ctx, ctxsub)" ~type_:"context";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
let font_scheme_new = HorzBox.(ctx.font_scheme |> CharBasis.ScriptSchemeMap.add script font_info) in
Context(HorzBox.({ ctx with font_scheme = font_scheme_new; }), ctxsub)
|}
    ; inst "PrimitiveGetFont"
        ~name:"get-font"
        ~type_:Type.(tSCR @-> tCTX @-> tFONTWR)
        ~fields:[
        ]
        ~params:[
          param "script" ~type_:"script";
          param "(ctx, _)" ~type_:"context";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
let fontwr = HorzBox.get_font_with_ratio ctx script in
make_font_with_ratio_value fontwr
|}
    ; inst "PrimitiveSetMathFont"
        ~name:"set-math-font"
        ~type_:Type.(tFONTKEY @-> tCTX @-> tCTX)
        ~fields:[
        ]
        ~params:[
          param "mathkey" ~type_:"font_key";
          param "(ctx, ctxsub)" ~type_:"context";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
Context(HorzBox.({ ctx with math_font_key = Some(mathkey); }), ctxsub)
|}
    ; inst "PrimitiveSetDominantWideScript"
        ~name:"set-dominant-wide-script"
        ~type_:Type.(tSCR @-> tCTX @-> tCTX)
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
        ~type_:Type.(tCTX @-> tSCR)
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
        ~type_:Type.(tSCR @-> tCTX @-> tCTX)
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
        ~type_:Type.(tCTX @-> tSCR)
        ~fields:[
        ]
        ~params:[
          param "(ctx, _)" ~type_:"context";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
make_script_value ctx.HorzBox.dominant_narrow_script
|}
    ; inst "PrimitiveSetLanguage"
        ~name:"set-language"
        ~type_:Type.(tSCR @-> tLANG @-> tCTX @-> tCTX)
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
    ; inst "PrimitiveGetLanguage"
        ~name:"get-language"
        ~type_:Type.(tSCR @-> tCTX @-> tLANG)
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
        ~type_:Type.(tCLR @-> tCTX @-> tCTX)
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
        ~type_:Type.(tCTX @-> tCLR)
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
        ~type_:Type.(tLN @-> tCTX @-> tCTX)
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
        ~type_:Type.(tCTX @-> tLN)
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
        ~type_:Type.(tLN @-> tCTX @-> tCTX)
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
    ; inst "PrimitiveRaiseInline"
        ~name:"raise-inline"
        ~type_:Type.(tLN @-> tIB @-> tIB)
        ~fields:[
        ]
        ~params:[
          param "rising" ~type_:"length";
          param "contents" ~type_:"inline_boxes";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
make_inline_boxes HorzBox.([ HorzPure(PHGRising{ rising; contents }) ])
|}
    ; inst "PrimitiveSetHyphenPenalty"
        ~name:"set-hyphen-penalty"
        ~type_:Type.(tI @-> tCTX @-> tCTX)
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
    ; inst "PrimitiveEmbedString"
        ~name:"embed-string"
        ~type_:Type.(tS @-> tIT)
        ~fields:[
        ]
        ~params:[
          param "str" ~type_:"string";
        ]
        ~is_pdf_mode_primitive:true
        ~is_text_mode_primitive:true
        ~code_interp:{|
InlineTextValue([ InlineTextValueString(str) ])
|}
        ~code:{|
let _ = str in
failwith "TODO: vm, embed-string"
(*
CompiledInlineTextClosure([CompiledImInlineTextText(str)], env)
*)
|}
    ; inst "PrimitiveExtractString"
        ~name:"extract-string"
        ~type_:Type.(tIB @-> tS)
        ~fields:[
        ]
        ~params:[
          param "ibs" ~type_:"inline_boxes";
        ]
        ~is_pdf_mode_primitive:true
        ~is_text_mode_primitive:true
        ~code:{|
make_string (HorzBox.extract_string ibs)
|}
    ; inst "PrimitiveInlineSkip"
        ~name:"inline-skip"
        ~type_:Type.(tLN @-> tIB)
        ~fields:[
        ]
        ~params:[
          param "width" ~type_:"length";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
make_inline_boxes [ HorzBox.HorzPure(HorzBox.PHSFixedEmpty{ width }) ]
|}
    ; inst "PrimitiveInlineGlue"
        ~name:"inline-glue"
        ~type_:Type.(tLN @-> tLN @-> tLN @-> tIB)
        ~fields:[
        ]
        ~params:[
          param "natural" ~type_:"length";
          param "shrinkable" ~type_:"length";
          param "stretchable" ~type_:"length";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
make_inline_boxes HorzBox.([ HorzPure(PHSOuterEmpty{ natural; shrinkable; stretchable }) ])
|}
    ; inst "PrimitiveInlineFrameOuter"
        ~name:"inline-frame-outer"
        ~type_:Type.(tPADS @-> tDECO @-> tIB @-> tIB)
        ~fields:[
        ]
        ~params:[
          param "paddings" ~type_:"paddings";
          param "value_deco";
          param "contents" ~type_:"inline_boxes";
        ]
        ~is_pdf_mode_primitive:true
        ~needs_reducef:true
        ~code:{|
let decoration = make_frame_deco (reducef ~msg:"inline-frame-outer") value_deco in
make_inline_boxes HorzBox.([ HorzPure(PHGOuterFrame{ paddings; decoration; contents }) ])
|}
    ; inst "PrimitiveInlineFrameInner"
        ~name:"inline-frame-inner"
        ~type_:Type.(tPADS @-> tDECO @-> tIB @-> tIB)
        ~fields:[
        ]
        ~params:[
          param "paddings" ~type_:"paddings";
          param "value_deco";
          param "contents" ~type_:"inline_boxes";
        ]
        ~is_pdf_mode_primitive:true
        ~needs_reducef:true
        ~code:{|
let decoration = make_frame_deco (reducef ~msg:"inline-frame-inner") value_deco in
make_inline_boxes HorzBox.([ HorzPure(PHGInnerFrame{ paddings; decoration; contents }) ])
|}
    ; inst "PrimitiveInlineFrameFixed"
        ~name:"inline-frame-fixed"
        ~type_:Type.(tLN @-> tPADS @-> tDECO @-> tIB @-> tIB)
        ~fields:[
        ]
        ~params:[
          param "required_width" ~type_:"length";
          param "paddings" ~type_:"paddings";
          param "value_deco";
          param "contents" ~type_:"inline_boxes";
        ]
        ~is_pdf_mode_primitive:true
        ~needs_reducef:true
        ~code:{|
let decoration = make_frame_deco (reducef ~msg:"inline-frame-fixed") value_deco in
make_inline_boxes HorzBox.([ HorzPure(PHGFixedFrame{ required_width; paddings; decoration; contents }) ])
|}
    ; inst "PrimitiveInlineFrameBreakable"
        ~name:"inline-frame-breakable"
        ~type_:Type.(tPADS @-> tDECOSET @-> tIB @-> tIB)
        ~fields:[
        ]
        ~params:[
          param "paddings" ~type_:"paddings";
          param "(value_decoS, value_decoH, value_decoM, value_decoT)" ~type_:"decoset";
          param "contents" ~type_:"inline_boxes";
        ]
        ~is_pdf_mode_primitive:true
        ~needs_reducef:true
        ~code:{|
make_inline_boxes HorzBox.([ HorzFrameBreakable{
  paddings;
  decoration_standalone = make_frame_deco (reducef ~msg:"inline-frame-breakable 1") value_decoS;
  decoration_head       = make_frame_deco (reducef ~msg:"inline-frame-breakable 2") value_decoH;
  decoration_middle     = make_frame_deco (reducef ~msg:"inline-frame-breakable 3") value_decoM;
  decoration_tail       = make_frame_deco (reducef ~msg:"inline-frame-breakable 4") value_decoT;
  contents;
}])
|}
    ; inst "PrimitiveInlineGraphics"
        ~name:"inline-graphics"
        ~type_:Type.(tLN @-> tLN @-> tLN @-> tIGR @-> tIB)
        ~fields:[
        ]
        ~params:[
          param "width" ~type_:"length";
          param "height" ~type_:"length";
          param "nonneg_depth" ~type_:"length";
          param "value_g";
        ]
        ~is_pdf_mode_primitive:true
        ~needs_reducef:true
        ~code:{|
let depth = Length.negate nonneg_depth in
let graphics = make_inline_graphics (reducef ~msg:"inline-graphics") value_g in
make_inline_boxes HorzBox.([ HorzPure(PHGFixedGraphics{ width; height; depth; graphics }) ])
|}
    ; inst "PrimitiveInlineGraphicsOuter"
        ~name:"inline-graphics-outer"
        ~type_:Type.(tLN @-> tLN @-> tIGRO @-> tIB)
        ~fields:[
        ]
        ~params:[
          param "height" ~type_:"length";
          param "nonneg_depth" ~type_:"length";
          param "value_g";
        ]
        ~is_pdf_mode_primitive:true
        ~needs_reducef:true
        ~code:{|
let depth = Length.negate nonneg_depth in
let graphics = make_inline_graphics_outer (reducef ~msg:"inline-graphics-outer") value_g in
make_inline_boxes HorzBox.([ HorzPure(PHGOuterFilGraphics{ height; depth; graphics }) ])
|}
    ; inst "PrimitiveScriptGuard"
        ~name:"script-guard"
        ~type_:Type.(tSCR @-> tIB @-> tIB)
        ~fields:[
        ]
        ~params:[
          param "script" ~type_:"script";
          param "contents" ~type_:"inline_boxes";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
make_inline_boxes HorzBox.([ HorzScriptGuard{ left = script; right = script; contents } ])
|}
    ; inst "PrimitiveScriptGuardBoth"
        ~name:"script-guard-both"
        ~type_:Type.(tSCR @-> tSCR @-> tIB @-> tIB)
        ~fields:[
        ]
        ~params:[
          param "left" ~type_:"script";
          param "right" ~type_:"script";
          param "contents" ~type_:"inline_boxes";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
make_inline_boxes HorzBox.([ HorzScriptGuard{ left; right; contents } ])
|}
    ; inst "PrimitiveGetLeftmostScript"
        ~name:"get-leftmost-script"
        ~type_:Type.(tIB @-> tOPT tSCR)
        ~fields:[
        ]
        ~params:[
          param "ibs" ~type_:"inline_boxes";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
let script_opt = LineBreak.get_leftmost_script ibs in
make_option make_script_value script_opt
|}
    ; inst "PrimitiveGetRightmostScript"
        ~name:"get-rightmost-script"
        ~type_:Type.(tIB @-> tOPT tSCR)
        ~fields:[
        ]
        ~params:[
          param "ibs" ~type_:"inline_boxes";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
let script_opt = LineBreak.get_rightmost_script ibs in
make_option make_script_value script_opt
|}
    ; inst "PrimitiveDiscretionary"
        ~name:"discretionary"
        ~type_:Type.(tI @-> tIB @-> tIB @-> tIB @-> tIB)
        ~fields:[
        ]
        ~params:[
          param "penalty" ~type_:"int";
          param "no_break" ~type_:"inline_boxes";
          param "pre" ~type_:"inline_boxes";
          param "post" ~type_:"inline_boxes";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
make_inline_boxes HorzBox.([ HorzDiscretionary{ penalty; no_break; pre; post } ])
|}
    ; inst "PrimitiveRegisterCrossReference"
        ~name:"register-cross-reference"
        ~type_:Type.(tS @-> tS @-> tU)
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
    ; inst "PrimitiveGetCrossReference"
        ~name:"get-cross-reference"
        ~type_:Type.(tS @-> (tOPT tS))
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
        ~type_:Type.(tIB @-> tPROD [tLN; tLN; tLN])
        ~fields:[
        ]
        ~params:[
          param "ibs" ~type_:"inline_boxes";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
let (width, height, depth) = LineBreak.get_natural_metrics ibs in
Tuple([
  make_length width;
  make_length height;
  make_length (Length.negate depth);
])
|}
    ; inst "PrimitiveGetNaturalLength"
        ~name:"get-natural-length"
        ~type_:Type.(tBB @-> tLN)
        ~fields:[
        ]
        ~params:[
          param "bbs" ~type_:"block_boxes";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
let imbbs = PageBreak.solidify bbs in
let (hgt, dpt) = PageBreak.adjust_to_first_line imbbs in
make_length (hgt +% (Length.negate dpt))
|}
    ; inst "PrimitiveDisplayMessage"
        ~name:"display-message"
        ~type_:Type.(tS @-> tU)
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
    ; inst "PrimitiveStringSame"
        ~name:"string-same"
        ~type_:Type.(tS @-> tS @-> tB)
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
        ~type_:Type.(tS @-> tI @-> tI @-> tS)
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
  | Invalid_argument(_) -> report_dynamic_error "illegal index for string-sub"
in
make_string resstr
|}
    ; inst "PrimitiveStringSubBytes"
        ~name:"string-sub-bytes"
        ~type_:Type.(tS @-> tI @-> tI @-> tS)
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
  | Invalid_argument(_) -> report_dynamic_error "illegal index for string-sub-bytes"
in
make_string resstr
|}
    ; inst "PrimitiveStringLength"
        ~name:"string-length"
        ~type_:Type.(tS @-> tI)
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
        ~type_:Type.(tS @-> tI)
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
        ~type_:Type.(tRE @-> tS @-> tOPT (tPROD [tS; tS]))
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
        ~type_:Type.((tL tI) @-> tS)
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
        ~type_:Type.(tS @-> (tL tI))
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
    ; inst "PrimitiveRegexpOfString"
        ~name:"regexp-of-string"
        ~type_:Type.(tS @-> tRE)
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
        ~type_:Type.(tRE @-> tS @-> tB)
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
        ~type_:Type.(tS @-> (tL (tPROD [tI; tS])))
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
    ; inst "PrimitiveSplitOnRegexp"
        ~name:"split-on-regexp"
        ~type_:Type.(tRE @-> tS @-> (tL (tPROD [tI; tS])))
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
        ~type_:Type.(tI @-> tS)
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
        ~type_:Type.(tFL @-> tS)
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
        ~type_:Type.(tI @-> tFL)
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
        ~type_:Type.(tFL @-> tI)
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
        ~type_:Type.(tPT @-> tIB @-> tGR)
        ~fields:[
        ]
        ~params:[
          param "pt" ~type_:"point";
          param "ibs" ~type_:"inline_boxes";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
let (imibs, _, _) = LineBreak.natural ibs in
let gr = GraphicD.make_text pt imibs in
make_graphics gr
|}
    ; inst "PrimitiveStroke"
        ~name:"stroke"
        ~type_:Type.(tLN @-> tCLR @-> tPATH @-> tGR)
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
    ; inst "PrimitiveFill"
        ~name:"fill"
        ~type_:Type.(tCLR @-> tPATH @-> tGR)
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
    ; inst "PrimitiveDashedStroke"
        ~name:"dashed-stroke"
        ~type_:Type.(tLN @-> tDASH @-> tCLR @-> tPATH @-> tGR)
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
        ~type_:Type.(tFL @-> tFL @-> tFL @-> tFL @-> tGR @-> tGR)
        ~fields:[
        ]
        ~params:[
          param "a" ~type_:"float";
          param "b" ~type_:"float";
          param "c" ~type_:"float";
          param "d" ~type_:"float";
          param "gr" ~type_:"graphics";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
make_graphics (GraphicD.make_linear_trans (a, b, c, d) gr)
|}
    ; inst "PrimitiveShiftGraphics"
        ~name:"shift-graphics"
        ~type_:Type.(tPT @-> tGR @-> tGR)
        ~fields:[
        ]
        ~params:[
          param "vec" ~type_:"point";
          param "gr" ~type_:"graphics";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
make_graphics (GraphicD.shift vec gr)
|}
    ; inst "PrimitiveGetGraphicsBbox"
        ~name:"get-graphics-bbox"
        ~type_:Type.(tGR @-> tOPT (tPROD [tPT; tPT]))
        ~fields:[
        ]
        ~params:[
          param "gr" ~type_:"graphics";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
let bbox_opt =
  GraphicD.get_bbox (fun (x, y) imibs ->
    let (wid, hgt, dpt) = HorzBox.get_metrics_of_intermediate_horz_box_list imibs in
      ((x, y +% dpt), (x +% wid, y +% hgt))
  ) gr
in
bbox_opt |> make_option (fun (ptmin, ptmax) ->
  let value1 = make_point_value ptmin in
  let value2 = make_point_value ptmax in
  Tuple([value1; value2])
)
|}
    ; inst "PrimitiveTimes"
        ~name:"*"
        ~type_:Type.(tI @-> tI @-> tI)
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
    ; inst "PrimitiveDivides"
        ~name:"/"
        ~type_:Type.(tI @-> tI @-> tI)
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
    ; inst "PrimitiveMod"
        ~name:"mod"
        ~type_:Type.(tI @-> tI @-> tI)
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
    ; inst "PrimitivePlus"
        ~name:"+"
        ~type_:Type.(tI @-> tI @-> tI)
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
    ; inst "PrimitiveMinus"
        ~name:"-"
        ~type_:Type.(tI @-> tI @-> tI)
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
    ; inst "PrimitiveEqualTo"
        ~name:"=="
        ~type_:Type.(tI @-> tI @-> tB)
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
    ; inst "PrimitiveGreaterThan"
        ~name:">"
        ~type_:Type.(tI @-> tI @-> tB)
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
    ; inst "PrimitiveLessThan"
        ~name:"<"
        ~type_:Type.(tI @-> tI @-> tB)
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
    ; inst "PrimitiveLogicalAnd"
        ~name:"&&"
        ~type_:Type.(tB @-> tB @-> tB)
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
    ; inst "PrimitiveLogicalOr"
        ~name:"||"
        ~type_:Type.(tB @-> tB @-> tB)
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
    ; inst "PrimitiveLogicalNot"
        ~name:"not"
        ~type_:Type.(tB @-> tB)
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
    ; inst "PrimitiveBitShiftRight"
        ~name:">>"
        ~type_:Type.(tI @-> tI @-> tI)
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
  | Invalid_argument(_) -> report_dynamic_error "Bit offset out of bounds for '>>'"
in
make_int bits
|}
    ; inst "PrimitiveBitShiftLeft"
        ~name:"<<"
        ~type_:Type.(tI @-> tI @-> tI)
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
  | Invalid_argument(_) -> report_dynamic_error "Bit offset out of bounds for '<<'"
in
make_int bits
|}
    ; inst "PrimitiveBxor"
        ~name:"bxor"
        ~type_:Type.(tI @-> tI @-> tI)
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
    ; inst "PrimitiveBand"
        ~name:"band"
        ~type_:Type.(tI @-> tI @-> tI)
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
    ; inst "PrimitiveBor"
        ~name:"bor"
        ~type_:Type.(tI @-> tI @-> tI)
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
    ; inst "PrimitiveBnot"
        ~name:"bnot"
        ~type_:Type.(tI @-> tI)
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
    ; inst "PrimitiveFloatPlus"
        ~name:"+."
        ~type_:Type.(tFL @-> tFL @-> tFL)
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
    ; inst "PrimitiveFloatMinus"
        ~name:"-."
        ~type_:Type.(tFL @-> tFL @-> tFL)
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
    ; inst "PrimitiveFloatTimes"
        ~name:"*."
        ~type_:Type.(tFL @-> tFL @-> tFL)
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
    ; inst "PrimitiveFloatDivides"
        ~name:"/."
        ~type_:Type.(tFL @-> tFL @-> tFL)
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
    ; inst "PrimitiveSin"
        ~name:"sin"
        ~type_:Type.(tFL @-> tFL)
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
    ; inst "PrimitiveAsin"
        ~name:"asin"
        ~type_:Type.(tFL @-> tFL)
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
    ; inst "PrimitiveCos"
        ~name:"cos"
        ~type_:Type.(tFL @-> tFL)
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
    ; inst "PrimitiveAcos"
        ~name:"acos"
        ~type_:Type.(tFL @-> tFL)
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
    ; inst "PrimitiveTan"
        ~name:"tan"
        ~type_:Type.(tFL @-> tFL)
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
    ; inst "PrimitiveAtan"
        ~name:"atan"
        ~type_:Type.(tFL @-> tFL)
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
    ; inst "PrimitiveAtan2"
        ~name:"atan2"
        ~type_:Type.(tFL @-> tFL @-> tFL)
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
    ; inst "PrimitiveLog"
        ~name:"log"
        ~type_:Type.(tFL @-> tFL)
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
    ; inst "PrimitiveExp"
        ~name:"exp"
        ~type_:Type.(tFL @-> tFL)
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
        ~type_:Type.(tFL @-> tFL)
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
        ~type_:Type.(tFL @-> tFL)
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
    ; inst "PrimitiveLengthPlus"
        ~name:"+'"
        ~type_:Type.(tLN @-> tLN @-> tLN)
        ~fields:[
        ]
        ~params:[
          param "len1" ~type_:"length";
          param "len2" ~type_:"length";
        ]
        ~is_pdf_mode_primitive:true
        ~is_text_mode_primitive:true
        ~code:{|
make_length (len1 +% len2)
|}
    ; inst "PrimitiveLengthMinus"
        ~name:"-'"
        ~type_:Type.(tLN @-> tLN @-> tLN)
        ~fields:[
        ]
        ~params:[
          param "len1" ~type_:"length";
          param "len2" ~type_:"length";
        ]
        ~is_pdf_mode_primitive:true
        ~is_text_mode_primitive:true
        ~code:{|
make_length (len1 -% len2)
|}
    ; inst "PrimitiveLengthTimes"
        ~name:"*'"
        ~type_:Type.(tLN @-> tFL @-> tLN)
        ~fields:[
        ]
        ~params:[
          param "len1" ~type_:"length";
          param "flt2" ~type_:"float";
        ]
        ~is_pdf_mode_primitive:true
        ~is_text_mode_primitive:true
        ~code:{|
make_length (len1 *% flt2)
|}
    ; inst "PrimitiveLengthDivides"
        ~name:"/'"
        ~type_:Type.(tLN @-> tLN @-> tFL)
        ~fields:[
        ]
        ~params:[
          param "len1" ~type_:"length";
          param "len2" ~type_:"length";
        ]
        ~is_pdf_mode_primitive:true
        ~is_text_mode_primitive:true
        ~code:{|
make_float (len1 /% len2)
|}
    ; inst "PrimitiveLengthLessThan"
        ~name:"<'"
        ~type_:Type.(tLN @-> tLN @-> tB)
        ~fields:[
        ]
        ~params:[
          param "len1" ~type_:"length";
          param "len2" ~type_:"length";
        ]
        ~is_pdf_mode_primitive:true
        ~is_text_mode_primitive:true
        ~code:{|
make_bool (len1 <% len2)
|}
    ; inst "PrimitiveLengthGreaterThan"
        ~name:">'"
        ~type_:Type.(tLN @-> tLN @-> tB)
        ~fields:[
        ]
        ~params:[
          param "len1" ~type_:"length";
          param "len2" ~type_:"length";
        ]
        ~is_pdf_mode_primitive:true
        ~is_text_mode_primitive:true
        ~code:{|
make_bool (len2 <% len1)
|}
    ; inst "PrimitiveSetWordBreakPenalty"
        ~name:"set-word-break-penalty"
        ~type_:Type.(tI @-> tCTX @-> tCTX)
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
        ~type_:Type.(tIB @-> tIB @-> tCTX @-> tCTX)
        ~fields:[
        ]
        ~params:[
          param "ibs1" ~type_:"inline_boxes";
          param "ibs2" ~type_:"inline_boxes";
          param "(ctx, ctxsub)" ~type_:"context";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
Context(HorzBox.({ ctx with
  before_word_break = ibs1;
  after_word_break  = ibs2;
}), ctxsub)
|}
    ; inst "PrimitiveGetEveryWordBreak"
        ~name:"get-every-word-break"
        ~type_:Type.(tCTX @-> tPROD [tIB; tIB])
        ~fields:[
        ]
        ~params:[
          param "(ctx, _)" ~type_:"context";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
let ibs1 = ctx.HorzBox.before_word_break in
let ibs2 = ctx.HorzBox.after_word_break in
Tuple([ make_inline_boxes ibs1; make_inline_boxes ibs2 ])
|}
    ; inst "PrimitiveProbeCrossReference"
        ~name:"probe-cross-reference"
        ~type_:Type.(tS @-> (tOPT tS))
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
    ; inst "PrimitiveRegisterDestination"
        ~name:"register-destination"
        ~type_:Type.(tS @-> tPT @-> tU)
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
    ; inst "PrimitiveRegisterLinkToUri"
        ~name:"register-link-to-uri"
        ~type_:Type.(tS @-> tPT @-> tLN @-> tLN @-> tLN @-> (tOPT (tPROD [tLN; tCLR])) @-> tU)
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
    ; inst "PrimitiveRegisterLinkToLocation"
        ~name:"register-link-to-location"
        ~type_:Type.(tS @-> tPT @-> tLN @-> tLN @-> tLN @-> (tOPT (tPROD [tLN; tCLR])) @-> tU)
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
    ; inst "PrimitiveRegisterOutline"
        ~name:"register-outline"
        ~type_:Type.((tL(tPROD [tI; tS; tS; tB])) @-> tU)
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
    ; inst "PrimitiveRegisterDocumentInformation"
        ~name:"register-document-information"
        ~type_:Type.(tDOCINFODIC @-> tU)
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
    ; inst "PrimitiveAbortWithMessage"
        ~name:"abort-with-message"
        ~type_:Type.(forall "a" (fun a -> tS @-> a))
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
    ; inst "PrimitiveLiftString"
        ~name:"lift-string"
        ~type_:Type.(tS @-> tCODE tS)
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
    ; inst "PrimitiveLiftInt"
        ~name:"lift-int"
        ~type_:Type.(tI @-> tCODE tI)
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
    ; inst "PrimitiveLiftFloat"
        ~name:"lift-float"
        ~type_:Type.(tFL @-> tCODE tFL)
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
    ; inst "PrimitiveLiftLength"
        ~name:"lift-length"
        ~type_:Type.(tLN @-> tCODE tLN)
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
    ; inst "PrimitivePrimitiveGetInputPosition"
        ~name:"get-input-position"
        ~type_:Type.(tIPOS @-> tPROD [tS; tI; tI])
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
    ; inst "PrimitiveReadFile"
        ~name:"read-file"
        ~type_:Type.(tS @-> tL tS)
        ~fields:[
        ]
        ~params:[
          param "relpath" ~type_:"string";
        ]
        ~is_pdf_mode_primitive:true
        ~is_text_mode_primitive:true
        ~code:{|
let parts = Core.Filename.parts relpath in
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
    ; inst "PrimitiveClipGraphicsByPath"
        ~name:"clip-graphics-by-path"
        ~type_:Type.(tPATH @-> tGR @-> tGR)
        ~fields:[
        ]
        ~params:[
          param "pathlst" ~type_:"path_value";
          param "gr" ~type_:"graphics";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
make_graphics (GraphicD.make_clip gr pathlst)
  (* Does it work correctly when len(pathlst) > 1 ?? *)
|}
    ; inst "PrimitiveUniteGraphics"
        ~name:"unite-graphics"
        ~type_:Type.(tL tGR @-> tGR)
        ~fields:[
        ]
        ~params:[
          param "value_grs"
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
let grs = get_list get_graphics value_grs in
make_graphics (GraphicD.concat grs)
|}
    ])
