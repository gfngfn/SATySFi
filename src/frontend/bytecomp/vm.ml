
module Types = Types_
open MyUtil
open LengthInterface
open Types

exception ExecError of string

type compiled_nom_input_horz_element =
  | CompiledNomInputHorzText     of string
  | CompiledNomInputHorzEmbedded of instruction list
  | CompiledNomInputHorzContent  of compiled_nom_input_horz_element list * vmenv


let report_bug_vm msg =
  Format.printf "[Bug]@ %s:" msg;
  failwith ("bug: " ^ msg)


let local_get_value env lv off =
  let (_, frames) = env in
    if lv = 0 then
      (List.hd frames).(off)
    else
      (List.nth frames lv).(off)

let local_set_value env lv off value =
  let (_, frames) = env in
    if lv = 0 then
      (List.hd frames).(off) <- value
    else
      (List.nth frames lv).(off) <- value

let vmenv_global env =
  let (global, _) = env in
    global

let newframe env size =
  let (global, local) = env in
    (global, (Array.make size Nil) :: local)

let newframe_recycle env preenv size =
  let (global, local) = env in
    match preenv with
    | (_, prefrm::_) ->
      if size > Array.length prefrm then
        (global, (Array.make size Nil) :: local)
      else
        (global, prefrm::local)
    | _ ->
      (global, (Array.make size Nil) :: local)


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

let get_string (value : syntactic_value) : string =
  match value with
  | StringEmpty       -> ""
  | StringConstant(s) -> s
  | _                 -> report_bug_vm "get_string"

let graphics_of_list value : (HorzBox.intermediate_horz_box list) Graphics.t =
  let rec aux gracc value =
    match value with
    | EndOfList                             -> gracc
    | ListCons(GraphicsValue(grelem), tail) -> aux (Graphics.extend gracc grelem) tail
    | _                                     -> report_bug_vm "make_frame_deco"
  in
    aux Graphics.empty value



let get_paddings (value : syntactic_value) =
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

  | _ -> report_bug_vm "interpret_paddings"


let get_cell (value : syntactic_value) =
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

  | _ -> report_bug_vm "get_cell"


let get_color (value : syntactic_value) : GraphicData.color =
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

    | _ -> report_bug_vm "interpret_color"




let get_decoset (value : syntactic_value) =
  match value with
  | TupleCons(valuedecoS,
              TupleCons(valuedecoH,
                        TupleCons(valuedecoM,
                                  TupleCons(valuedecoT, EndOfTuple)))) ->
    (valuedecoS, valuedecoH, valuedecoM, valuedecoT)

  | _ -> report_bug_vm "interpret_decoset"


let get_font (value : syntactic_value) : HorzBox.font_with_ratio =
  match value with
  | TupleCons(valueabbrev,
              TupleCons(FloatConstant(sizer),
                        TupleCons(FloatConstant(risingr), EndOfTuple))) ->
    let abbrev = get_string valueabbrev in
      (abbrev, sizer, risingr)

  | _ -> report_bug_vm "interpret_font"


let get_input_horz_content env (value : syntactic_value) =
  match value with
  | CompiledInputHorzIntermediate(imihlist) ->
    CompiledInputHorzWithEnvironment(imihlist, env)
  | _ -> report_bug_vm "bad stack top item"


let get_input_vert_content env (value : syntactic_value) =
  match value with
  | CompiledInputVertIntermediate(imivlist) ->
    CompiledInputVertWithEnvironment(imivlist, env)
  | _ -> report_bug_vm "bad stack top item"


let get_language_system (value : syntactic_value) =
  match value with
  | Constructor("Japanese"        , UnitConstant) -> CharBasis.Japanese
  | Constructor("English"         , UnitConstant) -> CharBasis.English
  | Constructor("NoLanguageSystem", UnitConstant) -> CharBasis.NoLanguageSystem
  | _ ->
    report_bug_vm "interpret_language_system"


let get_list getf (value : syntactic_value) =
  let rec aux acc value =
    match value with
    | ListCons(vhead, vtail) -> aux (Alist.extend acc (getf vhead)) vtail
    | EndOfList              -> Alist.to_list acc
    | _                      -> report_bug_vm "get_list"
  in
    aux Alist.empty value

let get_math_char_class (value : syntactic_value) =
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
    report_bug_vm "interpret_math_char_class"


let get_math_class (value : syntactic_value) =
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
    report_bug_vm "interpret_math_class"


let get_option getf (value : syntactic_value) =
  match value with
  | Constructor("None", UnitConstant) -> None
  | Constructor("Some", valuesub)     -> Some(getf valuesub)
  | _                                 -> report_bug_vm "interpret_option"

let get_page_size (value : syntactic_value) : HorzBox.page_size =
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

  | _ -> report_bug_vm "interpret_page"


let get_point (value : syntactic_value) =
  match value with
  | TupleCons(LengthConstant(lenx),
              TupleCons(LengthConstant(leny), EndOfTuple)) -> (lenx, leny)

  | _ -> report_bug_vm "get_point"


let get_script (value : syntactic_value) =
  match value with
  | Constructor("HanIdeographic", UnitConstant) -> CharBasis.HanIdeographic
  | Constructor("Kana"          , UnitConstant) -> CharBasis.HiraganaOrKatakana
  | Constructor("Latin"         , UnitConstant) -> CharBasis.Latin
  | Constructor("Other"         , UnitConstant) -> CharBasis.OtherScript
  | _ ->
    report_bug_vm "interpret_script: not a script value"


let get_uchar_list (value : syntactic_value) =
  match value with
  | StringEmpty       -> []
  | StringConstant(s) -> InternalText.to_uchar_list (InternalText.of_utf8 s)
  | _                 -> report_bug_vm "get_uchar_list"


let make_page_break_info pbinfo =
  let asc =
    Assoc.of_list [
      ("page-number", IntegerConstant(pbinfo.HorzBox.current_page_number));
    ]
  in
    RecordValue(asc)

let make_page_content_info pcinfo =
  make_page_break_info pcinfo  (* temporary *)


let make_length_list lenlst =
  List.fold_right (fun l acc ->
      ListCons(LengthConstant(l), acc)
    ) lenlst EndOfList


let lex_horz_text (ctx : HorzBox.context_main) (s_utf8 : string) : HorzBox.horz_box list =
  let uchlst = InternalText.to_uchar_list (InternalText.of_utf8 s_utf8) in
    HorzBox.([HorzPure(PHCInnerString(ctx, uchlst))])


let make_font_value (abbrev, sizer, risingr) =
  TupleCons(StringConstant(abbrev),
            TupleCons(FloatConstant(sizer),
                      TupleCons(FloatConstant(risingr), EndOfTuple)))


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


let make_script_value script =
  let label =
    match script with
    | CharBasis.HanIdeographic     -> "HanIdeographic"
    | CharBasis.HiraganaOrKatakana -> "Kana"
    | CharBasis.Latin              -> "Latin"
    | CharBasis.OtherScript        -> "OtherScript"
    | _                            -> report_bug_vm "make_script_value"
  in
    Constructor(label, UnitConstant)


let make_language_system_value langsys =
  let label =
    match langsys with
    | CharBasis.Japanese         -> "Japanese"
    | CharBasis.English          -> "English"
    | CharBasis.NoLanguageSystem -> "NoLanguageSystem"
  in
    Constructor(label, UnitConstant)


let make_color_value color =
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

let get_tuple3 getf value =
  match value with
  | TupleCons(v1, TupleCons(v2, TupleCons(v3, EndOfTuple))) ->
    let c1 = getf v1 in
    let c2 = getf v2 in
    let c3 = getf v3 in
      (c1, c2, c3)
  | _ -> report_bug_vm "interpret_tuple3"

let popn stack n =
  let rec iter st n acc =
    if n = 0 then
      (acc, st)
    else
      match st with
      | x :: xs -> iter xs (n-1) (x::acc)
      | [] -> report_bug_vm "stack underflow!"
  in
    iter stack n []

let rec make_hook env (valuehook : syntactic_value) : (HorzBox.page_break_info -> point -> unit) =
  (fun pbinfo (xpos, yposbaseline) ->
     let valuept = TupleCons(LengthConstant(xpos), TupleCons(LengthConstant(yposbaseline), EndOfTuple)) in
     let valuepbinfo = make_page_break_info pbinfo in
     let valueret = exec [valuehook; valuept; valuepbinfo] env [OpApplyT(2)] [] in
       match valueret with
       | UnitConstant -> ()
       | _            -> report_bug_vm "make_hook"
  )


and get_path env c_pathcomplst c_cycleopt =
  let pathelemlst =
    c_pathcomplst |> List.map (function
        | CompiledPathLineTo(ptcode) ->
          let pt = get_point @@ exec [] env ptcode [] in
            GraphicData.LineTo(pt)

        | CompiledPathCubicBezierTo(pt1code, pt2code, ptcode) ->
          let pt1 = get_point @@ exec [] env pt1code [] in
          let pt2 = get_point @@ exec [] env pt2code [] in
          let pt = get_point @@ exec [] env ptcode [] in
            GraphicData.CubicBezierTo(pt1, pt2, pt)
      )
  in
  let closingopt =
    match c_cycleopt with
    | None -> None

    | Some(CompiledPathLineTo(())) -> Some(GraphicData.LineTo(()))

    | Some(CompiledPathCubicBezierTo(pt1code, pt2code, ())) ->
      let pt1 = get_point @@ exec [] env pt1code [] in
      let pt2 = get_point @@ exec [] env pt2code [] in
        Some(GraphicData.CubicBezierTo(pt1, pt2, ()))
  in
    (pathelemlst, closingopt)


and make_page_content_scheme_func env valuef : HorzBox.page_content_scheme_func =
  (fun pbinfo ->
     let valuepbinfo = make_page_break_info pbinfo in
     let valueret = exec [valuef; valuepbinfo] env [OpApplyT(1)] [] in
       match valueret with
       | RecordValue(asc) ->
         begin
           match
             (Assoc.find_opt asc "text-origin",
              Assoc.find_opt asc "text-height")
           with
           | (Some(vTO), Some(LengthConstant(vTHlen))) ->
             HorzBox.({
                 page_content_origin = get_point(vTO);
                 page_content_height = vTHlen;
               })

           | _ -> report_bug_vm "make_page_scheme_func"
         end

       | _ -> report_bug_vm "make_page_scheme_func"
  )


and make_page_parts_scheme_func env valuef : HorzBox.page_parts_scheme_func =
  (fun pcinfo ->
     let valuepcinfo = make_page_content_info pcinfo in
     let valueret = exec [valuef; valuepcinfo] env [OpApplyT(1)] [] in
       match valueret with
       | RecordValue(asc) ->
         begin
           match
             (Assoc.find_opt asc "header-origin",
              Assoc.find_opt asc "header-content",
              Assoc.find_opt asc "footer-origin",
              Assoc.find_opt asc "footer-content")
           with
           | (Some(vHO), Some(Vert(vHCvert)), Some(vFO), Some(Vert(vFCvert))) ->
             HorzBox.({
                 header_origin  = get_point vHO;
                 header_content = PageBreak.solidify vHCvert;
                 footer_origin  = get_point vFO;
                 footer_content = PageBreak.solidify vFCvert;
               })

           | _ -> report_bug_vm "make_page_parts_scheme_func"
         end

       | _ -> report_bug_vm "make_page_parts_scheme_func"
  )


and make_frame_deco env valuedeco =
  (fun (xpos, ypos) wid hgt dpt ->
     let valuepos = TupleCons(LengthConstant(xpos), TupleCons(LengthConstant(ypos), EndOfTuple)) in
     let valuewid = LengthConstant(wid) in
     let valuehgt = LengthConstant(hgt) in
     let valuedpt = LengthConstant(Length.negate dpt) in
     (* -- depth values for users are nonnegative -- *)
     let valueret = exec [valuedeco; valuedpt; valuehgt; valuewid; valuepos] env [OpApplyT(4)] [] in
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
     let valueret = exec [valueparenf; valuecolor; valuefontsize; valuehgtaxis; valuedpt; valuehgt] env [OpApplyT(5)] [] in
       match valueret with
       | TupleCons(Horz(hblst), TupleCons(valuekernf, EndOfTuple)) ->
         let kernf = make_math_kern_func env valuekernf in
           (hblst, kernf)

       | _ ->
         report_bug_vm "make_paren"
  )


and make_math_kern_func env valuekernf : HorzBox.math_kern_func =
  (fun corrhgt ->
    let astcorrhgt = LengthConstant(corrhgt) in
       match exec [valuekernf; astcorrhgt] env [OpApplyT(1)] [] with
       | LengthConstant(valueret) -> valueret
       | _ -> report_bug_vm "bad return value"
  )


and make_math_char_kern_func env valuekernf : HorzBox.math_char_kern_func =
  (fun fontsize ypos ->
     let valuefontsize = LengthConstant(fontsize) in
     let valueypos     = LengthConstant(ypos) in
       match exec [valuekernf; valueypos; valuefontsize] env [OpApplyT(2)] [] with
       | LengthConstant(valueret) -> valueret
       | _ -> report_bug_vm "bad return value"
  )

and make_inline_graphics env valueg =
  (fun (xpos, ypos) ->
     let valuepos = TupleCons(LengthConstant(xpos), TupleCons(LengthConstant(ypos), EndOfTuple)) in
     let valueret = exec [valueg; valuepos] env [OpApplyT(1)] [] in
       graphics_of_list valueret
  )

and exec_intermediate_input_vert (env : vmenv) (valuectx : syntactic_value) (imivlst : compiled_intermediate_input_vert_element list) : syntactic_value =
  let rec interpret_commands env imivlst =
    imivlst |> List.map (fun imiv ->
        match imiv with
        | CompiledImInputVertEmbedded(code) ->
          begin
            match exec [valuectx] env (code) [] with
            | Vert(valueret) -> valueret
            | _ -> report_bug_vm "bad return value"
          end

        | CompiledImInputVertContent(ctcode) ->
          let value = exec [] env ctcode [] in
            begin
              match value with
              | CompiledInputVertWithEnvironment(imivlst, envsub) ->
                interpret_commands envsub imivlst

              | _ -> report_bug_vm "interpret_input_vert_content"
            end

      ) |> List.concat
  in
  let imvblst = interpret_commands env imivlst in
    Vert(imvblst)


and exec_intermediate_input_horz (env : vmenv) (valuectx : syntactic_value) (imihlst : compiled_intermediate_input_horz_element list) : syntactic_value =
  match valuectx with
  | Context((ctx, valuemcmd)) ->
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
                | _                                   -> (Alist.extend acc (CompiledNomInputHorzText(s2)))
              end

            | CompiledImInputHorzEmbeddedMath(mathcode) ->
              let nmih = CompiledNomInputHorzEmbedded(mathcode @ [OpPush(valuemcmd); OpApplyT(2)]) in
                Alist.extend acc nmih

            | CompiledImInputHorzContent(ctcode) ->
              let value = exec [] env ctcode [] in
                begin
                  match value with
                  | CompiledInputHorzWithEnvironment(imihlst, envsub) ->
                    let nmihlstsub = normalize imihlst in
                    let nmih = CompiledNomInputHorzContent(nmihlstsub, envsub) in
                      Alist.extend acc nmih

                  | _ -> report_bug_vm "interpret_input_horz_content"
                end

          ) Alist.empty |> Alist.to_list
      in

      let rec interpret_commands env (nmihlst : compiled_nom_input_horz_element list) : HorzBox.horz_box list =
        nmihlst |> List.map (fun nmih ->
            match nmih with
            | CompiledNomInputHorzEmbedded(code) ->
              begin
                match exec [valuectx] env code [] with
                | Horz(valueret) -> valueret
                | _ -> report_bug_vm "bad return value"
              end

            | CompiledNomInputHorzText(s) ->
              lex_horz_text ctx s

            | CompiledNomInputHorzContent(nmihlstsub, envsub) ->
              interpret_commands envsub nmihlstsub

          ) |> List.concat
      in

      let nmihlst = normalize imihlst in
      let hblst = interpret_commands env nmihlst in
        Horz(hblst)
    end
  | _ ->
    report_bug_vm "Context expected"

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

  | c :: code ->
    (*Format.printf "\nexec ---> %a\n" pp_instruction c;*)
    match c with
    (**** include: __vm.ml ****)
