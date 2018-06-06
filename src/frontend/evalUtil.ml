
module Types = Types_
open LengthInterface
open Types


let report_bug_vm msg =
  Format.printf "[Bug]@ %s:" msg;
  failwith ("bug: " ^ msg)


let report_bug_ast msg ast =
  Format.printf "[Bug]@ %s:" msg;
  Format.printf "%a" pp_abstract_tree ast;
  failwith ("bug: " ^ msg)


let report_bug_value msg value =
  Format.printf "[Bug]@ %s:" msg;
  Format.printf "%a" pp_syntactic_value value;
  failwith ("bug: " ^ msg)


let report_bug_reduction msg ast value =
  Format.printf "[Bug]@ %s:" msg;
  Format.printf "%a@ --->*@ %a" pp_abstract_tree ast pp_syntactic_value value;
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


let get_string (value : syntactic_value) : string =
  match value with
  | StringEmpty       -> ""
  | StringConstant(s) -> s
  | _                 -> report_bug_value "get_string" value


let get_list getf value =
  let rec aux acc value =
    match value with
    | ListCons(vhead, vtail) -> aux (Alist.extend acc (getf vhead)) vtail
    | EndOfList              -> Alist.to_list acc
    | _                      -> report_bug_value "get_list" value
  in
    aux Alist.empty value


let graphics_of_list value : (HorzBox.intermediate_horz_box list) Graphics.t =
  let rec aux gracc value =
    match value with
    | EndOfList                             -> gracc
    | ListCons(GraphicsValue(grelem), tail) -> aux (Graphics.extend gracc grelem) tail
    | _                                     -> report_bug_value "make_frame_deco" value
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

  | _ -> report_bug_value "interpret_paddings" value


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

  | _ -> report_bug_value "get_cell" value


let get_color (value : syntactic_value) : GraphicData.color =
  let open GraphicData in
    match value with
    | Constructor("Gray", FloatConstant(gray)) -> DeviceGray(gray)

    | Constructor("RGB", TupleCons(FloatConstant(fltR),
                                   TupleCons(FloatConstant(fltG),
                                             TupleCons(FloatConstant(fltB), EndOfTuple)))) -> DeviceRGB(fltR, fltG, fltB)

    | Constructor("CMYK", TupleCons(FloatConstant(fltC),
                                    TupleCons(FloatConstant(fltM),
                                              TupleCons(FloatConstant(fltY),
                                                        TupleCons(FloatConstant(fltK), EndOfTuple))))) -> DeviceCMYK(fltC, fltM, fltY, fltK)

    | _ -> report_bug_value "interpret_color" value


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


let get_decoset (value : syntactic_value) =
  match value with
  | TupleCons(valuedecoS,
              TupleCons(valuedecoH,
                        TupleCons(valuedecoM,
                                  TupleCons(valuedecoT, EndOfTuple)))) ->
    (valuedecoS, valuedecoH, valuedecoM, valuedecoT)

  | _ -> report_bug_value "interpret_decoset" value


let get_font (value : syntactic_value) : HorzBox.font_with_ratio =
  match value with
  | TupleCons(valueabbrev,
              TupleCons(FloatConstant(sizer),
                        TupleCons(FloatConstant(risingr), EndOfTuple))) ->
    let abbrev = get_string valueabbrev in
      (abbrev, sizer, risingr)

  | _ -> report_bug_value "interpret_font" value


let get_vert value =
  match value with
  | Vert(imvblst) -> imvblst
  | _             -> report_bug_value "get_vert" value


let get_horz value =
  match value with
  | Horz(hblst) -> hblst
  | _           -> report_bug_value "get_horz" value


let get_point value =
  match value with
  | TupleCons(LengthConstant(lenx),
      TupleCons(LengthConstant(leny), EndOfTuple)) -> (lenx, leny)

  | _ -> report_bug_value "get_point" value


let get_script (value : syntactic_value) =
  match value with
  | Constructor("HanIdeographic", UnitConstant) -> CharBasis.HanIdeographic
  | Constructor("Kana"          , UnitConstant) -> CharBasis.HiraganaOrKatakana
  | Constructor("Latin"         , UnitConstant) -> CharBasis.Latin
  | Constructor("Other"         , UnitConstant) -> CharBasis.OtherScript
  | _                                           -> report_bug_value "get_script" value


let make_script_value script =
  let label =
    match script with
    | CharBasis.HanIdeographic     -> "HanIdeographic"
    | CharBasis.HiraganaOrKatakana -> "Kana"
    | CharBasis.Latin              -> "Latin"
    | CharBasis.OtherScript        -> "OtherScript"
    | _                            -> report_bug_value "make_script_value" UnitConstant
  in
    Constructor(label, UnitConstant)


let get_uchar_list (value : syntactic_value) =
  match value with
  | StringEmpty       -> []
  | StringConstant(s) -> InternalText.to_uchar_list (InternalText.of_utf8 s)
  | _                 -> report_bug_value "get_uchar_list" value


let get_language_system (value : syntactic_value) =
  match value with
  | Constructor("Japanese"        , UnitConstant) -> CharBasis.Japanese
  | Constructor("English"         , UnitConstant) -> CharBasis.English
  | Constructor("NoLanguageSystem", UnitConstant) -> CharBasis.NoLanguageSystem
  | _                                             -> report_bug_value "get_language_system" value


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
  | _                                             -> report_bug_value "get_math_char_class" value


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
  | _                                       -> report_bug_value "get_math_class" value


let get_option (getf : syntactic_value -> 'a) (value : syntactic_value) : 'a option =
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


let get_tuple3 getf value =
  match value with
  | TupleCons(v1, TupleCons(v2, TupleCons(v3, EndOfTuple))) ->
      let c1 = getf v1 in
      let c2 = getf v2 in
      let c3 = getf v3 in
        (c1, c2, c3)

  | _ -> report_bug_value "get_tuple3" value


let get_context (value : syntactic_value) : input_context =
  match value with
  | Context(ictx) -> ictx
  | _             -> report_bug_value "get_context" value


let get_length (value : syntactic_value) : length =
  match value with
  | LengthConstant(len) -> len
  | _                   -> report_bug_value "get_length" value


let get_math value : math list =
    match value with
    | MathValue(mlst) -> mlst
    | _               -> report_bug_value "get_math" value


let get_bool value : bool =
  match value with
  | BooleanConstant(bc) -> bc
  | other               -> report_bug_value "get_bool" value


let get_int (value : syntactic_value) : int =
  match value with
  | IntegerConstant(nc) -> nc
  | _                   -> report_bug_value "get_int" value


let get_float value : float =
  match value with
  | FloatConstant(nc) -> nc
  | _                 -> report_bug_value "get_float" value


let get_page_size value =
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

  | _ -> report_bug_value "interpret_page" value
