
open LengthInterface
open GraphicBase
open Types


let report_bug_vm msg =
  Format.printf "[Bug]@ %s:" msg;
  failwith ("bug: " ^ msg)


let report_bug_vm_value msg value =
  Format.printf "[Bug]@ %s:" msg;
  Format.printf "%a" pp_syntactic_value value;
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
  | BaseConstant(BCString(s)) -> s
  | _                         -> report_bug_value "get_string" value


let get_list getf value =
  match value with
  | List(vlst) -> List.map getf vlst
  | _          -> report_bug_value "get_list" value


let get_graphics_element value =
  match value with
  | BaseConstant(BCGraphics(grelem)) -> grelem
  | _                                -> report_bug_value "get_graphics_element" value


let graphics_of_list value : (HorzBox.intermediate_horz_box list) GraphicD.t =
    match value with
    | List(vlst) ->
        vlst |> List.fold_left (fun gracc v ->
          match v with
          | BaseConstant(BCGraphics(grelem)) -> GraphicD.extend gracc grelem
          | _                                -> report_bug_value "graphics_of_list:1" v
        ) GraphicD.empty

    | _ ->
        report_bug_value "graphics_of_list:2" value


let get_paddings (value : syntactic_value) =
  match value with
  | Tuple([
      BaseConstant(BCLength(lenL));
      BaseConstant(BCLength(lenR));
      BaseConstant(BCLength(lenT));
      BaseConstant(BCLength(lenB));
    ]) ->
      HorzBox.{
        paddingL = lenL;
        paddingR = lenR;
        paddingT = lenT;
        paddingB = lenB;
      }

  | _ ->
      report_bug_value "interpret_paddings" value


let get_cell value : HorzBox.cell =
    match value with
    | Constructor("NormalCell", Tuple([valuepads; BaseConstant(BCHorz(hblst))])) ->
        let pads = get_paddings valuepads in
        HorzBox.NormalCell(pads, hblst)

    | Constructor("EmptyCell", BaseConstant(BCUnit)) ->
        HorzBox.EmptyCell

    | Constructor("MultiCell", Tuple([
        BaseConstant(BCInt(nr));
        BaseConstant(BCInt(nc));
        valuepads;
        BaseConstant(BCHorz(hblst));
      ])) ->
        let pads = get_paddings valuepads in
        HorzBox.MultiCell(nr, nc, pads, hblst)

    | _ ->
        report_bug_value "get_cell" value


let get_color (value : syntactic_value) : color =
  match value with
  | Constructor("Gray", BaseConstant(BCFloat(gray))) ->
      DeviceGray(gray)

  | Constructor("RGB", Tuple([
      BaseConstant(BCFloat(fltR));
      BaseConstant(BCFloat(fltG));
      BaseConstant(BCFloat(fltB));
    ])) ->
      DeviceRGB(fltR, fltG, fltB)

  | Constructor("CMYK", Tuple([
      BaseConstant(BCFloat(fltC));
      BaseConstant(BCFloat(fltM));
      BaseConstant(BCFloat(fltY));
      BaseConstant(BCFloat(fltK));
    ])) ->
      DeviceCMYK(fltC, fltM, fltY, fltK)

  | _ ->
      report_bug_value "interpret_color" value


let make_color_value color =
  match color with
  | DeviceGray(gray) ->
      Constructor("Gray", BaseConstant(BCFloat(gray)))

  | DeviceRGB(r, g, b) ->
      Constructor("RGB", Tuple([
        BaseConstant(BCFloat(r));
        BaseConstant(BCFloat(g));
        BaseConstant(BCFloat(b));
      ]))

  | DeviceCMYK(c, m, y, k) ->
      Constructor("CMYK", Tuple([
        BaseConstant(BCFloat(c));
        BaseConstant(BCFloat(m));
        BaseConstant(BCFloat(y));
        BaseConstant(BCFloat(k));
      ]))


let get_decoset (value : syntactic_value) =
  match value with
  | Tuple([
      valuedecoS;
      valuedecoH;
      valuedecoM;
      valuedecoT;
    ]) ->
      (valuedecoS, valuedecoH, valuedecoM, valuedecoT)

  | _ ->
      report_bug_value "interpret_decoset" value


let get_font (value : syntactic_value) : HorzBox.font_with_ratio =
  match value with
  | Tuple([
      BaseConstant(BCString(abbrev));
      BaseConstant(BCFloat(sizer));
      BaseConstant(BCFloat(risingr));
    ]) ->
      (abbrev, sizer, risingr)

  | _ ->
      report_bug_value "interpret_font" value


let make_font_value (abbrev, sizer, risingr) =
  Tuple([
    BaseConstant(BCString(abbrev));
    BaseConstant(BCFloat(sizer));
    BaseConstant(BCFloat(risingr));
  ])


let get_vert value : HorzBox.vert_box list =
  match value with
  | BaseConstant(BCVert(vblst)) -> vblst
  | _                           -> report_bug_value "get_vert" value


let get_horz value : HorzBox.horz_box list =
  match value with
  | BaseConstant(BCHorz(hblst)) -> hblst
  | _                           -> report_bug_value "get_horz" value


let get_point value =
  match value with
  | Tuple([
      BaseConstant(BCLength(lenx));
      BaseConstant(BCLength(leny));
    ]) ->
      (lenx, leny)

  | _ ->
      report_bug_value "get_point" value


let make_point_value (x, y) =
  Tuple([
    BaseConstant(BCLength(x));
    BaseConstant(BCLength(y));
  ])


let get_script (value : syntactic_value) =
  match value with
  | Constructor("HanIdeographic", BaseConstant(BCUnit)) -> CharBasis.HanIdeographic
  | Constructor("Kana"          , BaseConstant(BCUnit)) -> CharBasis.HiraganaOrKatakana
  | Constructor("Latin"         , BaseConstant(BCUnit)) -> CharBasis.Latin
  | Constructor("OtherScript"   , BaseConstant(BCUnit)) -> CharBasis.OtherScript
  | _                                           -> report_bug_value "get_script" value


let make_script_value script =
  let label =
    match script with
    | CharBasis.HanIdeographic     -> "HanIdeographic"
    | CharBasis.HiraganaOrKatakana -> "Kana"
    | CharBasis.Latin              -> "Latin"
    | CharBasis.OtherScript        -> "OtherScript"
    | _                            -> report_bug_vm "make_script_value"
  in
  Constructor(label, BaseConstant(BCUnit))


let get_uchar_list (value : syntactic_value) =
  match value with
  | BaseConstant(BCString(s)) -> InternalText.to_uchar_list (InternalText.of_utf8 s)
  | _                         -> report_bug_value "get_uchar_list" value


let get_language_system (value : syntactic_value) =
  match value with
  | Constructor("Japanese"        , BaseConstant(BCUnit)) -> CharBasis.Japanese
  | Constructor("English"         , BaseConstant(BCUnit)) -> CharBasis.English
  | Constructor("NoLanguageSystem", BaseConstant(BCUnit)) -> CharBasis.NoLanguageSystem
  | _                                                     -> report_bug_value "get_language_system" value


let make_language_system_value langsys =
  let label =
    match langsys with
    | CharBasis.Japanese         -> "Japanese"
    | CharBasis.English          -> "English"
    | CharBasis.NoLanguageSystem -> "NoLanguageSystem"
  in
  Constructor(label, BaseConstant(BCUnit))


let get_math_char_class (value : syntactic_value) =
  match value with
  | Constructor("MathItalic"      , BaseConstant(BCUnit)) -> HorzBox.MathItalic
  | Constructor("MathBoldItalic"  , BaseConstant(BCUnit)) -> HorzBox.MathBoldItalic
  | Constructor("MathRoman"       , BaseConstant(BCUnit)) -> HorzBox.MathRoman
  | Constructor("MathBoldRoman"   , BaseConstant(BCUnit)) -> HorzBox.MathBoldRoman
  | Constructor("MathScript"      , BaseConstant(BCUnit)) -> HorzBox.MathScript
  | Constructor("MathBoldScript"  , BaseConstant(BCUnit)) -> HorzBox.MathBoldScript
  | Constructor("MathFraktur"     , BaseConstant(BCUnit)) -> HorzBox.MathFraktur
  | Constructor("MathBoldFraktur" , BaseConstant(BCUnit)) -> HorzBox.MathBoldFraktur
  | Constructor("MathDoubleStruck", BaseConstant(BCUnit)) -> HorzBox.MathDoubleStruck
  | _                                                     -> report_bug_value "get_math_char_class" value


let get_math_class (value : syntactic_value) =
  let open HorzBox in
    match value with
    | Constructor("MathOrd"   , BaseConstant(BCUnit)) -> MathOrdinary
    | Constructor("MathBin"   , BaseConstant(BCUnit)) -> MathBinary
    | Constructor("MathRel"   , BaseConstant(BCUnit)) -> MathRelation
    | Constructor("MathOp"    , BaseConstant(BCUnit)) -> MathOperator
    | Constructor("MathPunct" , BaseConstant(BCUnit)) -> MathPunct
    | Constructor("MathOpen"  , BaseConstant(BCUnit)) -> MathOpen
    | Constructor("MathClose" , BaseConstant(BCUnit)) -> MathClose
    | Constructor("MathPrefix", BaseConstant(BCUnit)) -> MathPrefix
    | Constructor("MathInner" , BaseConstant(BCUnit)) -> MathInner
    | _                                               -> report_bug_value "get_math_class" value


let make_math_class_option_value (mathcls : HorzBox.math_kind) =
  let open HorzBox in
    let labelopt =
      match mathcls with
      | MathOrdinary -> Some("MathOrd")
      | MathBinary   -> Some("MathBin")
      | MathRelation -> Some("MathRel")
      | MathOperator -> Some("MathOp")
      | MathPunct    -> Some("MathPunct")
      | MathOpen     -> Some("MathOpen")
      | MathClose    -> Some("MathClose")
      | MathPrefix   -> Some("MathPrefix")
      | MathInner    -> Some("MathInner")
      | MathEnd      -> None
    in
    match labelopt with
    | None ->        Constructor("None", BaseConstant(BCUnit))
    | Some(label) -> Constructor("Some", Constructor(label, BaseConstant(BCUnit)))


let get_option (getf : syntactic_value -> 'a) (value : syntactic_value) : 'a option =
  match value with
  | Constructor("None", BaseConstant(BCUnit)) -> None
  | Constructor("Some", valuesub)             -> Some(getf valuesub)
  | _                                         -> report_bug_vm "get_option"


let get_pair (getf1 : syntactic_value -> 'a) (getf2 : syntactic_value -> 'b) (value : syntactic_value) : 'a * 'b =
  match value with
  | Tuple([v1; v2]) -> (getf1 v1, getf2 v2)
  | _               -> report_bug_vm "get_pair"


let get_page_size (value : syntactic_value) : HorzBox.page_size =
  match value with
  | Constructor("A0Paper" , BaseConstant(BCUnit)) -> HorzBox.A0Paper
  | Constructor("A1Paper" , BaseConstant(BCUnit)) -> HorzBox.A1Paper
  | Constructor("A2Paper" , BaseConstant(BCUnit)) -> HorzBox.A2Paper
  | Constructor("A3Paper" , BaseConstant(BCUnit)) -> HorzBox.A3Paper
  | Constructor("A4Paper" , BaseConstant(BCUnit)) -> HorzBox.A4Paper
  | Constructor("A5Paper" , BaseConstant(BCUnit)) -> HorzBox.A5Paper
  | Constructor("USLetter", BaseConstant(BCUnit)) -> HorzBox.USLetter
  | Constructor("USLegal" , BaseConstant(BCUnit)) -> HorzBox.USLegal

  | Constructor("UserDefinedPaper", Tuple([
      BaseConstant(BCLength(pgwid));
      BaseConstant(BCLength(pghgt));
    ])) ->
      HorzBox.UserDefinedPaper(pgwid, pghgt)

  | _ ->
      report_bug_vm "get_page_size"


let get_tuple3 getf value =
  match value with
  | Tuple([v1; v2; v3]) ->
      let c1 = getf v1 in
      let c2 = getf v2 in
      let c3 = getf v3 in
      (c1, c2, c3)

  | _ ->
      report_bug_value "get_tuple3" value


let get_context (value : syntactic_value) : input_context =
  match value with
  | Context(ictx) -> ictx
  | _             -> report_bug_value "get_context" value


let get_text_mode_context (value : syntactic_value) : TextBackend.text_mode_context =
  match value with
  | BaseConstant(BCTextModeContext(tctx)) -> tctx
  | _                                     -> report_bug_value "get_text_mode_context" value


let get_length (value : syntactic_value) : length =
  match value with
  | BaseConstant(BCLength(len)) -> len
  | _                           -> report_bug_value "get_length" value


let get_length_list = get_list get_length


let get_math value : math list =
    match value with
    | MathValue(mlst) -> mlst
    | _               -> report_bug_value "get_math" value


let get_math_list = get_list get_math


let get_bool value : bool =
  match value with
  | BaseConstant(BCBool(bc)) -> bc
  | other                    -> report_bug_value "get_bool" value


let get_int (value : syntactic_value) : int =
  match value with
  | BaseConstant(BCInt(nc)) -> nc
  | _                       -> report_bug_value "get_int" value


let get_float value : float =
  match value with
  | BaseConstant(BCFloat(nc)) -> nc
  | _                         -> report_bug_value "get_float" value


let get_regexp (value : syntactic_value) : Str.regexp =
  match value with
  | BaseConstant(BCRegExp(regexp)) -> regexp
  | _                              -> report_bug_value "get_regexp" value


let get_code (value : syntactic_value) : code_value =
  match value with
  | CodeValue(cv) -> cv
  | _             -> report_bug_value "get_code" value


let get_path_value (value : syntactic_value) : path list =
  match value with
  | BaseConstant(BCPath(pathlst)) -> pathlst
  | _                             -> report_bug_value "get_path_value" value


let get_prepath (value : syntactic_value) : PrePath.t =
  match value with
  | BaseConstant(BCPrePath(prepath)) -> prepath
  | _                                -> report_bug_value "get_prepath" value


let get_math_variant_style value =
  let rcd =
    match value with
    | RecordValue(rcd) -> rcd
    | _                -> report_bug_value "get_math_variant_style: not a record" value
  in
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

    | _ -> report_bug_value "get_math_variant_style: missing some fields" value


let get_outline (value : syntactic_value) =
  match value with
  | Tuple([
      BaseConstant(BCInt(level));
      BaseConstant(BCString(text));
      BaseConstant(BCString(key));
      BaseConstant(BCBool(isopen));
    ]) ->
      (level, text, key, isopen)

  | _ ->
      report_bug_value "get_outline" value


let make_page_break_info pbinfo =
  let asc =
    Assoc.of_list [
      ("page-number", BaseConstant(BCInt(pbinfo.HorzBox.current_page_number)));
    ]
  in
  RecordValue(asc)


let make_page_content_info pcinfo =
  make_page_break_info pcinfo  (* temporary *)
(*
  let asc =
    Assoc.of_list [
      ("page-number", IntegerConstant(pcinfo.HorzBox.page_number));
    ]
  in
    RecordValue(asc)
*)


let make_hook (reducef : syntactic_value -> syntactic_value list -> syntactic_value) (valuehook : syntactic_value) : (HorzBox.page_break_info -> point -> unit) =
  (fun pbinfo (xpos, yposbaseline) ->
     let valuept = Tuple([BaseConstant(BCLength(xpos)); BaseConstant(BCLength(yposbaseline))]) in
     let valuepbinfo = make_page_break_info pbinfo in
     let valueret = reducef valuehook [valuepbinfo; valuept] in
       match valueret with
       | BaseConstant(BCUnit) -> ()
       | _                    -> report_bug_vm "make_hook"
  )


let make_column_hook_func reducef valuef : HorzBox.column_hook_func =
  (fun () ->
    let valueret = reducef valuef [BaseConstant(BCUnit)] in
    get_vert valueret
  )


let make_page_content_scheme_func reducef valuef : HorzBox.page_content_scheme_func =
  (fun pbinfo ->
     let valuepbinfo = make_page_break_info pbinfo in
     let valueret = reducef valuef [valuepbinfo] in
     match valueret with
     | RecordValue(asc) ->
         begin
           match
             (Assoc.find_opt asc "text-origin",
              Assoc.find_opt asc "text-height")
           with
           | (Some(vTO), Some(BaseConstant(BCLength(vTHlen)))) ->
               HorzBox.({
                 page_content_origin = get_point(vTO);
                 page_content_height = vTHlen;
               })

           | _ ->
               report_bug_value "make_page_scheme_func:1" valueret
         end

     | _ ->
         report_bug_value "make_page_scheme_func:2" valueret
  )


and make_page_parts_scheme_func reducef valuef : HorzBox.page_parts_scheme_func =
  (fun pcinfo ->
     let valuepcinfo = make_page_content_info pcinfo in
     let valueret = reducef valuef [valuepcinfo] in
       match valueret with
       | RecordValue(asc) ->
         begin
           match
             (Assoc.find_opt asc "header-origin",
              Assoc.find_opt asc "header-content",
              Assoc.find_opt asc "footer-origin",
              Assoc.find_opt asc "footer-content")
           with
           | (Some(vHO), Some(BaseConstant(BCVert(vHCvert))), Some(vFO), Some(BaseConstant(BCVert(vFCvert)))) ->
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


let make_frame_deco reducef valuedeco =
  (fun (xpos, ypos) wid hgt dpt ->
     let valuepos = Tuple([BaseConstant(BCLength(xpos)); BaseConstant(BCLength(ypos))]) in
     let valuewid = BaseConstant(BCLength(wid)) in
     let valuehgt = BaseConstant(BCLength(hgt)) in
     let valuedpt = BaseConstant(BCLength(Length.negate dpt)) in
       (* -- depth values for users are nonnegative -- *)
     let valueret = reducef valuedeco [valuepos; valuewid; valuehgt; valuedpt] in
     graphics_of_list valueret
  )


let make_math_kern_func reducef valuekernf : HorzBox.math_kern_func =
  (fun corrhgt ->
    let astcorrhgt = BaseConstant(BCLength(corrhgt)) in
    let valueret = reducef valuekernf [astcorrhgt] in
    get_length valueret
  )


let make_paren reducef valueparenf : HorzBox.paren =
  (fun hgt dpt hgtaxis fontsize color ->
     let valuehgt      = BaseConstant(BCLength(hgt)) in
     let valuedpt      = BaseConstant(BCLength(Length.negate dpt)) in
     (* -- depth values for users are nonnegative -- *)
     let valuehgtaxis  = BaseConstant(BCLength(hgtaxis)) in
     let valuefontsize = BaseConstant(BCLength(fontsize)) in
     let valuecolor    = make_color_value color in
     let valueret = reducef valueparenf [valuehgt; valuedpt; valuehgtaxis; valuefontsize; valuecolor] in
     match valueret with
     | Tuple([BaseConstant(BCHorz(hblst)); valuekernf]) ->
         let kernf = make_math_kern_func reducef valuekernf in
         (hblst, kernf)

     | _ ->
         report_bug_vm "make_paren"
  )


let make_math (mlst : math list) : syntactic_value =
  MathValue(mlst)


let make_option (type a) (makef : a -> syntactic_value) (opt : a option) : syntactic_value =
  match opt with
  | None    -> Constructor("None", BaseConstant(BCUnit))
  | Some(x) -> let value = makef x in Constructor("Some", value)


let make_pull_in_scripts reducef valuef =
  (fun mopt1 mopt2 ->
     let value1 = make_option make_math mopt1 in
     let value2 = make_option make_math mopt2 in
     let valueret = reducef valuef [value1; value2] in
     get_math valueret
  )


let make_math_char_kern_func reducef valuekernf : HorzBox.math_char_kern_func =
  (fun fontsize ypos ->
     let valuefontsize = BaseConstant(BCLength(fontsize)) in
     let valueypos     = BaseConstant(BCLength(ypos)) in
     let valueret = reducef valuekernf [valuefontsize; valueypos] in
     get_length valueret
  )


let make_inline_graphics reducef valueg : HorzBox.fixed_graphics =
  (fun (xpos, ypos) ->
     let valuepos = Tuple([BaseConstant(BCLength(xpos)); BaseConstant(BCLength(ypos))]) in
     let valueret = reducef valueg [valuepos] in
     graphics_of_list valueret
  )


let make_inline_graphics_outer reducef valueg : HorzBox.outer_fil_graphics =
  (fun wid (xpos, ypos) ->
     let valuepos = Tuple([BaseConstant(BCLength(xpos)); BaseConstant(BCLength(ypos))]) in
     let valuewid = BaseConstant(BCLength(wid)) in
     let valueret = reducef valueg [valuewid; valuepos] in
     graphics_of_list valueret
  )


(*
let get_math_command_func reducef valuemcmd : math_command_func =
  MathCommand(fun ctx mlst ->
    let valuectx = Context(ctx) in
    let valuemath = MathValue(mlst) in
    let valueret = reducef valuemcmd [valuectx; valuemath] in
    get_horz valueret
  )
*)
let get_math_command_func _ valuemcmd =
  MathCommand(valuemcmd)

let make_math_command_func (MathCommand(valuemcmd)) = valuemcmd

let get_code_text_command_func _ valuectcmd =
  CodeTextCommand(valuectcmd)

let make_list (type a) (makef : a -> syntactic_value) (lst : a list) : syntactic_value =
  List(lst |> List.map makef)


let make_length_list lenlst =
  List(lenlst |> List.map (fun l -> BaseConstant(BCLength(l))))


let make_line_stack (hbss : (HorzBox.horz_box list) list) =
  let open HorzBox in
  let wid =
    hbss |> List.fold_left (fun widacc hbs ->
      let (wid, _, _) = LineBreak.get_natural_metrics hbs in
       Length.max wid widacc
    ) Length.zero
  in
  let quads = hbss |> List.map (fun hblst -> LineBreak.fit hblst wid) in
  let vbs =
    quads |> List.fold_left (fun vbacc (imhbs, ratios, hgt, dpt) ->
      let vb =
        VertParagraph({
          margin_top    = None;
          margin_bottom = None;
        }, [VertParagLine(Reachable(ratios), hgt, dpt, imhbs)])
      in
      Alist.extend vbacc vb
    ) Alist.empty |> Alist.to_list
  in
  (wid, vbs)


let const_unit = BaseConstant(BCUnit)
let make_bool b = BaseConstant(BCBool(b))
let make_int n = BaseConstant(BCInt(n))
let make_float x = BaseConstant(BCFloat(x))
let make_length l = BaseConstant(BCLength(l))
let make_string s = BaseConstant(BCString(s))
let make_regexp re = BaseConstant(BCRegExp(re))
let make_horz h = BaseConstant(BCHorz(h))
let make_vert v = BaseConstant(BCVert(v))
let make_path p = BaseConstant(BCPath(p))
let make_prepath pp = BaseConstant(BCPrePath(pp))
let make_graphics g = BaseConstant(BCGraphics(g))
let make_image_key i = BaseConstant(BCImageKey(i))


let lift_string_to_code_value (s : string) = CodeValue(CdBaseConstant(BCString(s)))
let lift_integer_to_code_value (n : int) = CodeValue(CdBaseConstant(BCInt(n)))
let lift_float_to_code_value (r : float) = CodeValue(CdBaseConstant(BCFloat(r)))
let lift_length_to_code_value (len : length) = CodeValue(CdBaseConstant(BCLength(len)))


let get_input_position (v : syntactic_value) : input_position =
  match v with
  | BaseConstant(BCInputPos(ipos)) -> ipos
  | _                              -> report_bug_value "get_input_position" v


let make_input_position (ipos : input_position) : syntactic_value =
  BaseConstant(BCInputPos(ipos))
