
open MyUtil
open LengthInterface
open Types

(* -- type IDs for predefined data types -- *)
let tyid_option   = Typeenv.Raw.fresh_type_id "option"
let tyid_itemize  = Typeenv.Raw.fresh_type_id "itemize"
let tyid_color    = Typeenv.Raw.fresh_type_id "color"
let tyid_script   = Typeenv.Raw.fresh_type_id "script"
let tyid_language = Typeenv.Raw.fresh_type_id "language"
let tyid_page     = Typeenv.Raw.fresh_type_id "page"
let tyid_mathcls  = Typeenv.Raw.fresh_type_id "math-class"
let tyid_mccls    = Typeenv.Raw.fresh_type_id "math-char-class"
let tyid_cell     = Typeenv.Raw.fresh_type_id "cell"
let tyid_image    = Typeenv.Raw.fresh_type_id "image"

(* -- type IDs for alias types -- *)
let tyid_deco     = Typeenv.Raw.fresh_type_id "deco"
let tyid_decoset  = Typeenv.Raw.fresh_type_id "deco-set"
let tyid_igraf    = Typeenv.Raw.fresh_type_id "inline-graphics"

let ( ~! ) = Range.dummy

(* -- base types and base type constructors -- *)
let tU            = (~! "unit"    , BaseType(UnitType)    )
let tI            = (~! "int"     , BaseType(IntType)     )
let tFL           = (~! "float"   , BaseType(FloatType)   )
let tB            = (~! "bool"    , BaseType(BoolType)    )
let tLN           = (~! "length"  , BaseType(LengthType)  )
let tS            = (~! "string"  , BaseType(StringType)  )
let tIT           = (~! "itext"   , BaseType(TextRowType) )
let tBT           = (~! "btext"   , BaseType(TextColType) )
let tIB           = (~! "iboxes"  , BaseType(BoxRowType)  )
let tBB           = (~! "bboxes"  , BaseType(BoxColType)  )
(*
let tFT           = (~! "font"    , BaseType(FontType)    )
*)
let tCTX          = (~! "context" , BaseType(ContextType) )
let tPATH         = (~! "path"    , BaseType(PathType)    )
let tPRP          = (~! "pre-path", BaseType(PrePathType) )
let tDOC          = (~! "document", BaseType(DocumentType))
let tMATH         = (~! "math"    , BaseType(MathType)    )
let tGR           = (~! "graphics", BaseType(GraphicsType))
let tIMG          = (~! "image"   , BaseType(ImageType)   )
let tL ty         = (~! "list"    , ListType(ty)          )
let tR ty         = (~! "ref"     , RefType(ty)           )
let tPROD tylst   = (~! "product" , ProductType(tylst)    )
let (@->) dom cod = (~! "func"    , FuncType(dom, cod)    )

(* -- predefined data types -- *)
let tOPT ty       = (~! "option"  , VariantType([ty], tyid_option))
let tITMZ         = (~! "itemize" , VariantType([], tyid_itemize) )
let tSCR          = (~! "script"  , VariantType([], tyid_script)  )
let tLANG         = (~! "language", VariantType([], tyid_language))
let tCLR          = (~! "color"   , VariantType([], tyid_color)   )
let tPG           = (~! "page"    , VariantType([], tyid_page)    )
let tMATHCLS      = (~! "mathcls" , VariantType([], tyid_mathcls) )
let tMCCLS        = (~! "mccls"   , VariantType([], tyid_mccls)   )
let tCELL         = (~! "cell"    , VariantType([], tyid_cell)    )

(* -- predefined alias types -- *)
let tFONT         = tPROD [tS; tFL; tFL]
let tPT           = tPROD [tLN; tLN]
let tDASH         = tPROD [tLN; tLN; tLN]
let tPADS         = tPROD [tLN; tLN; tLN; tLN]

let tDECO_raw = tPT @-> tLN @-> tLN @-> tLN @-> (tL tGR)
let tDECO = (~! "deco", SynonymType([], tyid_deco, tDECO_raw))

let tDECOSET_raw = tPROD [tDECO; tDECO; tDECO; tDECO]
let tDECOSET = (~! "deco-set", SynonymType([], tyid_decoset, tDECOSET_raw))

let tIGR_raw = tPT @-> (tL tGR)
let tIGR = (~! "igraf", SynonymType([], tyid_igraf, tIGR_raw))

let tPAREN = tLN @-> tLN @-> tLN @-> tLN @-> tCLR @-> tPROD [tIB; tLN @-> tLN]

let tCMD = (~! "cmd", HorzCommandType([MandatoryArgumentType(tMATH)]))

let tMCSTY =
  let asc =
    Assoc.of_list (List.map (fun k -> (k, tS)) [
      "italic";
      "bold-italic";
      "roman";
      "bold-roman";
      "script";
      "bold-script";
      "fraktur";
      "bold-fraktur";
      "double-struck";
    ])  (* temporary *)
  in
    (~! "math-char-style", RecordType(asc))

let tPBINFO =
  let asc =
    Assoc.of_list [
      ("page-number", tI);
    ]
  in
    (~! "page-break-info", RecordType(asc))

let tPAGECONT =
  let asc =
    Assoc.of_list [
      ("text-origin", tPT);
      ("text-height", tLN);
    ]
  in
    (~! "page-content-scheme", RecordType(asc))

let tPAGECONTF = tPBINFO @-> tPAGECONT

let tPCINFO =
  tPBINFO  (* temporary; may have more fields in the future *)

let tPAGEPARTS =
  let asc =
    Assoc.of_list [
      ("header-origin" , tPT);
      ("header-content", tBB);
      ("footer-origin" , tPT);
      ("footer-content", tBB);
    ]
  in
    (~! "page-parts", RecordType(asc))

let tPAGEPARTSF = tPCINFO @-> tPAGEPARTS

let tRULESF = (tL tLN) @-> (tL tLN) @-> (tL tGR)


let option_type = tOPT


let add_default_types (tyenvmid : Typeenv.t) : Typeenv.t =
  let dr = Range.dummy "add_default_types" in
  let bid = BoundID.fresh UniversalKind () in
  let typaram = (dr, TypeVariable(ref (Bound(bid)))) in

  tyenvmid
  |> Typeenv.Raw.register_type "option" tyid_option (Typeenv.Data(1))
  |> Typeenv.Raw.add_constructor "None" ([bid], Poly(tU)) tyid_option
  |> Typeenv.Raw.add_constructor "Some" ([bid], Poly(typaram)) tyid_option

  |> Typeenv.Raw.register_type "itemize" tyid_itemize (Typeenv.Data(0))
  |> Typeenv.Raw.add_constructor "Item" ([], Poly(tPROD [tIT; tL tITMZ])) tyid_itemize

  |> Typeenv.Raw.register_type "color" tyid_color (Typeenv.Data(0))
  |> Typeenv.Raw.add_constructor "Gray" ([], Poly(tFL)) tyid_color
  |> Typeenv.Raw.add_constructor "RGB"  ([], Poly(tPROD [tFL; tFL; tFL])) tyid_color
  |> Typeenv.Raw.add_constructor "CMYK" ([], Poly(tPROD [tFL; tFL; tFL; tFL])) tyid_color

  |> Typeenv.Raw.register_type "script" tyid_script (Typeenv.Data(0))
  |> Typeenv.Raw.add_constructor "HanIdeographic" ([], Poly(tU)) tyid_script
  |> Typeenv.Raw.add_constructor "Kana"           ([], Poly(tU)) tyid_script
  |> Typeenv.Raw.add_constructor "Latin"          ([], Poly(tU)) tyid_script
  |> Typeenv.Raw.add_constructor "OtherScript"    ([], Poly(tU)) tyid_script

  |> Typeenv.Raw.register_type "language" tyid_language (Typeenv.Data(0))
  |> Typeenv.Raw.add_constructor "English"          ([], Poly(tU)) tyid_language
  |> Typeenv.Raw.add_constructor "Japanese"         ([], Poly(tU)) tyid_language
  |> Typeenv.Raw.add_constructor "NoLanguageSystem" ([], Poly(tU)) tyid_language

  |> Typeenv.Raw.register_type "page" tyid_page (Typeenv.Data(0))
  |> Typeenv.Raw.add_constructor "A0Paper"          ([], Poly(tU)) tyid_page
  |> Typeenv.Raw.add_constructor "A1Paper"          ([], Poly(tU)) tyid_page
  |> Typeenv.Raw.add_constructor "A2Paper"          ([], Poly(tU)) tyid_page
  |> Typeenv.Raw.add_constructor "A3Paper"          ([], Poly(tU)) tyid_page
  |> Typeenv.Raw.add_constructor "A4Paper"          ([], Poly(tU)) tyid_page
  |> Typeenv.Raw.add_constructor "A5Paper"          ([], Poly(tU)) tyid_page
  |> Typeenv.Raw.add_constructor "USLetter"         ([], Poly(tU)) tyid_page
  |> Typeenv.Raw.add_constructor "USLegal"          ([], Poly(tU)) tyid_page
  |> Typeenv.Raw.add_constructor "UserDefinedPaper" ([], Poly(tPROD [tLN; tLN])) tyid_page

  |> Typeenv.Raw.register_type "math-class" tyid_mathcls (Typeenv.Data(0))
  |> Typeenv.Raw.add_constructor "MathOrd"    ([], Poly(tU)) tyid_mathcls
  |> Typeenv.Raw.add_constructor "MathBin"    ([], Poly(tU)) tyid_mathcls
  |> Typeenv.Raw.add_constructor "MathRel"    ([], Poly(tU)) tyid_mathcls
  |> Typeenv.Raw.add_constructor "MathOp"     ([], Poly(tU)) tyid_mathcls
  |> Typeenv.Raw.add_constructor "MathPunct"  ([], Poly(tU)) tyid_mathcls
  |> Typeenv.Raw.add_constructor "MathOpen"   ([], Poly(tU)) tyid_mathcls
  |> Typeenv.Raw.add_constructor "MathClose"  ([], Poly(tU)) tyid_mathcls
  |> Typeenv.Raw.add_constructor "MathPrefix" ([], Poly(tU)) tyid_mathcls

  |> Typeenv.Raw.register_type "math-char-class" tyid_mccls (Typeenv.Data(0))
  |> Typeenv.Raw.add_constructor "MathItalic"       ([], Poly(tU)) tyid_mccls
  |> Typeenv.Raw.add_constructor "MathBoldItalic"   ([], Poly(tU)) tyid_mccls
  |> Typeenv.Raw.add_constructor "MathRoman"        ([], Poly(tU)) tyid_mccls
  |> Typeenv.Raw.add_constructor "MathBoldRoman"    ([], Poly(tU)) tyid_mccls
  |> Typeenv.Raw.add_constructor "MathScript"       ([], Poly(tU)) tyid_mccls
  |> Typeenv.Raw.add_constructor "MathBoldScript"   ([], Poly(tU)) tyid_mccls
  |> Typeenv.Raw.add_constructor "MathFraktur"      ([], Poly(tU)) tyid_mccls
  |> Typeenv.Raw.add_constructor "MathBoldFraktur"  ([], Poly(tU)) tyid_mccls
  |> Typeenv.Raw.add_constructor "MathDoubleStruck" ([], Poly(tU)) tyid_mccls

  |> Typeenv.Raw.register_type "cell" tyid_cell (Typeenv.Data(0))
  |> Typeenv.Raw.add_constructor "NormalCell" ([], Poly(tPROD [tPADS; tIB]))         tyid_cell
  |> Typeenv.Raw.add_constructor "EmptyCell"  ([], Poly(tU))                         tyid_cell
  |> Typeenv.Raw.add_constructor "MultiCell"  ([], Poly(tPROD [tI; tI; tPADS; tIB])) tyid_cell

  |> Typeenv.Raw.register_type "deco" tyid_deco (Typeenv.Alias(([], Poly(tDECO_raw))))

  |> Typeenv.Raw.register_type "deco-set" tyid_decoset (Typeenv.Alias(([], Poly(tDECOSET_raw))))

  |> Typeenv.Raw.register_type "inline-graphics" tyid_igraf (Typeenv.Alias(([], Poly(tIGR_raw))))


let lam evid ast = Function([PatternBranch(PVariable(evid), ast)])
let lamenv env evid ast = FuncWithEnvironment([PatternBranch(PVariable(evid), ast)], env)
let ( !- ) evid = ContentOf(Range.dummy "temporary", evid)

let rec lambda1 astf env =
  let evid1 = EvalVarID.fresh "(dummy:lambda1-1)" in
    lamenv env evid1 (astf (!- evid1))

let rec lambda2 astf env =
  let evid1 = EvalVarID.fresh "(dummy:lambda2-1)" in
  let evid2 = EvalVarID.fresh "(dummy:lambda2-2)" in
    lamenv env evid1 (lam evid2 (astf (!- evid1) (!- evid2)))

let rec lambda3 astf env =
  let evid1 = EvalVarID.fresh "(dummy:lambda3-1)" in
  let evid2 = EvalVarID.fresh "(dummy:lambda3-2)" in
  let evid3 = EvalVarID.fresh "(dummy:lambda3-3)" in
    lamenv env evid1 (lam evid2 (lam evid3 (astf (!- evid1) (!- evid2) (!- evid3))))

let rec lambda4 astf env =
  let evid1 = EvalVarID.fresh "(dummy:lambda4-1)" in
  let evid2 = EvalVarID.fresh "(dummy:lambda4-2)" in
  let evid3 = EvalVarID.fresh "(dummy:lambda4-3)" in
  let evid4 = EvalVarID.fresh "(dummy:lambda4-4)" in
    lamenv env evid1 (lam evid2 (lam evid3 (lam evid4 (astf (!- evid1) (!- evid2) (!- evid3) (!- evid4)))))

let rec lambda5 astf env =
  let evid1 = EvalVarID.fresh "(dummy:lambda5-1)" in
  let evid2 = EvalVarID.fresh "(dummy:lambda5-2)" in
  let evid3 = EvalVarID.fresh "(dummy:lambda5-3)" in
  let evid4 = EvalVarID.fresh "(dummy:lambda5-4)" in
  let evid5 = EvalVarID.fresh "(dummy:lambda5-5)" in
    lamenv env evid1 (lam evid2 (lam evid3 (lam evid4 (lam evid5 (astf (!- evid1) (!- evid2) (!- evid3) (!- evid4) (!- evid5))))))


let pdfpt = Length.of_pdf_point


(*
let default_math_left_paren hgt dpt hgtaxis fontsize color =
  let open HorzBox in
  let lenappend = fontsize *% 0.1 in
  let minhalflen = fontsize *% 0.5 in
  let halflen = Length.max minhalflen ((Length.max (hgt -% hgtaxis) (hgtaxis -% dpt)) +% lenappend) in
  let widparen = halflen *% 0.375 in
  let wid = widparen +% fontsize *% 0.1 in
  let graphics (xpos, ypos) =
    Graphics.pdfops_of_stroke (pdfpt 0.5) color [
      GeneralPath((xpos +% wid, ypos +% hgtaxis +% halflen), [
        LineTo((xpos +% wid -% widparen, ypos +% hgtaxis));
        LineTo((xpos +% wid, ypos +% hgtaxis -% halflen));
      ], None);
    ]
  in
  let kerninfo y =
    let widkern = widparen in
    let r = 0. in
    let gap = Length.abs (y -% hgtaxis) in
    let topdfpt = Length.to_pdf_point in  (* for debug *)
    let () = Printf.printf "Primitives> y = %f, hgtaxis = %f\n" (topdfpt y) (topdfpt hgtaxis) in  (* for debug *)
      if halflen *% r <% gap then
        widkern *% ((gap -% halflen *% r) /% (halflen *% (1. -. r)))
      else
        Length.zero
  in
  let hgtparen = hgtaxis +% halflen in
  let dptparen = hgtaxis -% halflen in
    ([HorzPure(PHGFixedGraphics(wid, hgtparen, dptparen, graphics))], kerninfo)


let default_math_right_paren hgt dpt hgtaxis fontsize color =
  let open HorzBox in
  let lenappend = fontsize *% 0.1 in
  let minhalflen = fontsize *% 0.5 in
  let halflen = Length.max minhalflen ((Length.max (hgt -% hgtaxis) (hgtaxis -% dpt)) +% lenappend) in
  let widparen = halflen *% 0.375 in
  let wid = widparen +% fontsize *% 0.1 in
  let graphics (xpos, ypos) =
    Graphics.pdfops_of_stroke (pdfpt 0.5) color [
      GeneralPath((xpos, ypos +% hgtaxis +% halflen), [
        LineTo((xpos +% widparen, ypos +% hgtaxis));
        LineTo((xpos, ypos +% hgtaxis -% halflen));
      ], None);
    ]
  in
  let kerninfo y =
    let widkern = widparen in
    let r = 0. in
    let gap = Length.abs (y -% hgtaxis) in
    let topdfpt = Length.to_pdf_point in  (* for debug *)
    let () = Printf.printf "Primitives> y = %f, hgtaxis = %f\n" (topdfpt y) (topdfpt hgtaxis) in  (* for debug *)
      if halflen *% r <% gap then
        widkern *% ((gap -% halflen *% r) /% (halflen *% (1. -. r)))
      else
        Length.zero
  in
  let hgtparen = hgtaxis +% halflen in
  let dptparen = hgtaxis -% halflen in
    ([HorzPure(PHGFixedGraphics(wid, hgtparen, dptparen, graphics))], kerninfo)
*)

let default_radical hgt_bar t_bar dpt fontsize color =
  let open HorzBox in
  let wM = fontsize *% 0.02 in
  let w1 = fontsize *% 0.1 in
  let w2 = fontsize *% 0.15 in
  let w3 = fontsize *% 0.4 in
  let wA = fontsize *% 0.18 in
  let h1 = fontsize *% 0.3 in
  let h2 = fontsize *% 0.375 in

  let dpt = dpt +% fontsize *% 0.1 in

  let lR = hgt_bar +% dpt in

  let wid = wM +% w1 +% w2 +% w3 in
  let a1 = (h2 -% h1) /% w1 in
  let a2 = h2 /% w2 in
  let a3 = lR /% w3 in
  let t1 = t_bar *% (sqrt (1. +. a1 *. a1)) in
  let t3 = t_bar *% ((sqrt (1. +. a3 *. a3) -. 1.) /. a3) in
  let hA = h1 +% t1 +% wA *% a1 in
  let wB = (lR +% t_bar -% hA -% (w1 +% w2 +% w3 -% t3 -% wA) *% a3) *% (-. 1. /. (a2 +. a3)) in
  let hB = hA -% wB *% a2 in

  let graphics (xpos, ypos) =
    let grelem =
      let open GraphicData in
      Graphics.make_fill color [
        GeneralPath((xpos +% wid, ypos +% hgt_bar), [
          LineTo(xpos +% wM +% w1 +% w2, ypos -% dpt);
          LineTo(xpos +% wM +% w1      , ypos -% dpt +% h2);
          LineTo(xpos +% wM            , ypos -% dpt +% h1);
          LineTo(xpos +% wM            , ypos -% dpt +% h1 +% t1);
          LineTo(xpos +% wM +% wA      , ypos -% dpt +% hA);
          LineTo(xpos +% wM +% wA +% wB, ypos -% dpt +% hB);
          LineTo(xpos +% wid -% t3     , ypos +% hgt_bar +% t_bar);
          LineTo(xpos +% wid           , ypos +% hgt_bar +% t_bar);
        ], Some(LineTo(())))
      ]
    in
      Graphics.singleton grelem
  in
    [HorzPure(PHGFixedGraphics(wid, hgt_bar +% t_bar, dpt, graphics))]


let default_math_variant_char_map : (HorzBox.math_variant_value) HorzBox.MathVariantCharMap.t =
  let open HorzBox in
  let code_point cp = MathVariantToChar(false, [Uchar.of_int cp]) in

  List.fold_left (fun map (s, mccls, mk, mvvmain) -> map |> MathVariantCharMap.add (s, mccls) (mk, mvvmain)) MathVariantCharMap.empty
    (List.concat [

    (* -- Latin capital letter to its normal italic -- *)
      (range 0 25) |> List.map (fun i ->
        (ascii_capital_of_index i, MathItalic, MathOrdinary, code_point (0x1D434 + i)));
    (* -- Latin small letter to its normal italic -- *)
      (List.append (range 0 6) (range 8 25)) |> List.map (fun i ->
        (ascii_small_of_index i, MathItalic, MathOrdinary, code_point (0x1D44E + i)));
      [("h", MathItalic, MathOrdinary, code_point 0x210E)];

    (* -- Latin capital letter to its bold italic -- *)
      (range 0 25) |> List.map (fun i ->
        (ascii_capital_of_index i, MathBoldItalic, MathOrdinary, code_point (0x1D468 + i)));
    (* -- Latin small letter to its bold italic -- *)
      (range 0 25) |> List.map (fun i ->
        (ascii_small_of_index i, MathBoldItalic, MathOrdinary, code_point (0x1D482 + i)));

    (* -- Latin capital letter to its roman -- *)
      (range 0 25) |> List.map (fun i ->
        (ascii_capital_of_index i, MathRoman, MathOrdinary, code_point (Char.code 'A' + i)));
    (* -- Latin small letter to its roman -- *)
      (range 0 25) |> List.map (fun i ->
        (ascii_small_of_index i, MathRoman, MathOrdinary, code_point (Char.code 'a' + i)));

    (* -- Latin capital letter to its bold romain -- *)
      (range 0 25) |> List.map (fun i ->
        (ascii_capital_of_index i, MathBoldRoman, MathOrdinary, code_point (0x1D400 + i)));
    (* -- Latin small letter to its bold roman -- *)
      (range 0 25) |> List.map (fun i ->
        (ascii_small_of_index i, MathBoldRoman, MathOrdinary, code_point (0x1D41A + i)));

    (* -- Latin capital letter to its script -- *)
      [[0]; [2; 3]; [6]; [9; 10]; range 13 16; range 18 25] |> List.concat |> List.map (fun i ->
        (ascii_capital_of_index i, MathScript, MathOrdinary, code_point (0x1D49C + i)));
      [
        ("B", MathScript, MathOrdinary, code_point 0x212C);
        ("E", MathScript, MathOrdinary, code_point 0x2130);
        ("F", MathScript, MathOrdinary, code_point 0x2131);
        ("H", MathScript, MathOrdinary, code_point 0x210B);
        ("I", MathScript, MathOrdinary, code_point 0x2110);
        ("L", MathScript, MathOrdinary, code_point 0x2112);
        ("M", MathScript, MathOrdinary, code_point 0x2133);
        ("R", MathScript, MathOrdinary, code_point 0x211B);
      ];
    (* -- Latin small letter to its script -- *)
      [range 0 3; [5]; range 7 13; range 15 25] |> List.concat |> List.map (fun i ->
        (ascii_small_of_index i, MathScript, MathOrdinary, code_point (0x1D4B6 + i)));
      [
        ("e", MathScript, MathOrdinary, code_point 0x212F);
        ("g", MathScript, MathOrdinary, code_point 0x210A);
        ("o", MathScript, MathOrdinary, code_point 0x2134);
      ];

    (* -- Latin capital letter to its bold script -- *)
      (range 0 25) |> List.map (fun i ->
        (ascii_capital_of_index i, MathBoldScript, MathOrdinary, code_point (0x1D4D0 + i)));
    (* -- Latin small letter to its bold script -- *)
      (range 0 25) |> List.map (fun i ->
        (ascii_small_of_index i, MathBoldScript, MathOrdinary, code_point (0x1D4EA + i)));

    (* -- Latin capital letter to its Fraktur -- *)
      [[0; 1]; range 3 6; range 9 16; range 18 24] |> List.concat |> List.map (fun i ->
        (ascii_capital_of_index i, MathFraktur, MathOrdinary, code_point (0x1D504 + i)));
      [
        ("C", MathFraktur, MathOrdinary, code_point 0x212D);
        ("H", MathFraktur, MathOrdinary, code_point 0x210C);
        ("I", MathFraktur, MathOrdinary, code_point 0x2111);
        ("R", MathFraktur, MathOrdinary, code_point 0x211C);
        ("Z", MathFraktur, MathOrdinary, code_point 0x2128);
      ];
    (* -- Latin small letter to its Fraktur -- *)
      (range 0 25) |> List.map (fun i ->
        (ascii_small_of_index i, MathFraktur, MathOrdinary, code_point (0x1D51E + i)));

    (* -- Latin capital letter to its bold Fraktur -- *)
      (range 0 25) |> List.map (fun i ->
        (ascii_capital_of_index i, MathBoldFraktur, MathOrdinary, code_point (0x1D56C + i)));
    (* -- Latin small letter to its bold Fraktur -- *)
      (range 0 25) |> List.map (fun i ->
        (ascii_small_of_index i, MathBoldFraktur, MathOrdinary, code_point (0x1D586 + i)));

    (* -- Latin capital letter to its double struck -- *)
      [[0; 1]; range 3 6; range 8 12; [14]; range 18 24] |> List.concat |> List.map (fun i ->
        (ascii_capital_of_index i, MathDoubleStruck, MathOrdinary, code_point (0x1D538 + i)));
      [
        ("C", MathDoubleStruck, MathOrdinary, code_point 0x2102);
        ("H", MathDoubleStruck, MathOrdinary, code_point 0x210D);
        ("N", MathDoubleStruck, MathOrdinary, code_point 0x2115);
        ("P", MathDoubleStruck, MathOrdinary, code_point 0x2119);
        ("Q", MathDoubleStruck, MathOrdinary, code_point 0x211A);
        ("R", MathDoubleStruck, MathOrdinary, code_point 0x211D);
        ("Z", MathDoubleStruck, MathOrdinary, code_point 0x2124);
      ];
    (* -- Latin small letter to its double struck -- *)
      (range 0 25) |> List.map (fun i ->
        (ascii_small_of_index i, MathDoubleStruck, MathOrdinary, code_point (0x1D552 + i)));

    (* -- invariant ascii symbols -- *)
      [
        ("=", Char.code '=', MathRelation);
        ("<", Char.code '<', MathRelation);
        (">", Char.code '>', MathRelation);
        (":", Char.code ':', MathRelation);
        ("+", Char.code '+', MathBinary  );
        ("-", 0x2212       , MathBinary  );
        ("|", Char.code '|', MathBinary  );
        ("/", Char.code '/', MathOrdinary);
        (",", Char.code ',', MathPunct   );
      ] |> List.map (fun (s, cp, mk) ->
        (s, MathItalic, mk, code_point cp)); (* temporary *)
    ])


let default_font_scheme_ref = ref CharBasis.ScriptSchemeMap.empty

let default_hyphen_dictionary = ref LoadHyph.empty

let get_initial_context wid =
  let open HorzBox in
    {
      hyphen_dictionary      = !default_hyphen_dictionary;
      hyphen_badness         = 100;
      font_scheme            = !default_font_scheme_ref;
      font_size              = pdfpt 12.;
      math_font              = "lmodern";  (* TEMPORARY *)
      dominant_wide_script   = CharBasis.OtherScript;
      dominant_narrow_script = CharBasis.OtherScript;
      langsys_scheme         = CharBasis.ScriptSchemeMap.empty;
      space_natural          = 0.33;
      space_shrink           = 0.08;
      space_stretch          = 0.16;
      adjacent_stretch       = 0.025;
      paragraph_width        = wid;
      paragraph_top          = pdfpt 18.;
      paragraph_bottom       = pdfpt 18.;
      leading                = pdfpt 18.;
      min_gap_of_lines       = pdfpt 2.;
      text_color             = GraphicData.DeviceGray(0.);
      manual_rising          = pdfpt 0.;
      badness_space          = 100;
      math_variant_char_map  = default_math_variant_char_map;
      math_char_class        = MathItalic;
    }
(*
let margin = pdfpt 2.


let frame_deco_VS =
  (fun (xpos, ypos) wid hgt dpt ->
    let xposb = xpos in
    let hgtb = hgt in
    let dptb = dpt in
    let widb = wid in
      Graphics.pdfops_of_graphics default_graphics_context HorzBox.DrawStroke [
        HorzBox.Rectangle((xposb, ypos +% dptb), (widb, hgtb -% dptb));
      ]
  )

let frame_deco_VH =
  (fun (xpos, ypos) wid hgt dpt ->
    let xposb = xpos in
    let hgtb = hgt in
    let dptb = dpt in
    let widb = wid in
      Graphics.pdfops_of_graphics default_graphics_context HorzBox.DrawStroke [
        HorzBox.GeneralPath((xposb, ypos +% dptb), [
          HorzBox.LineTo(xposb, ypos +% hgtb);
          HorzBox.LineTo(xposb +% widb, ypos +% hgtb);
          HorzBox.LineTo(xposb +% widb, ypos +% dptb);
        ], None);
      ]
  )

let frame_deco_VT =
  (fun (xpos, ypos) wid hgt dpt ->
    let xposb = xpos in
    let hgtb = hgt in
    let dptb = dpt in
    let widb = wid in
      Graphics.pdfops_of_graphics default_graphics_context HorzBox.DrawStroke [
        HorzBox.GeneralPath((xposb, ypos +% hgtb), [
          HorzBox.LineTo(xposb, ypos +% dptb);
          HorzBox.LineTo(xposb +% widb, ypos +% dptb);
          HorzBox.LineTo(xposb +% widb, ypos +% hgtb);
        ], None);
      ]
  )

let frame_deco_VM =
  (fun (xpos, ypos) wid hgt dpt ->
    let xposb = xpos in
    let hgtb = hgt in
    let dptb = dpt in
    let widb = wid in
    List.append (
      Graphics.pdfops_of_graphics default_graphics_context HorzBox.DrawStroke [
        HorzBox.GeneralPath((xposb, ypos +% hgtb), [
          HorzBox.LineTo(xposb, ypos +% dptb);
        ], None);
      ]
    ) (
      Graphics.pdfops_of_graphics default_graphics_context HorzBox.DrawStroke [
        HorzBox.GeneralPath((xposb +% widb, ypos +% hgtb), [
          HorzBox.LineTo(xposb +% widb, ypos +% dptb);
        ], None);
      ]
    )
  )
*)
(* -- end: constants just for experimental use -- *)


let make_environments () =
  let tyenvinit = add_default_types Typeenv.empty in
  let envinit : environment = (EvalVarIDMap.empty, ref (StoreIDHashTable.create 128)) in

  let (~@) n        = (~! "tv"      , TypeVariable(n)      ) in
  let (-%) n ptysub = ptysub in
  let (~%) ty       = Poly(ty) in

  let mckf          = tLN @-> tLN @-> tLN in

  let tv1 = (let bid1 = BoundID.fresh UniversalKind () in ref (Bound(bid1))) in
  let tv2 = (let bid2 = BoundID.fresh UniversalKind () in ref (Bound(bid2))) in

  let table : (var_name * poly_type * (environment -> syntactic_value)) list =
    let ptyderef  = tv1 -% (~% ((tR (~@ tv1)) @-> (~@ tv1))) in
    let ptycons   = tv2 -% (~% ((~@ tv2) @-> (tL (~@ tv2)) @-> (tL (~@ tv2)))) in
    let ptyappinv = tv1 -% (tv2 -% (~% ((~@ tv1) @-> ((~@ tv1) @-> (~@ tv2)) @-> (~@ tv2)))) in
      [
        ( "+"  , ~% (tI @-> tI @-> tI)   , lambda2 (fun v1 v2 -> Plus(v1, v2))                    );
        ( "-"  , ~% (tI @-> tI @-> tI)   , lambda2 (fun v1 v2 -> Minus(v1, v2))                   );
        ( "mod", ~% (tI @-> tI @-> tI)   , lambda2 (fun v1 v2 -> Mod(v1, v2))                     );
        ( "*"  , ~% (tI @-> tI @-> tI)   , lambda2 (fun v1 v2 -> Times(v1, v2))                   );
        ( "/"  , ~% (tI @-> tI @-> tI)   , lambda2 (fun v1 v2 -> Divides(v1, v2))                 );
        ( "^"  , ~% (tS @-> tS @-> tS)   , lambda2 (fun v1 v2 -> Concat(v1, v2))                  );
        ( "==" , ~% (tI @-> tI @-> tB)   , lambda2 (fun v1 v2 -> EqualTo(v1, v2))                 );
        ( "<>" , ~% (tI @-> tI @-> tB)   , lambda2 (fun v1 v2 -> LogicalNot(EqualTo(v1, v2)))     );
        ( ">"  , ~% (tI @-> tI @-> tB)   , lambda2 (fun v1 v2 -> GreaterThan(v1, v2))             );
        ( "<"  , ~% (tI @-> tI @-> tB)   , lambda2 (fun v1 v2 -> LessThan(v1, v2))                );
        ( ">=" , ~% (tI @-> tI @-> tB)   , lambda2 (fun v1 v2 -> LogicalNot(LessThan(v1, v2)))    );
        ( "<=" , ~% (tI @-> tI @-> tB)   , lambda2 (fun v1 v2 -> LogicalNot(GreaterThan(v1, v2))) );
        ( "&&" , ~% (tB @-> tB @-> tB)   , lambda2 (fun v1 v2 -> LogicalAnd(v1, v2))              );
        ( "||" , ~% (tB @-> tB @-> tB)   , lambda2 (fun v1 v2 -> LogicalOr(v1, v2))               );
        ( "not", ~% (tB @-> tB)          , lambda1 (fun v1 -> LogicalNot(v1))                     );
        ( "!"  , ptyderef                , lambda1 (fun v1 -> Dereference(v1))                    );
        ( "::" , ptycons                 , lambda2 (fun v1 v2 -> PrimitiveListCons(v1, v2))       );
        ( "+." , ~% (tFL @-> tFL @-> tFL), lambda2 (fun v1 v2 -> FloatPlus(v1, v2))               );
        ( "-." , ~% (tFL @-> tFL @-> tFL), lambda2 (fun v1 v2 -> FloatMinus(v1, v2))              );
        ( "*." , ~% (tFL @-> tFL @-> tFL), lambda2 (fun v1 v2 -> FloatTimes(v1, v2))              );
        ( "/." , ~% (tFL @-> tFL @-> tFL), lambda2 (fun v1 v2 -> FloatDivides(v1, v2))            );
        ( "+'" , ~% (tLN @-> tLN @-> tLN), lambda2 (fun v1 v2 -> LengthPlus(v1, v2))              );
        ( "-'" , ~% (tLN @-> tLN @-> tLN), lambda2 (fun v1 v2 -> LengthMinus(v1, v2))             );
        ( "*'" , ~% (tLN @-> tFL @-> tLN), lambda2 (fun v1 v2 -> LengthTimes(v1, v2))             );
        ( "/'" , ~% (tLN @-> tLN @-> tFL), lambda2 (fun v1 v2 -> LengthDivides(v1, v2))           );
        ( "<'" , ~% (tLN @-> tLN @-> tB) , lambda2 (fun v1 v2 -> LengthLessThan(v1, v2))          );
        ( ">'" , ~% (tLN @-> tLN @-> tB) , lambda2 (fun v1 v2 -> LengthGreaterThan(v1, v2))       );
        ( "++" , ~% (tIB @-> tIB @-> tIB), lambda2 (fun vbr1 vbr2 -> HorzConcat(vbr1, vbr2))      );
        ( "+++", ~% (tBB @-> tBB @-> tBB), lambda2 (fun vbc1 vbc2 -> VertConcat(vbc1, vbc2))      );
        ( "|>" , ptyappinv               , lambda2 (fun vx vf -> Apply(vf, vx)));

        ( "string-same"  , ~% (tS @-> tS @-> tB)       , lambda2 (fun v1 v2 -> PrimitiveSame(v1, v2)) );
        ( "string-sub"   , ~% (tS @-> tI @-> tI @-> tS), lambda3 (fun vstr vpos vwid -> PrimitiveStringSub(vstr, vpos, vwid)) );
        ( "string-length", ~% (tS @-> tI)              , lambda1 (fun vstr -> PrimitiveStringLength(vstr)) );
        ( "arabic"       , ~% (tI @-> tS)              , lambda1 (fun vnum -> PrimitiveArabic(vnum)) );
        ( "float"        , ~% (tI @-> tFL)             , lambda1 (fun vi -> PrimitiveFloat(vi)) );
        ( "round"        , ~% (tFL @-> tI)             , lambda1 (fun vf -> PrimitiveRound(vf)) );
        ( "sin"          , ~% (tFL @-> tFL)            , lambda1 (fun vf -> FloatSine(vf)) );
        ( "asin"         , ~% (tFL @-> tFL)            , lambda1 (fun vf -> FloatArcSine(vf)) );
        ( "cos"          , ~% (tFL @-> tFL)            , lambda1 (fun vf -> FloatCosine(vf)) );
        ( "acos"         , ~% (tFL @-> tFL)            , lambda1 (fun vf -> FloatArcCosine(vf)) );
        ( "tan"          , ~% (tFL @-> tFL)            , lambda1 (fun vf -> FloatTangent(vf)) );
        ( "atan"         , ~% (tFL @-> tFL)            , lambda1 (fun vf -> FloatArcTangent(vf)) );
        ( "atan2"        , ~% (tFL @-> tFL @-> tFL)    , lambda2 (fun vf1 vf2 -> FloatArcTangent2(vf1, vf2)) );
        ("split-into-lines", ~% (tS @-> (tL (tPROD [tI; tS]))), lambda1 (fun vs -> PrimitiveSplitIntoLines(vs)));

        ("line-break"            , ~% (tB @-> tB @-> tCTX @-> tIB @-> tBB)                   , lambda4 (fun vb1 vb2 vctx vbr -> BackendLineBreaking(vb1, vb2, vctx, vbr)) );
        ("page-break"            , ~% (tPG @-> tPAGECONTF @-> tPAGEPARTSF @-> tBB @-> tDOC)  , lambda4 (fun vpgsz vpcf vppf vbb -> BackendPageBreaking(vpgsz, vpcf, vppf, vbb)));
        ("inline-skip"           , ~% (tLN @-> tIB)                                          , lambda1 (fun vwid -> BackendFixedEmpty(vwid))   );
        ("inline-glue"           , ~% (tLN @-> tLN @-> tLN @-> tIB)                          , lambda3 (fun vn vp vm -> BackendOuterEmpty(vn, vp, vm)) );
        ("inline-fil"            , ~% tIB                                                    , (fun _ -> Horz(HorzBox.([HorzPure(PHSOuterFil)]))));
        ("inline-nil"            , ~% tIB                                                    , (fun _ -> Horz([])));
        ("inline-frame-outer"    , ~% (tPADS @-> tDECO @-> tIB @-> tIB)                      , lambda3 (fun vpads vdeco vbr -> BackendOuterFrame(vpads, vdeco, vbr)));
        ("inline-frame-inner"    , ~% (tPADS @-> tDECO @-> tIB @-> tIB)                      , lambda3 (fun vpads vdeco vbr -> BackendInnerFrame(vpads, vdeco, vbr)));
        ("inline-frame-fixed"    , ~% (tLN @-> tPADS @-> tDECO @-> tIB @-> tIB)              , lambda4 (fun vwid vpads vdeco vbr -> BackendFixedFrame(vwid, vpads, vdeco, vbr)));
        ("inline-frame-breakable", ~% (tPADS @-> tDECOSET @-> tIB @-> tIB)                   , lambda3 (fun vpads vdecoset vbr -> BackendOuterFrameBreakable(vpads, vdecoset, vbr)));
(*
        ("font"                  , ~% (tS @-> tFL @-> tFL @-> tFT)                           , lambda3 (fun vabbrv vszrat vrsrat -> BackendFont(vabbrv, vszrat, vrsrat)));
*)
        ("block-nil"             , ~% tBB                                                    , (fun _ -> Vert([])));
        ("block-frame-breakable" , ~% (tCTX @-> tPADS @-> tDECOSET @-> (tCTX @-> tBB) @-> tBB), lambda4 (fun vctx vpads vdecoset vbc -> BackendVertFrame(vctx, vpads, vdecoset, vbc)));
        ("block-skip"            , ~% (tLN @-> tBB)                                          , lambda1 (fun vlen -> BackendVertSkip(vlen)));
        ("clear-page"            , ~% tBB                                                    , (fun _ -> Vert(HorzBox.([VertClearPage]))));
        ("embed-block-top"       , ~% (tCTX @-> tLN @-> (tCTX @-> tBB) @-> tIB)              , lambda3 (fun vctx vlen vk -> BackendEmbeddedVertTop(vctx, vlen, vk)));
        ("embed-block-bottom"    , ~% (tCTX @-> tLN @-> (tCTX @-> tBB) @-> tIB)              , lambda3 (fun vctx vlen vk -> BackendEmbeddedVertBottom(vctx, vlen, vk)));
        ("line-stack-top"        , ~% ((tL tIB) @-> tIB)                                     , lambda1 (fun vlst -> BackendLineStackTop(vlst)));
        ("line-stack-bottom"     , ~% ((tL tIB) @-> tIB)                                     , lambda1 (fun vlst -> BackendLineStackBottom(vlst)));

        ("read-inline", ~% (tCTX @-> tIT @-> tIB), lambda2 (fun vctx vtr -> HorzLex(vctx, vtr)));
        ("read-block" , ~% (tCTX @-> tBT @-> tBB), lambda2 (fun vctx vtc -> VertLex(vctx, vtc)));

        ("get-initial-context", ~% (tLN @-> tCMD @-> tCTX)               , lambda2 (fun vwid vcmd -> PrimitiveGetInitialContext(vwid, vcmd)));
        ("set-space-ratio"    , ~% (tFL @-> tCTX @-> tCTX)               , lambda2 (fun vratio vctx -> PrimitiveSetSpaceRatio(vratio, vctx)));
        ("set-paragraph-margin", ~% (tLN @-> tLN @-> tCTX @-> tCTX)      , lambda3 (fun vl1 vl2 vctx -> PrimitiveSetParagraphMargin(vl1, vl2, vctx)));
        ("set-font-size"      , ~% (tLN @-> tCTX @-> tCTX)               , lambda2 (fun vsize vctx -> PrimitiveSetFontSize(vsize, vctx)));
        ("get-font-size"      , ~% (tCTX @-> tLN)                        , lambda1 (fun vctx -> PrimitiveGetFontSize(vctx)));
        ("set-font"           , ~% (tSCR @-> tFONT @-> tCTX @-> tCTX)    , lambda3 (fun vscript vfont vctx -> PrimitiveSetFont(vscript, vfont, vctx)));
        ("get-font"           , ~% (tSCR @-> tCTX @-> tFONT)             , lambda2 (fun vscript vctx -> PrimitiveGetFont(vscript, vctx)));
        ("set-language"       , ~% (tSCR @-> tLANG @-> tCTX @-> tCTX)    , lambda3 (fun vscript vlang vctx -> PrimitiveSetLangSys(vscript, vlang, vctx)));
        ("get-language"       , ~% (tSCR @-> tCTX @-> tLANG)             , lambda2 (fun vscript vctx -> PrimitiveGetLangSys(vscript, vctx)));
        ("set-math-font"      , ~% (tS @-> tCTX @-> tCTX)                , lambda2 (fun vs vctx -> PrimitiveSetMathFont(vs, vctx)));
        ("set-dominant-wide-script", ~% (tSCR @-> tCTX @-> tCTX)              , lambda2 (fun vscript vctx -> PrimitiveSetDominantWideScript(vscript, vctx)));
        ("get-dominant-wide-script", ~% (tCTX @-> tSCR)                       , lambda1 (fun vctx -> PrimitiveGetDominantWideScript(vctx)));
        ("set-dominant-narrow-script", ~% (tSCR @-> tCTX @-> tCTX)              , lambda2 (fun vscript vctx -> PrimitiveSetDominantNarrowScript(vscript, vctx)));
        ("get-dominant-narrow-script", ~% (tCTX @-> tSCR)                       , lambda1 (fun vctx -> PrimitiveGetDominantNarrowScript(vctx)));
        ("set-text-color"     , ~% (tCLR @-> tCTX @-> tCTX)              , lambda2 (fun vcolor vctx -> PrimitiveSetTextColor(vcolor, vctx)));
        ("get-text-color"     , ~% (tCTX @-> tCLR)                       , lambda1 (fun vctx -> PrimitiveGetTextColor(vctx)));
        ("set-leading"        , ~% (tLN @-> tCTX @-> tCTX)               , lambda2 (fun vlen vctx -> PrimitiveSetLeading(vlen, vctx)));
        ("set-manual-rising"  , ~% (tLN @-> tCTX @-> tCTX)               , lambda2 (fun vlen vctx -> PrimitiveSetManualRising(vlen, vctx)));
        ("set-hyphen-penalty" , ~% (tI @-> tCTX @-> tCTX)                , lambda2 (fun vpnlty vctx -> PrimitiveSetHyphenPenalty(vpnlty, vctx)));
        ("get-text-width"     , ~% (tCTX @-> tLN)                        , lambda1 (fun vctx -> PrimitiveGetTextWidth(vctx)));

        ("embed-string"       , ~% (tS @-> tIT)                          , lambda1 (fun vstr -> PrimitiveEmbed(vstr)));
        ("inline-graphics"    , ~% (tLN @-> tLN @-> tLN @-> tIGR @-> tIB), lambda4 (fun vwid vhgt vdpt vg -> BackendInlineGraphics(vwid, vhgt, vdpt, vg)));
        ("get-natural-width"  , ~% (tIB @-> tLN)                         , lambda1 (fun vbr -> PrimitiveGetNaturalWidth(vbr)));
        ("get-natural-length" , ~% (tBB @-> tLN)                         , lambda1 (fun vbb -> PrimitiveGetNaturalLength(vbb)));
        ("display-message"    , ~% (tS @-> tU)                           , lambda1 (fun vs -> PrimitiveDisplayMessage(vs)));

        ("stroke"                  , ~% (tLN @-> tCLR @-> tPATH @-> tGR)             , lambda3 (fun vwid vclr vpath -> PrimitiveDrawStroke(vwid, vclr, vpath)));
        ("dashed-stroke"           , ~% (tLN @-> tDASH @-> tCLR @-> tPATH @-> tGR)   , lambda4 (fun vwid vdash vclr vpath -> PrimitiveDrawDashedStroke(vwid, vdash, vclr, vpath)));
        ("fill"                    , ~% (tCLR @-> tPATH @-> tGR)                     , lambda2 (fun vclr vpath -> PrimitiveDrawFill(vclr, vpath)));
        ("draw-text"               , ~% (tPT @-> tIB @-> tGR)                        , lambda2 (fun vpt vbr -> PrimitiveDrawText(vpt, vbr)));
        ("start-path"              , ~% (tPT @-> tPRP)                               , lambda1 (fun vpt -> PrePathBeginning(vpt)));
        ("line-to"                 , ~% (tPT @-> tPRP @-> tPRP)                      , lambda2 (fun vpt vprp -> PrePathLineTo(vpt, vprp)));
        ("bezier-to"               , ~% (tPT @-> tPT @-> tPT @-> tPRP @-> tPRP)      , lambda4 (fun vptS vptT vpt1 vprp -> PrePathCubicBezierTo(vptS, vptT, vpt1, vprp)));
        ("terminate-path"          , ~% (tPRP @-> tPATH)                             , lambda1 (fun vprp -> PrePathTerminate(vprp)));
        ("close-with-line"         , ~% (tPRP @-> tPATH)                             , lambda1 (fun vprp -> PrePathCloseWithLine(vprp)));
        ("close-with-bezier"       , ~% (tPT @-> tPT @-> tPRP @-> tPATH)             , lambda3 (fun vptS vptT vprp -> PrePathCloseWithCubicBezier(vptS, vptT, vprp)));
        ("unite-path"              , ~% (tPATH @-> tPATH @-> tPATH)                  , lambda2 (fun vpath1 vpath2 -> PathUnite(vpath1, vpath2)));

        ("math-char"               , ~% (tMATHCLS @-> tS @-> tMATH)                  , lambda2 (fun vmc vs -> BackendMathChar(vmc, false, vs)));
        ("math-big-char"           , ~% (tMATHCLS @-> tS @-> tMATH)                  , lambda2 (fun vmc vs -> BackendMathChar(vmc, true, vs)));
        ("math-char-with-kern"     , ~% (tMATHCLS @-> tS @-> mckf @-> mckf @-> tMATH), lambda4 (fun vmc vs vkfL vkfR -> BackendMathCharWithKern(vmc, false, vs, vkfL, vkfR)));
        ("math-big-char-with-kern" , ~% (tMATHCLS @-> tS @-> mckf @-> mckf @-> tMATH), lambda4 (fun vmc vs vkfL vkfR -> BackendMathCharWithKern(vmc, true, vs, vkfL, vkfR)));
        ("math-group"              , ~% (tMATHCLS @-> tMATHCLS @-> tMATH @-> tMATH)  , lambda3 (fun vmc1 vmc2 vm -> BackendMathGroup(vmc1, vmc2, vm)));
        ("math-sup"                , ~% (tMATH @-> tMATH @-> tMATH)                  , lambda2 (fun vm1 vm2 -> BackendMathSuperscript(vm1, vm2)));
        ("math-sub"                , ~% (tMATH @-> tMATH @-> tMATH)                  , lambda2 (fun vm1 vm2 -> BackendMathSubscript(vm1, vm2)));
        ("math-frac"               , ~% (tMATH @-> tMATH @-> tMATH)                  , lambda2 (fun vm1 vm2 -> BackendMathFraction(vm1, vm2)));
        ("math-radical"            , ~% (tOPT tMATH @-> tMATH @-> tMATH)             , lambda2 (fun vm1 vm2 -> BackendMathRadical(vm1, vm2)));
        ("math-paren"              , ~% (tPAREN @-> tPAREN @-> tMATH @-> tMATH)      , lambda3 (fun vpL vpR vm -> BackendMathParen(vpL, vpR, vm)));
        ("math-upper"              , ~% (tMATH @-> tMATH @-> tMATH)                  , lambda2 (fun vm1 vm2 -> BackendMathUpperLimit(vm1, vm2)));
        ("math-lower"              , ~% (tMATH @-> tMATH @-> tMATH)                  , lambda2 (fun vm1 vm2 -> BackendMathLowerLimit(vm1, vm2)));
        ("math-concat"             , ~% (tMATH @-> tMATH @-> tMATH)                  , lambda2 (fun vm1 vm2 -> BackendMathConcat(vm1, vm2)));
        ("math-variant-char"       , ~% (tMATHCLS @-> tMCSTY @-> tMATH)              , lambda2 (fun vmc vrcd -> BackendMathVariantCharDirect(vmc, vrcd)));
            (* TEMPORARY; shold extend more *)
        ("math-color"              , ~% (tCLR @-> tMATH @-> tMATH)                   , lambda2 (fun vcolor vm -> BackendMathColor(vcolor, vm)));
        ("math-char-class"         , ~% (tMCCLS @-> tMATH @-> tMATH)                 , lambda2 (fun vmcc vm -> BackendMathCharClass(vmcc, vm)));
        ("set-math-variant-char"   , ~% (tS @-> tMCCLS @-> tMATHCLS @-> tI @-> tCTX @-> tCTX), lambda5 (fun vs vmcc vmc vcp vctx -> PrimitiveSetMathVariantToChar(vs, vmcc, vmc, vcp, vctx)));
        ("set-math-command"        , ~% (tCMD @-> tCTX @-> tCTX)                     , lambda2 (fun vcmd vctx -> PrimitiveSetMathCommand(vcmd, vctx)));
        ("text-in-math"            , ~% (tMATHCLS @-> (tCTX @-> tIB) @-> tMATH)      , lambda2 (fun vmc vbrf -> BackendMathText(vmc, vbrf)));
        ("embed-math"              , ~% (tCTX @-> tMATH @-> tIB)                     , lambda2 (fun vctx vm -> BackendEmbeddedMath(vctx, vm)));
        ("get-axis-height"         , ~% (tCTX @-> tLN)                               , lambda1 (fun vctx -> PrimitiveGetAxisHeight(vctx)));
        ("string-unexplode"        , ~% ((tL tI) @-> tS)                             , lambda1 (fun vil -> PrimitiveStringUnexplode(vil)));
        ("tabular"                 , ~% ((tL (tL tCELL)) @-> tRULESF @-> tIB)        , lambda2 (fun vtblr vrulesf -> BackendTabular(vtblr, vrulesf)));
        ("load-pdf-image"          , ~% (tS @-> tI @-> tIMG)                         , lambda2 (fun vs vpn -> BackendRegisterPdfImage(vs, vpn)));
        ("load-image"              , ~% (tS @-> tIMG)                                , lambda1 (fun vs -> BackendRegisterOtherImage(vs)));
        ("use-image-by-width"      , ~% (tIMG @-> tLN @-> tIB)                       , lambda2 (fun vimg vlen -> BackendUseImageByWidth(vimg, vlen)));
        ("script-guard"            , ~% (tSCR @-> tIB @-> tIB)                       , lambda2 (fun vscr vh -> BackendScriptGuard(vscr, vh)));
        ("discretionary"           , ~% (tI @-> tIB @-> tIB @-> tIB @-> tIB)         , lambda4 (fun vpb vib0 vib1 vib2 -> BackendDiscretionary(vpb, vib0, vib1, vib2)));
        ("register-cross-reference", ~% (tS @-> tS @-> tU)                           , lambda2 (fun vk vv -> BackendRegisterCrossReference(vk, vv)));
        ("get-cross-reference"     , ~% (tS @-> (tOPT tS))                           , lambda1 (fun vk -> BackendGetCrossReference(vk)));
        ("hook-page-break"         , ~% ((tPBINFO @-> tPT @-> tU) @-> tIB)           , lambda1 (fun vhook -> BackendHookPageBreak(vhook)));
      ]
  in
  let temporary_ast = StringEmpty in
  let (tyenvfinal, envfinal, locacc) =
    table |> List.fold_left (fun (tyenv, env, acc) (varnm, pty, deff) ->
      let evid = EvalVarID.fresh varnm in
      let loc = ref temporary_ast in
      let tyenvnew = Typeenv.add tyenv varnm (pty, evid) in
      let envnew = add_to_environment env evid loc in
        (tyenvnew, envnew, Alist.extend acc (loc, deff))
    ) (tyenvinit, envinit, Alist.empty)
  in
  locacc |> Alist.to_list |> List.iter (fun (loc, deff) -> loc := deff envfinal);
  default_font_scheme_ref := SetDefaultFont.main ();
  default_hyphen_dictionary := LoadHyph.main "english.satysfi-hyph";
      (* temporary; should depend on the current language -- *)
    (tyenvfinal, envfinal)
