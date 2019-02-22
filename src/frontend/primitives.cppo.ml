
open MyUtil
open LengthInterface
open GraphicBase
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
let tyid_igrafo   = Typeenv.Raw.fresh_type_id "inline-graphics-outer"

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

let tCTX          = (~! "context" , BaseType(ContextType) )
let tTCTX         = (~! "text-info", BaseType(TextInfoType))
let tPATH         = (~! "path"    , BaseType(PathType)    )
let tPRP          = (~! "pre-path", BaseType(PrePathType) )
let tDOC          = (~! "document", BaseType(DocumentType))
let tMATH         = (~! "math"    , BaseType(MathType)    )
let tGR           = (~! "graphics", BaseType(GraphicsType))
let tIMG          = (~! "image"   , BaseType(ImageType)   )
let tRE           = (~! "regexp"  , BaseType(RegExpType)  )
let tL ty         = (~! "list"    , ListType(ty)          )
let tR ty         = (~! "ref"     , RefType(ty)           )
let tPROD tylst   = (~! "product" , ProductType(tylst)    )
let (@->) dom cod = (~! "func"    , FuncType(OptionRowEmpty, dom, cod))

(* -- predefined data types -- *)
let tOPT ty       = (~! "option"  , VariantType([ty], tyid_option))
let tITMZ ()      = (~! "itemize" , VariantType([], tyid_itemize) )
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

let tIGRO_raw = tLN @-> tPT @-> (tL tGR)
let tIGRO = (~! "igrafo", SynonymType([], tyid_igrafo, tIGRO_raw))

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

let itemize_type () = tITMZ ()


let add_general_default_types (tyenvmid : Typeenv.t) : Typeenv.t =
  let dr = Range.dummy "add_default_types" in
  let bid = BoundID.fresh UniversalKind () in
  let typaram = (dr, TypeVariable(PolyBound(bid))) in

  tyenvmid
  |> Typeenv.Raw.register_type "option" tyid_option (Typeenv.Data(1))
  |> Typeenv.Raw.add_constructor "None" ([bid], Poly(tU)) tyid_option
  |> Typeenv.Raw.add_constructor "Some" ([bid], Poly(typaram)) tyid_option

  |> Typeenv.Raw.register_type "itemize" tyid_itemize (Typeenv.Data(0))
  |> Typeenv.Raw.add_constructor "Item" ([], Poly(tPROD [tIT; tL (tITMZ ())])) tyid_itemize

  |> Typeenv.Raw.register_type "math-class" tyid_mathcls (Typeenv.Data(0))
  |> Typeenv.Raw.add_constructor "MathOrd"    ([], Poly(tU)) tyid_mathcls
  |> Typeenv.Raw.add_constructor "MathBin"    ([], Poly(tU)) tyid_mathcls
  |> Typeenv.Raw.add_constructor "MathRel"    ([], Poly(tU)) tyid_mathcls
  |> Typeenv.Raw.add_constructor "MathOp"     ([], Poly(tU)) tyid_mathcls
  |> Typeenv.Raw.add_constructor "MathPunct"  ([], Poly(tU)) tyid_mathcls
  |> Typeenv.Raw.add_constructor "MathOpen"   ([], Poly(tU)) tyid_mathcls
  |> Typeenv.Raw.add_constructor "MathClose"  ([], Poly(tU)) tyid_mathcls
  |> Typeenv.Raw.add_constructor "MathPrefix" ([], Poly(tU)) tyid_mathcls
  |> Typeenv.Raw.add_constructor "MathInner"  ([], Poly(tU)) tyid_mathcls

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


let add_pdf_mode_default_types (tyenvmid : Typeenv.t) : Typeenv.t =

  tyenvmid
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

  |> Typeenv.Raw.register_type "cell" tyid_cell (Typeenv.Data(0))
  |> Typeenv.Raw.add_constructor "NormalCell" ([], Poly(tPROD [tPADS; tIB]))         tyid_cell
  |> Typeenv.Raw.add_constructor "EmptyCell"  ([], Poly(tU))                         tyid_cell
  |> Typeenv.Raw.add_constructor "MultiCell"  ([], Poly(tPROD [tI; tI; tPADS; tIB])) tyid_cell

  |> Typeenv.Raw.register_type "deco" tyid_deco (Typeenv.Alias(([], Poly(tDECO_raw))))

  |> Typeenv.Raw.register_type "deco-set" tyid_decoset (Typeenv.Alias(([], Poly(tDECOSET_raw))))

  |> Typeenv.Raw.register_type "inline-graphics" tyid_igraf (Typeenv.Alias(([], Poly(tIGR_raw))))


let lam evid ast =
  Function([], PatternBranch(PVariable(evid), ast))

let lamenv env evid arity ast astf =
  PrimitiveClosure(PatternBranch(PVariable(evid), ast), env, arity, astf)

let ( !- ) evid = ContentOf(Range.dummy "temporary", evid)

let dr = Range.dummy "dummy:lambda"

let rec lambda1 astf env =
  let evid1 = EvalVarID.fresh (dr, "(dummy:lambda1-1)") in
    lamenv env evid1 1 (astf (!- evid1))
      (fun lst -> match lst with
                  | [a1] -> astf a1
                  | _ -> failwith "internal error")

let rec lambda2 astf env =
  let evid1 = EvalVarID.fresh (dr, "(dummy:lambda2-1)") in
  let evid2 = EvalVarID.fresh (dr, "(dummy:lambda2-2)") in
    lamenv env evid1 2 (lam evid2 (astf (!- evid1) (!- evid2)))
      (fun lst -> match lst with
                  | [a1;a2] -> astf a1 a2
                  | _ -> failwith "internal error")

let rec lambda3 astf env =
  let evid1 = EvalVarID.fresh (dr, "(dummy:lambda3-1)") in
  let evid2 = EvalVarID.fresh (dr, "(dummy:lambda3-2)") in
  let evid3 = EvalVarID.fresh (dr, "(dummy:lambda3-3)") in
    lamenv env evid1 3 (lam evid2 (lam evid3 (astf (!- evid1) (!- evid2) (!- evid3))))
      (fun lst -> match lst with
                  | [a1;a2;a3] -> astf a1 a2 a3
                  | _ -> failwith "internal error")

let rec lambda4 astf env =
  let evid1 = EvalVarID.fresh (dr, "(dummy:lambda4-1)") in
  let evid2 = EvalVarID.fresh (dr, "(dummy:lambda4-2)") in
  let evid3 = EvalVarID.fresh (dr, "(dummy:lambda4-3)") in
  let evid4 = EvalVarID.fresh (dr, "(dummy:lambda4-4)") in
    lamenv env evid1 4 (lam evid2 (lam evid3 (lam evid4 (astf (!- evid1) (!- evid2) (!- evid3) (!- evid4)))))
      (fun lst -> match lst with
                  | [a1;a2;a3;a4] -> astf a1 a2 a3 a4
                  | _ -> failwith "internal error")

let rec lambda5 astf env =
  let evid1 = EvalVarID.fresh (dr, "(dummy:lambda5-1)") in
  let evid2 = EvalVarID.fresh (dr, "(dummy:lambda5-2)") in
  let evid3 = EvalVarID.fresh (dr, "(dummy:lambda5-3)") in
  let evid4 = EvalVarID.fresh (dr, "(dummy:lambda5-4)") in
  let evid5 = EvalVarID.fresh (dr, "(dummy:lambda5-5)") in
    lamenv env evid1 5 (lam evid2 (lam evid3 (lam evid4 (lam evid5 (astf (!- evid1) (!- evid2) (!- evid3) (!- evid4) (!- evid5))))))
      (fun lst -> match lst with
                  | [a1;a2;a3;a4;a5] -> astf a1 a2 a3 a4 a5
                  | _ -> failwith "internal error")

let rec lambda6 astf env =
  let evid1 = EvalVarID.fresh (dr, "(dummy:lambda6-1)") in
  let evid2 = EvalVarID.fresh (dr, "(dummy:lambda6-2)") in
  let evid3 = EvalVarID.fresh (dr, "(dummy:lambda6-3)") in
  let evid4 = EvalVarID.fresh (dr, "(dummy:lambda6-4)") in
  let evid5 = EvalVarID.fresh (dr, "(dummy:lambda6-5)") in
  let evid6 = EvalVarID.fresh (dr, "(dummy:lambda6-6)") in
    lamenv env evid1 6 (lam evid2 (lam evid3 (lam evid4 (lam evid5 (lam evid6 (astf (!- evid1) (!- evid2) (!- evid3) (!- evid4) (!- evid5) (!- evid6)))))))
      (fun lst -> match lst with
                  | [a1;a2;a3;a4;a5;a6] -> astf a1 a2 a3 a4 a5 a6
                  | _ -> failwith "internal error")

let rec lambda7 astf env =
  let evid1 = EvalVarID.fresh (dr, "(dummy:lambda7-1)") in
  let evid2 = EvalVarID.fresh (dr, "(dummy:lambda7-2)") in
  let evid3 = EvalVarID.fresh (dr, "(dummy:lambda7-3)") in
  let evid4 = EvalVarID.fresh (dr, "(dummy:lambda7-4)") in
  let evid5 = EvalVarID.fresh (dr, "(dummy:lambda7-5)") in
  let evid6 = EvalVarID.fresh (dr, "(dummy:lambda7-6)") in
  let evid7 = EvalVarID.fresh (dr, "(dummy:lambda7-7)") in
    lamenv env evid1 7 (lam evid2 (lam evid3 (lam evid4 (lam evid5 (lam evid6 (lam evid7 (astf (!- evid1) (!- evid2) (!- evid3) (!- evid4) (!- evid5) (!- evid6) (!- evid7))))))))
      (fun lst -> match lst with
                  | [a1;a2;a3;a4;a5;a6;a7] -> astf a1 a2 a3 a4 a5 a6 a7
                  | _ -> failwith "internal error")



let pdfpt = Length.of_pdf_point


let default_radical hgt_bar t_bar dpt fontsize color =
  let open HorzBox in
  let wM = fontsize *% 0.02 in
  let w1 = fontsize *% 0.1 in
  let w2 = fontsize *% 0.15 in
  let w3 = fontsize *% 0.4 in
  let wA = fontsize *% 0.18 in
  let h1 = fontsize *% 0.3 in
  let h2 = fontsize *% 0.375 in

  let nonnegdpt = (Length.negate dpt) +% fontsize *% 0.1 in

  let lR = hgt_bar +% nonnegdpt in

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
      GraphicD.make_fill color [
        GeneralPath((xpos +% wid, ypos +% hgt_bar), [
          LineTo(xpos +% wM +% w1 +% w2, ypos -% nonnegdpt);
          LineTo(xpos +% wM +% w1      , ypos -% nonnegdpt +% h2);
          LineTo(xpos +% wM            , ypos -% nonnegdpt +% h1);
          LineTo(xpos +% wM            , ypos -% nonnegdpt +% h1 +% t1);
          LineTo(xpos +% wM +% wA      , ypos -% nonnegdpt +% hA);
          LineTo(xpos +% wM +% wA +% wB, ypos -% nonnegdpt +% hB);
          LineTo(xpos +% wid -% t3     , ypos +% hgt_bar +% t_bar);
          LineTo(xpos +% wid           , ypos +% hgt_bar +% t_bar);
        ], Some(LineTo(())))
      ]
    in
      GraphicD.singleton grelem
  in
    [HorzPure(PHGFixedGraphics(wid, hgt_bar +% t_bar, nonnegdpt, graphics))]


let code_point cp = Uchar.of_int cp


let default_math_variant_char_map : Uchar.t HorzBox.MathVariantCharMap.t =
  let open HorzBox in

  List.fold_left (fun map (uchfrom, mccls, uchto) ->
    map |> MathVariantCharMap.add (uchfrom, mccls) uchto
  ) MathVariantCharMap.empty
    (List.concat [

    (* -- Latin capital letter to its normal italic -- *)
      (range 0 25) |> List.map (fun i ->
        (ascii_capital_of_index i, MathItalic, code_point (0x1D434 + i)));
    (* -- Latin small letter to its normal italic -- *)
      (List.append (range 0 6) (range 8 25)) |> List.map (fun i ->
        (ascii_small_of_index i, MathItalic, code_point (0x1D44E + i)));
      [(uchar_of_char 'h', MathItalic, code_point 0x210E)];

    (* -- Latin capital letter to its bold italic -- *)
      (range 0 25) |> List.map (fun i ->
        (ascii_capital_of_index i, MathBoldItalic, code_point (0x1D468 + i)));
    (* -- Latin small letter to its bold italic -- *)
      (range 0 25) |> List.map (fun i ->
        (ascii_small_of_index i, MathBoldItalic, code_point (0x1D482 + i)));

    (* -- Latin capital letter to its roman -- *)
      (range 0 25) |> List.map (fun i ->
        (ascii_capital_of_index i, MathRoman, code_point (Char.code 'A' + i)));
    (* -- Latin small letter to its roman -- *)
      (range 0 25) |> List.map (fun i ->
        (ascii_small_of_index i, MathRoman, code_point (Char.code 'a' + i)));

    (* -- Latin capital letter to its bold romain -- *)
      (range 0 25) |> List.map (fun i ->
        (ascii_capital_of_index i, MathBoldRoman, code_point (0x1D400 + i)));
    (* -- Latin small letter to its bold roman -- *)
      (range 0 25) |> List.map (fun i ->
        (ascii_small_of_index i, MathBoldRoman, code_point (0x1D41A + i)));

    (* -- Latin capital letter to its script -- *)
      [[0]; [2; 3]; [6]; [9; 10]; range 13 16; range 18 25] |> List.concat |> List.map (fun i ->
        (ascii_capital_of_index i, MathScript, code_point (0x1D49C + i)));
      [
        (uchar_of_char 'B', MathScript, code_point 0x212C);
        (uchar_of_char 'E', MathScript, code_point 0x2130);
        (uchar_of_char 'F', MathScript, code_point 0x2131);
        (uchar_of_char 'H', MathScript, code_point 0x210B);
        (uchar_of_char 'I', MathScript, code_point 0x2110);
        (uchar_of_char 'L', MathScript, code_point 0x2112);
        (uchar_of_char 'M', MathScript, code_point 0x2133);
        (uchar_of_char 'R', MathScript, code_point 0x211B);
      ];
    (* -- Latin small letter to its script -- *)
      [range 0 3; [5]; range 7 13; range 15 25] |> List.concat |> List.map (fun i ->
        (ascii_small_of_index i, MathScript, code_point (0x1D4B6 + i)));
      [
        (uchar_of_char 'e', MathScript, code_point 0x212F);
        (uchar_of_char 'g', MathScript, code_point 0x210A);
        (uchar_of_char 'o', MathScript, code_point 0x2134);
      ];

    (* -- Latin capital letter to its bold script -- *)
      (range 0 25) |> List.map (fun i ->
        (ascii_capital_of_index i, MathBoldScript, code_point (0x1D4D0 + i)));
    (* -- Latin small letter to its bold script -- *)
      (range 0 25) |> List.map (fun i ->
        (ascii_small_of_index i, MathBoldScript, code_point (0x1D4EA + i)));

    (* -- Latin capital letter to its Fraktur -- *)
      [[0; 1]; range 3 6; range 9 16; range 18 24] |> List.concat |> List.map (fun i ->
        (ascii_capital_of_index i, MathFraktur, code_point (0x1D504 + i)));
      [
        (uchar_of_char 'C', MathFraktur, code_point 0x212D);
        (uchar_of_char 'H', MathFraktur, code_point 0x210C);
        (uchar_of_char 'I', MathFraktur, code_point 0x2111);
        (uchar_of_char 'R', MathFraktur, code_point 0x211C);
        (uchar_of_char 'Z', MathFraktur, code_point 0x2128);
      ];
    (* -- Latin small letter to its Fraktur -- *)
      (range 0 25) |> List.map (fun i ->
        (ascii_small_of_index i, MathFraktur, code_point (0x1D51E + i)));

    (* -- Latin capital letter to its bold Fraktur -- *)
      (range 0 25) |> List.map (fun i ->
        (ascii_capital_of_index i, MathBoldFraktur, code_point (0x1D56C + i)));
    (* -- Latin small letter to its bold Fraktur -- *)
      (range 0 25) |> List.map (fun i ->
        (ascii_small_of_index i, MathBoldFraktur, code_point (0x1D586 + i)));

    (* -- Latin capital letter to its double struck -- *)
      [[0; 1]; range 3 6; range 8 12; [14]; range 18 24] |> List.concat |> List.map (fun i ->
        (ascii_capital_of_index i, MathDoubleStruck, code_point (0x1D538 + i)));
      [
        (uchar_of_char 'C', MathDoubleStruck, code_point 0x2102);
        (uchar_of_char 'H', MathDoubleStruck, code_point 0x210D);
        (uchar_of_char 'N', MathDoubleStruck, code_point 0x2115);
        (uchar_of_char 'P', MathDoubleStruck, code_point 0x2119);
        (uchar_of_char 'Q', MathDoubleStruck, code_point 0x211A);
        (uchar_of_char 'R', MathDoubleStruck, code_point 0x211D);
        (uchar_of_char 'Z', MathDoubleStruck, code_point 0x2124);
      ];
    (* -- Latin small letter to its double struck -- *)
      (range 0 25) |> List.map (fun i ->
        (ascii_small_of_index i, MathDoubleStruck, code_point (0x1D552 + i)));

    ])


let default_math_class_map =
  let open HorzBox in
    List.fold_left (fun map (s, cp, mk) ->
      map |> MathClassMap.add s ([code_point cp], mk)
    ) MathClassMap.empty
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
      ]


let default_font_scheme_ref = ref CharBasis.ScriptSchemeMap.empty

let default_hyphen_dictionary = ref LoadHyph.empty


let default_script_space_map =
  let space_latin_cjk = (0.24, 0.08, 0.16) in
  let open CharBasis in
    ScriptSpaceMap.empty
      |> ScriptSpaceMap.add (Latin, HiraganaOrKatakana) space_latin_cjk
      |> ScriptSpaceMap.add (HiraganaOrKatakana, Latin) space_latin_cjk
      |> ScriptSpaceMap.add (Latin, HanIdeographic) space_latin_cjk
      |> ScriptSpaceMap.add (HanIdeographic, Latin) space_latin_cjk


let get_pdf_mode_initial_context wid =
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
      script_space_map       = default_script_space_map;
      space_natural          = 0.33;
      space_shrink           = 0.08;
      space_stretch          = 0.16;
      adjacent_stretch       = 0.025;
      paragraph_width        = wid;
      paragraph_top          = pdfpt 18.;
      paragraph_bottom       = pdfpt 18.;
      min_first_line_ascender = pdfpt 9.;
      min_last_line_descender = pdfpt 3.;
      leading                = pdfpt 18.;
      min_gap_of_lines       = pdfpt 2.;
      text_color             = DeviceGray(0.);
      manual_rising          = pdfpt 0.;
      space_badness          = 100;
      math_variant_char_map  = default_math_variant_char_map;
      math_class_map         = default_math_class_map;
      math_char_class        = MathItalic;
      before_word_break      = [];
      after_word_break       = [];
      space_math_bin         = (0.25, 0.08, 0.16);
      space_math_rel         = (0.375, 0.12, 0.24);
      space_math_op          = (0.125, 0.04, 0.08);
      space_math_punct       = (0.125, 0.04, 0.08);
      space_math_inner       = (0.125, 0.04, 0.08);
      space_math_prefix      = (0.125, 0.04, 0.08);
      left_hyphen_min        = 3;
      right_hyphen_min       = 2;
    }


let (~%) ty = Poly(ty)


let general_table : (var_name * poly_type * (environment -> syntactic_value)) list =
  let (~@) n = (~! "tv", TypeVariable(n)) in
  let (-%) n ptysub = ptysub in
  let tv1 = (let bid1 = BoundID.fresh UniversalKind () in PolyBound(bid1)) in
  let tv2 = (let bid2 = BoundID.fresh UniversalKind () in PolyBound(bid2)) in
  let ptyderef  = tv1 -% (~% ((tR (~@ tv1)) @-> (~@ tv1))) in
  let ptycons   = tv2 -% (~% ((~@ tv2) @-> (tL (~@ tv2)) @-> (tL (~@ tv2)))) in
  let ptyappinv = tv1 -% (tv2 -% (~% ((~@ tv1) @-> ((~@ tv1) @-> (~@ tv2)) @-> (~@ tv2)))) in
    [
      ( "!"  , ptyderef             , lambda1 (fun v1 -> Dereference(v1))                   );
      ( "::" , ptycons              , lambda2 (fun v1 v2 -> PrimitiveListCons(v1, v2))      );
      ( "|>" , ptyappinv            , lambda2 (fun vx vf -> Apply(vf, vx))                  );
      ( "<>" , ~% (tI @-> tI @-> tB), lambda2 (fun v1 v2 -> LogicalNot(EqualTo(v1, v2)))    );
      ( ">=" , ~% (tI @-> tI @-> tB), lambda2 (fun v1 v2 -> LogicalNot(LessThan(v1, v2)))   );
      ( "<=" , ~% (tI @-> tI @-> tB), lambda2 (fun v1 v2 -> LogicalNot(GreaterThan(v1, v2))));
    ]


let base bc = BaseConstant(bc)


let pdf_mode_table =
  List.append general_table
    [
      ("inline-fil", ~% tIB, (fun _ -> base (BCHorz(HorzBox.([HorzPure(PHSOuterFil)])))));
      ("inline-nil", ~% tIB, (fun _ -> base (BCHorz([])))                               );
      ("block-nil" , ~% tBB, (fun _ -> base (BCVert([])))                               );
      ("clear-page", ~% tBB, (fun _ -> base (BCVert(HorzBox.([VertClearPage]))))        );

#include "__primitives_pdf_mode.gen.ml"
    ]


let text_mode_table =
  List.append general_table
    [
#include "__primitives_text_mode.gen.ml"
    ]


let make_environments table =
  let tyenvinit =
    Typeenv.empty
    |> add_general_default_types
    |> add_pdf_mode_default_types
  in
  let envinit : environment = (EvalVarIDMap.empty, ref (StoreIDHashTable.create 128)) in


  let temporary_ast = Nil in
  let (tyenvfinal, envfinal, locacc) =
    table |> List.fold_left (fun (tyenv, env, acc) (varnm, pty, deff) ->
      let evid = EvalVarID.fresh (dr, varnm) in
      let loc = ref temporary_ast in
      let tyenvnew = Typeenv.add tyenv varnm (pty, evid, Persistent0) in  (* temporary *)
      let envnew = add_to_environment env evid loc in
        (tyenvnew, envnew, Alist.extend acc (loc, deff))
    ) (tyenvinit, envinit, Alist.empty)
  in
  locacc |> Alist.to_list |> List.iter (fun (loc, deff) -> loc := deff envfinal);
    (tyenvfinal, envfinal)


let make_pdf_mode_environments () =
  default_font_scheme_ref := SetDefaultFont.main (Config.resolve_lib_file_exn (make_lib_path "dist/hash/default-font.satysfi-hash"));
  default_hyphen_dictionary := LoadHyph.main (Config.resolve_lib_file_exn (make_lib_path "dist/hyph/english.satysfi-hyph"));
    (* temporary; should depend on the current language -- *)
  make_environments pdf_mode_table


let make_text_mode_environments () =
  make_environments text_mode_table
