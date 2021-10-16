
open MyUtil
open LengthInterface
open GraphicBase
open Types
open StaticEnv

(* -- type IDs for predefined data types -- *)
let vid_option   = TypeID.fresh "option"
let vid_itemize  = TypeID.fresh "itemize"
let vid_color    = TypeID.fresh "color"
let vid_script   = TypeID.fresh "script"
let vid_language = TypeID.fresh "language"
let vid_page     = TypeID.fresh "page"
let vid_mathcls  = TypeID.fresh "math-class"
let vid_mccls    = TypeID.fresh "math-char-class"
let vid_cell     = TypeID.fresh "cell"
let vid_image    = TypeID.fresh "image"

let ( ~! ) = Range.dummy

let variant tyargs tyid = DataType(tyargs, tyid)


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

let tIPOS         = (~! "input-position", BaseType(InputPosType))

let tL ty         = (~! "list"    , ListType(ty)          )
let tR ty         = (~! "ref"     , RefType(ty)           )
let tCODE ty      = (~! "code"    , CodeType(ty)          )

let tPROD = function
  | ty1 :: ty2 :: tyrest -> (~! "product", ProductType(TupleList.make ty1 ty2 tyrest))
  | _                    -> assert false

let ( @-> ) dom cod = (~! "func", FuncType(RowEmpty, dom, cod))

(* -- predefined data types -- *)
let tOPT ty       = (~! "option"  , variant [ty] vid_option)
let tITMZ ()      = (~! "itemize" , variant [] vid_itemize )
let tSCR          = (~! "script"  , variant [] vid_script  )
let tLANG         = (~! "language", variant [] vid_language)
let tCLR          = (~! "color"   , variant [] vid_color   )
let tPG           = (~! "page"    , variant [] vid_page    )
let tMATHCLS      = (~! "mathcls" , variant [] vid_mathcls )
let tMCCLS        = (~! "mccls"   , variant [] vid_mccls   )
let tCELL         = (~! "cell"    , variant [] vid_cell    )

(* -- predefined alias types -- *)
let tFONT         = tPROD [tS; tFL; tFL]
let tPT           = tPROD [tLN; tLN]
let tDASH         = tPROD [tLN; tLN; tLN]
let tPADS         = tPROD [tLN; tLN; tLN; tLN]

let tDECO_raw = tPT @-> tLN @-> tLN @-> tLN @-> (tL tGR)
let tDECO = tDECO_raw

let tDECOSET_raw = tPROD [tDECO; tDECO; tDECO; tDECO]
let tDECOSET = tDECOSET_raw

let tIGR_raw = tPT @-> (tL tGR)
let tIGR = tIGR_raw

let tIGRO_raw = tLN @-> tPT @-> (tL tGR)
let tIGRO = tIGRO_raw

let tPAREN = tLN @-> tLN @-> tLN @-> tLN @-> tCLR @-> tPROD [tIB; tLN @-> tLN]

let tICMD ty = (~! "cmd", HorzCommandType([MandatoryArgumentType(ty)]))


let make_row kts =
  kts |> List.fold_left (fun row (k, ty) ->
    RowCons((Range.dummy "math-char-style", k), ty, row)
  ) RowEmpty


let tMCSTY =
  let row =
    [
      "italic";
      "bold-italic";
      "roman";
      "bold-roman";
      "script";
      "bold-script";
      "fraktur";
      "bold-fraktur";
      "double-struck";
    ] |> List.map (fun k -> (k, tS)) |> make_row
  in
  (~! "math-char-style", RecordType(row))


let tDOCINFODIC =
  let row =
    make_row [
      ("title"   , tOPT tS);
      ("subject" , tOPT tS);
      ("author"  , tOPT tS);
      ("keywords", tL tS);
    ]
  in
  (~! "document-information-dictionary", RecordType(row))


let tPBINFO =
  let row =
    make_row [
      ("page-number", tI);
    ]
  in
    (~! "page-break-info", RecordType(row))


let tPAGECONT =
  let row =
    make_row [
      ("text-origin", tPT);
      ("text-height", tLN);
    ]
  in
  (~! "page-content-scheme", RecordType(row))


let tPAGECONTF = tPBINFO @-> tPAGECONT


let tPCINFO =
  tPBINFO  (* temporary; may have more fields in the future *)


let tPAGEPARTS =
  let row =
    make_row [
      ("header-origin" , tPT);
      ("header-content", tBB);
      ("footer-origin" , tPT);
      ("footer-content", tBB);
    ]
  in
  (~! "page-parts", RecordType(row))


let tPAGEPARTSF =
  tPCINFO @-> tPAGEPARTS


let tRULESF =
  (tL tLN) @-> (tL tLN) @-> (tL tGR)


let option_type =
  tOPT

let itemize_type () =
  tITMZ ()


let fresh_bound_id () =
  let bid = BoundID.fresh () in
  KindStore.set_bound_id bid KindStore.{ poly_kind = UniversalKind };
  bid


let add_variant_types vntdefs tyenv =
  List.fold_left (fun tyenv (tynm, vid, arity, ctors) ->
    let tentry =
      {
        type_scheme = failwith "TODO: make type scheme from vid";
        type_kind   = failwith "TODO: kind";
      }
    in
    let tyenv = tyenv |> Typeenv.add_type tynm tentry in
    ctors |> List.fold_left (fun tyenv (ctornm, tyscheme) ->
      let centry =
        {
          ctor_belongs_to = vid;
          ctor_parameter  = tyscheme;
        }
      in
      tyenv |> Typeenv.add_constructor ctornm centry
    ) tyenv
  ) tyenv vntdefs


let add_synonym_types syndefs tyenv =
  List.fold_left (fun tyenv (tynm, tyscheme) ->
    let tentry =
      {
        type_scheme = tyscheme;
        type_kind   = failwith "TODO: add_synonym_types";
      }
    in
    tyenv |> Typeenv.add_type tynm tentry
  ) tyenv syndefs


let no_parameter = ([], Poly(tU))


let add_general_default_types (tyenvmid : Typeenv.t) : Typeenv.t =
  let dr = Range.dummy "add_default_types" in
  let bid = fresh_bound_id () in
  let typaram = (dr, TypeVariable(PolyBound(bid))) in

  tyenvmid
    |> add_variant_types [
      ("option", vid_option, 1, [
        ("None", ([bid], Poly(tU)));
        ("Some", ([bid], Poly(typaram)))
      ]);
      ("itemize", vid_itemize, 0, [
        ("Item", ([], Poly(tPROD [tIT; tL (tITMZ ())])))
      ]);
      ("math-class", vid_mathcls, 0, [
        ("MathOrd"   , no_parameter);
        ("MathBin"   , no_parameter);
        ("MathRel"   , no_parameter);
        ("MathOp"    , no_parameter);
        ("MathPunct" , no_parameter);
        ("MathOpen"  , no_parameter);
        ("MathClose" , no_parameter);
        ("MathPrefix", no_parameter);
        ("MathInner" , no_parameter);
      ]);
      ("math-char-class", vid_mccls, 0, [
        ("MathItalic"      , no_parameter);
        ("MathBoldItalic"  , no_parameter);
        ("MathRoman"       , no_parameter);
        ("MathBoldRoman"   , no_parameter);
        ("MathScript"      , no_parameter);
        ("MathBoldScript"  , no_parameter);
        ("MathFraktur"     , no_parameter);
        ("MathBoldFraktur" , no_parameter);
        ("MathDoubleStruck", no_parameter);
      ]);
    ]


let add_pdf_mode_default_types (tyenvmid : Typeenv.t) : Typeenv.t =
  tyenvmid
    |> add_variant_types [
      ("color", vid_color, 0, [
        ("Gray", ([], Poly(tFL)));
        ("RGB" , ([], Poly(tPROD [tFL; tFL; tFL])));
        ("CMYK", ([], Poly(tPROD [tFL; tFL; tFL; tFL])));
      ]);
      ("script", vid_script, 0, [
        ("HanIdeographic", no_parameter);
        ("Kana"          , no_parameter);
        ("Latin"         , no_parameter);
        ("OtherScript"   , no_parameter);
      ]);
      ("language", vid_language, 0, [
        ("English"         , no_parameter);
        ("Japanese"        , no_parameter);
        ("NoLanguageSystem", no_parameter);
      ]);

      ("page", vid_page, 0, [
        ("A0Paper"         , no_parameter);
        ("A1Paper"         , no_parameter);
        ("A2Paper"         , no_parameter);
        ("A3Paper"         , no_parameter);
        ("A4Paper"         , no_parameter);
        ("A5Paper"         , no_parameter);
        ("USLetter"        , no_parameter);
        ("USLegal"         , no_parameter);
        ("UserDefinedPaper", ([], Poly(tPROD [tLN; tLN])));
      ]);
      ("cell", vid_cell, 0, [
        ("NormalCell", ([], Poly(tPROD [tPADS; tIB])));
        ("EmptyCell" , no_parameter);
        ("MultiCell" , ([], Poly(tPROD [tI; tI; tPADS; tIB])));
      ]);
    ]
    |> add_synonym_types [
      ("deco",            ([], Poly(tDECO)));
      ("deco-set",        ([], Poly(tDECOSET)));
      ("inline-graphics", ([], Poly(tIGR)));
    ]


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


let default_math_variant_char_map : (Uchar.t * HorzBox.math_kind) HorzBox.MathVariantCharMap.t =
  let open HorzBox in
  List.fold_left (fun map (uchfrom, mccls, uchto) ->
    map |> MathVariantCharMap.add (uchfrom, mccls) (uchto, HorzBox.MathOrdinary)
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


    (* -- Digit to its boldface -- *)
      (range 0 9) |> List.concat_map (fun i ->
        [
          (ascii_digit_of_index i, MathBoldItalic, code_point (0x1D7CE + i));
          (ascii_digit_of_index i, MathBoldRoman, code_point (0x1D7CE + i));
          (ascii_digit_of_index i, MathBoldScript, code_point (0x1D7CE + i));
          (ascii_digit_of_index i, MathBoldFraktur, code_point (0x1D7CE + i));
        ]);

    (* -- Digit to its double struck -- *)
      (range 0 9) |> List.map (fun i ->
        (ascii_digit_of_index i, MathDoubleStruck, code_point (0x1D7D8 + i)));
    ])


let default_math_class_map =
  let open HorzBox in
    List.fold_left (fun map (uch_from, uch_to_opt, mk) ->
      let uch_to = uch_to_opt |> Option.value ~default:uch_from in
      map |> MathClassMap.add uch_from (uch_to, mk)
    ) MathClassMap.empty
      [
        (uchar_of_char '=', None,                      MathRelation);
        (uchar_of_char '<', None,                      MathRelation);
        (uchar_of_char '>', None,                      MathRelation);
        (uchar_of_char ':', None,                      MathRelation);
        (uchar_of_char '+', None,                      MathBinary);
        (uchar_of_char '-', Some(Uchar.of_int 0x2212), MathBinary);
        (uchar_of_char '|', None,                      MathBinary);
        (uchar_of_char '/', None,                      MathOrdinary);
        (uchar_of_char ',', None,                      MathPunct);
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
let (~@) n = (~! "tv", TypeVariable(n))


let general_table : (var_name * poly_type * (environment -> syntactic_value)) list =
  let tv1 = (let bid1 = fresh_bound_id () in PolyBound(bid1)) in
  let tv2 = (let bid2 = fresh_bound_id () in PolyBound(bid2)) in
  let ptyderef  = ~% ((tR (~@ tv1)) @-> (~@ tv1)) in
  let ptycons   = ~% ((~@ tv2) @-> (tL (~@ tv2)) @-> (tL (~@ tv2))) in
  let ptyappinv = ~% ((~@ tv1) @-> ((~@ tv1) @-> (~@ tv2)) @-> (~@ tv2)) in
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
      ("omit-skip-after", ~% tIB, (fun _ -> base (BCHorz(HorzBox.([HorzOmitSkipAfter])))));
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
      let ventry =
        {
          val_name  = evid;
          val_type  = pty;
          val_stage = Persistent0;
        }
      in
      let tyenvnew = tyenv |> Typeenv.add_value varnm ventry in  (* temporary *)
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
