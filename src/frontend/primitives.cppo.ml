(* -unused-value-declaration *)
[@@@ocaml.warning "-32"]

open MyUtil
open LengthInterface
open GraphicBase
open SyntaxBase
open Types
open StaticEnv
open ConfigError


(* -- type IDs for predefined data types -- *)
let vid_option   = TypeID.fresh "option"
let vid_itemize  = TypeID.fresh "itemize"
let vid_color    = TypeID.fresh "color"
let vid_script   = TypeID.fresh "script"
let vid_language = TypeID.fresh "language"
let vid_mathcls  = TypeID.fresh "math-class"
let vid_mccls    = TypeID.fresh "math-char-class"
let vid_cell     = TypeID.fresh "cell"

let ( ~! ) = Range.dummy

let variant tyargs tyid = DataType(tyargs, tyid)


(* Base types and base type constructors: *)
let tU    = (~! "unit"          , BaseType(UnitType))
let tB    = (~! "bool"          , BaseType(BoolType))
let tI    = (~! "int"           , BaseType(IntType))
let tFL   = (~! "float"         , BaseType(FloatType))
let tLN   = (~! "length"        , BaseType(LengthType))
let tS    = (~! "string"        , BaseType(StringType))
let tIT   = (~! "inline-text"   , BaseType(InlineTextType))
let tBT   = (~! "block-text"    , BaseType(BlockTextType))
let tMT   = (~! "math-text"     , BaseType(MathTextType))
let tIB   = (~! "inline-boxes"  , BaseType(InlineBoxesType))
let tBB   = (~! "block-boxes"   , BaseType(BlockBoxesType))
let tMB   = (~! "math-boxes"    , BaseType(MathBoxesType))
let tCTX  = (~! "context"       , BaseType(ContextType))
let tPRP  = (~! "pre-path"      , BaseType(PrePathType))
let tPATH = (~! "path"          , BaseType(PathType))
let tGR   = (~! "graphics"      , BaseType(GraphicsType))
let tIMG  = (~! "image"         , BaseType(ImageType))
let tFONTKEY = (~! "font"       , BaseType(FontType))
let tDOC  = (~! "document"      , BaseType(DocumentType))
let tRE   = (~! "regexp"        , BaseType(RegExpType))
let tTCTX = (~! "text-info"     , BaseType(TextInfoType))
let tIPOS = (~! "input-position", BaseType(InputPosType))

let tL ty = (~! "list", ListType(ty))
let tR ty = (~! "ref", RefType(ty))
let tCODE ty = (~! "code", CodeType(ty))

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
let tMATHCLS      = (~! "mathcls" , variant [] vid_mathcls )
let tMCCLS        = (~! "mccls"   , variant [] vid_mccls   )
let tCELL         = (~! "cell"    , variant [] vid_cell    )

(* -- predefined alias types -- *)
let tFONTWR       = tPROD [tFONTKEY; tFL; tFL]
let tPT           = tPROD [tLN; tLN]
let tDASH         = tPROD [tLN; tLN; tLN]
let tPADS         = tPROD [tLN; tLN; tLN; tLN]

let tDECO_raw = tPT @-> tLN @-> tLN @-> tLN @-> tGR
let tDECO = tDECO_raw

let tDECOSET_raw = tPROD [tDECO; tDECO; tDECO; tDECO]
let tDECOSET = tDECOSET_raw

let tIGR_raw = tPT @-> tGR
let tIGR = tIGR_raw

let tIGRO_raw = tLN @-> tPT @-> tGR
let tIGRO = tIGRO_raw

let tPAREN = tLN @-> tLN @-> tCTX @-> tPROD [tIB; tLN @-> tLN]

let tICMD ty = (~! "cmd", InlineCommandType([CommandArgType(LabelMap.empty, ty)]))


let make_row kts =
  kts |> List.fold_left (fun row (k, ty) ->
    RowCons((Range.dummy "math-char-style", k), ty, row)
  ) RowEmpty


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


let option_type =
  tOPT

let itemize_type () =
  tITMZ ()


let fresh_bound_id () =
  BoundID.fresh ()


let add_variant_types vntdefs (tyenv : Typeenv.t) : Typeenv.t =
  List.fold_left (fun tyenv (tynm, tyid, arity, ctors) ->
    let tentry =
      {
        type_scheme = TypeConv.make_opaque_type_scheme arity tyid;
        type_kind   = Kind(List.init arity (fun _ -> TypeKind));
      }
    in
    let tyenv = tyenv |> Typeenv.add_type tynm tentry in
    ctors |> List.fold_left (fun tyenv (ctornm, tyscheme) ->
      let centry =
        {
          ctor_belongs_to = tyid;
          ctor_parameter  = tyscheme;
        }
      in
      tyenv |> Typeenv.add_constructor ctornm centry
    ) tyenv
  ) tyenv vntdefs


let add_synonym_types (syndefs : (type_name * type_scheme) list) (tyenv : Typeenv.t) : Typeenv.t =
  List.fold_left (fun tyenv (tynm, tyscheme) ->
    let (bids, _) = tyscheme in
    let tentry =
      {
        type_scheme = tyscheme;
        type_kind   = Kind(bids |> List.map (fun _ -> TypeKind));
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
        ("MathItalic"             , no_parameter);
        ("MathBoldItalic"         , no_parameter);
        ("MathRoman"              , no_parameter);
        ("MathBoldRoman"          , no_parameter);
        ("MathScript"             , no_parameter);
        ("MathBoldScript"         , no_parameter);
        ("MathFraktur"            , no_parameter);
        ("MathBoldFraktur"        , no_parameter);
        ("MathDoubleStruck"       , no_parameter);
        ("MathSansSerif"          , no_parameter);
        ("MathBoldSansSerif"      , no_parameter);
        ("MathItalicSansSerif"    , no_parameter);
        ("MathBoldItalicSansSerif", no_parameter);
        ("MathTypewriter"         , no_parameter);
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


let lam (evid : EvalVarID.t) (ast : abstract_tree) : abstract_tree =
  Function(LabelMap.empty, PatternBranch(PVariable(evid), ast))

let lamenv env evid arity ast astf =
  PrimitiveClosure(PatternBranch(PVariable(evid), ast), env, arity, astf)

let ( !- ) evid = ContentOf(Range.dummy "temporary", evid)

let dr = Range.dummy "dummy:lambda"

let lambda1 astf env =
  let evid1 = EvalVarID.fresh (dr, "(dummy:lambda1-1)") in
    lamenv env evid1 1 (astf (!- evid1))
      (fun lst -> match lst with
                  | [a1] -> astf a1
                  | _ -> failwith "internal error")

let lambda2 astf env =
  let evid1 = EvalVarID.fresh (dr, "(dummy:lambda2-1)") in
  let evid2 = EvalVarID.fresh (dr, "(dummy:lambda2-2)") in
    lamenv env evid1 2 (lam evid2 (astf (!- evid1) (!- evid2)))
      (fun lst -> match lst with
                  | [a1;a2] -> astf a1 a2
                  | _ -> failwith "internal error")

let lambda3 astf env =
  let evid1 = EvalVarID.fresh (dr, "(dummy:lambda3-1)") in
  let evid2 = EvalVarID.fresh (dr, "(dummy:lambda3-2)") in
  let evid3 = EvalVarID.fresh (dr, "(dummy:lambda3-3)") in
    lamenv env evid1 3 (lam evid2 (lam evid3 (astf (!- evid1) (!- evid2) (!- evid3))))
      (fun lst -> match lst with
                  | [a1;a2;a3] -> astf a1 a2 a3
                  | _ -> failwith "internal error")

let lambda4 astf env =
  let evid1 = EvalVarID.fresh (dr, "(dummy:lambda4-1)") in
  let evid2 = EvalVarID.fresh (dr, "(dummy:lambda4-2)") in
  let evid3 = EvalVarID.fresh (dr, "(dummy:lambda4-3)") in
  let evid4 = EvalVarID.fresh (dr, "(dummy:lambda4-4)") in
    lamenv env evid1 4 (lam evid2 (lam evid3 (lam evid4 (astf (!- evid1) (!- evid2) (!- evid3) (!- evid4)))))
      (fun lst -> match lst with
                  | [a1;a2;a3;a4] -> astf a1 a2 a3 a4
                  | _ -> failwith "internal error")

let lambda5 astf env =
  let evid1 = EvalVarID.fresh (dr, "(dummy:lambda5-1)") in
  let evid2 = EvalVarID.fresh (dr, "(dummy:lambda5-2)") in
  let evid3 = EvalVarID.fresh (dr, "(dummy:lambda5-3)") in
  let evid4 = EvalVarID.fresh (dr, "(dummy:lambda5-4)") in
  let evid5 = EvalVarID.fresh (dr, "(dummy:lambda5-5)") in
    lamenv env evid1 5 (lam evid2 (lam evid3 (lam evid4 (lam evid5 (astf (!- evid1) (!- evid2) (!- evid3) (!- evid4) (!- evid5))))))
      (fun lst -> match lst with
                  | [a1;a2;a3;a4;a5] -> astf a1 a2 a3 a4 a5
                  | _ -> failwith "internal error")

let lambda6 astf env =
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

let lambda7 astf env =
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
  [ HorzPure(PHGFixedGraphics{ width = wid; height = hgt_bar +% t_bar; depth = nonnegdpt; graphics }) ]


let code_point =
  Uchar.of_int


let uchar_of_char =
  Core.Uchar.of_char


let ascii_capital_of_index (i : int) : Uchar.t =
  Uchar.of_int ((Char.code 'A') + i)


let ascii_small_of_index (i : int) : Uchar.t =
  Uchar.of_int ((Char.code 'a') + i)


let ascii_digit_of_index (i : int) : Uchar.t =
  Uchar.of_int ((Char.code '0') + i)


let default_math_variant_char_map : HorzBox.math_variant_char_map =
  let open HorzBox in
  let map = MathVariantCharMap.empty in

  (* Adds Latin capital letters: *)
  let map =
    (range 0 25) |> List.fold_left (fun map i ->
      let uch_from = ascii_capital_of_index i in
      let uch_italic = code_point (0x1D434 + i) in
      let uch_bold_italic = code_point (0x1D468 + i) in
      let uch_roman = code_point (Char.code 'A' + i) in
      let uch_bold_roman = code_point (0x1D400 + i) in
      let uch_script =
        if Uchar.equal uch_from (uchar_of_char 'B') then
          code_point 0x212C
        else if Uchar.equal uch_from (uchar_of_char 'E') then
          code_point 0x2130
        else if Uchar.equal uch_from (uchar_of_char 'F') then
          code_point 0x2131
        else if Uchar.equal uch_from (uchar_of_char 'H') then
          code_point 0x210B
        else if Uchar.equal uch_from (uchar_of_char 'I') then
          code_point 0x2110
        else if Uchar.equal uch_from (uchar_of_char 'L') then
          code_point 0x2112
        else if Uchar.equal uch_from (uchar_of_char 'M') then
          code_point 0x2133
        else if Uchar.equal uch_from (uchar_of_char 'R') then
          code_point 0x211B
        else
          code_point (0x1D49C + i)
      in
      let uch_bold_script = code_point (0x1D4D0 + i) in
      let uch_fraktur =
        if Uchar.equal uch_from (uchar_of_char 'C') then
          code_point 0x212D
        else if Uchar.equal uch_from (uchar_of_char 'H') then
          code_point 0x210C
        else if Uchar.equal uch_from (uchar_of_char 'I') then
          code_point 0x2111
        else if Uchar.equal uch_from (uchar_of_char 'R') then
          code_point 0x211C
        else if Uchar.equal uch_from (uchar_of_char 'Z') then
          code_point 0x2128
        else
          code_point (0x1D504 + i)
      in
      let uch_bold_fraktur = code_point (0x1D56C + i) in
      let uch_double_struck =
        if Uchar.equal uch_from (uchar_of_char 'C') then
          code_point 0x2102
        else if Uchar.equal uch_from (uchar_of_char 'H') then
          code_point 0x210D
        else if Uchar.equal uch_from (uchar_of_char 'N') then
          code_point 0x2115
        else if Uchar.equal uch_from (uchar_of_char 'P') then
          code_point 0x2119
        else if Uchar.equal uch_from (uchar_of_char 'Q') then
          code_point 0x211A
        else if Uchar.equal uch_from (uchar_of_char 'R') then
          code_point 0x211D
        else if Uchar.equal uch_from (uchar_of_char 'Z') then
          code_point 0x2124
        else
          code_point (0x1D538 + i)
      in
      let uch_sans_serif = code_point (0x1D5A0 + i) in
      let uch_bold_sans_serif = code_point (0x1D5D4 + i) in
      let uch_italic_sans_serif = code_point (0x1D608 + i) in
      let uch_bold_italic_sans_serif = code_point (0x1D63C + i) in
      let uch_typewriter = code_point (0x1D670 + i) in
      map |> MathVariantCharMap.add uch_from (function
        | MathItalic              -> (uch_italic, MathOrdinary)
        | MathBoldItalic          -> (uch_bold_italic, MathOrdinary)
        | MathRoman               -> (uch_roman, MathOrdinary)
        | MathBoldRoman           -> (uch_bold_roman, MathOrdinary)
        | MathScript              -> (uch_script, MathOrdinary)
        | MathBoldScript          -> (uch_bold_script, MathOrdinary)
        | MathFraktur             -> (uch_fraktur, MathOrdinary)
        | MathBoldFraktur         -> (uch_bold_fraktur, MathOrdinary)
        | MathDoubleStruck        -> (uch_double_struck, MathOrdinary)
        | MathSansSerif           -> (uch_sans_serif, MathOrdinary)
        | MathBoldSansSerif       -> (uch_bold_sans_serif, MathOrdinary)
        | MathItalicSansSerif     -> (uch_italic_sans_serif, MathOrdinary)
        | MathBoldItalicSansSerif -> (uch_bold_italic_sans_serif, MathOrdinary)
        | MathTypewriter          -> (uch_typewriter, MathOrdinary)
      )
    ) map
  in

  (* Adds Latin small letters: *)
  let map =
    (range 0 25) |> List.fold_left (fun map i ->
      let uch_from = ascii_small_of_index i in
      let uch_italic =
        if Uchar.equal uch_from (uchar_of_char 'h') then
          code_point 0x210E
        else
          code_point (0x1D44E + i)
      in
      let uch_bold_italic = code_point (0x1D482 + i) in
      let uch_roman = code_point (Char.code 'a' + i) in
      let uch_bold_roman = code_point (0x1D41A + i) in
      let uch_script =
        if Uchar.equal uch_from (uchar_of_char 'e') then
          code_point 0x212F
        else if Uchar.equal uch_from (uchar_of_char 'g') then
          code_point 0x210A
        else if Uchar.equal uch_from (uchar_of_char 'o') then
          code_point 0x2134
        else
          code_point (0x1D4B6 + i)
      in
      let uch_bold_script = code_point (0x1D4EA + i) in
      let uch_fraktur = code_point (0x1D51E + i) in
      let uch_bold_fraktur = code_point (0x1D586 + i) in
      let uch_double_struck = code_point (0x1D552 + i) in
      let uch_sans_serif = code_point (0x1D5BA + i) in
      let uch_bold_sans_serif = code_point (0x1D5EE + i) in
      let uch_italic_sans_serif = code_point (0x1D622 + i) in
      let uch_bold_italic_sans_serif = code_point (0x1D656 + i) in
      let uch_typewriter = code_point (0x1D68A + i) in
      map |> MathVariantCharMap.add uch_from (function
        | MathItalic              -> (uch_italic, MathOrdinary)
        | MathBoldItalic          -> (uch_bold_italic, MathOrdinary)
        | MathRoman               -> (uch_roman, MathOrdinary)
        | MathBoldRoman           -> (uch_bold_roman, MathOrdinary)
        | MathScript              -> (uch_script, MathOrdinary)
        | MathBoldScript          -> (uch_bold_script, MathOrdinary)
        | MathFraktur             -> (uch_fraktur, MathOrdinary)
        | MathBoldFraktur         -> (uch_bold_fraktur, MathOrdinary)
        | MathDoubleStruck        -> (uch_double_struck, MathOrdinary)
        | MathSansSerif           -> (uch_sans_serif, MathOrdinary)
        | MathBoldSansSerif       -> (uch_bold_sans_serif, MathOrdinary)
        | MathItalicSansSerif     -> (uch_italic_sans_serif, MathOrdinary)
        | MathBoldItalicSansSerif -> (uch_bold_italic_sans_serif, MathOrdinary)
        | MathTypewriter          -> (uch_typewriter, MathOrdinary)
      )
    ) map
  in

  (* Adds digits: *)
  let map =
    (range 0 9) |> List.fold_left (fun map i ->
      let uch_from = ascii_digit_of_index i in

      let uch_roman = uch_from in
      let uch_bold_roman = code_point (0x1D7CE + i) in
      let uch_double_struck = code_point (0x1D7D8 + i) in
      let uch_sans_serif = code_point (0x1D7E2 + i) in
      let uch_bold_sans_serif = code_point (0x1D7EC + i) in
      let uch_typewriter = code_point (0x1D7F6 + i) in

      let uch_italic = uch_from in
      let uch_bold_italic = uch_bold_roman in
      let uch_bold_script = uch_bold_roman in
      let uch_script = uch_from in
      let uch_fraktur = uch_from in
      let uch_bold_fraktur = uch_bold_roman in
      let uch_italic_sans_serif = uch_sans_serif in
      let uch_bold_italic_sans_serif = uch_bold_sans_serif in

      map |> MathVariantCharMap.add uch_from (function
        | MathItalic              -> (uch_italic, MathOrdinary)
        | MathBoldItalic          -> (uch_bold_italic, MathOrdinary)
        | MathRoman               -> (uch_roman, MathOrdinary)
        | MathBoldRoman           -> (uch_bold_roman, MathOrdinary)
        | MathScript              -> (uch_script, MathOrdinary)
        | MathBoldScript          -> (uch_bold_script, MathOrdinary)
        | MathFraktur             -> (uch_fraktur, MathOrdinary)
        | MathBoldFraktur         -> (uch_bold_fraktur, MathOrdinary)
        | MathDoubleStruck        -> (uch_double_struck, MathOrdinary)
        | MathSansSerif           -> (uch_sans_serif, MathOrdinary)
        | MathBoldSansSerif       -> (uch_bold_sans_serif, MathOrdinary)
        | MathItalicSansSerif     -> (uch_italic_sans_serif, MathOrdinary)
        | MathBoldItalicSansSerif -> (uch_bold_italic_sans_serif, MathOrdinary)
        | MathTypewriter          -> (uch_typewriter, MathOrdinary)
      )
    ) map
  in

  map


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
        (uchar_of_char '(', None,                      MathOpen);
        (uchar_of_char ')', None,                      MathClose);
      ]


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
      font_scheme            = CharBasis.ScriptSchemeMap.empty;
      font_size              = pdfpt 12.;
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
      math_font_key          = None;
      math_script_level      = HorzBox.BaseLevel;
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
      ( "|>" , ptyappinv            , lambda2 (fun vx vf -> Apply(LabelMap.empty, vf, vx))  );
      ( "<>" , ~% (tI @-> tI @-> tB), lambda2 (fun v1 v2 -> PrimitiveLogicalNot(PrimitiveEqualTo(v1, v2)))    );
      ( ">=" , ~% (tI @-> tI @-> tB), lambda2 (fun v1 v2 -> PrimitiveLogicalNot(PrimitiveLessThan(v1, v2)))   );
      ( "<=" , ~% (tI @-> tI @-> tB), lambda2 (fun v1 v2 -> PrimitiveLogicalNot(PrimitiveGreaterThan(v1, v2))));
    ]


let base bc = BaseConstant(bc)


let pdf_mode_table =
  List.append general_table
    [
      ("inline-fil"     , ~% tIB, (fun _ -> base (BCInlineBoxes(HorzBox.([HorzPure(PHSOuterFil)])))));
      ("inline-nil"     , ~% tIB, (fun _ -> base (BCInlineBoxes([]))));
      ("omit-skip-after", ~% tIB, (fun _ -> base (BCInlineBoxes(HorzBox.([HorzOmitSkipAfter])))));
      ("block-nil"      , ~% tBB, (fun _ -> base (BCBlockBoxes([]))));
      ("clear-page"     , ~% tBB, (fun _ -> base (BCBlockBoxes(HorzBox.([VertClearPage])))));

#include "__primitives_pdf_mode.gen.ml"
    ]


let text_mode_table =
  List.append general_table
    [
#include "__primitives_text_mode.gen.ml"
    ]


let make_environments table =
  let tyenv =
    Typeenv.empty
      |> add_general_default_types
      |> add_pdf_mode_default_types
  in
  let env : environment = (EvalVarIDMap.empty, ref (StoreIDHashTable.create 128)) in
  let temporary_ast = Nil in
  let (tyenv, env, locacc) =
    table |> List.fold_left (fun (tyenv, env, acc) (varnm, pty, deff) ->
      let evid = EvalVarID.fresh (dr, varnm) in
      let loc = ref temporary_ast in
      let ventry =
        {
          val_name  = Some(evid);
          val_type  = pty;
          val_stage = Persistent0;
        }
      in
      let tyenv = tyenv |> Typeenv.add_value varnm ventry in  (* temporary *)
      let env = add_to_environment env evid loc in
        (tyenv, env, Alist.extend acc (loc, deff))
    ) (tyenv, env, Alist.empty)
  in
  locacc |> Alist.to_list |> List.iter (fun (loc, deff) -> loc := deff env);
  (tyenv, env)


let resolve_lib_file (libpath : lib_path) =
  Config.resolve_lib_file libpath
    |> Result.map_error (fun candidates -> CannotFindLibraryFile(libpath, candidates))


let make_pdf_mode_environments () =
  let open ResultMonad in
  let* abspath_hyphen = resolve_lib_file (make_lib_path "hyph/english.satysfi-hyph") in
  default_hyphen_dictionary := LoadHyph.main abspath_hyphen;
    (* TODO: should depend on the current language *)
  return @@ make_environments pdf_mode_table


let make_text_mode_environments () =
  let open ResultMonad in
  return @@ make_environments text_mode_table
