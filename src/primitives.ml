open Types


let add_default_types tyenvmid =
  let dr = Range.dummy "add_default_types" in
  let mutvarntcons =
    UTMutualVariantCons(
      [(dr, "%a", MUniversalKind)],  Range.dummy "primitives-maybe", "maybe",
        (dr, UTVariantCons("Nothing", (dr, MTypeName([], "unit")),
        (dr, UTVariantCons("Just", (dr, MTypeParam("%a")),
        (dr, UTEndOfVariant))))),
    UTMutualVariantCons(
      [], Range.dummy "primitives-itemize", "itemize",
        (dr, UTVariantCons("Item", (dr, MProductType([(dr, MTypeName([], "text-row")); (dr, MTypeName([(dr, MTypeName([], "itemize"))], "list"))])),
        (dr, UTEndOfVariant))),
    UTEndOfMutualVariant))
  in
    Typeenv.add_mutual_cons tyenvmid FreeID.bottom_level mutvarntcons


let add_to_environment env varnm rfast =
  Hashtbl.add env varnm rfast


let rec lambda1 astf env =
  let evid1 = EvalVarID.fresh "(dummy:lambda1-1)" in
    FuncWithEnvironment(evid1, astf (ContentOf(evid1)), env)


let rec lambda2 astf env =
  let evid1 = EvalVarID.fresh "(dummy:lambda2-1)" in
  let evid2 = EvalVarID.fresh "(dummy:lambda2-2)" in
    FuncWithEnvironment(evid1, LambdaAbstract(evid2, astf (ContentOf(evid1)) (ContentOf(evid2))), env)


let rec lambda3 astf env =
  let evid1 = EvalVarID.fresh "(dummy:lambda3-1)" in
  let evid2 = EvalVarID.fresh "(dummy:lambda3-2)" in
  let evid3 = EvalVarID.fresh "(dummy:lambda3-3)" in
    FuncWithEnvironment(evid1, LambdaAbstract(evid2, LambdaAbstract(evid3,
      astf (ContentOf(evid1)) (ContentOf(evid2)) (ContentOf(evid3)))), env)


(* -- begin: constants just for experimental use -- *)

let pdfpt = HorzBox.Length.of_pdf_point

let ( +% ) = HorzBox.( +% )
let ( -% ) = HorzBox.( -% )
let ( *% ) = HorzBox.( *% )

let default_context =
  {
    title           = Horz([]);
    author          = Horz([]);
    font_info       = ("Arno", HorzBox.Length.of_pdf_point 12.);
    space_natural   = 0.33;
    space_shrink    = pdfpt 1.;
    space_stretch   = pdfpt 2.;
    paragraph_width = pdfpt 400.;
    leading         = pdfpt 18.;
  }

let margin = pdfpt 2.

let frame_deco_S =
  (fun (xpos, ypos) wid hgt dpt ->
    let xposb = xpos +% margin in
    let hgtb = hgt -% margin in
    let dptb = dpt +% margin in
    let widb = wid -% margin *% 2. in
    [
      HorzBox.Rectangle((xposb, ypos +% dptb), (widb, hgtb -% dptb));
    ]
  )

let frame_deco_H =
  (fun (xpos, ypos) wid hgt dpt ->
    let xposb = xpos +% margin in
    let hgtb = hgt -% margin in
    let dptb = dpt +% margin in
    let widb = wid -% margin in
    [
      HorzBox.GeneralPath((xposb +% widb, ypos +% hgtb), [
        HorzBox.LineTo(xposb, ypos +% hgtb);
        HorzBox.LineTo(xposb, ypos +% dptb);
        HorzBox.LineTo(xposb +% widb, ypos +% dptb);
      ]);
    ]
  )

let frame_deco_M =
  (fun (xpos, ypos) wid hgt dpt ->
    let xposb = xpos in
    let hgtb = hgt -% margin in
    let dptb = dpt +% margin in
    let widb = wid in
    [
      HorzBox.GeneralPath((xposb, ypos +% hgtb), [HorzBox.LineTo(xposb +% widb, ypos +% hgtb)]);
      HorzBox.GeneralPath((xposb, ypos +% dptb), [HorzBox.LineTo(xposb +% widb, ypos +% dptb)]);
    ]
  )

let frame_deco_T =
  (fun (xpos, ypos) wid hgt dpt ->
    let xposb = xpos in
    let hgtb = hgt -% margin in
    let dptb = dpt +% margin in
    let widb = wid -% margin in
    [
      HorzBox.GeneralPath((xposb, ypos +% hgtb), [
        HorzBox.LineTo(xposb +% widb, ypos +% hgtb);
        HorzBox.LineTo(xposb +% widb, ypos +% dptb);
        HorzBox.LineTo(xposb, ypos +% dptb);
      ]);
    ]
  )

let default_paddings =
  {
    HorzBox.paddingL = pdfpt 2. +% margin;
    HorzBox.paddingR = pdfpt 2. +% margin;
    HorzBox.paddingT = pdfpt 2. +% margin;
    HorzBox.paddingB = pdfpt 2. +% margin;
  }

(* -- end: constants just for experimental use -- *)


let make_environments () =

  let i             = (Range.dummy "int"     , BaseType(IntType)    ) in
  let fl            = (Range.dummy "float"   , BaseType(FloatType)  ) in
  let b             = (Range.dummy "bool"    , BaseType(BoolType)   ) in
  let s             = (Range.dummy "string"  , BaseType(StringType) ) in
  let (~@) n        = (Range.dummy "tv"      , TypeVariable(n)      ) in
  let (-%) n ptysub = ptysub in
  let (~%) ty       = Poly(ty) in
  let l cont        = (Range.dummy "list"    , ListType(cont)       ) in
  let r cont        = (Range.dummy "ref"     , RefType(cont)        ) in
  let prod tylst    = (Range.dummy "product" , ProductType(tylst)   ) in
  let (-->) dom cod = (Range.dummy "func"    , FuncType(dom, cod)   ) in
  let (@->) = (-->) in

  let tr            = (Range.dummy "text-row", BaseType(TextRowType)) in
  let tc            = (Range.dummy "text-col", BaseType(TextColType)) in
  let br            = (Range.dummy "box-row" , BaseType(BoxRowType) ) in
  let bc            = (Range.dummy "box-col" , BaseType(BoxColType) ) in
  let ft            = (Range.dummy "font"    , BaseType(FontType)   ) in
  let ctx           = (Range.dummy "context" , BaseType(ContextType)) in
  let path          = (Range.dummy "path"    , BaseType(PathType)   ) in
  let deco          = (prod [fl; fl]) @-> fl @-> fl @-> fl @-> (l path) in

  let tv1 = (let bid1 = BoundID.fresh UniversalKind () in ref (Bound(bid1))) in
  let tv2 = (let bid2 = BoundID.fresh UniversalKind () in ref (Bound(bid2))) in

  let table : (var_name * poly_type * (environment -> abstract_tree)) list =
    let ptyderef = tv1 -% (~% ((r (~@ tv1)) --> (~@ tv1))) in
    let ptycons  = tv2 -% (~% ((~@ tv2) --> ((l (~@ tv2)) --> (l (~@ tv2))))) in
      [
        ( "+"  , ~% (i --> (i --> i)), lambda2 (fun v1 v2 -> Plus(v1, v2))                    );
        ( "-"  , ~% (i --> (i --> i)), lambda2 (fun v1 v2 -> Minus(v1, v2))                   );
        ( "mod", ~% (i --> (i --> i)), lambda2 (fun v1 v2 -> Mod(v1, v2))                     );
        ( "*"  , ~% (i --> (i --> i)), lambda2 (fun v1 v2 -> Times(v1, v2))                   );
        ( "/"  , ~% (i --> (i --> i)), lambda2 (fun v1 v2 -> Divides(v1, v2))                 );
        ( "^"  , ~% (s --> (s --> s)), lambda2 (fun v1 v2 -> Concat(v1, v2))                  );
        ( "==" , ~% (i --> (i --> b)), lambda2 (fun v1 v2 -> EqualTo(v1, v2))                 );
        ( "<>" , ~% (i --> (i --> b)), lambda2 (fun v1 v2 -> LogicalNot(EqualTo(v1, v2)))     );
        ( ">"  , ~% (i --> (i --> b)), lambda2 (fun v1 v2 -> GreaterThan(v1, v2))             );
        ( "<"  , ~% (i --> (i --> b)), lambda2 (fun v1 v2 -> LessThan(v1, v2))                );
        ( ">=" , ~% (i --> (i --> b)), lambda2 (fun v1 v2 -> LogicalNot(LessThan(v1, v2)))    );
        ( "<=" , ~% (i --> (i --> b)), lambda2 (fun v1 v2 -> LogicalNot(GreaterThan(v1, v2))) );
        ( "&&" , ~% (b --> (b --> b)), lambda2 (fun v1 v2 -> LogicalAnd(v1, v2))              );
        ( "||" , ~% (b --> (b --> b)), lambda2 (fun v1 v2 -> LogicalOr(v1, v2))               );
        ( "not", ~% (b --> b)        , lambda1 (fun v1 -> LogicalNot(v1))                     );
        ( "!"  , ptyderef            , lambda1 (fun v1 -> Reference(v1))                      );
        ( "::" , ptycons             , lambda2 (fun v1 v2 -> ListCons(v1, v2))                );
        ( "+." , ~% (fl @-> fl @-> fl), lambda2 (fun v1 v2 -> FloatPlus(v1, v2))              );
        ( "-." , ~% (fl @-> fl @-> fl), lambda2 (fun v1 v2 -> FloatMinus(v1, v2))             );

        ( "same"         , ~% (s --> (s --> b))         , lambda2 (fun v1 v2 -> PrimitiveSame(v1, v2)) );
        ( "string-sub"   , ~% (s --> (i --> (i --> s))) , lambda3 (fun vstr vpos vwid -> PrimitiveStringSub(vstr, vpos, vwid)) );
        ( "string-length", ~% (s --> i)                 , lambda1 (fun vstr -> PrimitiveStringLength(vstr)) );
        ( "arabic"       , ~% (i --> s)                 , lambda1 (fun vnum -> PrimitiveArabic(vnum)) );

        ("form-paragraph", ~% (ctx --> (br --> bc))     , lambda2 (fun vleading vrow -> BackendLineBreaking(vleading, vrow)) );
        ("fixed-empty"   , ~% (i --> br)                , lambda1 (fun vwid -> BackendFixedEmpty(vwid))   );
        ("fixed-string"  , ~% (ft --> (tr --> br))      , lambda2 (fun vfont vwid -> BackendFixedString(vfont, vwid))   );
        ("outer-empty"   , ~% (i --> (i --> (i --> br))), lambda3 (fun vn vp vm -> BackendOuterEmpty(vn, vp, vm)) );
        ("outer-fil"     , ~% br                        , (fun _ -> Horz([HorzBox.HorzPure(HorzBox.PHOuterFil)])));
        ("outer-frame-block", ~% (deco --> (br --> br)) , lambda2 (fun vdeco vbr -> BackendOuterFrame(vdeco, vbr)));
        ("outer-frame-inline", ~% (br --> br)           , lambda1 (fun vbr -> BackendOuterFrameBreakable(vbr)));
        ("font"          , ~% (s --> (i --> ft))        , lambda2 (fun vabbrv vsize -> BackendFont(vabbrv, vsize)));
        ("col-nil"       , ~% bc                        , (fun _ -> Vert([])));

        ("++"            , ~% (br --> (br --> br))      , lambda2 (fun vbr1 vbr2 -> HorzConcat(vbr1, vbr2)));
        ("+++"           , ~% (bc --> (bc --> bc))      , lambda2 (fun vbc1 vbc2 -> VertConcat(vbc1, vbc2)));
        ("lex-row"       , ~% (ctx --> (tr --> br))     , lambda2 (fun vctx vtr -> HorzLex(vctx, vtr)));
        ("lex-col"       , ~% (ctx --> (tc --> bc))     , lambda2 (fun vctx vtc -> VertLex(vctx, vtc)));

        ("set-space-ratio", ~% (fl --> (ctx --> ctx))   , lambda2 (fun vratio vctx -> PrimitiveSetSpaceRatio(vratio, vctx)));
        ("set-font"      , ~% (ft --> (ctx --> ctx))    , lambda2 (fun vfont vctx -> PrimitiveSetFont(vfont, vctx)));
        ("get-font"      , ~% (ctx --> ft)              , lambda1 (fun vctx -> PrimitiveGetFont(vctx)));
        ("set-title"     , ~% (tr --> (ctx --> ctx))    , lambda2 (fun vtitle vctx -> PrimitiveSetTitle(vtitle, vctx)));
        ("get-title"     , ~% (ctx --> tr)              , lambda1 (fun vctx -> PrimitiveGetTitle(vctx)));
        ("default-context", ~% ctx                      , (fun _ -> Context(default_context)));
      ]
  in
  let temporary_ast = StringEmpty in
  let env : environment = Hashtbl.create 128 in
  let (tyenvmid, locacc) =
    table |> List.fold_left (fun (tyenv, acc) (varnm, pty, deff) ->
      let evid = EvalVarID.fresh varnm in
      let loc = ref temporary_ast in
      let tyenvnew = Typeenv.add tyenv varnm (pty, evid) in
      begin
        add_to_environment env evid loc;
        (tyenvnew, (loc, deff) :: acc)
      end
    ) (Typeenv.empty, [])
  in
  let () =
    locacc |> List.iter (fun (loc, deff) -> begin loc := deff env; end)
  in
    (add_default_types tyenvmid, env)
