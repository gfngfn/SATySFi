open Types


let tyid_option  = Typeenv.Raw.fresh_type_id "option"
let tyid_itemize = Typeenv.Raw.fresh_type_id "itemize"
let tyid_color   = Typeenv.Raw.fresh_type_id "color"
let tyid_script  = Typeenv.Raw.fresh_type_id "script"


let add_default_types (tyenvmid : Typeenv.t) : Typeenv.t =
  let dr = Range.dummy "add_default_types" in
  let unit_type = (dr, BaseType(UnitType)) in
  let float_type = (dr, BaseType(FloatType)) in
  let bid = BoundID.fresh UniversalKind () in
  let typaram = (dr, TypeVariable(ref (Bound(bid)))) in

  tyenvmid
  |> Typeenv.Raw.register_type "option" tyid_option (Typeenv.Data(1))
  |> Typeenv.Raw.add_constructor "None" ([bid], Poly(unit_type)) tyid_option
  |> Typeenv.Raw.add_constructor "Some" ([bid], Poly(typaram)) tyid_option

  |> Typeenv.Raw.register_type "itemize" tyid_itemize (Typeenv.Data(0))
  |> Typeenv.Raw.add_constructor "Item" ([], Poly(
       (dr, ProductType([(dr, BaseType(TextRowType)); (dr, ListType((dr, VariantType([], tyid_itemize)))); ]))
     )) tyid_itemize

  |> Typeenv.Raw.register_type "color" tyid_color (Typeenv.Data(0))
  |> Typeenv.Raw.add_constructor "Gray" ([], Poly(float_type)) tyid_color
  |> Typeenv.Raw.add_constructor "RGB"  ([], Poly((dr, ProductType([float_type; float_type; float_type])))) tyid_color
  |> Typeenv.Raw.add_constructor "CMYK" ([], Poly((dr, ProductType([float_type; float_type; float_type; float_type])))) tyid_color

  |> Typeenv.Raw.register_type "script" tyid_script (Typeenv.Data(0))
  |> Typeenv.Raw.add_constructor "Latin"          ([], Poly(unit_type)) tyid_script
  |> Typeenv.Raw.add_constructor "HanIdeographic" ([], Poly(unit_type)) tyid_script
  |> Typeenv.Raw.add_constructor "Kana"           ([], Poly(unit_type)) tyid_script
  |> Typeenv.Raw.add_constructor "OtherScript"    ([], Poly(unit_type)) tyid_script


let add_to_environment env varnm rfast =
  Hashtbl.add env varnm rfast


let lam evid ast = LambdaAbstract(evid, ast)
let lamenv env evid ast = FuncWithEnvironment(evid, ast, env)
let ( !- ) evid = ContentOf(evid)

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


(* -- begin: constants just for experimental use -- *)

let pdfpt = HorzBox.Length.of_pdf_point

let ( +% ) = HorzBox.( +% )
let ( -% ) = HorzBox.( -% )
let ( *% ) = HorzBox.( *% )

let default_font_scheme =
  List.fold_left (fun mapacc (script, font_info) -> mapacc |> FontSchemeMap.add script font_info)
    FontSchemeMap.empty
    [
      (CharBasis.HanIdeographic    , ("KozMin", 0.92, 0.));
      (CharBasis.HiraganaOrKatakana, ("KozMin", 0.92, 0.));
      (CharBasis.Latin             , ("Arno"  , 1., 0.));
      (CharBasis.Other             , default_font_with_ratio);
    ]

let envinit : environment = Hashtbl.create 128

let default_context =
  {
    title            = InputHorzWithEnvironment([], envinit);
    author           = InputHorzWithEnvironment([], envinit);
    font_scheme      = default_font_scheme;
    font_size        = pdfpt 12.;
    dominant_script  = CharBasis.Other;
    space_natural    = 0.33;
    space_shrink     = 0.08;
    space_stretch    = 0.16; (* 0.32; *)
    adjacent_stretch = 0.05;
    paragraph_width  = pdfpt 400.;
    paragraph_top    = pdfpt 18.;
    paragraph_bottom = pdfpt 18.;
    leading          = pdfpt 18.;
    text_color       = HorzBox.DeviceGray(0.);
    manual_rising    = pdfpt 0.;
  }

let default_graphics_context =
  {
    HorzBox.line_width   = pdfpt 1.;
    HorzBox.line_dash    = HorzBox.SolidLine;
    HorzBox.line_cap     = HorzBox.ButtCap;
    HorzBox.line_join    = HorzBox.MiterJoin;
    HorzBox.miter_limit  = pdfpt 10.;
    HorzBox.fill_color   = HorzBox.DeviceGray(0.);
    HorzBox.stroke_color = HorzBox.DeviceGray(0.);
  }

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

let default_paddings =
  {
    HorzBox.paddingL = pdfpt 2. +% margin;
    HorzBox.paddingR = pdfpt 2. +% margin;
    HorzBox.paddingT = pdfpt 2. +% margin;
    HorzBox.paddingB = pdfpt 2. +% margin;
  }

(* -- end: constants just for experimental use -- *)


let ( ~! ) = Range.dummy

let make_environments () =
  let tyenvinit = add_default_types Typeenv.empty in

  let i             = (~! "int"     , BaseType(IntType)    ) in
  let fl            = (~! "float"   , BaseType(FloatType)  ) in
  let b             = (~! "bool"    , BaseType(BoolType)   ) in
  let ln            = (~! "length"  , BaseType(LengthType) ) in
  let s             = (~! "string"  , BaseType(StringType) ) in
  let (~@) n        = (~! "tv"      , TypeVariable(n)      ) in
  let (-%) n ptysub = ptysub in
  let (~%) ty       = Poly(ty) in
  let l ty          = (~! "list"    , ListType(ty)       ) in
  let r ty          = (~! "ref"     , RefType(ty)        ) in
  let prod tylst    = (~! "product" , ProductType(tylst)   ) in
(*
  let opt ty        = (~! "option"  , VariantType([ty], tyid_option)) in
*)
  let (@->) dom cod = (~! "func"    , FuncType(dom, cod)   ) in

  let tr            = (~! "text-row", BaseType(TextRowType)) in
  let tc            = (~! "text-col", BaseType(TextColType)) in
  let br            = (~! "box-row" , BaseType(BoxRowType) ) in
  let bc            = (~! "box-col" , BaseType(BoxColType) ) in
  let ft            = (~! "font"    , BaseType(FontType)   ) in
  let ctx           = (~! "context" , BaseType(ContextType)) in
  let path          = (~! "path"    , BaseType(PathType)   ) in
  let prp           = (~! "pre-path", BaseType(PrePathType)) in
  let scr           = (~! "script"  , VariantType([], tyid_script)) in
(*
  let gctx          = (~! "graphic-context", BaseType(GraphicsContextType)) in
*)
  let gr            = (~! "graphics", BaseType(GraphicsType)) in
  let clr           = (~! "color"   , VariantType([], tyid_color)) in
  let pads          = prod [ln; ln; ln; ln] in
  let pt            = prod [ln; ln] in
  let dash          = prod [ln; ln; ln] in
  let deco          = pt @-> ln @-> ln @-> ln @-> (l gr) in
  let decoset       = prod [deco; deco; deco; deco] in
  let igr           = pt @-> (l gr) in

  let tv1 = (let bid1 = BoundID.fresh UniversalKind () in ref (Bound(bid1))) in
  let tv2 = (let bid2 = BoundID.fresh UniversalKind () in ref (Bound(bid2))) in

  let table : (var_name * poly_type * (environment -> abstract_tree)) list =
    let ptyderef  = tv1 -% (~% ((r (~@ tv1)) @-> (~@ tv1))) in
    let ptycons   = tv2 -% (~% ((~@ tv2) @-> (l (~@ tv2)) @-> (l (~@ tv2)))) in
    let ptyappinv = tv1 -% (tv2 -% (~% ((~@ tv1) @-> ((~@ tv1) @-> (~@ tv2)) @-> (~@ tv2)))) in
      [
        ( "+"  , ~% (i @-> i @-> i)   , lambda2 (fun v1 v2 -> Plus(v1, v2))                    );
        ( "-"  , ~% (i @-> i @-> i)   , lambda2 (fun v1 v2 -> Minus(v1, v2))                   );
        ( "mod", ~% (i @-> i @-> i)   , lambda2 (fun v1 v2 -> Mod(v1, v2))                     );
        ( "*"  , ~% (i @-> i @-> i)   , lambda2 (fun v1 v2 -> Times(v1, v2))                   );
        ( "/"  , ~% (i @-> i @-> i)   , lambda2 (fun v1 v2 -> Divides(v1, v2))                 );
        ( "^"  , ~% (s @-> s @-> s)   , lambda2 (fun v1 v2 -> Concat(v1, v2))                  );
        ( "==" , ~% (i @-> i @-> b)   , lambda2 (fun v1 v2 -> EqualTo(v1, v2))                 );
        ( "<>" , ~% (i @-> i @-> b)   , lambda2 (fun v1 v2 -> LogicalNot(EqualTo(v1, v2)))     );
        ( ">"  , ~% (i @-> i @-> b)   , lambda2 (fun v1 v2 -> GreaterThan(v1, v2))             );
        ( "<"  , ~% (i @-> i @-> b)   , lambda2 (fun v1 v2 -> LessThan(v1, v2))                );
        ( ">=" , ~% (i @-> i @-> b)   , lambda2 (fun v1 v2 -> LogicalNot(LessThan(v1, v2)))    );
        ( "<=" , ~% (i @-> i @-> b)   , lambda2 (fun v1 v2 -> LogicalNot(GreaterThan(v1, v2))) );
        ( "&&" , ~% (b @-> b @-> b)   , lambda2 (fun v1 v2 -> LogicalAnd(v1, v2))              );
        ( "||" , ~% (b @-> b @-> b)   , lambda2 (fun v1 v2 -> LogicalOr(v1, v2))               );
        ( "not", ~% (b @-> b)         , lambda1 (fun v1 -> LogicalNot(v1))                     );
        ( "!"  , ptyderef             , lambda1 (fun v1 -> Reference(v1))                      );
        ( "::" , ptycons              , lambda2 (fun v1 v2 -> ListCons(v1, v2))                );
        ( "+." , ~% (fl @-> fl @-> fl), lambda2 (fun v1 v2 -> FloatPlus(v1, v2))               );
        ( "-." , ~% (fl @-> fl @-> fl), lambda2 (fun v1 v2 -> FloatMinus(v1, v2))              );
        ( "+'" , ~% (ln @-> ln @-> ln), lambda2 (fun v1 v2 -> LengthPlus(v1, v2))              );
        ( "-'" , ~% (ln @-> ln @-> ln), lambda2 (fun v1 v2 -> LengthMinus(v1, v2))             );
        ( "*'" , ~% (ln @-> fl @-> ln), lambda2 (fun v1 v2 -> LengthTimes(v1, v2))             );
        ( "++" , ~% (br @-> br @-> br), lambda2 (fun vbr1 vbr2 -> HorzConcat(vbr1, vbr2))      );
        ( "+++", ~% (bc @-> bc @-> bc), lambda2 (fun vbc1 vbc2 -> VertConcat(vbc1, vbc2))      );
        ( "|>" , ptyappinv            , lambda2 (fun vx vf -> Apply(vf, vx)));

        ( "same"         , ~% (s @-> s @-> b)           , lambda2 (fun v1 v2 -> PrimitiveSame(v1, v2)) );
        ( "string-sub"   , ~% (s @-> i @-> i @-> s)     , lambda3 (fun vstr vpos vwid -> PrimitiveStringSub(vstr, vpos, vwid)) );
        ( "string-length", ~% (s @-> i)                 , lambda1 (fun vstr -> PrimitiveStringLength(vstr)) );
        ( "arabic"       , ~% (i @-> s)                 , lambda1 (fun vnum -> PrimitiveArabic(vnum)) );
        ( "float"        , ~% (i @-> fl)                , lambda1 (fun vi -> PrimitiveFloat(vi)) );

        ("form-paragraph"    , ~% (ctx @-> br @-> bc)                 , lambda2 (fun vleading vrow -> BackendLineBreaking(vleading, vrow)) );
        ("fixed-empty"       , ~% (ln @-> br)                         , lambda1 (fun vwid -> BackendFixedEmpty(vwid))   );
        ("outer-empty"       , ~% (ln @-> ln @-> ln @-> br)           , lambda3 (fun vn vp vm -> BackendOuterEmpty(vn, vp, vm)) );
        ("outer-fil"         , ~% br                                  , (fun _ -> Horz([HorzBox.HorzPure(HorzBox.PHOuterFil)])));
        ("outer-frame-block" , ~% (pads @-> deco @-> br @-> br)       , lambda3 (fun vpads vdeco vbr -> BackendOuterFrame(vpads, vdeco, vbr)));
        ("outer-frame-inline", ~% (pads @-> decoset @-> br @-> br)    , lambda3 (fun vpads vdecoset vbr -> BackendOuterFrameBreakable(vpads, vdecoset, vbr)));
        ("font"              , ~% (s @-> fl @-> fl @-> ft)            , lambda3 (fun vabbrv vszrat vrsrat -> BackendFont(vabbrv, vszrat, vrsrat)));
        ("col-nil"           , ~% bc                                  , (fun _ -> Vert([])));
        ("col-frame"         , ~% (ctx @-> pads @-> decoset @-> (ctx @-> bc) @-> bc), lambda4 (fun vctx vpads vdecoset vbc -> BackendVertFrame(vctx, vpads, vdecoset, vbc)));
        ("pbox-top"          , ~% (ctx @-> ln @-> (ctx @-> bc) @-> br), lambda3 (fun vctx vlen vk -> BackendEmbeddedVert(vctx, vlen, vk)));

        ("lex-row", ~% (ctx @-> tr @-> br), lambda2 (fun vctx vtr -> HorzLex(vctx, vtr)));
        ("lex-col", ~% (ctx @-> tc @-> bc), lambda2 (fun vctx vtc -> VertLex(vctx, vtc)));

        ("default-context"    , ~% ctx                                 , (fun _ -> Context(default_context)));
        ("set-space-ratio"    , ~% (fl @-> ctx @-> ctx)                , lambda2 (fun vratio vctx -> PrimitiveSetSpaceRatio(vratio, vctx)));
        ("set-font-size"      , ~% (ln @-> ctx @-> ctx)                , lambda2 (fun vsize vctx -> PrimitiveSetFontSize(vsize, vctx)));
        ("get-font-size"      , ~% (ctx @-> ln)                        , lambda1 (fun vctx -> PrimitiveGetFontSize(vctx)));
        ("set-font"           , ~% (scr @-> ft @-> ctx @-> ctx)        , lambda3 (fun vscript vfont vctx -> PrimitiveSetFont(vscript, vfont, vctx)));
        ("get-font"           , ~% (scr @-> ctx @-> ft)                , lambda2 (fun vscript vctx -> PrimitiveGetFont(vscript, vctx)));
        ("set-dominant-script", ~% (scr @-> ctx @-> ctx)               , lambda2 (fun vscript vctx -> PrimitiveSetDominantScript(vscript, vctx)));
        ("set-title"          , ~% (tr @-> ctx @-> ctx)                , lambda2 (fun vtitle vctx -> PrimitiveSetTitle(vtitle, vctx)));
        ("get-title"          , ~% (ctx @-> tr)                        , lambda1 (fun vctx -> PrimitiveGetTitle(vctx)));
        ("set-text-color"     , ~% (clr @-> ctx @-> ctx)               , lambda2 (fun vcolor vctx -> PrimitiveSetTextColor(vcolor, vctx)));
        ("set-leading"        , ~% (ln @-> ctx @-> ctx)                , lambda2 (fun vlen vctx -> PrimitiveSetLeading(vlen, vctx)));
        ("set-manual-rising"  , ~% (ln @-> ctx @-> ctx)                , lambda2 (fun vlen vctx -> PrimitiveSetManualRising(vlen, vctx)));
        ("get-text-width"     , ~% (ctx @-> ln)                        , lambda1 (fun vctx -> PrimitiveGetTextWidth(vctx)));
        ("embed"              , ~% (s @-> tr)                          , lambda1 (fun vstr -> PrimitiveEmbed(vstr)));
        ("inline-graphics"    , ~% (ln @-> ln @-> ln @-> igr @-> br)   , lambda4 (fun vwid vhgt vdpt vg -> BackendInlineGraphics(vwid, vhgt, vdpt, vg)));
        ("get-natural-width"  , ~% (br @-> ln)                         , lambda1 (fun vbr -> PrimitiveGetNaturalWidth(vbr)));

        ("stroke"                  , ~% (ln @-> clr @-> path @-> gr)                , lambda3 (fun vwid vclr vpath -> PrimitiveDrawStroke(vwid, vclr, vpath)));
        ("dashed-stroke"           , ~% (ln @-> dash @-> clr @-> path @-> gr)       , lambda4 (fun vwid vdash vclr vpath -> PrimitiveDrawDashedStroke(vwid, vdash, vclr, vpath)));
        ("fill"                    , ~% (clr @-> path @-> gr)                       , lambda2 (fun vclr vpath -> PrimitiveDrawFill(vclr, vpath)));
        ("draw-text"               , ~% (pt @-> br @-> gr)                          , lambda2 (fun vpt vbr -> PrimitiveDrawText(vpt, vbr)));
        ("start-path"              , ~% (pt @-> prp)                                , lambda1 (fun vpt -> PrePathBeginning(vpt)));
        ("line-to"                 , ~% (pt @-> prp @-> prp)                        , lambda2 (fun vpt vprp -> PrePathLineTo(vpt, vprp)));
        ("bezier-to"               , ~% (pt @-> pt @-> pt @-> prp @-> prp)          , lambda4 (fun vptS vptT vpt1 vprp -> PrePathCubicBezierTo(vptS, vptT, vpt1, vprp)));
        ("terminate-path"          , ~% (prp @-> path)                              , lambda1 (fun vprp -> PrePathTerminate(vprp)));
        ("close-with-line"         , ~% (prp @-> path)                              , lambda1 (fun vprp -> PrePathCloseWithLine(vprp)));
        ("close-with-bezier"       , ~% (pt @-> pt @-> prp @-> path)                , lambda3 (fun vptS vptT vprp -> PrePathCloseWithCubicBezier(vptS, vptT, vprp)));
        ("unite-path"              , ~% (path @-> path @-> path)                    , lambda2 (fun vpath1 vpath2 -> PathUnite(vpath1, vpath2)));
      ]
  in
  let temporary_ast = StringEmpty in
  let (tyenvfinal, locacc) =
    table |> List.fold_left (fun (tyenv, acc) (varnm, pty, deff) ->
      let evid = EvalVarID.fresh varnm in
      let loc = ref temporary_ast in
      let tyenvnew = Typeenv.add tyenv varnm (pty, evid) in
      begin
        add_to_environment envinit evid loc;
        (tyenvnew, (loc, deff) :: acc)
      end
    ) (tyenvinit, [])
  in
  let () =
    locacc |> List.iter (fun (loc, deff) -> begin loc := deff envinit; end)
  in
    (tyenvfinal, envinit)
