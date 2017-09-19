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
        (dr, UTVariantCons("Item", (dr, MProductType([(dr, MTypeName([], "string")); (dr, MTypeName([(dr, MTypeName([], "itemize"))], "list"))])),
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


let default_context =
  {
    font_info     = ("Arno", HorzBox.Length.of_pdf_point 12.);
    space_natural = HorzBox.Length.of_pdf_point 4.;
    space_shrink  = HorzBox.Length.of_pdf_point 1.;
    space_stretch = HorzBox.Length.of_pdf_point 2.;
    leading       = HorzBox.Length.of_pdf_point 18.;
  }


let make_environments () =

  let i             = (Range.dummy "int"     , BaseType(IntType)    ) in
  let b             = (Range.dummy "bool"    , BaseType(BoolType)   ) in
  let s             = (Range.dummy "string"  , BaseType(StringType) ) in
  let (~@) n        = (Range.dummy "tv"      , TypeVariable(n)      ) in
  let (-%) n ptysub = ptysub in
  let (~%) ty       = Poly(ty) in
  let l cont        = (Range.dummy "list"    , ListType(cont)       ) in
  let r cont        = (Range.dummy "ref"     , RefType(cont)        ) in
  let (-->) dom cod = (Range.dummy "func"    , FuncType(dom, cod)   ) in

  let tr            = (Range.dummy "text-row", BaseType(TextRowType)) in
  let tc            = (Range.dummy "text-col", BaseType(TextColType)) in
  let br            = (Range.dummy "box-row" , BaseType(BoxRowType) ) in
  let bc            = (Range.dummy "box-col" , BaseType(BoxColType) ) in
  let ft            = (Range.dummy "font"    , BaseType(FontType)   ) in
  let ctx           = (Range.dummy "context" , BaseType(ContextType)) in

  let tv1 = (let bid1 = BoundID.fresh UniversalKind () in ref (Bound(bid1))) in
  let tv2 = (let bid2 = BoundID.fresh UniversalKind () in ref (Bound(bid2))) in

  let table : (var_name * poly_type * (environment -> abstract_tree)) list =
    let ptyderef = tv1 -% (~% ((r (~@ tv1)) --> (~@ tv1))) in
    let ptycons  = tv2 -% (~% ((~@ tv2) --> ((l (~@ tv2)) --> (l (~@ tv2))))) in
    let astfdeeper = lambda1 (fun vstr -> Concat(DeeperIndent(Concat(SoftBreakAndIndent, vstr)), SoftBreakAndIndent)) in
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

        ( "same"         , ~% (s --> (s --> b))         , lambda2 (fun v1 v2 -> PrimitiveSame(v1, v2)) );
        ( "string-sub"   , ~% (s --> (i --> (i --> s))) , lambda3 (fun vstr vpos vwid -> PrimitiveStringSub(vstr, vpos, vwid)) );
        ( "string-length", ~% (s --> i)                 , lambda1 (fun vstr -> PrimitiveStringLength(vstr)) );
        ( "\\deeper"     , ~% (s --> s)                 , astfdeeper );
        ( "deeper"       , ~% (s --> s)                 , astfdeeper );
        ( "break"        , ~% s                         , (fun _ -> BreakAndIndent) );
        ( "soft-break"   , ~% s                         , (fun _ -> SoftBreakAndIndent) );
        ( "space"        , ~% s                         , (fun _ -> StringConstant(" ")) );
        ( "arabic"       , ~% (i --> s)                 , lambda1 (fun vnum -> PrimitiveArabic(vnum)) );

        ("form-paragraph", ~% (ctx --> (br --> bc))     , lambda2 (fun vleading vrow -> BackendLineBreaking(vleading, vrow)) );
        ("fixed-empty"   , ~% (i --> br)                , lambda1 (fun vwid -> BackendFixedEmpty(vwid))   );
        ("fixed-string"  , ~% (ft --> (tr --> br))      , lambda2 (fun vfont vwid -> BackendFixedString(vfont, vwid))   );
        ("outer-empty"   , ~% (i --> (i --> (i --> br))), lambda3 (fun vn vp vm -> BackendOuterEmpty(vn, vp, vm)) );
        ("outer-fil"     , ~% br                        , (fun _ -> Horz([HorzBox.HorzPure(HorzBox.PHOuterFil)])));
        ("font"          , ~% (s --> (i --> ft))        , lambda2 (fun vabbrv vsize -> BackendFont(vabbrv, vsize)));
        ("set-font"      , ~% (ft --> (ctx --> ctx))    , lambda2 (fun vfont vctx -> BackendSetFont(vfont, vctx)));
        ("default-context", ~% ctx                      , (fun _ -> Context(default_context)));
        ("++"            , ~% (br --> (br --> br))      , lambda2 (fun vbr1 vbr2 -> HorzConcat(vbr1, vbr2)));
        ("+++"           , ~% (bc --> (bc --> bc))      , lambda2 (fun vbc1 vbc2 -> VertConcat(vbc1, vbc2)));
        ("lex-row"       , ~% (ctx --> (tr --> br))     , lambda2 (fun vctx vtr -> HorzLex(vctx, vtr)));
        ("lex-col"       , ~% (ctx --> (tc --> bc))     , lambda2 (fun vctx vtc -> VertLex(vctx, vtc)));
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
