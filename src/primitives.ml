open Types


let add_default_types tyenvmid =
  let dr = Range.dummy "make_variant_environment" in
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
    Typeenv.add_mutual_cons tyenvmid Tyvarid.bottom_level mutvarntcons


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
      astf (ContentOf(evid1)) (ContentOf(evid2)) (ContentOf(evid2)))), env)


let make_environments () =

  let i             = (Range.dummy "int", IntType) in
  let b             = (Range.dummy "bool", BoolType) in
  let s             = (Range.dummy "string", StringType) in
  let (~@) n        = (Range.dummy "tv", TypeVariable(n)) in
  let (-%) n ptysub = ptysub in
  let (~%) ty       = Poly(ty) in
  let l cont        = (Range.dummy "list", ListType(cont)) in
  let r cont        = (Range.dummy "ref", RefType(cont)) in
  let (-->) dom cod = (Range.dummy "func", FuncType(dom, cod)) in
  let tv1 = (let bid1 = Boundid.fresh UniversalKind () in ref (Bound(bid1))) in
  let tv2 = (let bid2 = Boundid.fresh UniversalKind () in ref (Bound(bid2))) in

  let temporary_ast = StringEmpty in
  let loc_plus         : location = ref temporary_ast in
  let loc_minus        : location = ref temporary_ast in
  let loc_mod          : location = ref temporary_ast in
  let loc_times        : location = ref temporary_ast in
  let loc_divides      : location = ref temporary_ast in
  let loc_concat       : location = ref temporary_ast in
  let loc_equalto      : location = ref temporary_ast in
  let loc_neq          : location = ref temporary_ast in
  let loc_greaterthan  : location = ref temporary_ast in
  let loc_lessthan     : location = ref temporary_ast in
  let loc_geq          : location = ref temporary_ast in
  let loc_leq          : location = ref temporary_ast in
  let loc_land         : location = ref temporary_ast in
  let loc_lor          : location = ref temporary_ast in
  let loc_lnot         : location = ref temporary_ast in
  let loc_deref        : location = ref temporary_ast in
  let loc_cons         : location = ref temporary_ast in
  let loc_same         : location = ref temporary_ast in
  let loc_stringsub    : location = ref temporary_ast in
  let loc_stringlength : location = ref temporary_ast in
  let loc_deeper       : location = ref temporary_ast in
  let loc_break        : location = ref temporary_ast in
  let loc_softbreak    : location = ref temporary_ast in
  let loc_space        : location = ref temporary_ast in
(*  let loc_breakchar    : location = ref temporary_ast in *)
(*  let loc_include      : location = ref temporary_ast in *)
  let loc_arabic       : location = ref temporary_ast in

  let table : (var_name * poly_type * location * (environment -> abstract_tree)) list =
    let ptyderef = tv1 -% (~% ((r (~@ tv1)) --> (~@ tv1))) in
    let ptycons  = tv2 -% (~% ((~@ tv2) --> ((l (~@ tv2)) --> (l (~@ tv2))))) in
      [ ( "+"  , ~% (i --> (i --> i)), loc_plus       , lambda2 (fun v1 v2 -> Plus(v1, v2)) );
        ( "-"  , ~% (i --> (i --> i)), loc_minus      , lambda2 (fun v1 v2 -> Minus(v1, v2)) );
        ( "mod", ~% (i --> (i --> i)), loc_mod        , lambda2 (fun v1 v2 -> Mod(v1, v2)) );
        ( "*"  , ~% (i --> (i --> i)), loc_times      , lambda2 (fun v1 v2 -> Times(v1,v2)) );
        ( "/"  , ~% (i --> (i --> i)), loc_divides    , lambda2 (fun v1 v2 -> Divides(v1,v2)) );
        ( "^"  , ~% (s --> (s --> s)), loc_concat     , lambda2 (fun v1 v2 -> Concat(v1,v2)) );
        ( "==" , ~% (i --> (i --> b)), loc_equalto    , lambda2 (fun v1 v2 -> EqualTo(v1,v2)) );
        ( "<>" , ~% (i --> (i --> b)), loc_neq        , lambda2 (fun v1 v2 -> LogicalNot(EqualTo(v1, v2))) );
        ( ">"  , ~% (i --> (i --> b)), loc_greaterthan, lambda2 (fun v1 v2 -> GreaterThan(v1, v2)) );
        ( "<"  , ~% (i --> (i --> b)), loc_lessthan   , lambda2 (fun v1 v2 -> LessThan(v1, v2)) );
        ( ">=" , ~% (i --> (i --> b)), loc_geq        , lambda2 (fun v1 v2 -> LogicalNot(LessThan(v1, v2))) );
        ( "<=" , ~% (i --> (i --> b)), loc_leq        , lambda2 (fun v1 v2 -> LogicalNot(GreaterThan(v1, v2))) );
        ( "&&" , ~% (b --> (b --> b)), loc_land       , lambda2 (fun v1 v2 -> LogicalAnd(v1, v2)) );
        ( "||" , ~% (b --> (b --> b)), loc_lor        , lambda2 (fun v1 v2 -> LogicalOr(v1, v2)) );
        ( "not", ~% (b --> b)        , loc_lnot       , lambda1 (fun v1 -> LogicalNot(v1)) );
        ( "!"  , ptyderef            , loc_deref      , lambda1 (fun v1 -> Reference(v1)) );
        ( "::" , ptycons             , loc_cons       , lambda2 (fun v1 v2 -> ListCons(v1, v2)) );

        ( "same"         , ~% (s --> (s --> b))        , loc_same        , lambda2 (fun v1 v2 -> PrimitiveSame(v1, v2)) );
        ( "string-sub"   , ~% (s --> (i --> (i --> s))), loc_stringsub   , lambda3 (fun vstr vpos vwid -> PrimitiveStringSub(vstr, vpos, vwid)) );
        ( "string-length", ~% (s --> i)                , loc_stringlength, lambda1 (fun vstr -> PrimitiveStringLength(vstr)) );
        ( "\\deeper"     , ~% (s --> s)                , loc_deeper      , lambda1 (fun vstr -> Concat(DeeperIndent(Concat(SoftBreakAndIndent, vstr)), SoftBreakAndIndent)) );
        ( "deeper"       , ~% (s --> s)                , loc_deeper      , lambda1 (fun vstr -> Concat(DeeperIndent(Concat(SoftBreakAndIndent, vstr)), SoftBreakAndIndent)) );
        ( "break"        , ~% s                        , loc_break       , (fun _ -> BreakAndIndent) );
        ( "soft-break"   , ~% s                        , loc_softbreak   , (fun _ -> SoftBreakAndIndent) );
        ( "space"        , ~% s                        , loc_space       , (fun _ -> StringConstant(" ")) );
        ( "arabic"       , ~% (i --> s)                , loc_arabic      , lambda1 (fun vnum -> PrimitiveArabic(vnum)) ;);
      ]
  in
  let env : environment = Hashtbl.create 128 in
  let tyenvmid =
    table |> List.fold_left (fun tyenv (varnm, pty, loc, _) ->
      let evid = EvalVarID.fresh varnm in
      begin
        add_to_environment env evid loc;
        Typeenv.add tyenv varnm (pty, evid)
      end
    ) Typeenv.empty
  in
  let () =
    table |> List.iter (fun (_, _, loc, deff) ->
      loc := deff env
    )
  in
    (add_default_types tyenvmid, env)
