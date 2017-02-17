open Types


let make_variant_environment =
  let dr = Range.dummy "make_variant_environment" in
  let tv1 = Tyvarid.fresh Tyvarid.Quantifiable in
  let varntenv = Variantenv.add_list Variantenv.empty
    [ ("Item", (dr, ProductType([(dr, StringType); (dr, ListType((dr, VariantType([], "itemize"))))])), "itemize");
      ("Just", (dr, ForallType(tv1, UniversalKind, (dr, TypeVariable(tv1)))), "maybe");
      ("Nothing", (dr, ForallType(tv1, UniversalKind, (dr, UnitType))), "maybe") ]
  in
    Variantenv.register_variant_list varntenv [ (0, "itemize"); (1, "maybe") ]


let make_type_environment =
  let i             = (Range.dummy "int", IntType) in
  let b             = (Range.dummy "bool", BoolType) in
  let s             = (Range.dummy "string", StringType) in
  let v n           = (Range.dummy "tv", TypeVariable(n)) in
  let (-%) n cont   = (Range.dummy "forall", ForallType(n, UniversalKind, cont)) in
  let l cont        = (Range.dummy "list", ListType(cont)) in
  let r cont        = (Range.dummy "ref", RefType(cont)) in
  let (-->) dom cod = (Range.dummy "func", FuncType(dom, cod)) in
  let tv1           = Tyvarid.fresh Tyvarid.Quantifiable in
  let tv2           = Tyvarid.fresh Tyvarid.Quantifiable in

    Typeenv.from_list
      [ ( "+",   i --> (i --> i) );
        ( "-",   i --> (i --> i) );
        ( "mod", i --> (i --> i) );
        ( "*",   i --> (i --> i) );
        ( "/",   i --> (i --> i) );
        ( "^",   s --> (s --> s) );
        ( "==",  i --> (i --> b) );
        ( "<>",  i --> (i --> b) );
        ( ">",   i --> (i --> b) );
        ( "<",   i --> (i --> b) );
        ( ">=",  i --> (i --> b) );
        ( "<=",  i --> (i --> b) );
        ( "&&",  b --> (b --> b) );
        ( "||",  b --> (b --> b) );
        ( "not", b --> b );
        ( "!",   tv1 -% ((r (v tv1)) --> (v tv1)) );
        ( "::",  tv2 -% ((v tv2) --> ((l (v tv2)) --> (l (v tv2)))) );

        ( "same",          s --> (s --> b) );
        ( "string-sub",    s --> (i --> (i --> s)) );
        ( "string-length", s --> i );
        ( "\\deeper",      s --> s );
        ( "deeper",        s --> s );
        ( "break",         s );
        ( "soft-break",    s );
        ( "space",         s );
(*        ( "break-char",    s ); *)
(*        ( "\\include",     s --> s ); *)
        ( "arabic",      i --> s );
      ]

let rec lambdas env vlst ast =
  match vlst with
  | []         -> ast
  | vn :: tail -> FuncWithEnvironment(vn, lambdas_sub tail ast, env)

and lambdas_sub vlst ast =
  match vlst with
  | []         -> ast
  | vn :: tail -> LambdaAbstract(vn, lambdas_sub tail ast)

let add_to_environment env varnm rfast =
  Hashtbl.add env varnm rfast

let make_environment () =
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
  let loc_refnow       : location = ref temporary_ast in
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
  let env : environment = Hashtbl.create 128 in
    add_to_environment env "+"             loc_plus ;
    add_to_environment env "-"             loc_minus ;
    add_to_environment env "mod"           loc_mod ;
    add_to_environment env "*"             loc_times ;
    add_to_environment env "/"             loc_divides ;
    add_to_environment env "^"             loc_concat ;
    add_to_environment env "=="            loc_equalto ;
    add_to_environment env "<>"            loc_neq ;
    add_to_environment env ">"             loc_greaterthan ;
    add_to_environment env "<"             loc_lessthan ;
    add_to_environment env ">="            loc_geq ;
    add_to_environment env "<="            loc_leq ;
    add_to_environment env "&&"            loc_land ;
    add_to_environment env "||"            loc_lor ;
    add_to_environment env "not"           loc_lnot ;
    add_to_environment env "!"             loc_refnow ;
    add_to_environment env "::"            loc_cons ;
    add_to_environment env "same"          loc_same ;
    add_to_environment env "string-sub"    loc_stringsub ;
    add_to_environment env "string-length" loc_stringlength ;
    add_to_environment env "\\deeper"      loc_deeper ;
    add_to_environment env "deeper"        loc_deeper ;
    add_to_environment env "break"         loc_break ;
    add_to_environment env "soft-break"    loc_softbreak ;
    add_to_environment env "space"         loc_space ;
(*    add_to_environment env "break-char"    loc_breakchar ; *)
(*    add_to_environment env "\\include"     loc_include ; *)
    add_to_environment env "arabic"        loc_arabic ;

    loc_plus         := lambdas env ["~opl"; "~opr"]
                          (Plus(ContentOf("~opl"), ContentOf("~opr"))) ;

    loc_minus        := lambdas env ["~opl"; "~opr"]
                          (Minus(ContentOf("~opl"), ContentOf("~opr"))) ;

    loc_mod          := lambdas env ["~opl"; "~opr"]
                          (Mod(ContentOf("~opl"), ContentOf("~opr"))) ;

    loc_times        := lambdas env ["~opl"; "~opr"]
                          (Times(ContentOf("~opl"), ContentOf("~opr"))) ;

    loc_divides      := lambdas env ["~opl"; "~opr"]
                          (Divides(ContentOf("~opl"), ContentOf("~opr"))) ;

    loc_concat       := lambdas env ["~opl"; "~opr"]
                          (Concat(ContentOf("~opl"), ContentOf("~opr"))) ;

    loc_equalto      := lambdas env ["~opl"; "~opr"]
                          (EqualTo(ContentOf("~opl"), ContentOf("~opr"))) ;

    loc_neq          := lambdas env ["~opl"; "~opr"]
                          (LogicalNot(EqualTo(ContentOf("~opl"), ContentOf("~opr")))) ;

    loc_greaterthan  := lambdas env ["~opl"; "~opr"]
                          (GreaterThan(ContentOf("~opl"), ContentOf("~opr"))) ;

    loc_lessthan     := lambdas env ["~opl"; "~opr"]
                          (LessThan(ContentOf("~opl"), ContentOf("~opr"))) ;

    loc_geq          := lambdas env ["~opl"; "~opr"]
                          (LogicalNot(LessThan(ContentOf("~opl"), ContentOf("~opr")))) ;

    loc_leq          := lambdas env ["~opl"; "~opr"]
                          (LogicalNot(GreaterThan(ContentOf("~opl"), ContentOf("~opr")))) ;

    loc_land         := lambdas env ["~opl"; "~opr"]
                          (LogicalAnd(ContentOf("~opl"), ContentOf("~opr"))) ;

    loc_lor          := lambdas env ["~opl"; "~opr"]
                          (LogicalOr(ContentOf("~opl"), ContentOf("~opr"))) ;

    loc_lnot         := lambdas env ["~op"]
                          (LogicalNot(ContentOf("~op"))) ;

    loc_refnow       := lambdas env ["~op"] (Reference(ContentOf("~op"))) ;

    loc_cons         := lambdas env ["~opl"; "~opr"] (ListCons(ContentOf("~opl"), ContentOf("~opr"))) ;

    loc_same         := lambdas env ["~stra"; "~strb"]
                          (PrimitiveSame(ContentOf("~stra"), ContentOf("~strb"))) ;

    loc_stringsub    := lambdas env ["~str"; "~pos"; "~wid"]
                          (PrimitiveStringSub(ContentOf("~str"), ContentOf("~pos"), ContentOf("~wid"))) ;

    loc_stringlength := lambdas env ["~str"]
                          (PrimitiveStringLength(ContentOf("~str"))) ;

    loc_deeper       := lambdas env ["~content"]
                          (Concat(DeeperIndent(Concat(SoftBreakAndIndent, ContentOf("~content"))), SoftBreakAndIndent)) ;

    loc_break        := lambdas env [] BreakAndIndent ;

    loc_softbreak    := lambdas env [] SoftBreakAndIndent ;

    loc_space        := lambdas env [] (StringConstant(" ")) ;

(*    loc_breakchar    := lambdas env [] (StringConstant("\n")) ; *)

(*    loc_include      := lambdas env ["~filename"] (PrimitiveInclude(ContentOf("~filename"))) ; *)

    loc_arabic       := lambdas env ["~num"] (PrimitiveArabic(ContentOf("~num"))) ;

    env
