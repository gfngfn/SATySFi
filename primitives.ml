open Types


(* unit -> Variantenv.t *)
let make_variant_environment () = Variantenv.empty


(* type_environment -> (var_name * type_struct) list *)
let rec add_to_type_environment tyenv lst =
  match lst with
  | []                     -> tyenv
  | (varnm, tystr) :: tail ->
      let tyenv_new = Typeenv.add tyenv varnm tystr in
        add_to_type_environment tyenv_new tail


(* unit -> type_environment *)
let make_type_environment () =
  let i             = IntType((-32, 0, 0, 0)) in
  let b             = BoolType((-33, 0, 0, 0)) in
  let s             = StringType((-34, 0, 0, 0)) in
  let (-->) dom cod = FuncType((-38, 0, 0, 0), dom, cod) in
  let l cont        = ListType((-36, 0, 0, 0), cont) in
  let r cont        = RefType((-37, 0, 0, 0), cont) in
  let v n           = TypeVariable((-35, 0, 0, 0), n) in
  let (-%) n cont   = ForallType(n, cont) in
  let (==>) rl cont = TypeWithRestriction((-36, 0, 0, 0), rl, cont) in

    add_to_type_environment Typeenv.empty
      [ ( "+",   ([(v (-7), "Num")] ==> ((v (-7)) --> (v (-7) --> v (-7)))) );
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
        ( "!",   (-5) -% ((r (v (-5))) --> (v (-5))) );
        ( "::",  (-6) -% ((v (-6)) --> ((l (v (-6))) --> (l (v (-6))))) );

        ( "same",          s --> (s --> b) );
        ( "string-sub",    s --> (i --> (i --> s)) );
        ( "string-length", s --> i );
        ( "\\deeper",      s --> s );
        ( "break",       s );
        ( "space",       s );
(*        ( "break-char",    s ); *)
(*        ( "\\include",     s --> s ); *)
        ( "arabic",      i --> s );

        ( "list-head", (-2) -% ((l (v (-2))) --> (v (-2))) );
        ( "list-tail", (-3) -% ((l (v (-3))) --> (l (v (-3)))) );
        ( "is-empty",  (-4) -% ((l (v (-4))) --> b) )
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
  let loc_plus         : location = ref NoContent in
  let loc_minus        : location = ref NoContent in
  let loc_mod          : location = ref NoContent in
  let loc_times        : location = ref NoContent in
  let loc_divides      : location = ref NoContent in
  let loc_concat       : location = ref NoContent in
  let loc_equalto      : location = ref NoContent in
  let loc_neq          : location = ref NoContent in
  let loc_greaterthan  : location = ref NoContent in
  let loc_lessthan     : location = ref NoContent in
  let loc_geq          : location = ref NoContent in
  let loc_leq          : location = ref NoContent in
  let loc_land         : location = ref NoContent in
  let loc_lor          : location = ref NoContent in
  let loc_lnot         : location = ref NoContent in
  let loc_refnow       : location = ref NoContent in
  let loc_cons         : location = ref NoContent in
  let loc_same         : location = ref NoContent in
  let loc_stringsub    : location = ref NoContent in
  let loc_stringlength : location = ref NoContent in
  let loc_deeper       : location = ref NoContent in
  let loc_break        : location = ref NoContent in
  let loc_space        : location = ref NoContent in
  let loc_breakchar    : location = ref NoContent in
  let loc_include      : location = ref NoContent in
  let loc_arabic       : location = ref NoContent in
  let loc_listhead     : location = ref NoContent in
  let loc_listtail     : location = ref NoContent in
  let loc_isempty      : location = ref NoContent in
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
    add_to_environment env "break"       loc_break ;
    add_to_environment env "space"       loc_space ;
(*    add_to_environment env "break-char"    loc_breakchar ; *)
(*    add_to_environment env "\\include"     loc_include ; *)
    add_to_environment env "arabic"        loc_arabic ;
    add_to_environment env "list-head"     loc_listhead ;
    add_to_environment env "list-tail"     loc_listtail ;
    add_to_environment env "is-empty"      loc_isempty ;

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
                          (Concat(DeeperIndent(Concat(BreakAndIndent, ContentOf("~content"))), BreakAndIndent)) ;

    loc_break        := lambdas env [] BreakAndIndent ;

    loc_space        := lambdas env [] (StringConstant(" ")) ;

    loc_breakchar    := lambdas env [] (StringConstant("\n")) ;

    loc_include      := lambdas env ["~filename"] (PrimitiveInclude(ContentOf("~filename"))) ;

    loc_arabic       := lambdas env ["~num"] (PrimitiveArabic(ContentOf("~num"))) ;

    loc_listhead     := lambdas env ["~list"] (PrimitiveListHead(ContentOf("~list"))) ;

    loc_listtail     := lambdas env ["~list"] (PrimitiveListTail(ContentOf("~list"))) ;

    loc_isempty      := lambdas env ["~list"] (PrimitiveIsEmpty(ContentOf("~list"))) ;

    env
