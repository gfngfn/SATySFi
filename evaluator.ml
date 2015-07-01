open Types

exception EvalError of string

let print_process msg =
  (*
    print_string (msg ^ "\n") ;
  *)
  ()

let rec string_of_ast ast =
  match ast with
  | LambdaAbstract(x, m) -> "(" ^ x ^ " -> " ^ (string_of_ast m) ^ ")"
  | FuncWithEnvironment(x, m, _) -> "(" ^ x ^ " *-> " ^ (string_of_ast m) ^ ")"
  | ContentOf(v) -> "#" ^ v ^ "#"
  | Apply(m, n) -> "(" ^ (string_of_ast m) ^ " " ^ (string_of_ast n) ^ ")"
  | Concat(s, t) -> (string_of_ast s) ^ (string_of_ast t)
  | StringEmpty -> ""
  | IfThenElse(b, t, f) ->
      "(if " ^ (string_of_ast b) ^ " then " ^ (string_of_ast t) ^ " else " ^ (string_of_ast f) ^ ")"
  | IfClassIsValid(t, f) -> "(if-class-is-valid " ^ (string_of_ast t) ^ " else " ^ (string_of_ast f) ^ ")"
  | Reference(vn) -> "!" ^ vn
  | ReferenceFinal(vn) -> "'!" ^ vn
  | Overwrite(vn, n) -> "(" ^ vn ^ " <- " ^ (string_of_ast n) ^ ")"
  | MutableValue(mv) -> "(mutable " ^ (string_of_ast mv) ^ ")"
  | UnitConstant -> "()"
  | LetMutableIn(vn, d, f) -> "(let-mutable " ^ vn ^ " <- " ^ (string_of_ast d) ^ " in " ^ (string_of_ast f) ^ ")"
  | _ -> "..."

let rec make_argument_cons lst =
  match lst with
  | [] -> EndOfArgumentVariable
  | head :: tail -> ArgumentVariableCons(head, make_argument_cons tail)
(* abstract_tree -> abstract_tree *)

let copy_environment env = Hashtbl.copy env

let add_to_environment env varnm rfast =
  ( print_process ("  add " ^ varnm ^ " := " ^ (string_of_ast !rfast)) ;
    Hashtbl.add env varnm rfast
  )

(* (macro_environment ref) -> int -> (var_environment ref) -> abstract_tree -> abstract_tree *)
let rec interpret env ast =
  match ast with
  | StringEmpty -> StringEmpty

  | NoContent -> NoContent

  | Concat(astf, astl) ->
      let valuef = interpret env astf in
      let valuel = interpret env astl in
      ( match (valuef, valuel) with
        | (StringEmpty, _) -> valuel
        | (_, StringEmpty) -> valuef
        | (_, _) -> Concat(valuef, valuel)
      )
  | StringConstant(c) -> StringConstant(c)

  | ContentOf(v) ->
      ( try
          let content = !(Hashtbl.find env v) in content
        with
        | Not_found -> raise (EvalError("undefined variable '" ^ v ^ "'"))
      )
  | LetIn(mutletcons, astrest) ->
      let env_func = copy_environment env in
      ( add_mutuals_to_environment env_func mutletcons ;
        interpret env_func astrest
      )
  | LambdaAbstract(varnm, ast) -> FuncWithEnvironment(varnm, ast, env)

  | FuncWithEnvironment(varnm, ast, env) -> FuncWithEnvironment(varnm, ast, env)

  | ApplyClassAndID(clsnmast, idnmast, astf) ->
    ( match interpret env astf with
      | FuncWithEnvironment(varnm, astdef, envf) ->
          FuncWithEnvironment(varnm,
            LetIn(MutualLetCons("class", clsnmast, EndOfMutualLet),
              LetIn(MutualLetCons("id", idnmast, EndOfMutualLet), astdef)
            ), envf)
      | other ->  interpret env
                    (LetIn(MutualLetCons("class", clsnmast, EndOfMutualLet),
                      LetIn(MutualLetCons("id", idnmast, EndOfMutualLet), astf))
                    )
    )
  | Apply(astf, astl) ->
      let valuel = interpret env astl in
      let fspec = interpret env astf in
      ( match fspec with
        | FuncWithEnvironment(varnm, astdef, envf) ->
            let env_new = copy_environment envf in
            ( add_to_environment env_new varnm (ref valuel) ;
              let intpd = interpret env_new astdef in intpd
            )
        | _ ->
            ( print_string ("!   " ^ (string_of_ast astf) ^ "\n") ;
              print_string ("!   " ^ (string_of_ast astl) ^ "\n") ;
              raise (EvalError("illegal apply"))
            )
      )
  | DeeperIndent(ast) -> let res = interpret env ast in DeeperIndent(res)

  | BreakAndIndent -> BreakAndIndent

  | PrimitiveSame(ast1, ast2) ->
      let str1 =
      ( try Out.main env (interpret env ast1) with
        | Out.IllegalOut(s) -> raise (EvalError("Illegal argument for 'same': " ^ s))
      ) in
      let str2 =
      ( try Out.main env (interpret env ast2) with
        | Out.IllegalOut(s) -> raise (EvalError("Illegal argument for 'same': " ^ s))
      ) in
        BooleanConstant((compare str1 str2) == 0)

  | PrimitiveStringSub(aststr, astpos, astwid) ->
      let str =
      ( try Out.main env (interpret env aststr) with
        | Out.IllegalOut(s) -> raise (EvalError("Illegal argument for 'string-sub': " ^ s))
      ) in
        let pos = interpret_int env astpos in
        let wid = interpret_int env astwid in
          StringConstant(String.sub str pos wid)

  | PrimitiveStringLength(aststr) ->
      let str =
      ( try Out.main env (interpret env aststr) with
        | Out.IllegalOut(s) -> raise (EvalError("Illegal argument for 'string-length': " ^ s))
      ) in
        NumericConstant(String.length str)

  | PrimitiveInclude(astfile_name) ->
      ( try
          let str_file_name = Out.main env (interpret env astfile_name) in
          let file = open_in str_file_name in
          let parsed = Parser.main Lexer.cut_token (Lexing.from_channel file) in
            interpret env parsed
        with
        | Out.IllegalOut(s) -> raise (EvalError("illegal argument of \\include: " ^ s))
        | Sys_error(s) -> raise (EvalError("System error at \\include - " ^ s))
      )
  | PrimitiveArabic(astnum) ->
      let num = interpret_int env (interpret env astnum) in StringConstant(string_of_int num)

  | ListCons(asthd, asttl) ->
      let valuehd = interpret env asthd in
      let valuetl = interpret env asttl in
        ListCons(valuehd, valuetl)

  | EndOfList -> EndOfList

  | PrimitiveListHead(astlst) ->
      let valuelst = interpret env astlst in
      ( match valuelst with
        | ListCons(vhd, vtl) -> vhd
        | EndOfList -> raise (EvalError("cannot apply empty list for 'list-head'"))
        | _ -> raise (EvalError("'list-head' expected argument to be a list, but is not"))
      )
  | PrimitiveListTail(astlst) ->
      let valuelst = interpret env astlst in
      ( match valuelst with
        | ListCons(vhd, vtl) -> vtl
        | EndOfList -> raise (EvalError("cannot apply empty list for 'list-tail'"))
        | _ -> raise (EvalError("'list-tail' expected argument to be a list, but is not"))
      )
  | PrimitiveIsEmpty(astlst) ->
      let valuelst = interpret env astlst in
      ( match valuelst with
        | EndOfList -> BooleanConstant(true)
        | ListCons(_, _) -> BooleanConstant(false)
        | _ -> raise (EvalError("not a list"))
      )
  | IfClassIsValid(asttru, astfls) ->
      let vcclass = interpret env (ContentOf("class")) in
      ( match vcclass with
        | NoContent -> interpret env astfls
        | _         -> interpret env asttru
      )
  | IfIDIsValid(asttru, astfls) ->
      let vcid = interpret env (ContentOf("id")) in
      ( match vcid with
        | NoContent -> interpret env astfls
        | _         -> interpret env asttru
      )
  | IfThenElse(astb, astf, astl) ->
      if interpret_bool env astb then interpret env astf else interpret env astl

  | WhileDo(astb, astc) ->
      if interpret_bool env astb then
        let _ = interpret env astc in interpret env (WhileDo(astb, astc))
      else UnitConstant

  | LetMutableIn(varnm, astdflt, astaft) ->
      let valuedflt = interpret env astdflt in
      let env_new = Hashtbl.copy env in
      ( add_to_environment env_new varnm (ref (MutableValue(valuedflt))) ;
        interpret env_new astaft
      )
  | Reference(varnm) ->
      ( try
          let valuemutvar = !(Hashtbl.find env varnm) in
          ( match valuemutvar with
            | MutableValue(astmv) -> astmv
            | _ -> raise (EvalError("'" ^ varnm ^ "' is not a mutable variable for '!'"))
          )
        with
        | Not_found -> raise (EvalError("undefined mutable variable '" ^ varnm ^ "' for '!'"))
      )
  | ReferenceFinal(varnm) -> ReferenceFinal(varnm)

  | Overwrite(varnm, astnew) ->
      ( try
          let rfvalue = Hashtbl.find env varnm in
          ( match !rfvalue with
            | MutableValue(astmv) ->
                ( rfvalue := MutableValue(interpret env astnew) ; UnitConstant )
            | _ -> raise (EvalError("'" ^ varnm ^ "' is not a mutable variable for '<-'"))
          )
        with
        | Not_found -> raise (EvalError("undefined mutable variable '" ^ varnm ^ "' for '<-'"))
      )
  | UnitConstant -> UnitConstant

  | Sequential(astf, astl) ->
      let valuef = interpret env astf in
      let valuel = interpret env astl in
      ( match valuef with
        | UnitConstant -> valuel
        | _ -> raise (EvalError("not of type unit"))
      )

  | MutableValue(astmv) -> MutableValue(astmv)

  | FinishHeaderFile -> EvaluatedEnvironment(env)

  | NumericConstant(nc) -> NumericConstant(nc)
  | Times(astl, astr) ->
      let numl = interpret_int env astl in
      let numr = interpret_int env astr in
        NumericConstant(numl * numr)
  | Divides(astl, astr) ->
      let numl = interpret_int env astl in
      let numr = interpret_int env astr in
      ( try NumericConstant(numl / numr) with
        | Division_by_zero -> raise (EvalError("division by zero")) )
  | Mod(astl, astr) ->
      let numl = interpret_int env astl in
      let numr = interpret_int env astr in
      ( try NumericConstant(numl mod numr) with
        | Division_by_zero -> raise (EvalError("division by zero")) )
  | Plus(astl, astr) ->
      let numl = interpret_int env astl in
      let numr = interpret_int env astr in
        NumericConstant(numl + numr)
  | Minus(astl, astr) ->
      let numl = interpret_int env astl in
      let numr = interpret_int env astr in
        NumericConstant(numl - numr)
  | BooleanConstant(bc) -> BooleanConstant(bc)
  | EqualTo(astl, astr) ->
      let numl = interpret_int env astl in
      let numr = interpret_int env astr in
        BooleanConstant(numl == numr)
  | GreaterThan(astl, astr) ->
      let numl = interpret_int env astl in
      let numr = interpret_int env astr in
        BooleanConstant(numl > numr)
  | LessThan(astl, astr) ->
      let numl = interpret_int env astl in
      let numr = interpret_int env astr in
        BooleanConstant(numl < numr)
  | LogicalAnd(astl, astr) ->
      let blnl = interpret_bool env astl in
      let blnr = interpret_bool env astr in
        BooleanConstant(blnl && blnr)
  | LogicalOr(astl, astr) ->
      let blnl = interpret_bool env astl in
      let blnr = interpret_bool env astr in
        BooleanConstant(blnl || blnr)
  | LogicalNot(astl) ->
      let blnl = interpret_bool env astl in
        BooleanConstant(not blnl)

  | other -> raise (EvalError("remains to be implemented: " ^ (string_of_ast other)))

and interpret_bool env ast =
  let vb = interpret env ast in
    match vb with
    | BooleanConstant(bc) -> bc
    | other -> raise (EvalError("not of type bool: " ^ (string_of_ast other)))

and interpret_int env ast =
  let vi = interpret env ast in
    match vi with
    | NumericConstant(nc) -> nc
    | other -> raise (EvalError("not of type int: " ^ (string_of_ast other)))


and add_mutuals_to_environment env_func mutletcons =
  match mutletcons with
  | EndOfMutualLet -> ()
  | MutualLetCons(nv, astcont, tailcons) ->
      let valuecont =
        ( let intprtd = interpret env_func astcont in
            match intprtd with
            | LambdaAbstract(varnm, ast) -> FuncWithEnvironment(varnm, ast, env_func)
            | other -> other
        )
      in
        ( add_to_environment env_func nv (ref valuecont) ;
          add_mutuals_to_environment env_func tailcons
        )
(*
(* abstract_tree -> abstract_tree -> (abstract_tree * abstract_tree) *)
and pop_from_separated_tree astin astconstr =
  match astin with
  | ListCons(asthd, asttl) ->
      ( match asthd with
        | ListCons(a, b) ->
            pop_from_separated_tree asthd (compensate astconstr (ListCons(UnderConstruction, asttl)))
        | _ -> (asthd, compensate astconstr asttl)
      )
  | _ -> (astin, EndOfList)

(* abstract_tree -> abstract_tree -> abstract_tree *)
and compensate astunder_constr astcmpnstd =
  match astunder_constr with
  | UnderConstruction -> astcmpnstd
  | ListCons(astformer, astlatter)
      -> ListCons((compensate astformer astcmpnstd), (compensate astlatter astcmpnstd))
  | astother -> astother
*)

and make_literal_legitimate ast =
  match ast with
  | Concat(astf, astl) ->
        Concat(make_literal_legitimate astf, make_literal_legitimate astl)
  | StringConstant(c) -> StringConstant(c)
  | _ -> raise (EvalError("illegal token in literal area"))
