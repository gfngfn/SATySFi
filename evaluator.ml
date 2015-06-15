open Types

exception EvalError of string

let print_process msg =
  (*
    print_string (msg ^ "\n") ;
  *)
  ()

let rec string_of_ast ast =
  match ast with
  | LambdaAbstract(x, m) -> "(Lam: " ^ x ^ ". " ^ (string_of_ast m) ^ ")"
  | FuncWithEnvironment(x, m, _) -> "(LamEnv: " ^ x ^ ". " ^ (string_of_ast m) ^ ")"
  | ContentOf(v) -> "(" ^ v ^ ")"
  | NumericApply(m, n) -> "($ " ^ (string_of_ast m) ^ " " ^ (string_of_ast n) ^ ")"
  | Concat(s, t) -> (string_of_ast s) ^ "-" ^ (string_of_ast t)
  | StringEmpty -> "!"
  | _ -> "_"

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

  | ConcatOperation(astf, astl) -> interpret env (Concat(astf, astl))

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
    ( print_process (">ContentOf: " ^ v) ;
      ( try
          let content = !(Hashtbl.find env v) in
          ( print_process ("  -> " ^ (string_of_ast content)) ;
            content )
        with
        | Not_found -> raise (EvalError("undefined variable '" ^ v ^ "'"))
      )
    )

  | LetIn(mutletcons, astrest) ->
    (* nv, astdef *)
      let env_func = copy_environment env in
      ( add_mutuals_to_environment env_func mutletcons ;
        interpret env_func astrest
      )
  | LambdaAbstract(varnm, ast) -> (
      print_process (">LambdaAbstract: " ^ varnm ^ ". " ^ (string_of_ast ast))  ;
      FuncWithEnvironment(varnm, ast, env)
    )
  | FuncWithEnvironment(varnm, ast, env) -> (
      print_process ">FuncWithEnvironment" ;
      FuncWithEnvironment(varnm, ast, env)
    )
  | ApplyClassAndID(clsnmast, idnmast, astf) ->
    ( match interpret env astf with
      | FuncWithEnvironment(varnm, astdef, envf) ->
          FuncWithEnvironment(varnm,
            LetIn(MutualLetCons("@class", clsnmast, EndOfMutualLet),
              LetIn(MutualLetCons("@id", idnmast, EndOfMutualLet), astdef)
            ), envf)
      | other ->  interpret env
                    (LetIn(MutualLetCons("@class", clsnmast, EndOfMutualLet),
                      LetIn(MutualLetCons("@id", idnmast, EndOfMutualLet), astf))
                    )
    )
  | NumericApply(astf, astl) ->
    ( print_process ">NumericApply" ;
      print_process ("  " ^ (string_of_ast astf) ^ " / " ^ (string_of_ast astl)) ;
      let valuel = interpret env astl in
      let fspec = interpret env astf in
      ( print_process ("  => " ^ (string_of_ast fspec) ^ " / " ^ (string_of_ast valuel)) ;
      ( match fspec with
        | FuncWithEnvironment(varnm, astdef, envf) ->
            let env_new = copy_environment envf in
            ( add_to_environment env_new varnm (ref valuel) ;
              let intpd = interpret env_new astdef in ( print_process ("  end " ^ varnm) ; intpd )
            )
        | _ -> raise (EvalError("illegal apply"))
      )
      )
    )
(*
  | StringApply(f, clsnmarg, idnmarg, argcons) ->
    ( try
        let fspec = !(Hashtbl.find env f) in
          match fspec with
          | FuncWithEnvironment(argvarcons, astf, envf) ->
              let env_new = copy_environment envf in
              ( ( match clsnmarg with
                  | NoClassName -> add_to_environment env_new "@class" (ref NoContent)
                  | ClassName(clsnm) -> add_to_environment env_new "@class" (ref (class_name_to_abstract_tree clsnm))
                ) ;
                ( match idnmarg with
                  | NoIDName -> add_to_environment env_new "@id" (ref NoContent)
                  | IDName(idnm) -> add_to_environment env_new "@id" (ref (id_name_to_abstract_tree idnm))
                ) ;
                deal_with_cons env_new argvarcons argcons ;
                let valuef = interpret env_new astf in
                ( Hashtbl.clear env_new ;
                  valuef
                )
              )
          | _ -> raise (EvalError("illegal apply of control sequence '" ^ f ^ "'"))
      with
      | Not_found -> raise (EvalError("undefined control sequence '" ^ f ^ "'"))
    )
*)
  | DeeperIndent(ast) -> let res = interpret env ast in DeeperIndent(res)

  | BreakAndIndent -> BreakAndIndent

  | PrimitiveSame(ast1, ast2) ->
      let str1 =
      ( try Out.main (interpret env ast1) with
        | Out.IllegalOut(_) -> raise (EvalError("illegal argument of 'same'"))
      ) in
      let str2 =
      ( try Out.main (interpret env ast2) with
        | Out.IllegalOut(_) -> raise (EvalError("illegal argument of 'same'"))
      ) in
        BooleanConstant((compare str1 str2) == 0)

  | PrimitiveIsValid(astf) ->
      ( match interpret env astf with
        | NoContent -> BooleanConstant(false)
        | _ -> BooleanConstant(true)
      )
  | PrimitiveInclude(astfile_name) ->
      ( try
          let str_file_name = Out.main (interpret env astfile_name) in
          let file = open_in str_file_name in
          let parsed = Parser.main Lexer.cut_token (Lexing.from_channel file) in
            interpret env parsed
        with
        | Out.IllegalOut(_) -> raise (EvalError("illegal argument of \\include"))
        | Sys_error(s) -> raise (EvalError("System error at \\include - " ^ s))
      )
  | PrimitiveArabic(astnum) ->
      let num = interpret_int env (interpret env astnum) in StringConstant(string_of_int num)

  | ListCons(asthd, asttl) ->
      let valuehd = interpret env asthd in
      let valuetl = interpret env asttl in
        ListCons(valuehd, valuetl)

  | EndOfList -> EndOfList

  | LiteralArea(ltrl) -> ltrl

  | PrimitiveListHead(astlst) ->
      let valuelst = interpret env astlst in
        let (valuehd, _) = pop_from_separated_tree valuelst UnderConstruction in valuehd

  | PrimitiveListTail(astlst) ->
      let valuelst = interpret env astlst in
        let (_, valuetl) = pop_from_separated_tree valuelst UnderConstruction in valuetl

  | PrimitiveIsEmpty(astlst) ->
      let valuelst = interpret env astlst in
      ( match valuelst with
        | EndOfList -> BooleanConstant(true)
        | ListCons(_, _) -> BooleanConstant(false)
        | _ -> raise (EvalError("not a list"))
      )

  | IfThenElse(astb, astf, astl) ->
      if interpret_bool env astb then interpret env astf else interpret env astl

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

  | _ -> raise (EvalError("remains to be implemented"))

and interpret_bool env ast =
  match interpret env ast with
  | BooleanConstant(bc) -> bc
  | _ -> raise (EvalError("not of type bool / remains to be implemented"))

and interpret_int env ast =
  match interpret env ast with
  | NumericConstant(nc) -> nc
  | _ -> raise (EvalError("not of type int / remains to be implemented"))


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

(* abstract_tree -> abstract_tree -> (abstract_tree * abstract_tree) *)
and pop_from_separated_tree astin astconstr =
  match astin with
  | ListCons(asthd, asttl) -> (
        match asthd with
        | ListCons(a, b) -> (
            pop_from_separated_tree asthd (compensate astconstr (ListCons(UnderConstruction, asttl)))
          )
        | _ -> (asthd, compensate astconstr asttl)
      )
  | _ -> (astin, EndOfList)


(* abstract_tree -> abstract_tree -> abstract_tree *)
and compensate astunder_constr astcmpnstd =
  match astunder_constr with
  | ListCons(astformer, astlatter)
      -> ListCons((compensate astformer astcmpnstd), (compensate astlatter astcmpnstd))
  | UnderConstruction -> astcmpnstd
  | astother -> astother

and make_literal_legitimate ast =
  match ast with
  | Concat(astf, astl) ->
        Concat(make_literal_legitimate astf, make_literal_legitimate astl)
  | StringConstant(c) -> StringConstant(c)
  | _ -> raise (EvalError("illegal token in literal area"))
