open Types
open Typeenv

exception EvalError of string


let rec make_argument_cons lst =
  match lst with
  | [] -> EndOfArgumentVariable
  | head :: tail -> ArgumentVariableCons(head, make_argument_cons tail)

let copy_environment env = Hashtbl.copy env

let add_to_environment env varnm rfast = Hashtbl.add env varnm rfast

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
        | (_, _)           -> Concat(valuef, valuel)
      )
  | StringConstant(c) -> StringConstant(c)

  | ContentOf(v) ->
      ( try
          let content = !(Hashtbl.find env v) in content
        with
        | Not_found ->  raise (EvalError("undefined variable '" ^ v ^ "'\n"
                          ^ "    maybe you wrote 0-ary meaningless mutual recursion"))
      )
  | LetIn(mutletcons, astrest) ->
      let env_func = copy_environment env in
      ( add_mutuals_to_environment env_func mutletcons ;
        interpret env_func astrest
      )
  | PatternMatch(astobj, pmcons) ->
      let valueobj = interpret env astobj in select_pattern env valueobj pmcons

  | LambdaAbstract(varnm, ast) -> FuncWithEnvironment(varnm, ast, env)

  | FuncWithEnvironment(varnm, ast, envf) -> FuncWithEnvironment(varnm, ast, envf)

  | ApplyClassAndID(clsnmast, idnmast, astf) ->
      ((* print_string ("%1 " ^ (string_of_ast astf) ^ "\n") ; *)
      let valuef =  interpret env
                      (LetIn(MutualLetCons("class", clsnmast, EndOfMutualLet),
                        LetIn(MutualLetCons("id", idnmast, EndOfMutualLet), astf))
                      ) in
      ((* print_string ("%2 " ^ (string_of_ast valuef) ^ "\n") ; *)
      ( match valuef with
        | FuncWithEnvironment(varnm, astdef, envf) ->
            FuncWithEnvironment(varnm,
              LetIn(MutualLetCons("class", clsnmast, EndOfMutualLet),
                LetIn(MutualLetCons("id", idnmast, EndOfMutualLet), astdef)
              ), envf)
        | other ->  valuef
      )
      ))
  | Apply(astf, astl) ->
      let fspec = interpret env astf in
      ( match fspec with
        | FuncWithEnvironment(varnm, astdef, envf) ->
            let valuel = interpret env astl in
            let env_new = copy_environment envf in
            ( add_to_environment env_new varnm (ref valuel) ;
              let intpd = interpret env_new astdef in intpd
            )
        | _ ->
            raise (EvalError("this cannot happen:\n    illegal apply\n\n"
              ^ "      " ^ (string_of_ast astf) ^ "\n"
              ^ "      " ^ (string_of_ast astl)))
      )
  | DeeperIndent(ast) -> let res = interpret env ast in DeeperIndent(res)

  | BreakAndIndent -> BreakAndIndent

  | PrimitiveSame(ast1, ast2) ->
      let str1 =
      ( try Out.main (interpret env ast1) with
        | Out.IllegalOut(s) -> raise (EvalError("this cannot happen:\n    llegal argument for 'same': " ^ s))
      ) in
      let str2 =
      ( try Out.main (interpret env ast2) with
        | Out.IllegalOut(s) -> raise (EvalError("this cannot happen:\n    Illegal argument for 'same': " ^ s))
      ) in
        BooleanConstant((compare str1 str2) == 0)

  | PrimitiveStringSub(aststr, astpos, astwid) ->
      let str =
      ( try Out.main (interpret env aststr) with
        | Out.IllegalOut(s) -> raise (EvalError("Illegal argument for 'string-sub': " ^ s))
      ) in
        let pos = interpret_int env astpos in
        let wid = interpret_int env astwid in
          StringConstant(String.sub str pos wid)

  | PrimitiveStringLength(aststr) ->
      let str =
      ( try Out.main (interpret env aststr) with
        | Out.IllegalOut(s) -> raise (EvalError("Illegal argument for 'string-length': " ^ s))
      ) in
        NumericConstant(String.length str)
(*
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
*)
  | PrimitiveArabic(astnum) ->
      let num = interpret_int env (interpret env astnum) in StringConstant(string_of_int num)

  | ListCons(asthd, asttl) ->
      let valuehd = interpret env asthd in
      let valuetl = interpret env asttl in
        ListCons(valuehd, valuetl)

  | EndOfList -> EndOfList

  | TupleCons(asthd, asttl) ->
      let valuehd = interpret env asthd in
      let valuetl = interpret env asttl in
        TupleCons(valuehd, valuetl)

  | EndOfTuple -> EndOfTuple

  | PrimitiveListHead(astlst) ->
      let valuelst = interpret env astlst in
      ( match valuelst with
        | ListCons(vhd, vtl) -> vhd
        | EndOfList          -> raise (EvalError("applied 'list-head' to an empty list"))
        | _                  -> raise (EvalError("this cannot happen: argument of 'list-head' is not a list"))
      )
  | PrimitiveListTail(astlst) ->
      let valuelst = interpret env astlst in
      ( match valuelst with
        | ListCons(vhd, vtl) -> vtl
        | EndOfList          -> raise (EvalError("applied 'list-tail' to an empty list"))
        | _                  -> raise (EvalError("'list-tail' expected argument to be a list, but is not"))
      )
  | PrimitiveIsEmpty(astlst) ->
      let valuelst = interpret env astlst in
      ( match valuelst with
        | EndOfList      -> BooleanConstant(true)
        | ListCons(_, _) -> BooleanConstant(false)
        | _              -> raise (EvalError("this cannot happen: not a list"))
      )
  | IfClassIsValid(asttru, astfls) ->
      ( try
          let vcclass = interpret env (ContentOf("class")) in
          ( match vcclass with
            | NoContent -> interpret env astfls
            | _         -> interpret env asttru
          )
        with
        | EvalError(_) -> raise (EvalError("illegal 'if-class-is-valid'; 'class' cannot be used here"))
      )
  | IfIDIsValid(asttru, astfls) ->
      ( try
          let vcid = interpret env (ContentOf("id")) in
          ( match vcid with
            | NoContent -> interpret env astfls
            | _         -> interpret env asttru
          )
        with
        | EvalError(_) -> raise (EvalError("illegal 'if-id-is-valid'; 'id' cannot be used here"))
      )
  | IfThenElse(astb, astf, astl) ->
      if interpret_bool env astb then interpret env astf else interpret env astl

  | WhileDo(astb, astc) ->
      if interpret_bool env astb then
        let _ = interpret env astc in interpret env (WhileDo(astb, astc))
      else UnitConstant

  | LetMutableIn(varnm, astdflt, astaft) ->
      let valuedflt = interpret env astdflt in
      let env_new = copy_environment env in
      ( add_to_environment env_new varnm (ref (MutableValue(valuedflt))) ;
        interpret env_new astaft
      )
  | Reference(astcont) ->
      let valuecont = interpret env astcont in
      ( match valuecont with
        | MutableValue(astmv) -> astmv
        | _                   -> raise (EvalError("this cannot happen:\n    not for '!'\n\n      "
                                   ^ (string_of_ast astcont)))
      )
  | ReferenceFinal(varnm) -> ReferenceFinal(interpret env varnm)

  | DeclareGlobalHash(astkey, astdflt) ->
      ( try
          let str_key = Out.main (interpret env astkey) in
          let valuedflt = interpret env astdflt in
          ( add_to_environment global_hash_env str_key (ref (MutableValue(valuedflt))) ;
            UnitConstant
          )
        with
        | Out.IllegalOut(_) -> raise (EvalError("this cannot hapen:\n    illegal hash key for 'declare-global-hash'"))
      )
  | OverwriteGlobalHash(astkey, astnew) ->
      ( try
          let str_key = Out.main (interpret env astkey) in
          ( try
              let rfvalue = Hashtbl.find global_hash_env str_key in
              ( match !rfvalue with
                | MutableValue(astmv) ->
                    ( rfvalue := MutableValue(interpret env astnew) ; UnitConstant )
                | _                   -> raise (EvalError("this cannot happen\n:"
                                           ^ "   global hash key '" ^ str_key ^ "' contains non-mutable value"))
              )
            with
            | Not_found -> raise (EvalError("undefined global hash key \"" ^ str_key ^ "\""))
          )
        with
        | Out.IllegalOut(_) -> raise (EvalError("this cannot happen:\n    illegal argument for '<<-'"))
      )
  | Overwrite(varnm, astnew) ->
      ( try
          let rfvalue = Hashtbl.find env varnm in
          ( match !rfvalue with
            | MutableValue(astmv) ->
                ( rfvalue := MutableValue(interpret env astnew) ; UnitConstant )
            | _ -> raise (EvalError("this cannot happen:\n    '" ^ varnm ^ "' is not a mutable variable for '<-'"))
          )
        with
        | Not_found ->  raise (EvalError("this cannot happen:\n"
                          ^ "    undefined mutable variable '" ^ varnm ^ "' for '<-'"))
      )
  | UnitConstant -> UnitConstant

  | Sequential(ast1, ast2) ->
      let value1 = interpret env ast1 in
      let value2 = interpret env ast2 in
      ( match value1 with
        | UnitConstant -> value2
        | _            -> raise (EvalError("this cannot happen:\n    not of type unit"))
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
    | other -> raise (EvalError("this cannot happen: not of type bool: " ^ (string_of_ast other)))

and interpret_int env ast =
  let vi = interpret env ast in
    match vi with
    | NumericConstant(nc) -> nc
    | other -> raise (EvalError("this cannot happen: not of type int: " ^ (string_of_ast other)))


and select_pattern env astobj pmcons =
  match pmcons with
  | EndOfPatternMatch               -> raise (EvalError("no matches"))
  | PatternMatchCons(pat, astto, tailcons) ->
      let envnew = copy_environment env in
      let b = check_pattern_matching envnew pat astobj in
        if b then interpret envnew astto else select_pattern env astobj tailcons
  | PatternMatchConsWhen(pat, astb, astto, tailcons) ->
      let envnew = copy_environment env in
      let b = check_pattern_matching envnew pat astobj in
      let bb = interpret_bool envnew astb in
        if b && bb then interpret envnew astto else select_pattern env astobj tailcons

and check_pattern_matching env pat astobj =
  match (pat, astobj) with
  | (PNumericConstant(pnc), NumericConstant(nc)) -> pnc = nc
  | (PBooleanConstant(pbc), BooleanConstant(bc)) -> pbc = bc
  | (PUnitConstant, UnitConstant)                -> true
  | (PWildCard, _)                               -> true
  | (PVariable(varnm), _) ->
      ( add_to_environment env varnm (ref astobj) ; true )
  | (PAsVariable(varnm, psub), sub) ->
      ( add_to_environment env varnm (ref sub) ; check_pattern_matching env psub sub )

  | (PEndOfList, EndOfList)                      -> true
  | (PListCons(phd, ptl), ListCons(hd, tl))
      -> (check_pattern_matching env phd hd) && (check_pattern_matching env ptl tl)

  | (PEndOfTuple, EndOfTuple)                    -> true
  | (PTupleCons(phd, ptl), TupleCons(hd, tl))
      -> (check_pattern_matching env phd hd) && (check_pattern_matching env ptl tl)

  | _ -> false


(* environment -> mutual_let_cons -> unit *)
and add_mutuals_to_environment env mutletcons =
  let lst = add_mutuals_to_environment_sub [] env mutletcons in
    add_zeroary_mutuals lst env

(* (var_name * abstract_tree) list -> environment -> mutual_let_cons
  -> (var_name * abstract_tree) list *)
and add_mutuals_to_environment_sub lst env mutletcons =
  match mutletcons with
  | EndOfMutualLet                       -> lst
  | MutualLetCons(nv, astcont, tailcons) ->
      ( try
          let valuecont = interpret env astcont in
          ( add_to_environment env nv (ref valuecont) ;
            add_mutuals_to_environment_sub lst env tailcons
          )
        with
        | EvalError(_) -> add_mutuals_to_environment_sub ((nv, astcont) :: lst) env tailcons
          (* 0-ary definition dependent of ``sibling'' functions *)
      )

(* (var_name * abstract_tree) list -> environment -> unit *)
and add_zeroary_mutuals lst env =
  let newlst = add_zeroary_mutuals_sub lst env [] in
    if List.length newlst == 0 then
      ()
    else if (List.length newlst) == (List.length lst) then
      raise (EvalError("meaningless 0-ary mutual recursion definition"))
    else
      add_zeroary_mutuals newlst env

and add_zeroary_mutuals_sub lst env constr =
  match lst with
  | []                    -> constr
  | (nv, astcont) :: tail ->
      ( try
          let valuecont = interpret env astcont in
          ( add_to_environment env nv (ref valuecont) ;
            add_zeroary_mutuals_sub tail env constr
          )
        with
        | EvalError(_) -> add_zeroary_mutuals_sub tail env ((nv, astcont) :: constr)
      )
