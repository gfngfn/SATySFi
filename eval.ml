(* module Mcdsemantics *)
  open Types

  exception EvalError of string

  let rec make_argument_cons lst =
    match lst with
    | [] -> EndOfArgumentVariable
    | head :: tail -> ArgumentVariableCons(head, make_argument_cons tail)

  (* abstract_tree -> abstract_tree *)
  let rec main ast_main =
    let loc_same : location = ref NoContent in
    let loc_deeper : location = ref NoContent in
    let loc_break : location = ref NoContent in
    let loc_include : location = ref NoContent in
    let env_main : environment = Hashtbl.create 128 in
      Hashtbl.add env_main "same" loc_same ;
      Hashtbl.add env_main "\\deeper" loc_deeper ;
      Hashtbl.add env_main "\\break" loc_break ;
      Hashtbl.add env_main "\\include" loc_include ;
      loc_same := FuncWithEnvironment(
                    make_argument_cons ["~stra"; "~strb"],
                    PrimitiveSame(ContentOf("~stra"), ContentOf("~strb")),
                    env_main
                  ) ;
      loc_deeper := FuncWithEnvironment(make_argument_cons ["~content"],
                      Concat(DeeperIndent(Concat(BreakAndIndent, ContentOf("~content"))), BreakAndIndent),
                      env_main
                    ) ;
      loc_break  := FuncWithEnvironment(
                      make_argument_cons [],
                      BreakAndIndent,
                      env_main
                    ) ;
      loc_include  := FuncWithEnvironment(make_argument_cons ["~filename"],
                        PrimitiveInclude(ContentOf("~filename")),
                        env_main
                      ) ;
      interpret env_main ast_main

  (* (macro_environment ref) -> int -> (var_environment ref) -> abstract_tree -> abstract_tree *)
  and interpret env ast =

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
        ( try !(Hashtbl.find env v) with
          | Not_found -> raise (EvalError("undefined variable '" ^ v ^ "'"))
        )

    | Separated(astf, astl) ->
        let valuef = interpret env astf in
        let valuel = interpret env astl in
          Separated(valuef, valuel)

    | StringApply(f, clsnmarg, idnmarg, argcons) ->
      ( try
          let fspec = !(Hashtbl.find env f) in
            match fspec with
            | FuncWithEnvironment(argvarcons, astf, envf) ->
                let env_new = Hashtbl.copy envf in
                ( ( match clsnmarg with
                    | NoClassName -> Hashtbl.add env_new "@class" (ref NoContent)
                    | ClassName(clsnm) -> Hashtbl.add env_new "@class" (ref (class_name_to_abstract_tree clsnm))
                  ) ;
                  ( match idnmarg with
                    | NoIDName -> Hashtbl.add env_new "@id" (ref NoContent)
                    | IDName(idnm) -> Hashtbl.add env_new "@id" (ref (id_name_to_abstract_tree idnm))
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

    | DeeperIndent(abstr) -> let res = interpret env abstr in DeeperIndent(res)

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

    | LetIn(nv, astdef, astrest) ->
        let env_func = Hashtbl.copy env in
        ( Hashtbl.add env_func nv (ref NoContent) ;
          let valuedef = interpret env_func astdef in
          ( Hashtbl.add env_func nv (ref valuedef) (* overwrite nv *) ;
            interpret env_func astrest
          )
        )

    | LambdaAbstract(argvarcons, ast) -> FuncWithEnvironment(argvarcons, ast, env)

    | _ -> raise (EvalError("remains to be implemented"))

  and deal_with_cons env argvarcons argcons =
    match (argvarcons, argcons) with
    | (EndOfArgumentVariable, EndOfArgument) -> ()
    | (ArgumentVariableCons(argvar, avtail), ArgumentCons(arg, atail)) ->
        ( Hashtbl.add env argvar (ref arg) ; deal_with_cons env avtail atail )
    | _ -> raise (EvalError("wrong number of argument"))

  and class_name_to_abstract_tree clsnm =
    StringConstant((String.sub clsnm 1 ((String.length clsnm) - 1)))

  and id_name_to_abstract_tree idnm =
    StringConstant((String.sub idnm 1 ((String.length idnm) - 1)))

  (* abstract_tree -> abstract_tree -> (abstract_tree * abstract_tree) *)
  and pop_from_separated_tree astin astconstr =
    match astin with
    | Separated(astf, astl) -> (
          match astf with
          | Separated(a, b) -> (
              pop_from_separated_tree astf (compensate astconstr (Separated(UnderConstruction, astl)))
            )
          | _ -> (astf, compensate astconstr astl)
        )
    | _ -> (astin, StringEmpty)

  (* abstract_tree -> abstract_tree -> abstract_tree *)
  and compensate astunder_constr astcmpnstd =
    match astunder_constr with
    | Separated(astformer, astlatter)
        -> Separated((compensate astformer astcmpnstd), (compensate astlatter astcmpnstd))
    | UnderConstruction -> astcmpnstd
    | astother -> astother

  and make_literal_legitimate ast =
    match ast with
    | Concat(astf, astl) ->
          Concat(make_literal_legitimate astf, make_literal_legitimate astl)
    | StringConstant(c) -> StringConstant(c)
    | _ -> raise (EvalError("illegal token in literal area"))
