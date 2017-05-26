open Types
open Display

exception EvalError of string


let print_for_debug_evaluator msg =
(*
  print_string msg ;
*)
  ()


let report_bug_evaluator msg =
  failwith msg


let rec make_argument_cons lst =
  match lst with
  | []           -> EndOfArgumentVariable
  | head :: tail -> ArgumentVariableCons(head, make_argument_cons tail)


let copy_environment (env : environment) = Hashtbl.copy env

let add_to_environment (env : environment) (varnm : var_name) (rfast : abstract_tree ref) = Hashtbl.add env varnm rfast

let find_in_environment (env : environment) (varnm : var_name) = Hashtbl.find env varnm


let rec interpret env ast =
  match ast with

(* ---- basic value ---- *)

  | StringEmpty                           -> StringEmpty
  | NumericConstant(nc)                   -> NumericConstant(nc)
  | StringConstant(c)                     -> StringConstant(c)
  | BooleanConstant(bc)                   -> BooleanConstant(bc)
  | UnitConstant                          -> UnitConstant
  | EvaluatedEnvironment(env)             -> EvaluatedEnvironment(env)
  | FuncWithEnvironment(varnm, ast, envf) -> FuncWithEnvironment(varnm, ast, envf)
  | DeeperIndent(ast)                     -> DeeperIndent(interpret env ast)
  | BreakAndIndent                        -> BreakAndIndent
  | SoftBreakAndIndent                    -> SoftBreakAndIndent
  | Concat(astf, astl)                    ->
      let valuef = interpret env astf in
      let valuel = interpret env astl in
        begin
          match (valuef, valuel) with
          | (StringEmpty, _) -> valuel
          | (_, StringEmpty) -> valuef
          | (_, _)           -> Concat(valuef, valuel)
        end

(* ---- list value ---- *)

  | EndOfList              -> EndOfList

  | ListCons(asthd, asttl) ->
      let valuehd = interpret env asthd in
      let valuetl = interpret env asttl in
        ListCons(valuehd, valuetl)

(* ---- tuple value ---- *)

  | EndOfTuple              -> EndOfTuple

  | TupleCons(asthd, asttl) ->
      let valuehd = interpret env asthd in
      let valuetl = interpret env asttl in
        TupleCons(valuehd, valuetl)

(* -- fundamentals -- *)

  | ContentOf(varnm) ->
      begin
        try
          let content = !(find_in_environment env varnm) in
            match content with
            | LazyContentWithEnvironmentRef(ast1, envref) -> interpret (!envref) ast1
            | _                                           -> content
        with
        | Not_found -> report_bug_evaluator ("ContentOf: variable '" ^ varnm ^ "' not found")
      end

  | LetIn(mutletcons, astrest) ->
      let env_func = copy_environment env in
        begin
          add_mutuals_to_environment false env_func env_func "" mutletcons ;
          interpret env_func astrest
        end

  | LambdaAbstract(varnm, ast) -> FuncWithEnvironment(varnm, ast, env)

  | Apply(astf, astl) ->
      let fspec = interpret env astf in
        begin
          match fspec with
          | FuncWithEnvironment(varnm, astdef, envf) ->
              let valuel = interpret env astl in
              let env_new = copy_environment envf in
                begin
                  add_to_environment env_new varnm (ref valuel) ;
                  interpret env_new astdef
                end
          | _ -> report_bug_evaluator "Apply: not a function"
        end

  | IfThenElse(astb, astf, astl) ->
      if interpret_bool env astb then interpret env astf else interpret env astl

(* ---- record ---- *)

  | Record(asc) -> Record(Assoc.map_value (interpret env) asc)

  | AccessField(ast1, fldnm) ->
      let value1 = interpret env ast1 in
      begin
        match value1 with
        | Record(asc1) -> Assoc.find asc1 fldnm
        | _            -> report_bug_evaluator "AccessField: not a Record"
      end

(* ---- class/id option ---- *)

  | ApplyClassAndID(clsnmast, idnmast, astf) ->
      begin                                                                       (* for debug *)
        print_for_debug_evaluator ("%1 " ^ (string_of_ast astf) ^ "\n") ;         (* for debug *)
        let valuef =  interpret env
                        (LetIn(MutualLetCons("class-name", clsnmast, EndOfMutualLet),
                          LetIn(MutualLetCons("id-name", idnmast, EndOfMutualLet), astf))) in
          begin                                                                   (* for debug *)
            print_for_debug_evaluator ("%2 " ^ (string_of_ast valuef) ^ "\n") ;   (* for debug *)
            match valuef with
            | FuncWithEnvironment(varnm, astdef, envf) ->
                FuncWithEnvironment(varnm,
                  LetIn(MutualLetCons("class-name", clsnmast, EndOfMutualLet),
                    LetIn(MutualLetCons("id-name", idnmast, EndOfMutualLet), astdef)
                  ), envf)
            | other ->  valuef
          end                                                           (* for debug *)
      end                                                               (* for debug *)

(* ---- imperatives ---- *)

  | LetMutableIn(varnm, astdflt, astaft) ->
      let valueini = interpret env astdflt in
      let loc = ref valueini in
      let env_new = copy_environment env in
        begin
          add_to_environment env_new varnm (ref (Location(loc))) ;
          interpret env_new astaft
        end

  | Sequential(ast1, ast2) ->
      let value1 = interpret env ast1 in
      let value2 = interpret env ast2 in
        begin
          match value1 with
          | UnitConstant -> value2
          | _            -> report_bug_evaluator "Sequential: first operand value is not a UnitConstant"
        end

  | Location(loc) -> Location(loc)

  | Overwrite(varnm, astnew) ->
      begin
        try
          let rfvalue = find_in_environment env varnm in
            match !rfvalue with
            | Location(loc) ->
                let newvalue = interpret env astnew in
                  begin
                    loc := newvalue ;
                    UnitConstant
                  end
            | _             -> report_bug_evaluator "Overwrite: value is not a Location"
        with
        | Not_found -> report_bug_evaluator ("Overwrite: mutable value '" ^ varnm ^ "' not found")
      end

  | WhileDo(astb, astc) ->
      if interpret_bool env astb then
        let _ = interpret env astc in interpret env (WhileDo(astb, astc))
      else
        UnitConstant

  | Reference(astcont) ->
      let valuecont = interpret env astcont in
        begin
          match valuecont with
          | Location(loc) -> !loc
          | _             -> report_bug_evaluator "Reference"
        end

  | LazyContent(ast1) -> LazyContentWithEnvironmentRef(ast1, (ref env))

  | LazyContentWithEnvironmentRef(ast1, envref) -> LazyContentWithEnvironmentRef(ast1, envref)

(* ---- final reference ---- *)

  | DeclareGlobalHash(astkey, astdflt) ->
      begin
        try
          let str_key = Out.main (interpret env astkey) in
          let valueini = interpret env astdflt in
          let loc = ref valueini in
            begin
              add_to_environment global_hash_env str_key (ref (Location(loc))) ;
              UnitConstant
            end
        with
        | Out.IllegalOut(_) -> raise (EvalError("this cannot hapen:\n    illegal hash key for 'declare-global-hash'"))
      end

  | OverwriteGlobalHash(astkey, astnew) ->
      begin
        try
          let str_key = Out.main (interpret env astkey) in
            try
              let rfvalue = find_in_environment global_hash_env str_key in
                match !rfvalue with
                | Location(loc) ->
                    let valuenew = interpret env astnew in
                      begin
                        loc := valuenew ;
                        UnitConstant
                      end
                | _             -> report_bug_evaluator "OverwriteGlobalHash: value is not a Location"
            with
            | Not_found -> raise (EvalError("undefined global hash key \"" ^ str_key ^ "\""))
        with
        | Out.IllegalOut(s) -> raise (EvalError("illegal argument for '<<-': " ^ s))
      end

  | ReferenceFinal(varnm) -> ReferenceFinal(interpret env varnm)

(* ---- others ---- *)

  | FinishHeaderFile -> EvaluatedEnvironment(env)

  | FinishStruct     -> EvaluatedEnvironment(env)

  | PatternMatch(astobj, pmcons) ->
      let valueobj = interpret env astobj in select_pattern env valueobj pmcons

  | Constructor(constrnm, astcont) ->
      let valuecont = interpret env astcont in
        Constructor(constrnm, valuecont)
(*
  | Module(mdlnm, astmdl, astaft) ->
      let env_out = copy_environment env in
      let env_in  = copy_environment env in
        begin
          add_module_to_environment env_out env_in mdlnm astmdl ;
          interpret env_out astaft
        end
*)
(* -- primitive operation -- *)

  | PrimitiveSame(ast1, ast2) ->
      let str1 =
        try Out.main (interpret env ast1) with
        | Out.IllegalOut(s) -> raise (EvalError("illegal argument for 'same':\n    " ^ s))
      in
      let str2 =
        try Out.main (interpret env ast2) with
        | Out.IllegalOut(s) -> raise (EvalError("illegal argument for 'same':\n    " ^ s))
      in
        BooleanConstant((compare str1 str2) = 0)

  | PrimitiveStringSub(aststr, astpos, astwid) ->
      let str =
        try Out.main (interpret env aststr) with
        | Out.IllegalOut(s) -> raise (EvalError("illegal argument for 'string-sub':\n    " ^ s))
      in
        let pos = interpret_int env astpos in
        let wid = interpret_int env astwid in
          StringConstant(String.sub str pos wid)

  | PrimitiveStringLength(aststr) ->
      let str =
        try Out.main (interpret env aststr) with
        | Out.IllegalOut(s) -> raise (EvalError("Illegal argument for 'string-length': " ^ s))
      in
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

  | Times(astl, astr) ->
      let numl = interpret_int env astl in
      let numr = interpret_int env astr in
        NumericConstant(numl * numr)

  | Divides(astl, astr) ->
      let numl = interpret_int env astl in
      let numr = interpret_int env astr in
        begin
          try NumericConstant(numl / numr) with
          | Division_by_zero -> raise (EvalError("division by zero"))
        end

  | Mod(astl, astr) ->
      let numl = interpret_int env astl in
      let numr = interpret_int env astr in
        begin
          try NumericConstant(numl mod numr) with
          | Division_by_zero -> raise (EvalError("division by zero"))
        end

  | Plus(astl, astr) ->
      let numl = interpret_int env astl in
      let numr = interpret_int env astr in
        NumericConstant(numl + numr)

  | Minus(astl, astr) ->
      let numl = interpret_int env astl in
      let numr = interpret_int env astr in
        NumericConstant(numl - numr)

  | EqualTo(astl, astr) ->
      let numl = interpret_int env astl in
      let numr = interpret_int env astr in
        BooleanConstant(numl = numr)

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

(*
  | other -> raise (EvalError("this cannot happen / remains to be implemented: " ^ (string_of_ast other)))
*)

(* environment -> abstract_tree -> bool *)
and interpret_bool env ast =
  let vb = interpret env ast in
    match vb with
    | BooleanConstant(bc) -> bc
    | other               -> report_bug_evaluator "interpret_bool: not a BooleanConstant"


(* environment -> abstract_tree -> int *)
and interpret_int env ast =
  let vi = interpret env ast in
    match vi with
    | NumericConstant(nc) -> nc
    | other               -> report_bug_evaluator ("interpret_int: not a NumericConstant; " ^ (string_of_ast ast) ^ " ->* " ^ (string_of_ast vi))


(* module_name -> var_name -> var_name *)
and make_variable_name mdlnm varnm =
  match mdlnm with
  | "" -> varnm
  | _  -> mdlnm ^ "." ^ varnm

(*
(* environment -> environment -> module_name -> module_tree -> unit *)
and add_module_to_environment eout ein mdlnm mdltrdef =
  match mdltrdef with
  | MFinishModule -> ()

  | MDirectLetIn(mutletcons, mdltraft) ->
      begin
        add_mutuals_to_environment true eout ein "" mutletcons ;
        add_module_to_environment eout ein mdlnm mdltraft
      end

  | MPublicLetIn(mutletcons, mdltraft) ->
      begin
        add_mutuals_to_environment true eout ein mdlnm mutletcons ;
        add_module_to_environment eout ein mdlnm mdltraft
      end

  | MPrivateLetIn(mutletcons, mdltraft) ->
      begin
        add_mutuals_to_environment false eout ein mdlnm mutletcons ;
        add_module_to_environment eout ein mdlnm mdltraft
      end

  | MPublicLetMutableIn(varnm, astini, mdltraft) ->
      let valueini = interpret ein astini in
      let loc = ref valueini in
        begin
          add_to_environment ein varnm (ref (Location(loc))) ;
          add_to_environment eout (make_variable_name mdlnm varnm) (ref (Location(loc))) ;
          add_module_to_environment eout ein mdlnm mdltraft
        end

  | MPrivateLetMutableIn(varnm, astini, mdltraft) ->
      let valueini = interpret ein astini in
      let loc = ref valueini in
        begin
          add_to_environment ein varnm (ref (Location(loc))) ;
          add_module_to_environment eout ein mdlnm mdltraft
        end
*)

(* environment -> abstract_tree -> pattern_match_cons -> abstract_tree *)
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


(* environment -> pattern_tree -> abstract_tree *)
and check_pattern_matching env pat astobj =
  match (pat, astobj) with
  | (PNumericConstant(pnc), NumericConstant(nc)) -> pnc = nc
  | (PBooleanConstant(pbc), BooleanConstant(bc)) -> pbc = bc
  | (PStringConstant(ast1), ast2)                ->
      let out1 =
        try Out.main ast1 with
        | Out.IllegalOut(s) -> raise (EvalError("Illegal argument for pattern matching of string: " ^ s))
      in
      let out2 =
        try Out.main ast2 with
        | Out.IllegalOut(s) -> raise (EvalError("Illegal argument for pattern matching of string: " ^ s))
      in
        out1 = out2

  | (PUnitConstant, UnitConstant)                -> true
  | (PWildCard, _)                               -> true
  | (PVariable(varnm), _)                        ->
      begin
        add_to_environment env varnm (ref astobj) ; true
      end
  | (PAsVariable(varnm, psub), sub)              ->
      begin
        add_to_environment env varnm (ref sub) ; check_pattern_matching env psub sub
      end

  | (PEndOfList, EndOfList)                      -> true
  | (PListCons(phd, ptl), ListCons(hd, tl))      ->
      (check_pattern_matching env phd hd) && (check_pattern_matching env ptl tl)

  | (PEndOfTuple, EndOfTuple)                    -> true
  | (PTupleCons(phd, ptl), TupleCons(hd, tl))    ->
      (check_pattern_matching env phd hd) && (check_pattern_matching env ptl tl)

  | (PConstructor(cnm1, psub), Constructor(cnm2, sub))
      when cnm1 = cnm2                           -> check_pattern_matching env psub sub

  | _                                            -> false


and add_mutuals_to_environment (is_public : bool) (eout : environment) (ein : environment) (mdlnm : module_name) (mutletcons : mutual_let_cons) =
  let lst = add_mutuals_to_environment_sub is_public [] mdlnm eout ein mutletcons in
    begin                                                                           (* for debug *)
      print_for_debug_evaluator ("add_mutuals_to_environment '" ^ mdlnm ^ "'\n") ;  (* for debug *)
      add_zeroary_mutuals is_public lst mdlnm eout ein
    end                                                                             (* for debug *)


and add_mutuals_to_environment_sub
    (is_public : bool) (lst : (var_name * abstract_tree) list) (mdlnm : module_name)
    (eout : environment) (ein : environment) (mutletcons : mutual_let_cons) =
  match mutletcons with
  | EndOfMutualLet                          -> lst
  | MutualLetCons(varnm, astcont, tailcons) ->
      begin
        try
          let valuecont = interpret ein astcont in
            begin
              add_to_environment ein (make_variable_name "" varnm) (ref valuecont) ;
              if is_public then
                begin                                                                 (* for debug *)
                  add_to_environment eout (make_variable_name mdlnm varnm) (ref valuecont)
                  ; print_for_debug_evaluator ("sub '" ^ mdlnm ^ "." ^ varnm ^ "'\n") (* for debug *)
                end                                                                   (* for debug *)
              else () ;
              add_mutuals_to_environment_sub is_public lst mdlnm eout ein tailcons
            end
        with
        | EvalError(_) -> add_mutuals_to_environment_sub is_public ((varnm, astcont) :: lst) mdlnm eout ein tailcons
            (* 0-ary definition dependent of ``sibling'' functions *)
      end


and add_zeroary_mutuals (is_public : bool) (lst : (var_name * abstract_tree) list) (mdlnm : module_name) (eout : environment) (ein : environment) =
  let newlst = add_zeroary_mutuals_sub is_public lst mdlnm eout ein [] in
    if List.length newlst = 0 then
      ()
    else if (List.length newlst) = (List.length lst) then
      raise (EvalError("meaningless 0-ary mutual recursion"))
    else
      add_zeroary_mutuals is_public newlst mdlnm eout ein


and add_zeroary_mutuals_sub
    (is_public : bool) (lst : (var_name * abstract_tree) list) (mdlnm : module_name)
    (eout : environment) (ein : environment) (acc : (var_name * abstract_tree) list) =
  match lst with
  | []                       -> acc
  | (varnm, astcont) :: tail ->
      begin
        try
          let valuecont = interpret ein astcont in
            begin
              add_to_environment ein (make_variable_name "" varnm) (ref valuecont) ;
              if is_public  then add_to_environment eout (make_variable_name mdlnm varnm) (ref valuecont)
                            else () ;
              add_zeroary_mutuals_sub is_public tail mdlnm eout ein acc
            end
        with
        | EvalError(_) -> add_zeroary_mutuals_sub is_public tail mdlnm eout ein ((varnm, astcont) :: acc)
      end
