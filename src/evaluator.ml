open Types

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

let add_to_environment (env : environment) (evid : EvalVarID.t) (rfast : abstract_tree ref) = Hashtbl.add env evid rfast

let find_in_environment (env : environment) (evid : EvalVarID.t) = Hashtbl.find env evid


let rec normalize_box_row ast =
  let iter = normalize_box_row in
    match ast with
    | Horz(hblst)            -> hblst
    | HorzConcat(ast1, ast2) -> List.append (iter ast1) (iter ast2)
    | _                      -> report_bug_evaluator "normalize_box_row"


let rec interpret env ast =
  match ast with

(* ---- basic value ---- *)

  | StringEmpty                           -> ast
  | NumericConstant(_)                    -> ast
  | StringConstant(_)                     -> ast
  | BooleanConstant(_)                    -> ast
  | UnitConstant                          -> ast
  | EvaluatedEnvironment(_)               -> ast
  | FuncWithEnvironment(_, _, _)          -> ast
  | BreakAndIndent                        -> ast
  | SoftBreakAndIndent                    -> ast
  | InText(_)                             -> ast
  | DeeperIndent(astsub)                  -> DeeperIndent(interpret env astsub)
  | Concat(astf, astl)                    ->
      let valuef = interpret env astf in
      let valuel = interpret env astl in
        begin
          match (valuef, valuel) with
          | (StringEmpty, _) -> valuel
          | (_, StringEmpty) -> valuef
          | (_, _)           -> Concat(valuef, valuel)
        end

(* ---- values for backend ---- *)

  | Horz(_) -> ast

  | HorzConcat(ast1, ast2) ->
      let value1 = interpret env ast1 in
      let value2 = interpret env ast2 in
      begin
        match (value1, value2) with
        | (Horz([]), _) -> value2
        | (_, Horz([])) -> value1
        | (_, _)        -> HorzConcat(value1, value2)
      end

  | Vert(_) -> ast

  | VertConcat(ast1, ast2) ->
      let value1 = interpret env ast1 in
      let value2 = interpret env ast2 in
      begin
        match (value1, value2) with
        | (Vert([]), _) -> value2
        | (_, Vert([])) -> value1
        | (_, _)        -> VertConcat(value1, value2)
      end

  | BackendLineBreaking(astrow) ->
      let hblst = normalize_box_row astrow in
      let paragraph_width = HorzBox.Length.of_pdf_point 300. in  (* temporary *)
      let imvblst = LineBreak.main paragraph_width hblst in
      Vert(imvblst)

  | BackendFixedEmpty(astwid) ->  (* temporary; should introduce a type for length *)
      let rawwid = interpret_int env astwid in
      let wid = HorzBox.Length.of_pdf_point (float_of_int rawwid) in
      Horz([HorzBox.HorzPure(HorzBox.PHFixedEmpty(wid))])

  | BackendOuterEmpty(astnat, astshrink, aststretch) ->  (* temporary; should introduce a type for length *)
      let rawnat = interpret_int env astnat in
      let widnat = HorzBox.Length.of_pdf_point (float_of_int rawnat) in
      let rawshrink = interpret_int env astshrink in
      let widshrink = HorzBox.Length.of_pdf_point (float_of_int rawshrink) in
      let rawstretch = interpret_int env aststretch in
      let widstretch = HorzBox.Length.of_pdf_point (float_of_int rawstretch) in
      Horz([HorzBox.HorzPure(HorzBox.PHOuterEmpty(widnat, widshrink, widstretch))])

  | BackendFixedString(astitxt) ->
      let font_info = ("Arno", HorzBox.Length.of_pdf_point 16.) in  (* temporary; should be variable *)
      begin
        match astitxt with
        | InText(itxt) -> Horz([HorzBox.HorzPure(HorzBox.PHFixedString(font_info, InternalText.of_utf_8 itxt))])
        | _            -> report_bug_evaluator "BackendFixedString"
      end

(* ---- list value ---- *)

  | EndOfList -> ast

  | ListCons(asthd, asttl) ->
      let valuehd = interpret env asthd in
      let valuetl = interpret env asttl in
        ListCons(valuehd, valuetl)

(* ---- tuple value ---- *)

  | EndOfTuple -> ast

  | TupleCons(asthd, asttl) ->
      let valuehd = interpret env asthd in
      let valuetl = interpret env asttl in
        TupleCons(valuehd, valuetl)

(* -- fundamentals -- *)

  | ContentOf(evid) ->
      begin
        try
          let content = !(find_in_environment env evid) in
            match content with
            | LazyContentWithEnvironmentRef(ast1, envref) -> interpret (!envref) ast1
            | _                                           -> content
        with
        | Not_found -> report_bug_evaluator ("ContentOf: variable '" ^ (EvalVarID.show_direct evid) ^ "' not found")
      end

  | LetIn(mutletcons, astrest) ->
      let envfunc = copy_environment env in
        begin
          add_mutuals_to_environment envfunc mutletcons ;
          interpret envfunc astrest
        end

  | LambdaAbstract(evid, ast) -> FuncWithEnvironment(evid, ast, env)

  | Apply(astf, astl) ->
      let fspec = interpret env astf in
        begin
          match fspec with
          | FuncWithEnvironment(evid, astdef, envf) ->
              let valuel = interpret env astl in
              let envnew = copy_environment envf in
                begin
                  add_to_environment envnew evid (ref valuel) ;
                  interpret envnew astdef
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

  | ApplyClassAndID(evidcls, evidid, clsnmast, idnmast, astf) ->
      let () = print_for_debug_evaluator ("%1 " ^ (Display.string_of_ast astf) ^ "\n") in  (* for debug *)
      let valuef =  interpret env
                      (LetIn(MutualLetCons(evidcls, clsnmast, EndOfMutualLet),
                        LetIn(MutualLetCons(evidid, idnmast, EndOfMutualLet), astf))) in
      begin
        print_for_debug_evaluator ("%2 " ^ (Display.string_of_ast valuef) ^ "\n") ;   (* for debug *)
        match valuef with
        | FuncWithEnvironment(varnm, astdef, envf) ->
            FuncWithEnvironment(varnm,
              LetIn(MutualLetCons(evidcls, clsnmast, EndOfMutualLet),
                LetIn(MutualLetCons(evidid, idnmast, EndOfMutualLet), astdef)
              ), envf)
        | other ->  valuef
      end

(* ---- imperatives ---- *)

  | LetMutableIn(evid, astdflt, astaft) ->
      let valueini = interpret env astdflt in
      let loc = ref valueini in
      let envnew = copy_environment env in
        begin
          add_to_environment envnew evid (ref (Location(loc))) ;
          interpret envnew astaft
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

  | Overwrite(evid, astnew) ->
      begin
        try
          let rfvalue = find_in_environment env evid in
            match !rfvalue with
            | Location(loc) ->
                let newvalue = interpret env astnew in
                  begin
                    loc := newvalue ;
                    UnitConstant
                  end
            | _             -> report_bug_evaluator "Overwrite: value is not a Location"
        with
        | Not_found -> report_bug_evaluator ("Overwrite: mutable value '" ^ (EvalVarID.show_direct evid) ^ "' not found")
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
              Hashtbl.add global_hash_env str_key (ref (Location(loc))) ;
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
              let rfvalue = Hashtbl.find global_hash_env str_key in
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

  | ReferenceFinal(astkey) -> ReferenceFinal(interpret env astkey)

(* ---- others ---- *)

  | FinishHeaderFile -> EvaluatedEnvironment(env)

  | FinishStruct     -> EvaluatedEnvironment(env)

  | PatternMatch(astobj, pmcons) ->
      let valueobj = interpret env astobj in select_pattern env valueobj pmcons

  | Constructor(constrnm, astcont) ->
      let valuecont = interpret env astcont in
        Constructor(constrnm, valuecont)

  | Module(astmdl, astaft) ->
      let value = interpret env astmdl in
      begin
        match value with
        | EvaluatedEnvironment(envfinal) -> interpret envfinal astaft
        | _                              -> report_bug_evaluator ("module did evaluate not to EvaluatedEnvironment; "
                                                                  ^ (Display.string_of_ast value))
      end

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
        let resstr =
          try String.sub str pos wid with
          | Invalid_argument(s) -> raise (EvalError("illegal index for 'string-sub'"))
        in
          StringConstant(resstr)

  | PrimitiveStringLength(aststr) ->
      let str =
        try Out.main (interpret env aststr) with
        | Out.IllegalOut(s) -> raise (EvalError("illegal argument for 'string-length': " ^ s))
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

and interpret_bool (env : environment) (ast : abstract_tree) : bool =
  let vb = interpret env ast in
    match vb with
    | BooleanConstant(bc) -> bc
    | other               -> report_bug_evaluator ("interpret_bool: not a BooleanConstant; "
                                                   ^ (Display.string_of_ast ast)
                                                   ^ " ->* " ^ (Display.string_of_ast vb))


and interpret_int (env : environment) (ast : abstract_tree) : int =
  let vi = interpret env ast in
    match vi with
    | NumericConstant(nc) -> nc
    | other               -> report_bug_evaluator ("interpret_int: not a NumericConstant; "
                                                   ^ (Display.string_of_ast ast)
                                                   ^ " ->* " ^ (Display.string_of_ast vi))


and select_pattern (env : environment) (astobj : abstract_tree) (pmcons : pattern_match_cons) =
  match pmcons with
  | EndOfPatternMatch -> raise (EvalError("no matches"))

  | PatternMatchCons(pat, astto, tailcons) ->
      let envnew = copy_environment env in
      let b = check_pattern_matching envnew pat astobj in
        if b then interpret envnew astto else select_pattern env astobj tailcons

  | PatternMatchConsWhen(pat, astb, astto, tailcons) ->
      let envnew = copy_environment env in
      let b = check_pattern_matching envnew pat astobj in
      let bb = interpret_bool envnew astb in
        if b && bb then interpret envnew astto else select_pattern env astobj tailcons


and check_pattern_matching (env : environment) (pat : pattern_tree) (astobj : abstract_tree) =
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
  | (PVariable(evid), _)                         ->
      begin
        add_to_environment env evid (ref astobj) ; true
      end
  | (PAsVariable(evid, psub), sub)              ->
      begin
        add_to_environment env evid (ref sub) ; check_pattern_matching env psub sub
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


and add_mutuals_to_environment (env : environment) (mutletcons : mutual_let_cons) =
  let lst = add_mutuals_to_environment_sub [] env mutletcons in
    begin                                                           (* for debug *)
      print_for_debug_evaluator ("add_mutuals_to_environment\n") ;  (* for debug *)
      add_zeroary_mutuals lst env
    end                                                             (* for debug *)


and add_mutuals_to_environment_sub (lst : (EvalVarID.t * abstract_tree) list) (env : environment) (mutletcons : mutual_let_cons) =
  match mutletcons with
  | EndOfMutualLet                         -> lst
  | MutualLetCons(evid, astcont, tailcons) ->
      begin
        try
          let valuecont = interpret env astcont in
            begin
              add_to_environment env evid (ref valuecont) ;
              add_mutuals_to_environment_sub lst env tailcons
            end
        with
        | EvalError(_) -> add_mutuals_to_environment_sub ((evid, astcont) :: lst) env tailcons
            (* 0-ary definition dependent of ``sibling'' functions *)
      end


and add_zeroary_mutuals (lst : (EvalVarID.t * abstract_tree) list) (env : environment) =
  let newlst = add_zeroary_mutuals_sub lst env [] in
    if List.length newlst = 0 then
      ()
    else if (List.length newlst) = (List.length lst) then
      let msg = lst |> List.fold_left (fun s (evid, _) -> s ^ (EvalVarID.show_direct evid) ^ " ") "" in
      raise (EvalError("meaningless 0-ary mutual recursion; " ^ msg))
    else
      add_zeroary_mutuals newlst env


and add_zeroary_mutuals_sub (lst : (EvalVarID.t * abstract_tree) list) (env : environment) (acc : (EvalVarID.t * abstract_tree) list) =
  match lst with
  | []                      -> acc
  | (evid, astcont) :: tail ->
      begin
        try
          let valuecont = interpret env astcont in
            begin
              add_to_environment env evid (ref valuecont) ;
              add_zeroary_mutuals_sub tail env acc
            end
        with
        | EvalError(_) -> add_zeroary_mutuals_sub tail env ((evid, astcont) :: acc)
      end
