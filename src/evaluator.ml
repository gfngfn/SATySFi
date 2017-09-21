open Types

exception EvalError of string


let print_for_debug_evaluator msg =
(*
  print_string msg;
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


(* temporary; should be variable *)
let lex_horz_text (ctx : input_context) (s : string) : HorzBox.horz_box list =
  let (_, font_size) = ctx.font_info in 
  let space_natural =
      HorzBox.(font_size *% ctx.space_natural)
  in
  let fullsplitlst = Str.full_split (Str.regexp "[ \t\r\n]+") s in
    fullsplitlst |> List.map (function
      | Str.Text(s)  -> HorzBox.HorzPure(HorzBox.PHFixedString(ctx.font_info, InternalText.of_utf_8 s))
      | Str.Delim(_) -> HorzBox.HorzDiscretionary(100, Some(HorzBox.PHOuterEmpty(space_natural, ctx.space_shrink, ctx.space_stretch)), None, None)
    )


let rec reduce_beta envf evid valuel astdef =
  let envnew = copy_environment envf in
    begin
      add_to_environment envnew evid (ref valuel);
      interpret envnew astdef
    end


and reduce_beta_list env valuef astarglst =
  match astarglst with
  | []                   -> valuef
  | astarg :: astargtail ->
      begin
        match valuef with
        | FuncWithEnvironment(evid, astdef, envf) ->
            let valuearg = interpret env astarg in
            let valuefnew = reduce_beta envf evid valuearg astdef in
            reduce_beta_list env valuefnew astargtail

        | _ -> report_bug_evaluator "reduce_beta_list"
      end


and interpret_horz_boxes env astrow =
  let valuerow = interpret env astrow in
  let rec aux value =
    match value with
    | Horz(hblst)                -> hblst
    | HorzConcat(value1, value2) -> List.append (aux value1) (aux value2)
    | _                          -> report_bug_evaluator ("interpret_horz_boxes; " ^ (Display.string_of_ast valuerow))
  in
    aux valuerow

and normalize_box_col valuecol =
  let iter = normalize_box_col in
    match valuecol with
    | Vert(imvblst)              -> imvblst
    | VertConcat(value1, value2) -> List.append (iter value1) (iter value2)
    | _                          -> report_bug_evaluator ("normalize_box_col; " ^ (Display.string_of_ast valuecol))


and interpret_point env astpt =
  let valuept = interpret env astpt in
    match valuept with
    | TupleCons(FloatConstant(x), TupleCons(FloatConstant(y), EndOfTuple))
        -> (HorzBox.Length.of_pdf_point x, HorzBox.Length.of_pdf_point y)
             (* temporary; should be modified to "native" length value *)
    | _ -> report_bug_evaluator ("interpret_point; " ^ (Display.string_of_ast valuept))


and interpret_path env pathcomplst =
  pathcomplst |> List.map (function
    | PathLineTo(astpt) ->
        let pt = interpret_point env astpt in
          HorzBox.LineTo(pt)

    | PathCubicBezierTo(astpt1, astpt2, astpt) ->
        let pt1 = interpret_point env astpt1 in
        let pt2 = interpret_point env astpt2 in
        let pt = interpret_point env astpt in
          HorzBox.CubicBezierTo(pt1, pt2, pt)
  )


and make_frame_deco env valuedeco =
  (fun (xpos, ypos) wid hgt dpt ->
    let flt = HorzBox.Length.to_pdf_point in
    let astpos = TupleCons(FloatConstant(flt xpos), TupleCons(FloatConstant(flt ypos), EndOfTuple)) in
    let astwid = FloatConstant(flt wid) in
    let asthgt = FloatConstant(flt hgt) in
    let astdpt = FloatConstant(flt dpt) in
    let astret = reduce_beta_list env valuedeco [astpos; astwid; asthgt; astdpt] in
      let rec aux acc ast =
        match ast with
        | EndOfList                       -> List.rev acc
        | ListCons(PathValue(path), tail) -> aux (path :: acc) tail
        | _                               -> report_bug_evaluator ("make_frame_deco; "
                                                                   ^ (Display.string_of_ast ast))
      in
        aux [] astret
  )


and interpret env ast =
  match ast with

(* ---- basic value ---- *)

  | StringEmpty                           -> ast
  | IntegerConstant(_)                    -> ast
  | FloatConstant(_)                      -> ast
  | StringConstant(_)                     -> ast
  | BooleanConstant(_)                    -> ast
  | UnitConstant                          -> ast
  | EvaluatedEnvironment(_)               -> ast
  | FuncWithEnvironment(_, _, _)          -> ast

  | InputHorz(ihlst)                      -> InputHorzWithEnvironment(ihlst, env)  (* -- lazy evaluation -- *)

  | InputHorzWithEnvironment(_, _)        -> ast

  | InputVert(ivlst)                      -> InputVertWithEnvironment(ivlst, env)  (* -- lazy evaluation -- *)

  | InputVertWithEnvironment(_, _)        -> ast

  | Concat(ast1, ast2) ->
      let value1 = interpret env ast1 in
      let value2 = interpret env ast2 in
        begin
          match (value1, value2) with
          | (StringEmpty, _) -> value2
          | (_, StringEmpty) -> value1
          | (_, _)           -> Concat(value1, value2)
        end

(* ---- values for backend ---- *)

  | Path(astpt0, pathcomplst) ->
      let pt0 = interpret_point env astpt0 in
      let pathelemlst = interpret_path env pathcomplst in
        PathValue(HorzBox.GeneralPath(pt0, pathelemlst))

  | PathCycle -> ast

  | PathValue(_) -> ast

  | FontDesignation(_) -> ast

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
        Vert(normalize_box_col (VertConcat(value1, value2)))  (* ad hoc *)
(*
      begin
        match (value1, value2) with
        | (Vert([]), _) -> value2
        | (_, Vert([])) -> value1
        | (_, _)        -> VertConcat(value1, value2)
      end
*)

  | Context(ctx) -> Context(ctx)  (* temporary; need detailed implementation *)

  | LambdaVert(evid, astdef) -> LambdaVertWithEnvironment(evid, astdef, env)

  | LambdaVertWithEnvironment(_, _, _) -> ast

  | LambdaVertDetailed(evid, astdef) -> LambdaVertDetailedWithEnv(evid, astdef, env)      

  | LambdaVertDetailedWithEnv(_, _, _) -> ast

  | LambdaHorz(evid, astdef) -> LambdaHorzWithEnvironment(evid, astdef, env)      

  | LambdaHorzWithEnvironment(_, _, _) -> ast

  | HorzLex(astctx, ast1) ->
      let ctx = interpret_context env astctx in
      let value1 = interpret env ast1 in
      begin
        match value1 with
        | InputHorzWithEnvironment(ihlst, envi) -> interpret_input_horz envi ctx ihlst
        | _                                     -> report_bug_evaluator "HorzLex"
      end

  | VertLex(astctx, ast1) ->
      let ctx = interpret_context env astctx in
      let value1 = interpret env ast1 in
      begin
        match value1 with
        | InputVertWithEnvironment(ivlst, envi) -> interpret_input_vert envi ctx ivlst
        | _                                     -> report_bug_evaluator "VertLex"
      end

  | BackendFont(astabbrev, astsize) ->
      let font_abbrev = interpret_string env astabbrev in
      let font_size_raw = interpret_int env astsize in  (* temporary; should deal with lengths directly *)
      let font_size = HorzBox.Length.of_pdf_point (float_of_int font_size_raw) in  (* temporary; should deal with lengths directly *)
        FontDesignation((font_abbrev, font_size))

  | BackendLineBreaking(astctx, astrow) ->
      let ctx = interpret_context env astctx in
      let hblst = interpret_horz_boxes env astrow in
      let imvblst = LineBreak.main ctx.paragraph_width ctx.leading hblst in
        Vert(imvblst)

  | PrimitiveSetSpaceRatio(astratio, astctx) ->
      let ratio = interpret_float env astratio in
      let ctx = interpret_context env astctx in
        Context({ ctx with space_natural = ratio })

  | PrimitiveSetFont(astfont, astctx) ->
      let font_info = interpret_font env astfont in
      let ctx = interpret_context env astctx in
        Context({ ctx with font_info = font_info; })

  | PrimitiveGetFont(astctx) ->
      let ctx = interpret_context env astctx in FontDesignation(ctx.font_info)

  | PrimitiveSetTitle(asttitle, astctx) ->
      let valuetitle = interpret env asttitle in
      let ctx = interpret_context env astctx in
        Context({ ctx with title = valuetitle; })

  | PrimitiveGetTitle(astctx) ->
      let ctx = interpret_context env astctx in ctx.title

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

  | BackendFixedString(astfont, aststr) ->
      let font_info =
        match astfont with
        | FontDesignation(font_info) -> font_info
        | _ -> report_bug_evaluator "BackendFixedString; not a font value"
      in
      let purestr = interpret_string env aststr in
        Horz([HorzBox.HorzPure(HorzBox.PHFixedString(font_info, InternalText.of_utf_8 purestr))])

  | BackendOuterFrame(astdeco, astbr) ->
      let hblst = interpret_horz_boxes env astbr in
      let valuedeco = interpret env astdeco in
        Horz([HorzBox.HorzPure(HorzBox.PHOuterFrame(
          Primitives.default_paddings,
          make_frame_deco env valuedeco (* Primitives.frame_deco_S *),
          hblst))])
            (* temporary; paddings should be variable *)

  | BackendOuterFrameBreakable(astbr) ->
      let hblst = interpret_horz_boxes env astbr in
        Horz([HorzBox.HorzFrameBreakable(
          Primitives.default_paddings, HorzBox.Length.zero, HorzBox.Length.zero,
          Primitives.frame_deco_S, Primitives.frame_deco_H,
          Primitives.frame_deco_M, Primitives.frame_deco_T,
          hblst
        )])

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
      let () = print_for_debug_evaluator ("$$ ContentOf " ^ (EvalVarID.show_direct evid) ^ "\n") in  (* for debug *)
      begin
        try
          let content = !(find_in_environment env evid) in
            content
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
              reduce_beta envf evid valuel astdef

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
(*
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
*)
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
(*
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
*)
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
      let str1 = interpret_string env ast1 in
      let str2 = interpret_string env ast2 in
        BooleanConstant(String.equal str1 str2)


  | PrimitiveStringSub(aststr, astpos, astwid) ->
      let str = interpret_string env aststr in
      let pos = interpret_int env astpos in
      let wid = interpret_int env astwid in
        let resstr =
          try String.sub str pos wid with
          | Invalid_argument(s) -> raise (EvalError("illegal index for 'string-sub'"))
        in
          StringConstant(resstr)

  | PrimitiveStringLength(aststr) ->
      let str = interpret_string env aststr in
        IntegerConstant(String.length str)
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
        IntegerConstant(numl * numr)

  | Divides(astl, astr) ->
      let numl = interpret_int env astl in
      let numr = interpret_int env astr in
        begin
          try IntegerConstant(numl / numr) with
          | Division_by_zero -> raise (EvalError("division by zero"))
        end

  | Mod(astl, astr) ->
      let numl = interpret_int env astl in
      let numr = interpret_int env astr in
        begin
          try IntegerConstant(numl mod numr) with
          | Division_by_zero -> raise (EvalError("division by zero"))
        end

  | Plus(astl, astr) ->
      let numl = interpret_int env astl in
      let numr = interpret_int env astr in
        IntegerConstant(numl + numr)

  | Minus(astl, astr) ->
      let numl = interpret_int env astl in
      let numr = interpret_int env astr in
        IntegerConstant(numl - numr)

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


and interpret_input_vert (env : environment) (ctx : input_context) (ivlst : input_vert_element list) : abstract_tree =
  let (ctxfinal, imvblstacc) =
    ivlst |> List.fold_left (fun (ctx, lstacc) iv ->
      match iv with
      | InputVertEmbedded(astcmd, astarglst) ->
          let valuecmd = interpret env astcmd in
          begin
            match valuecmd with
            | LambdaVertWithEnvironment(evid, astdef, envf) ->
                let valuedef = reduce_beta envf evid (Context(ctx)) astdef in
                let valueret = reduce_beta_list env valuedef astarglst in
                begin
                  match valueret with
                  | Vert(imvblst) -> (ctx, imvblst :: lstacc)
                  | _             -> report_bug_evaluator "interpret_input_vert; 1"
                end

            | LambdaVertDetailedWithEnv(evid, astdef, envf) ->
                let valuedef = reduce_beta envf evid (Context(ctx)) astdef in
                let valueret = reduce_beta_list env valuedef astarglst in
                begin
                  match valueret with
                  | TupleCons(Context(ctxnext), TupleCons(Vert(imvblst), EndOfTuple)) -> (ctxnext, imvblst :: lstacc)
                  | _                                                                 -> report_bug_evaluator "interpret_input_vert; 2"
                end

            | _ -> report_bug_evaluator "interpret_input_vert; other than LambdaVertWithEnvironment or LambdaVertDetailedWithEnv"
          end
    ) (ctx, [])
  in
  let imvblst = imvblstacc |> List.rev |> List.concat in
    Vert(imvblst)


and interpret_input_horz (env : environment) (ctx : input_context) (ihlst : input_horz_element list) : abstract_tree =
  let normalize ihlst =
    ihlst |> List.fold_left (fun acc ih ->
      match ih with
      | InputHorzEmbedded(_, _) -> (ih :: acc)
      | InputHorzText(s2) ->
          match acc with
          | InputHorzText(s1) :: acctail -> (InputHorzText(s1 ^ s2) :: acctail)
          | _                            -> (ih :: acc)
    ) [] |> List.rev
  in
  let ihlstnml = normalize ihlst in
  let hblstacc =
    ihlstnml |> List.fold_left (fun lstacc ih ->
      match ih with
      | InputHorzEmbedded(astcmd, astarglst) ->
          let valuecmd = interpret env astcmd in
          begin
            match valuecmd with
            | LambdaHorzWithEnvironment(evid, astdef, envf) ->
                let valuedef = reduce_beta envf evid (Context(ctx)) astdef in
                let valueret = reduce_beta_list env valuedef astarglst in
                begin
                  match valueret with
                  | Horz(hblst) -> hblst :: lstacc
                  | _           -> report_bug_evaluator "interpret_input_horz; other than Horz(_)"
                end

            | _ -> report_bug_evaluator "interpret_input_horz; other than LambdaHorzWithEnvironment(_, _, _)"
          end

      | InputHorzText(s) -> (lex_horz_text ctx s) :: lstacc
    ) []
  in
  let hblst = hblstacc |> List.rev |> List.concat in
  Horz(hblst)


and interpret_string (env : environment) (ast : abstract_tree) : string =
  let vs = interpret env ast in
    match vs with
    | StringEmpty       -> ""
    | StringConstant(s) -> s
    | _                 -> report_bug_evaluator ("interpret_string: not a StringEmpty nor a StringConstant; "
                                                 ^ (Display.string_of_ast ast)
                                                 ^ " ->* " ^ (Display.string_of_ast vs))


and interpret_context (env : environment) (ast : abstract_tree) : input_context =
  let value = interpret env ast in
    match value with
    | Context(ctx) -> ctx
    | _            -> report_bug_evaluator ("interpret_context: not a Context; "
                                            ^ (Display.string_of_ast ast)
                                            ^ " ->* " ^ (Display.string_of_ast value))


and interpret_font (env : environment) (ast : abstract_tree) : HorzBox.font_info =
  let value = interpret env ast in
    match value with
    | FontDesignation(font_info) -> font_info
    | _                          -> report_bug_evaluator ("interpret_font: not a FontDesignation; "
                                                          ^ (Display.string_of_ast ast)
                                                          ^ " ->* " ^ (Display.string_of_ast value))


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
    | IntegerConstant(nc) -> nc
    | other               -> report_bug_evaluator ("interpret_int: not a IntegerConstant; "
                                                   ^ (Display.string_of_ast ast)
                                                   ^ " ->* " ^ (Display.string_of_ast vi))


and interpret_float (env : environment) (ast : abstract_tree) : float =
  let vf = interpret env ast in
    match vf with
    | FloatConstant(nc) -> nc
    | other             -> report_bug_evaluator ("interpret_float: not a FloatConstant; "
                                                 ^ (Display.string_of_ast ast)
                                                 ^ " ->* " ^ (Display.string_of_ast vf))


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
  | (PIntegerConstant(pnc), IntegerConstant(nc)) -> pnc = nc
  | (PBooleanConstant(pbc), BooleanConstant(bc)) -> pbc = bc
  | (PStringConstant(ast1), ast2)                ->
      let str1 = interpret_string env ast1 in
      let str2 = interpret_string env ast2 in
        String.equal str1 str2

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
