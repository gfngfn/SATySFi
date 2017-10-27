open Types

exception EvalError of string


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
let lex_horz_text (ctx : input_context) (s_utf8 : string) : HorzBox.horz_box list =
  let uchlst = InternalText.to_uchar_list (InternalText.of_utf8 s_utf8) in
    ConvertText.to_boxes ctx uchlst


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
    | TupleCons(LengthConstant(lenx), TupleCons(LengthConstant(leny), EndOfTuple)) -> (lenx, leny)
    | _ -> report_bug_evaluator ("interpret_point; " ^ (Display.string_of_ast valuept))


and interpret_paddings env astpads =
  let valuepads = interpret env astpads in
  match valuepads with
  | TupleCons(LengthConstant(lenL), TupleCons(LengthConstant(lenR), TupleCons(LengthConstant(lenT), TupleCons(LengthConstant(lenB), EndOfTuple)))) ->
      {
        HorzBox.paddingL = lenL;
        HorzBox.paddingR = lenR;
        HorzBox.paddingT = lenT;
        HorzBox.paddingB = lenB;
      }
  | _ -> report_bug_evaluator ("interpret_paddings; " ^ (Display.string_of_ast valuepads))


and interpret_path env pathcomplst cycleopt =
  let pathelemlst =
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
  in
  let closingopt =
    match cycleopt with
    | None -> None

    | Some(PathLineTo(())) -> Some(HorzBox.LineTo(()))

    | Some(PathCubicBezierTo(astpt1, astpt2, ())) ->
        let pt1 = interpret_point env astpt1 in
        let pt2 = interpret_point env astpt2 in
          Some(HorzBox.CubicBezierTo(pt1, pt2, ()))
  in
    (pathelemlst, closingopt)


and make_frame_deco env valuedeco =
  (fun (xpos, ypos) wid hgt dpt ->
    let astpos = TupleCons(LengthConstant(xpos), TupleCons(LengthConstant(ypos), EndOfTuple)) in
    let astwid = LengthConstant(wid) in
    let asthgt = LengthConstant(hgt) in
    let astdpt = LengthConstant(dpt) in
    let astret = reduce_beta_list env valuedeco [astpos; astwid; asthgt; astdpt] in
      let rec aux acc ast =
        match ast with
        | EndOfList                             -> List.rev acc
        | ListCons(GraphicsValue(pdfops), tail) -> aux (pdfops :: acc) tail
        | _                                     -> report_bug_evaluator ("make_frame_deco; "
                                                                         ^ (Display.string_of_ast ast))
      in
        List.concat (aux [] astret)
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

  | LengthDescription(flt, unitnm) ->
      let len =
        match unitnm with  (* temporary; ad-hoc handling of unit names *)
        | "pt"   -> HorzBox.Length.of_pdf_point flt
        | "cm"   -> HorzBox.Length.of_centimeter flt
        | "mm"   -> HorzBox.Length.of_millimeter flt
        | "inch" -> HorzBox.Length.of_inch flt
        | _      -> report_bug_evaluator "LengthDescription; unknown unit name"
      in
        LengthConstant(len)

  | LengthConstant(_) -> ast

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

  | Path(astpt0, pathcomplst, cycleopt) ->
      let pt0 = interpret_point env astpt0 in
      let (pathelemlst, closingopt) = interpret_path env pathcomplst cycleopt in
        PathValue(HorzBox.GeneralPath(pt0, pathelemlst, closingopt))

  | PathValue(_) -> ast

  | GraphicsContext(_) -> ast

  | GraphicsValue(_) -> ast

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
        | _                                     -> report_bug_evaluator ("HorzLex; " ^ (Display.string_of_ast ast1) ^ " ->* " ^ (Display.string_of_ast value1))
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
      let font_size = interpret_length env astsize in
        FontDesignation((font_abbrev, font_size))

  | BackendLineBreaking(astctx, astrow) ->
      let ctx = interpret_context env astctx in
      let hblst = interpret_horz_boxes env astrow in
      let imvblst = LineBreak.main ctx.paragraph_top ctx.paragraph_bottom ctx.paragraph_width ctx.leading hblst in
        Vert(imvblst)

  | BackendVertFrame(astctx, astvert) ->
      let ctx = interpret_context env astctx in
      let valuevert = interpret env astvert in
      let imvblst = normalize_box_col valuevert in
        Vert([
          HorzBox.ImVertTopMargin(true, ctx.paragraph_top);
          HorzBox.ImVertFrame(Primitives.frame_deco_S, Primitives.frame_deco_H,
                              Primitives.frame_deco_M, Primitives.frame_deco_T, ctx.paragraph_width, imvblst);
          HorzBox.ImVertBottomMargin(true, ctx.paragraph_bottom);
        ])

  | PrimitiveSetSpaceRatio(astratio, astctx) ->
      let ratio = interpret_float env astratio in
      let ctx = interpret_context env astctx in
        Context({ ctx with space_natural = ratio })

  | PrimitiveSetFont(astscript, astfont, astctx) ->
      let script = interpret_script env astscript in
      let font_info = interpret_font env astfont in
      let ctx = interpret_context env astctx in
      let font_scheme_new = ctx.font_scheme |> FontSchemeMap.add script font_info in
        Context({ ctx with font_scheme = font_scheme_new; })

  | PrimitiveGetFont(astscript, astctx) ->
      let script = interpret_script env astscript in
      let ctx = interpret_context env astctx in
      let font_info = get_font_info ctx script in
        FontDesignation(font_info)

  | PrimitiveSetDominantScript(astscript, astctx) ->
      let script = interpret_script env astscript in
      let ctx = interpret_context env astctx in
        Context({ ctx with dominant_script = script; })

  | PrimitiveSetTitle(asttitle, astctx) ->
      let valuetitle = interpret env asttitle in
      let ctx = interpret_context env astctx in
        Context({ ctx with title = valuetitle; })

  | PrimitiveGetTitle(astctx) ->
      let ctx = interpret_context env astctx in ctx.title

  | BackendFixedEmpty(astwid) ->
      let wid = interpret_length env astwid in
        Horz([HorzBox.HorzPure(HorzBox.PHFixedEmpty(wid))])

  | BackendOuterEmpty(astnat, astshrink, aststretch) ->
      let widnat = interpret_length env astnat in
      let widshrink = interpret_length env astshrink in
      let widstretch = interpret_length env aststretch in
        Horz([HorzBox.HorzPure(HorzBox.PHOuterEmpty(widnat, widshrink, widstretch))])

  | BackendFixedString(astfont, aststr) ->
      let font_info = interpret_font env astfont in
      let purestr = interpret_string env aststr in
        Horz([HorzBox.HorzPure(HorzBox.PHFixedString(font_info, InternalText.to_uchar_list (InternalText.of_utf8 purestr)))])

  | BackendOuterFrame(astpads, astdeco, astbr) ->
      let hblst = interpret_horz_boxes env astbr in
      let valuedeco = interpret env astdeco in
        Horz([HorzBox.HorzPure(HorzBox.PHOuterFrame(
          interpret_paddings env astpads,
          make_frame_deco env valuedeco (* Primitives.frame_deco_S *),
          hblst))])

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
      let () = PrintForDebug.evalE ("ContentOf(" ^ (EvalVarID.show_direct evid) ^ ")") in  (* for debug *)
      begin
        try
          let content = !(find_in_environment env evid) in
          let () = PrintForDebug.evalE ("  -> " ^ (Display.string_of_ast content)) in  (* for debug *)
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
      let () = PrintForDebug.evalE ("Apply(" ^ (Display.string_of_ast astf) ^ ", " ^ (Display.string_of_ast astl) ^ ")") in  (* for debug *)
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
      let () = PrintForDebug.evaluator ("%1 " ^ (Display.string_of_ast astf) ^ "\n") in  (* for debug *)
      let valuef =  interpret env
                      (LetIn(MutualLetCons(evidcls, clsnmast, EndOfMutualLet),
                        LetIn(MutualLetCons(evidid, idnmast, EndOfMutualLet), astf))) in
      begin
        PrintForDebug.evaluator ("%2 " ^ (Display.string_of_ast valuef) ^ "\n") ;   (* for debug *)
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
      let () = PrintForDebug.evalE ("Sequential(" ^ (Display.string_of_ast ast1) ^ ", " ^ (Display.string_of_ast ast2) ^ ")") in  (* for debug *)
      let value1 = interpret env ast1 in
      let () = PrintForDebug.evalE ("value1 = " ^ (Display.string_of_ast value1)) in  (* for debug *)
      let value2 = interpret env ast2 in
      let () = PrintForDebug.evalE ("value2 = " ^ (Display.string_of_ast value2)) in  (* for debug *)
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
      let num = interpret_int env astnum in StringConstant(string_of_int num)

  | PrimitiveFloat(ast1) ->
      let ic1 = interpret_int env ast1 in FloatConstant(float_of_int ic1)

  | PrimitiveSetLineWidth(astlen, astgctx) ->
      let len = interpret_length env astlen in
      let gctx = interpret_graphics_context env astgctx in
        GraphicsContext({ gctx with HorzBox.line_width = len })

  | PrimitiveSetLineDash(astlensopt, astgctx) ->
      let valuelensopt = interpret env astlensopt in
      let gctx = interpret_graphics_context env astgctx in
      let line_dash =
        match valuelensopt with
        | Constructor("None", UnitConstant) -> HorzBox.SolidLine
        | Constructor("Some", TupleCons(LengthConstant(len1), TupleCons(LengthConstant(len2), TupleCons(LengthConstant(len3), EndOfTuple)))) -> HorzBox.DashedLine(len1, len2, len3)
        | _ -> report_bug_evaluator ("PrimitiveSetLineDash; " ^ (Display.string_of_ast valuelensopt))
      in
        GraphicsContext({ gctx with HorzBox.line_dash = line_dash })

  | PrimitiveDrawStroke(astgctx, astpath) ->
      let gctx = interpret_graphics_context env astgctx in
      let path = interpret_path_value env astpath in
      let pdfops = Graphics.pdfops_of_graphics gctx HorzBox.DrawStroke path in
        GraphicsValue(pdfops)

  | PrimitiveDrawFill(astgctx, astpath) ->
      let gctx = interpret_graphics_context env astgctx in
      let path = interpret_path_value env astpath in
      let pdfops = Graphics.pdfops_of_graphics gctx HorzBox.DrawFillByEvenOdd path in
        GraphicsValue(pdfops)

  | PrimitiveSetStrokeColor(astcol, astgctx) ->
      let color = interpret_color env astcol in
      let gctx = interpret_graphics_context env astgctx in
        GraphicsContext({ gctx with HorzBox.stroke_color = color })

  | PrimitiveSetFillColor(astcol, astgctx) ->
      let color = interpret_color env astcol in
      let gctx = interpret_graphics_context env astgctx in
        GraphicsContext({ gctx with HorzBox.fill_color = color })

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

  | FloatPlus(ast1, ast2) ->
      let flt1 = interpret_float env ast1 in
      let flt2 = interpret_float env ast2 in
        FloatConstant(flt1 +. flt2)

  | FloatMinus(ast1, ast2) ->
      let flt1 = interpret_float env ast1 in
      let flt2 = interpret_float env ast2 in
        FloatConstant(flt1 -. flt2)

  | LengthPlus(ast1, ast2) ->
      let len1 = interpret_length env ast1 in
      let len2 = interpret_length env ast2 in
        LengthConstant(HorzBox.(len1 +% len2))

  | LengthMinus(ast1, ast2) ->
      let len1 = interpret_length env ast1 in
      let len2 = interpret_length env ast2 in
        LengthConstant(HorzBox.(len1 -% len2))

  | LengthTimes(ast1, ast2) ->
      let len1 = interpret_length env ast1 in
      let flt2 = interpret_float env ast2 in
        LengthConstant(HorzBox.(len1 *% flt2))


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


and interpret_script env ast : CharBasis.script =
  let value = interpret env ast in
    match value with
    | Constructor("HanIdeographic", UnitConstant) -> CharBasis.HanIdeographic
    | Constructor("Kana"          , UnitConstant) -> CharBasis.HiraganaOrKatakana
    | Constructor("Latin"         , UnitConstant) -> CharBasis.Latin
    | Constructor("Other"         , UnitConstant) -> CharBasis.Other
    | _ ->
        report_bug_evaluator ("interpret_script: not a script value; "
                              ^ (Display.string_of_ast ast)
                              ^ " ->* " ^ (Display.string_of_ast value))


and interpret_string (env : environment) (ast : abstract_tree) : string =
  let vs = interpret env ast in
    match vs with
    | StringEmpty       -> ""
    | StringConstant(s) -> s
    | _                 -> report_bug_evaluator ("interpret_string: not a StringEmpty nor a StringConstant; "
                                                 ^ (Display.string_of_ast ast)
                                                 ^ " ->* " ^ (Display.string_of_ast vs))


and interpret_path_value env ast : HorzBox.path =
  let value = interpret env ast in
    match value with
    | PathValue(path) -> path
    | _               -> report_bug_evaluator ("interpret_path_value: not a PathValue; "
                                               ^ (Display.string_of_ast ast)
                                               ^ " ->* " ^ (Display.string_of_ast value))


and interpret_context (env : environment) (ast : abstract_tree) : input_context =
  let value = interpret env ast in
    match value with
    | Context(ctx) -> ctx
    | _            -> report_bug_evaluator ("interpret_context: not a Context; "
                                            ^ (Display.string_of_ast ast)
                                            ^ " ->* " ^ (Display.string_of_ast value))


and interpret_graphics_context (env : environment) (ast : abstract_tree) : HorzBox.graphics_state =
  let value = interpret env ast in
    match value with
    | GraphicsContext(gctx) -> gctx
    | _                     -> report_bug_evaluator ("interpret_graphics_context: not a GraphicsContext; "
                                                     ^ (Display.string_of_ast ast)
                                                     ^ " ->* " ^ (Display.string_of_ast value))


and interpret_color env ast : HorzBox.color =
  let value = interpret env ast in
    match value with
    | TupleCons(FloatConstant(fltR), TupleCons(FloatConstant(fltG), TupleCons(FloatConstant(fltB), EndOfTuple))) ->
        HorzBox.DeviceRGB(fltR, fltG, fltB)
    | _ -> report_bug_evaluator ("interpret_color; " ^ (Display.string_of_ast value))
      (* temporary; colors of other color spaces than DeviceRGB should be able to be specified *)


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
    | _                   -> report_bug_evaluator ("interpret_int: not a IntegerConstant; "
                                                   ^ (Display.string_of_ast ast)
                                                   ^ " ->* " ^ (Display.string_of_ast vi))


and interpret_float (env : environment) (ast : abstract_tree) : float =
  let vf = interpret env ast in
    match vf with
    | FloatConstant(nc) -> nc
    | _                 -> report_bug_evaluator ("interpret_float: not a FloatConstant; "
                                                 ^ (Display.string_of_ast ast)
                                                 ^ " ->* " ^ (Display.string_of_ast vf))


and interpret_length (env : environment) (ast : abstract_tree) : HorzBox.length =
  let vl = interpret env ast in
    match vl with
    | LengthConstant(lc) -> lc
    | _                  -> report_bug_evaluator ("interpret_float: not a FloatConstant; "
                                                  ^ (Display.string_of_ast ast)
                                                  ^ " ->* " ^ (Display.string_of_ast vl))


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
  let () = PrintForDebug.evalE ("add_mutuals_to_environment") in  (* for debug *)
      add_zeroary_mutuals lst env


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
