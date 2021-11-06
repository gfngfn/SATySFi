%{
  open Types


  type literal_reading_state = Normal | ReadingSpace

  type 'a range_kind =
    | Tok    of Range.t
    | Ranged of (Range.t * 'a)


  let make_range_from_list (rangeds : (Range.t * 'a) list) =
    match (rangeds, List.rev rangeds) with
    | ([], [])                         -> Range.dummy "empty"
    | ((rngL, _) :: _, (rngR, _) :: _) -> Range.unite rngL rngR
    | _                                -> assert false


  let make_range left right =
    let extract x =
      match x with
      | Tok(rng)          -> rng
      | Ranged((rng, _))  -> rng
    in
    Range.unite (extract left) (extract right)


  let make_list (utasts : untyped_abstract_tree list) : untyped_abstract_tree =
    List.fold_right (fun utast_elem utast_tail ->
      (Range.dummy "expr-list-cons", UTListCons(utast_elem, utast_tail))
    ) utasts (Range.dummy "expr-list-nil", UTEndOfList)


  let curry_lambda_abstraction (param_units : untyped_parameter_unit list) (utast : untyped_abstract_tree) : untyped_abstract_tree =
    let rng = Range.dummy "curry_lambda_abstraction" in
    utast |> List.fold_right (fun param_unit utast ->
      let UTParameterUnit(opts, utpat) = param_unit in
      (rng, UTFunction(opts, utpat, utast))
    ) param_units


  (* TODO (enhance): more efficient implementation *)
  let rec omit_pre_spaces str =
    let len = String.length str in
    if len = 0 then
      ""
    else
      match String.sub str 0 1 with
      | " " -> omit_pre_spaces (String.sub str 1 (len - 1))
      | _   -> str


  (* TODO (enhance): more efficient implementation *)
  let rec omit_post_spaces str =
    let len = String.length str in
    if len = 0 then
      ""
    else
      match String.sub str (len - 1) 1 with
      | " "  -> omit_post_spaces (String.sub str 0 (len - 1))
      | "\n" -> String.sub str 0 (len - 1)
      | _    -> str


  let rec min_indent_space_sub (str_ltrl : string) (index : int) (lrstate : literal_reading_state) (spnum : int) (minspnum : int) =
    if index >= String.length str_ltrl then
      minspnum
    else
      match lrstate with
      | Normal ->
          begin
            match str_ltrl.[index] with
            | '\n' -> min_indent_space_sub str_ltrl (index + 1) ReadingSpace 0 minspnum
            | _    -> min_indent_space_sub str_ltrl (index + 1) Normal 0 minspnum
          end

      | ReadingSpace ->
          begin
            match str_ltrl.[index] with
            | ' '  -> min_indent_space_sub str_ltrl (index + 1) ReadingSpace (spnum + 1) minspnum
            | '\n' -> min_indent_space_sub str_ltrl (index + 1) ReadingSpace 0 minspnum
            | _    -> min_indent_space_sub str_ltrl (index + 1) Normal 0 (if spnum < minspnum then spnum else minspnum)
          end
            (* Does not take space-only line into account. *)


  let min_indent_space (str_ltrl : string) =
    min_indent_space_sub str_ltrl 0 ReadingSpace 0 (String.length str_ltrl)


  let rec shave_indent_sub str_ltrl minspnum index str_constr lrstate spnum =
    if index >= String.length str_ltrl then
      str_constr
    else
      match lrstate with
      | Normal ->
          begin
            match str_ltrl.[index] with
            | '\n' -> shave_indent_sub str_ltrl minspnum (index + 1) (str_constr ^ "\n") ReadingSpace 0
            | ch   -> shave_indent_sub str_ltrl minspnum (index + 1) (str_constr ^ (String.make 1 ch)) Normal 0
          end

      | ReadingSpace ->
          begin
            match str_ltrl.[index] with
            | ' ' ->
                if spnum < minspnum then
                  shave_indent_sub str_ltrl minspnum (index + 1) str_constr ReadingSpace (spnum + 1)
                else
                  shave_indent_sub str_ltrl minspnum (index + 1) (str_constr ^ " ") ReadingSpace (spnum + 1)

            | '\n' ->
                shave_indent_sub str_ltrl minspnum (index + 1) (str_constr ^ "\n") ReadingSpace 0

            | ch ->
                shave_indent_sub str_ltrl minspnum (index + 1) (str_constr ^ (String.make 1 ch)) Normal 0
          end


  let shave_indent str_ltrl minspnum =
    shave_indent_sub str_ltrl minspnum 0 "" Normal 0


  let omit_spaces (omit_pre : bool) (omit_post : bool) (str_literal_raw : string) : string =
    let str_literal =
      let s1 = if omit_pre then omit_pre_spaces str_literal_raw else str_literal_raw in
      let s2 = if omit_post then omit_post_spaces s1 else s1 in
      s2
    in
    let min_indent = min_indent_space str_literal in
    let str_shaved = shave_indent str_literal min_indent in
    let len_shaved = String.length str_shaved in
    if len_shaved >= 1 && str_shaved.[len_shaved - 1] = '\n' then
      let str_no_last_break = String.sub str_shaved 0 (len_shaved - 1) in
      str_no_last_break
    else
      str_shaved


  let extract_main (_, x) = x


  let binary_operator (utastL : untyped_abstract_tree) (op : Range.t * var_name) (utastR : untyped_abstract_tree) : untyped_abstract_tree =
    let (rng_op, _) = op in
    let rng = make_range (Ranged utastL) (Ranged utastR) in
    (rng, UTApply([], (Range.dummy "binary_operator", UTApply([], (rng_op, UTContentOf([], op)), utastL)), utastR))


  let make_uminus op = function
    | (_, UTFloatConstant(_)) as arg ->
        binary_operator
          (Range.dummy "zero-of-unary-minus", UTFloatConstant(0.0))
          (op, "-.")
          arg

    | (_, UTLengthDescription (_, unit)) as arg ->
        binary_operator
          (Range.dummy "zero-of-unary-minus", UTLengthDescription(0.0, unit))
          (op, "-'")
          arg

    | arg ->
        binary_operator
          (Range.dummy "zero-of-unary-minus", UTIntegerConstant(0))
          (op, "-")
          arg


  let make_standard left right main =
    let rng = make_range left right in (rng, main)


  let base_kind_o =
    MKindName((Range.dummy "base_kind_o", "o"))
      (* TODO (enhance): fix such an ad-hoc insertion of kinds *)


  let decl_type_transparent (uttybinds : untyped_type_binding list) : untyped_declaration =
    let rng = Range.dummy "decl_type_transparent" in
    let decls : untyped_declaration list =
      uttybinds |> List.map (fun (tyident, tyvars, _syn_or_vnt) ->
        let mnbkddoms = tyvars |> List.map (fun _ -> base_kind_o) in
        let mnkd = MKind(mnbkddoms, base_kind_o) in
        UTDeclTypeOpaque(tyident, mnkd)
      )
    in
    UTDeclInclude((rng, UTSigWith((rng, UTSigDecls(decls)), [], uttybinds)))


  let rec insert_last (resitmzlst : untyped_itemize list) (itmz : untyped_itemize) (i : int) (depth : int) (utast : untyped_abstract_tree) : untyped_itemize =
    match itmz with
    | UTItem(uta, []) ->
        if i < depth then assert false else UTItem(uta, [UTItem(utast, [])])
    | UTItem(uta, hditmz :: []) ->
        if i < depth then
          UTItem(uta, resitmzlst @ [insert_last [] hditmz (i + 1) depth utast])
        else
          UTItem(uta, resitmzlst @ [hditmz] @ [UTItem(utast, [])])
    | UTItem(uta, hditmz :: tlitmzlst) ->
        insert_last (resitmzlst @ [hditmz]) (UTItem(uta, tlitmzlst)) i depth utast


  let rec make_list_to_itemize_sub (resitmz : untyped_itemize) (lst : (Range.t * int * untyped_abstract_tree) list) (crrntdp : int) =
    match lst with
    | [] ->
        resitmz

    | (rng, depth, utast) :: tail ->
        if depth <= crrntdp + 1 then
          let newresitmz = insert_last [] resitmz 1 depth utast in
          make_list_to_itemize_sub newresitmz tail depth
        else
          raise (ParseErrorDetail(rng, "syntax error: illegal item depth "
            ^ (string_of_int depth) ^ " after " ^ (string_of_int crrntdp)))


  let make_list_to_itemize (lst : (Range.t * int * untyped_abstract_tree) list) =
    let contents = make_list_to_itemize_sub (UTItem((Range.dummy "itemize2", UTInputHorz([])), [])) lst 0 in
    (Range.dummy "itemize1", UTItemize(contents))


  let primes (n : int) : Uchar.t list =
    List.init n (fun _ -> Uchar.of_int 0x2032)


  let make_sup ~range ?prime ?sup base =
    let make_primes r n =
      (r, UTMChars(primes n))
    in
    match (prime, sup) with
    | (None, None) ->
        (range, snd base)
    | (None, Some((b, p))) ->
        (range, UTMSuperScript(base, b, p))
    | (Some((r, n)), None) ->
        (range, UTMSuperScript(base, true, make_primes r n))
    | (Some((r, n)), Some((b, p))) ->
        let ps = make_standard (Tok r) (Ranged p) (UTMList [make_primes r n; p]) in
        (range, UTMSuperScript(base, b, ps))

  let make_sub ~range ~sub:(b, s) base =
    (range, UTMSubScript(base, b, s))
%}

%token<Range.t>
  AND AS BLOCK ELSE END FALSE FUN
  IF IN INCLUDE INLINE LET MOD MATCH MATH MODULE MUTABLE OF OPEN
  REC SIG SIGNATURE STRUCT THEN TRUE TYPE VAL WITH

%token<Range.t> BAR WILDCARD COLON ARROW REVERSED_ARROW SEMICOLON COMMA CONS ACCESS QUESTION COERCE

%token<Range.t>
  L_PAREN R_PAREN L_SQUARE R_SQUARE L_RECORD R_RECORD
  L_BLOCK_TEXT R_BLOCK_TEXT L_INLINE_TEXT R_INLINE_TEXT L_MATH_TEXT R_MATH_TEXT

%token<Range.t> EXACT_MINUS EXACT_TIMES EXACT_AMP EXACT_TILDE EXACT_EQ

%token<Range.t * Types.var_name>
  BINOP_TIMES BINOP_DIVIDES BINOP_PLUS BINOP_MINUS BINOP_HAT BINOP_AMP BINOP_BAR BINOP_GT BINOP_LT BINOP_EQ

%token<Range.t * Types.var_name> UNOP_EXCLAM

%token<Range.t * Types.var_name> LOWER
%token<Range.t * Types.constructor_name> UPPER
%token<Range.t * (Types.module_name Types.ranged) list * Types.var_name Types.ranged> LONG_LOWER
%token<Range.t * (Types.module_name Types.ranged) list * Types.constructor_name Types.ranged> LONG_UPPER

%token<Range.t * Types.ctrlseq_name> BACKSLASH_CMD PLUS_CMD

%token<Range.t * (Types.module_name Types.ranged) list * Types.ctrlseq_name Types.ranged>
  LONG_BACKSLASH_CMD LONG_PLUS_CMD

%token<Range.t * (Types.module_name Types.ranged) list * Types.var_name Types.ranged>
  VAR_IN_TEXT

%token<Range.t * Types.type_variable_name> TYPEVAR
%token<Range.t * Types.row_variable_name> ROWVAR

%token<Range.t * int> INT
%token<Range.t * float> FLOAT
%token<Range.t * float * Types.length_unit_name> LENGTH
%token<Range.t * string> CHAR
%token<Range.t * string * bool * bool> STRING
%token<Range.t * Types.input_position * string> POSITIONED_STRING

%token<Range.t> SPACE BREAK
%token<Range.t * string> MATHCHARS
%token<Range.t * int> PRIMES
%token<Range.t> SUBSCRIPT SUPERSCRIPT
%token<Range.t * int> ITEM

%token <Range.t * string> HEADER_REQUIRE HEADER_IMPORT
%token <Range.t> HEADER_STAGE0 HEADER_STAGE1 HEADER_PERSISTENT0

%token <Range.t * Types.ctrlseq_name> HORZMACRO
%token <Range.t * Types.ctrlseq_name> VERTMACRO

%token EOI

%left  BINOP_BAR
%left  BINOP_AMP
%right BINOP_EQ BINOP_GT BINOP_LT
%right BINOP_HAT CONS
%right BINOP_PLUS
%left  BINOP_MINUS EXACT_MINUS
%right BINOP_TIMES EXACT_TIMES BINOP_DIVIDES MOD

%start main
%type<Types.stage * Types.header_element list * Types.untyped_source_file> main
%type<Types.untyped_module> modexpr
%type<Types.module_name_chain Types.ranged> mod_chain
%type<Types.untyped_binding> bind
%type<Types.untyped_let_binding list> bind_value_rec
%type<Types.untyped_let_binding> bind_value_nonrec
%type<Types.untyped_let_binding> bind_inline
%type<Types.untyped_let_binding> bind_block
%type<Types.untyped_type_binding list> bind_type
%type<Types.untyped_type_binding> bind_type_single
%type<Types.untyped_declaration> decl
%type<Types.manual_kind> kind
%type<Types.manual_row_base_kind> kind_row
%type<Types.type_variable_name Types.ranged> tyquant
%type<Types.row_variable_name ranged * Types.manual_row_base_kind> rowquant
%type<Types.manual_type> typ_app
%type<Types.manual_type> typ_bot
%type<Types.untyped_abstract_tree> expr
%type<Types.untyped_abstract_tree> expr_op
%type<Types.untyped_abstract_tree> expr_app
%type<Types.untyped_abstract_tree> expr_bot
%type<Types.untyped_pattern_branch> branch
%type<Types.var_name Types.ranged> binop
%type<Types.untyped_pattern_tree> pattern
%type<Types.untyped_pattern_tree> pattern_bot
%type<Types.untyped_abstract_tree> inline
%type<Types.untyped_abstract_tree> inline_single
%type<Types.untyped_abstract_tree> block
%type<Types.untyped_input_vert_element> block_elem
%type<Range.t * Types.untyped_command_argument list> cmd_args_text
%type<Types.untyped_command_argument> cmd_arg_text
%type<Types.untyped_command_argument> cmd_arg_expr
%type<bool * Types.untyped_math> math_group
%type<Types.untyped_math> math_bot
%type<Range.t * (Types.module_name Types.ranged) list * Types.ctrlseq_name Types.ranged> backslash_cmd
%type<Range.t * (Types.module_name Types.ranged) list * Types.ctrlseq_name Types.ranged> plus_cmd


%%

optterm_list(sep, X):
  |   { [] }
  | x=X; sep?
      { [x] }
  | x=X; sep; xs=optterm_nonempty_list(sep, X)
      { x :: xs }
;
optterm_nonempty_list(sep, X):
  | x=X; sep?
      { [x] }
  | x=X; sep; xs=optterm_nonempty_list(sep, X)
      { x :: xs }
;
%inline bound_identifier:
  | ident=LOWER
      { ident }
  | L_PAREN; ident=binop; R_PAREN
      { ident }
;
main:
  | stage=stage; header=list(headerelem); lib=main_lib; EOI
      { (stage, header, UTLibraryFile(lib)) }
  | stage=stage; header=list(headerelem); utast=expr_app; EOI
      { (stage, header, UTDocumentFile(utast)) }
;
main_lib:
  | MODULE; modident=UPPER; EXACT_EQ; STRUCT; utbinds=list(bind); END
      { (modident, utbinds) }
;
stage:
  |                    { Stage1 }
  | HEADER_STAGE0      { Stage0 }
  | HEADER_STAGE1      { Stage1 }
  | HEADER_PERSISTENT0 { Persistent0 }
;
headerelem:
  | content=HEADER_REQUIRE { let (_, s) = content in HeaderRequire(s) }
  | content=HEADER_IMPORT  { let (_, s) = content in HeaderImport(s) }
;
modexpr:
  | tokL=FUN; L_PAREN; modident=UPPER; COLON; utsig=sigexpr; R_PAREN; ARROW; utmod=modexpr
      { make_standard (Tok tokL) (Ranged utmod) (UTModFunctor(modident, utsig, utmod)) }
  | modident=UPPER; COERCE; utsig=sigexpr
      { make_standard (Ranged modident) (Ranged utsig) (UTModCoerce(modident, utsig)) }
  | utmod=modexpr_app
      { utmod }
;
modexpr_app:
  | rmodchain1=mod_chain; rmodchain2=mod_chain
      {
        let (rng1, modchain1) = rmodchain1 in
        let (rng2, modchain2) = rmodchain2 in
        make_standard (Tok rng1) (Tok rng2) (UTModApply(modchain1, modchain2))
      }
  | utmod=modexpr_bot
      { utmod }
;
modexpr_bot:
  | rmodchain=mod_chain
      { let (rng, modchain) = rmodchain in (rng, UTModVar(modchain)) }
  | tokL=STRUCT; utbinds=list(bind); tokR=END
      { make_standard (Tok tokL) (Tok tokR) (UTModBinds(utbinds)) }
;
mod_chain:
  | modident=UPPER
      { let (rng, modnm) = modident in (rng, ((rng, modnm), [])) }
  | modpath=LONG_UPPER
      {
        let (rng, modidents, modident0) = modpath in
        match modidents with
        | []                          -> assert false
        | modident1 :: modidents_rest -> (rng, (modident1, List.append modidents_rest [ modident0 ]))
      }
;
bind:
  | tokL=VAL; valbind=bind_value
      { (tokL, UTBindValue(valbind)) }
  | tokL=TYPE; uttypebind=bind_type
      { (tokL, UTBindType(uttypebind)) }
  | tokL=MODULE; modident=UPPER; utsig_opt=option(sig_annot); EXACT_EQ; utmod=modexpr
      { (tokL, UTBindModule(modident, utsig_opt, utmod)) }
  | tokL=SIGNATURE; sigident=UPPER; EXACT_EQ; utsig=sigexpr
      { (tokL, UTBindSignature(sigident, utsig)) }
;
bind_value:
  | utnonrecbind=bind_value_nonrec
      { UTNonRec(utnonrecbind) }
  | utrecbinds=bind_value_rec
      { UTRec(utrecbinds) }
  | MUTABLE; ident=LOWER; REVERSED_ARROW; utast=expr
      { let mutbind = (ident, utast) in UTMutable(mutbind) }
  | INLINE; utnonrecbind=bind_inline
      { UTNonRec(utnonrecbind) }
  | BLOCK; utnonrecbind=bind_block
      { UTNonRec(utnonrecbind) }
  | MATH; utnonrecbind=bind_math
      { UTNonRec(utnonrecbind) }
/*
  | INLINE; dec=nxhorzmacrodec {
      let (rngcs, csnm, macparams, utast1) = dec in
      UTBindHorzMacro((rngcs, csnm), macparams, utast1)
    }
  | BLOCK; dec=nxvertmacrodec {
      let (rngcs, csnm, macparams, utast1) = dec in
      UTBindVertMacro((rngcs, csnm), macparams, utast1)
    }
*/
;
bind_value_rec:
  | REC; valbinds=separated_nonempty_list(AND, bind_value_nonrec);
      { valbinds }
;
bind_value_nonrec:
  | ident=bound_identifier; param_units=list(param_unit); EXACT_EQ; utast=expr
      {
        let curried = curry_lambda_abstraction param_units utast in
        (ident, curried)
      }
;
bind_inline:
  | ident_ctx=LOWER; cs=BACKSLASH_CMD; param_units=list(param_unit); EXACT_EQ; utast=expr
      {
        let curried = curry_lambda_abstraction param_units utast in
        let rng = make_range (Ranged ident_ctx) (Ranged utast) in
        (cs, (rng, UTLambdaHorz(ident_ctx, curried)))
      }
  | cs=BACKSLASH_CMD; param_units=list(param_unit); EXACT_EQ; utast=expr
      {
        let rng_ctx = Range.dummy "context-of-lightweight-let-inline" in
        let varnm_ctx = "%context" in
        let ident_ctx = (rng_ctx, varnm_ctx) in
        let utast_ctx = (rng_ctx, UTContentOf([], ident_ctx)) in
        let utast_read = (Range.dummy "read-inline-of-lightweight-let-inline", UTLexHorz(utast_ctx, utast)) in
        let curried = curry_lambda_abstraction param_units utast_read in
        let rng = make_range (Ranged cs) (Ranged utast) in
        (cs, (rng, UTLambdaHorz(ident_ctx, curried)))
      }
;
bind_block:
  | ident_ctx=LOWER; cs=PLUS_CMD; param_units=list(param_unit); EXACT_EQ; utast=expr
      {
        let rng = make_range (Ranged ident_ctx) (Ranged utast) in
        let curried = curry_lambda_abstraction param_units utast in
        (cs, (rng, UTLambdaVert(ident_ctx, curried)))
      }
  | cs=PLUS_CMD; param_units=list(param_unit); EXACT_EQ; utast=expr
      {
        let rng_ctx = Range.dummy "context-of-lightweight-let-block" in
        let varnm_ctx = "%context" in
        let ident_ctx = (rng_ctx, varnm_ctx) in
        let utast_ctx = (rng_ctx, UTContentOf([], ident_ctx)) in
        let utast_read = (Range.dummy "read-block-of-lightweight-let-block", UTLexVert(utast_ctx, utast)) in
        let curried = curry_lambda_abstraction param_units utast_read in
        let rng = make_range (Ranged cs) (Ranged utast) in
        (cs, (rng, UTLambdaVert(ident_ctx, curried)))
      }
;
bind_math:
  | cs=BACKSLASH_CMD; param_units=list(param_unit); EXACT_EQ; utast=expr
      {
        let rng = make_range (Ranged cs) (Ranged utast) in
        let curried = curry_lambda_abstraction param_units utast in
        (cs, (rng, UTLambdaMath(curried)))
      }
;
bind_type:
  | tybinds=separated_nonempty_list(AND, bind_type_single)
      { tybinds }
;
bind_type_single:
  | tyident=LOWER; tyvars=list(TYPEVAR); EXACT_EQ; BAR?; ctors=variants
      { (tyident, tyvars, UTBindVariant(ctors)) }
  | tyident=LOWER; tyvars=list(TYPEVAR); EXACT_EQ; mnty=typ
      { (tyident, tyvars, UTBindSynonym(mnty)) }
;
variants:
  | vs=separated_nonempty_list(BAR, variant)
      { vs }
;
variant:
  | ctor=UPPER; OF; mnty=typ
      { UTConstructorBranch(ctor, Some(mnty)) }
  | ctor=UPPER
      { UTConstructorBranch(ctor, None) }
;
sig_annot:
  | COLON; utsig=sigexpr { utsig }
;
sigexpr:
  | utsig=sigexpr_bot; WITH; TYPE; tybinds=bind_type
      {
        let (rng, _) = utsig in (* TODO (enhance): give appropriate ranges *)
        (rng, UTSigWith(utsig, [], tybinds))
      }
  | utsig=sigexpr_bot; WITH; rmodchain=mod_chain; TYPE; tybinds=bind_type
      {
        let (rng, _) = utsig in (* TODO (enhance): give appropriate ranges *)
        let (_, (modident, modidents)) = rmodchain in
        (rng, UTSigWith(utsig, modident :: modidents, tybinds))
      }
  | tokL=L_PAREN; modident=UPPER; COLON; utsig1=sigexpr; R_PAREN; ARROW; utsig2=sigexpr
      { make_standard (Tok tokL) (Ranged utsig2) (UTSigFunctor(modident, utsig1, utsig2)) }
  | utsig=sigexpr_bot
      { utsig }
;
sigexpr_bot:
  | sigident=UPPER
      {
        let (rng, signm) = sigident in
        (rng, UTSigVar(signm))
      }
  | sigpath=LONG_UPPER
      {
        let (rng, modidents, sigident) = sigpath in
        let modchain =
          match modidents with
          | []                 -> assert false
          | modident1 :: projs -> (modident1, projs)
        in
        (rng, UTSigPath(modchain, sigident))
      }
  | tokL=SIG; decls=list(decl); tokR=END
      {
        let rng = make_range (Tok tokL) (Tok tokR) in
        (rng, UTSigDecls(decls))
      }
;
decl:
  | VAL; ident=bound_identifier; mnquant=quant; COLON; mnty=typ
      { UTDeclValue(ident, mnquant, mnty) }
  | VAL; cs=BACKSLASH_CMD; mnquant=quant; COLON; mnty=typ
      { UTDeclValue(cs, mnquant, mnty) }
  | VAL; cs=PLUS_CMD; mnquant=quant; COLON; mnty=typ
      { UTDeclValue(cs, mnquant, mnty) }
  | TYPE; tyident=LOWER; CONS; mnkd=kind
      { UTDeclTypeOpaque(tyident, mnkd) }
  | TYPE; uttypebind=bind_type
      { decl_type_transparent uttypebind }
  | MODULE; modident=UPPER; COLON; utsig=sigexpr
      { UTDeclModule(modident, utsig) }
  | SIGNATURE; sigident=UPPER; EXACT_EQ; utsig=sigexpr
      { UTDeclSignature(sigident, utsig) }
  | INCLUDE; utsig=sigexpr
      { UTDeclInclude(utsig) }
;
quant:
  | tyquants=list(tyquant); rowquants=list(rowquant)
      { (tyquants, rowquants) }
;
tyquant:
  | tyvar=TYPEVAR
      { tyvar }
;
rowquant:
  | L_PAREN; rowvar=ROWVAR; CONS; mnrbkd=kind_row; R_PAREN
      { (rowvar, mnrbkd) }
;
param_unit:
  | opts_opt=option(opt_params); utpat=pattern_bot
      {
        let opts = opts_opt |> Option.value ~default:[] in
        UTParameterUnit(opts, utpat)
      }
;
opt_params:
  | QUESTION; L_PAREN; opts=optterm_nonempty_list(COMMA, opt_param); R_PAREN
      { opts }
;
opt_param:
  | rlabel=LOWER; EXACT_EQ; ident=LOWER
      { (rlabel, ident) }
;
/*
nxhorzmacrodec:
  | hmacro=HORZMACRO; macparams=list(macroparam); EXACT_EQ; utast=nxlet {
      let (rngcs, csnm) = hmacro in
      (rngcs, csnm, macparams, utast)
    }
;
nxvertmacrodec:
  | vmacro=VERTMACRO; macparams=list(macroparam); EXACT_EQ; utast=nxlet {
      let (rngcs, csnm) = vmacro in
      (rngcs, csnm, macparams, utast)
    }
;
macroparam:
  | var=LOWER              { UTLateMacroParam(var) }
  | EXACT_TILDE; var=LOWER { UTEarlyMacroParam(var) }
;
*/
kind:
  | bkd=kind_base; ARROW; kd=kind
      { let MKind(bkds_dom, bkd_cod) = kd in MKind(bkd :: bkds_dom, bkd_cod) }
  | bkd=kind_base
      { MKind([], bkd) }
;
kind_base:
  | kdident=LOWER
      { MKindName(kdident) }
;
kind_row:
  | L_RECORD; rlabels=optterm_nonempty_list(COMMA, LOWER); R_RECORD { rlabels }
;
typ:
  | mnty1=typ_prod; ARROW; mnty2=typ
      { make_standard (Ranged mnty1) (Ranged mnty2) (MFuncType([], mnty1, mnty2)) }
  | rmnopts=typ_opt_dom; mnty1=typ_prod; ARROW; mnty2=typ
      {
        let (tokL, mnopts) = rmnopts in
        make_standard (Tok tokL) (Ranged mnty2) (MFuncType(mnopts, mnty1, mnty2))
      }
  | mnty=typ_prod
      { mnty }
;
typ_prod:
  | mntys=separated_nonempty_list(EXACT_TIMES, typ_app) {
      match (mntys, List.rev mntys) with
      | ([mnty], [_]) ->
          mnty

      | (mnty1 :: mnty2 :: mntys_rest, mnty_last :: _) ->
          make_standard (Ranged mnty1) (Ranged mnty_last) (MProductType(TupleList.make mnty1 mnty2 mntys_rest))

      | (_, _) ->
          assert false
  }
;
typ_app:
  | tyident=LOWER mntys=nonempty_list(typ_bot)
      {
        let rng =
          match List.rev mntys with
          | (rng_last, _) :: _ -> make_range (Ranged tyident) (Tok rng_last)
          | _                  -> assert false
        in
        let (_, tynm) = tyident in
        (rng, MTypeName(tynm, mntys))
      }
  | tokL=INLINE; L_SQUARE; mncmdargtys=optterm_list(COMMA, typ_cmd_arg); tokR=R_SQUARE
      { let rng = make_range (Tok tokL) (Tok tokR) in (rng, MHorzCommandType(mncmdargtys)) }
  | tokL=BLOCK; L_SQUARE; mncmdargtys=optterm_list(COMMA, typ_cmd_arg); tokR=R_SQUARE
      { let rng = make_range (Tok tokL) (Tok tokR) in (rng, MVertCommandType(mncmdargtys)) }
  | tokL=MATH; L_SQUARE; mncmdargtys=optterm_list(COMMA, typ_cmd_arg); tokR=R_SQUARE
      { let rng = make_range (Tok tokL) (Tok tokR) in (rng, MMathCommandType(mncmdargtys)) }
  | mnty=typ_bot
      { mnty }
;
typ_bot:
  | tyident=LOWER
      { let (rng, tynm) = tyident in (rng, MTypeName(tynm, [])) }
  | tyvar=TYPEVAR
      { let (rng, tyvarnm) = tyvar in (rng, MTypeParam(tyvarnm)) }
  | tokL=L_RECORD; fields=optterm_nonempty_list(COMMA, typ_record_elem); tokR=R_RECORD
      { let rng = make_range (Tok tokL) (Tok tokR) in (rng, MRecordType(fields)) }
  | L_PAREN; mnty=typ; R_PAREN
      { mnty }
;
typ_opt_dom:
  | tokL=QUESTION; L_PAREN; mnopts=optterm_nonempty_list(COMMA, typ_opt_dom_entry); R_PAREN
      { (tokL, mnopts) }
;
typ_opt_dom_entry:
  | rlabel=LOWER; COLON; mnty=typ
      { (rlabel, mnty) }
;
typ_cmd_arg:
  | rmnopts_opt=option(typ_opt_dom); mnty=typ_prod
      {
        let mnopts =
          match rmnopts_opt with
          | None              -> []
          | Some((_, mnopts)) -> mnopts
        in
        MArgType(mnopts, mnty)
      }
;
typ_record_elem:
  | rlabel=LOWER; COLON; mnty=typ { (rlabel, mnty) }
;
expr:
  | tokL=MATCH; utast=expr; WITH; BAR?; branches=separated_nonempty_list(BAR, branch); tokR=END
      { make_standard (Tok tokL) (Tok tokR) (UTPatternMatch(utast, branches)) }
  | tok=LET; valbind=bind_value; IN; utast2=expr
      { make_standard (Tok tok) (Ranged utast2) (UTLetIn(valbind, utast2)) }
  | tok=LET; OPEN; modident=UPPER; IN; utast=expr
      { make_standard (Tok tok) (Ranged utast) (UTOpenIn(modident, utast)) }
  | tok=IF; utast0=expr; THEN; utast1=expr; ELSE; utast2=expr
      { make_standard (Tok tok) (Ranged utast2) (UTIfThenElse(utast0, utast1, utast2)) }
  | tok=FUN; param_units=list(param_unit); ARROW; utast=expr
      {
        let (_, utast_main) = curry_lambda_abstraction param_units utast in
        make_standard (Tok tok) (Ranged utast) utast_main
      }
  | utast=expr_overwrite
      { utast }
;
expr_overwrite:
  | ident=LOWER; REVERSED_ARROW; utast=expr_op
      { make_standard (Ranged ident) (Ranged utast) (UTOverwrite(ident, utast)) }
  | utast=expr_op
      { utast }
;
expr_op:
  | utastL=expr_op; op=BINOP_BAR;     utastR=expr_op
  | utastL=expr_op; op=BINOP_AMP;     utastR=expr_op
  | utastL=expr_op; op=BINOP_EQ;      utastR=expr_op
  | utastL=expr_op; op=BINOP_GT;      utastR=expr_op
  | utastL=expr_op; op=BINOP_LT;      utastR=expr_op
  | utastL=expr_op; op=BINOP_HAT;     utastR=expr_op
  | utastL=expr_op; op=BINOP_PLUS;    utastR=expr_op
  | utastL=expr_op; op=BINOP_MINUS;   utastR=expr_op
  | utastL=expr_op; op=BINOP_TIMES;   utastR=expr_op
  | utastL=expr_op; op=BINOP_DIVIDES; utastR=expr_op
      { binary_operator utastL op utastR }
  | utastL=expr_op; rng=CONS;         utastR=expr_op
      { binary_operator utastL (rng, "::") utastR }
  | utastL=expr_op; rng=EXACT_MINUS;  utastR=expr_op
      { binary_operator utastL (rng, "-") utastR }
  | utastL=expr_op; rng=EXACT_TIMES;  utastR=expr_op
      { binary_operator utastL (rng, "*") utastR }
  | utastL=expr_op; rng=MOD;          utastR=expr_op
      { binary_operator utastL (rng, "mod") utastR }
  | tok=EXACT_MINUS; utast2=expr_app
      { make_uminus tok utast2 }
  | ctor=UPPER; utast2=expr_un
      { make_standard (Ranged ctor) (Ranged utast2) (UTConstructor(extract_main ctor, utast2)) }
  | ctor=UPPER
      {
        let utast_unit = (Range.dummy "constructor-unitvalue", UTUnitConstant) in
        let (rng, ctornm) = ctor in
        (rng, UTConstructor(ctornm, utast_unit))
      }
  | utast=expr_app
      { utast }
;
expr_app:
  | utast1=expr_app; mnopts=expr_opts; utast2=expr_un
      {
        make_standard (Ranged utast1) (Ranged utast2) (UTApply(mnopts, utast1, utast2))
      }
  | utast1=expr_app; mnopts=expr_opts; ctor=UPPER
      {
        let utast_unit = (Range.dummy "constructor-unitvalue", UTUnitConstant) in
        let (rng, ctornm) = ctor in
        let utast2 = (rng, UTConstructor(ctornm, utast_unit)) in
        make_standard (Ranged utast1) (Tok rng) (UTApply(mnopts, utast1, utast2))
      }
  | utast=expr_un
      { utast }
;
expr_un:
  | unop=UNOP_EXCLAM; utast2=expr_bot
      {
        let (rng, _) = unop in
        make_standard (Tok rng) (Ranged utast2) (UTApply([], (rng, UTContentOf([], unop)), utast2))
      }
  | tok=EXACT_AMP; utast2=expr_bot
      { make_standard (Tok tok) (Ranged utast2) (UTNext(utast2)) }
  | tok=EXACT_TILDE; utast2=expr_bot
      { make_standard (Tok tok) (Ranged utast2) (UTPrev(utast2)) }
  | utast=expr_bot
      { utast }
;
expr_bot:
  | utast=expr_bot; ACCESS; rlabel=LOWER
      { make_standard (Ranged utast) (Ranged rlabel) (UTAccessField(utast, rlabel)) }
  | ident=LOWER
      { let (rng, _) = ident in (rng, UTContentOf([], ident)) }
  | long_ident=LONG_LOWER
      { let (rng, modidents, ident) = long_ident in (rng, UTContentOf(modidents, ident)) }
  | tokL=L_PAREN; ident=binop; tokR=R_PAREN
      { make_standard (Tok tokL) (Tok tokR) (UTContentOf([], ident)) }
  | ic=INT
      { let (rng, n) = ic in (rng, UTIntegerConstant(n)) }
  | fc=FLOAT
      { let (rng, r) = fc in (rng, UTFloatConstant(r)) }
  | lc=LENGTH
      { let (rng, r, unitnm) = lc in (rng, UTLengthDescription(r, unitnm)) }
  | rng=TRUE
      { (rng, UTBooleanConstant(true)) }
  | rng=FALSE
      { (rng, UTBooleanConstant(false)) }
  | tok=STRING
      {
        let (rng, str, pre, post) = tok in
        make_standard (Tok rng) (Tok rng) (UTStringConstant(omit_spaces pre post str))
      }
  | tok=POSITIONED_STRING
      {
        let (rng, ipos, s) = tok in
        make_standard (Tok rng) (Tok rng) (UTPositionedString(ipos, s))
      }
  | tokL=L_PAREN; tokR=R_PAREN
      { make_standard (Tok tokL) (Tok tokR) UTUnitConstant }
  | tokL=L_PAREN; utast=expr; tokR=R_PAREN
      { make_standard (Tok tokL) (Tok tokR) (extract_main utast) }
  | tokL=L_PAREN; utast1=expr; COMMA; utasts=separated_nonempty_list(COMMA, expr); tokR=R_PAREN
      {
        match utasts with
        | [] ->
            assert false

        | utast2 :: utast_rest ->
            make_standard (Tok tokL) (Tok tokR) (UTTuple(TupleList.make utast1 utast2 utast_rest))
      }
  | utast=expr_bot_list
      { utast }
  | utast=expr_bot_record
      { utast }
  | tokL=L_INLINE_TEXT; utast=inline; tokR=R_INLINE_TEXT
      { make_standard (Tok tokL) (Tok tokR) (extract_main utast) }
  | tokL=L_BLOCK_TEXT; utast=block; tokR=R_BLOCK_TEXT
      { make_standard (Tok tokL) (Tok tokR) (extract_main utast) }
  | tokL=L_MATH_TEXT; utast=math; tokR=R_MATH_TEXT
      { make_standard (Tok tokL) (Tok tokR) (extract_main utast) }
;
expr_bot_list:
  | tokL=L_SQUARE; utasts=optterm_list(COMMA, expr); tokR=R_SQUARE
      {
        let (_, utast_main) = make_list utasts in
        make_standard (Tok tokL) (Tok tokR) utast_main
      }
;
expr_bot_record:
  | tokL=L_RECORD; fields=optterm_list(COMMA, record_field); tokR=R_RECORD
      { make_standard (Tok tokL) (Tok tokR) (UTRecord(fields)) }
  | tokL=L_RECORD; utast=expr_bot; WITH; fields=optterm_nonempty_list(COMMA, record_field); tokR=R_RECORD
      {
        let (_, utast_main) =
          fields |> List.fold_left (fun utast1 (rlabel, utast2) ->
            (Range.dummy "update-field", UTUpdateField(utast1, rlabel, utast2))
          ) utast
        in
        make_standard (Tok tokL) (Tok tokR) utast_main
      }
;
record_field:
  | rlabel=LOWER; EXACT_EQ; utast=expr
      { (rlabel, utast) }
;
branch:
  | utpat=pattern; ARROW; utast=expr
      { UTPatternBranch(utpat, utast) }
;
binop:
  | tok=UNOP_EXCLAM
  | tok=BINOP_TIMES
  | tok=BINOP_DIVIDES
  | tok=BINOP_HAT
  | tok=BINOP_EQ
  | tok=BINOP_GT
  | tok=BINOP_LT
  | tok=BINOP_AMP
  | tok=BINOP_BAR
  | tok=BINOP_PLUS
  | tok=BINOP_MINUS { tok }
  | rng=EXACT_TIMES { (rng, "*") }
  | rng=EXACT_MINUS { (rng, "-") }
  | rng=MOD         { (rng, "mod") }
;
pattern:
  | utpat=pattern_cons; AS; ident=LOWER
      { make_standard (Ranged utpat) (Ranged ident) (UTPAsVariable(extract_main ident, utpat)) }
  | utpat=pattern_cons
      { utpat }
;
pattern_cons:
  | utpat1=pattern_bot; CONS; utpat2=pattern_cons
      { make_standard (Ranged utpat1) (Ranged utpat2) (UTPListCons(utpat1, utpat2)) }
  | ctor=UPPER; utpat=pattern_bot
      { make_standard (Ranged ctor) (Ranged utpat) (UTPConstructor(extract_main ctor, utpat)) }
  | ctor=UPPER
      {
        let utast_unit = (Range.dummy "constructor-unit-value", UTPUnitConstant) in
        let (rng, ctornm) = ctor in (rng, UTPConstructor(ctornm, utast_unit))
      }
  | utpat=pattern_bot
      { utpat }
;
pattern_bot:
  | ic=INT
      { let (rng, n) = ic in (rng, UTPIntegerConstant(n)) }
  | rng=TRUE
      { (rng, UTPBooleanConstant(true)) }
  | rng=FALSE
      { (rng, UTPBooleanConstant(false)) }
  | tokL=L_PAREN; tokR=R_PAREN
      { make_standard (Tok tokL) (Tok tokR) UTPUnitConstant }
  | rng=WILDCARD
      { (rng, UTPWildCard) }
  | ident=bound_identifier
      { let (rng, varnm) = ident in (rng, UTPVariable(varnm)) }
  | lit=STRING
      {
        let (rng, str, pre, post) = lit in
        make_standard (Tok rng) (Tok rng) (UTPStringConstant(omit_spaces pre post str))
      }
  | tokL=L_SQUARE; utpats=optterm_list(COMMA, pattern); tokR=R_SQUARE
      {
        let (_, utpat_main) =
          List.fold_right (fun utpat1 utpat2 ->
            (Range.dummy "list-pattern-cons", UTPListCons(utpat1, utpat2))
          ) utpats (Range.dummy "list-pattern-nil", UTPEndOfList)
        in
        make_standard (Tok tokL) (Tok tokR) utpat_main
      }
  | tokL=L_PAREN; utpat=pattern; tokR=R_PAREN
      { make_standard (Tok tokL) (Tok tokR) (extract_main utpat) }
  | tokL=L_PAREN; utpat1=pattern; COMMA; utpats=optterm_nonempty_list(COMMA, pattern); tokR=R_PAREN
      {
        match utpats with
        | [] ->
            assert false

        | utpat2 :: utpat_rest ->
            make_standard (Tok tokL) (Tok tokR) (UTPTuple(TupleList.make utpat1 utpat2 utpat_rest))
      }
;
inline:
  | BAR; utasts=list(terminated(inline_single, BAR))
      { make_list utasts }
  | utast=inline_single
      { utast }
  | itemizes=nonempty_list(inline_itemize_elem)
      { make_list_to_itemize itemizes }
;
inline_itemize_elem:
  | item=ITEM; utast=inline_single
      { let (rng, depth) = item in (rng, depth, utast) }
;
inline_single:
  | ielems=inline_elems
      { let rng = make_range_from_list ielems in (rng, UTInputHorz(ielems)) }
;
inline_elems:
  | itext=inline_elem_text; icmd=inline_elem_cmd; ielems=inline_elems
      { itext :: icmd :: ielems }
  | icmd=inline_elem_cmd; ielems=inline_elems
      { icmd :: ielems }
  | itext=inline_elem_text
      { itext :: [] }
  |   { [] }
;
inline_elem_cmd:
  | icmd=backslash_cmd; nargs=list(cmd_arg_expr); rsargs=cmd_args_text
      {
        let (rng_cs, modidents, cs) = icmd in
        let utast_cmd = (rng_cs, UTContentOf(modidents, cs)) in
        let (rng_last, sargs) = rsargs in
        let args = List.append nargs sargs in
        make_standard (Tok rng_cs) (Tok rng_last) (UTInputHorzEmbedded(utast_cmd, args))
      }
/*
  | hmacro=HORZMACRO; macargsraw=macroargs {
      let (rngcs, _) = hmacro in
      let (rnglast, macroargs) = macargsraw in
      make_standard (Tok rngcs) (Tok rnglast) (UTInputHorzMacro(hmacro, macroargs))
    }
*/
  | tokL=L_MATH_TEXT; utast=math; tokR=R_MATH_TEXT
      { make_standard (Tok tokL) (Tok tokR) (UTInputHorzEmbeddedMath(utast)) }
  | literal=STRING
      {
        let (rng, str, pre, post) = literal in
        make_standard (Tok rng) (Tok rng) (UTInputHorzEmbeddedCodeText(omit_spaces pre post str))
      }
  | long_ident=VAR_IN_TEXT; tokR=SEMICOLON
      {
        let (rng, modidents, ident) = long_ident in
        let utast = (rng, UTContentOf(modidents, ident)) in
        make_standard (Tok rng) (Tok tokR) (UTInputHorzContent(utast))
      }
;
inline_elem_text:
  | ichars=nonempty_list(inline_char)
      {
        let rng = make_range_from_list ichars in
        let text = String.concat "" (ichars |> List.map (fun (r, t) -> t)) in
        (rng, UTInputHorzText(text))
      }
;
inline_char:
  | char=CHAR { char }
  | rng=SPACE { (rng, " ") }
  | rng=BREAK { (rng, "\n") }
;
block:
  | belems=list(block_elem)
      { (make_range_from_list belems, UTInputVert(belems)) }
;
block_elem:
  | bcmd=plus_cmd; nargs=list(cmd_arg_expr); rsargs=cmd_args_text
      {
        let (rng_cs, modidents, cs) = bcmd in
        let (rng_last, sargs) = rsargs in
        let utast_cmd = (rng_cs, UTContentOf(modidents, cs)) in
        let args = List.append nargs sargs in
        make_standard (Tok rng_cs) (Tok rng_last) (UTInputVertEmbedded(utast_cmd, args))
      }
/*
  | vmacro=VERTMACRO; macargsraw=macroargs {
      let (rngcs, _) = vmacro in
      let (rnglast, macargs) = macargsraw in
      make_standard (Tok rngcs) (Tok rnglast) (UTInputVertMacro(vmacro, macargs))
    }
*/
  | long_ident=VAR_IN_TEXT; tokR=SEMICOLON
      {
        let (rng, modidents, ident) = long_ident in
        let utast = (rng, UTContentOf(modidents, ident)) in
        make_standard (Tok rng) (Tok tokR) (UTInputVertContent(utast))
      }
;
math:
  | BAR; utms=list(terminated(math_single, BAR))
      { utms |> List.map (fun utm -> let (rng, _) = utm in (rng, UTMath(utm))) |> make_list }
  | utm=math_single
      { let (rng, _) = utm in (rng, UTMath(utm)) }
;
math_single:
  | utms=list(math_elem)
      {
        let rng =
          match (utms, List.rev utms) with
          | ([], [])                         -> Range.dummy "empty-math"
          | ((rngL, _) :: _, (rngR, _) :: _) -> Range.unite rngL rngR
          | _                                -> assert false
        in
        (rng, UTMList(utms))
      }
;
math_elem:
  (* a: *)
  | base=math_bot
      { base }
  (* a^p: *)
  | base=math_bot; SUPERSCRIPT; sup=math_group
      {
        base
          |> make_sup ~sup ~range:(make_range (Ranged base) (Ranged (snd sup)))
      }
  (* a_b: *)
  | base=math_bot; SUBSCRIPT; sub=math_group
      {
        base
          |> make_sub ~sub ~range:(make_range (Ranged base) (Ranged (snd sub)))
      }
  (* a_b^p: *)
  | base=math_bot; SUBSCRIPT; sub=math_group; SUPERSCRIPT; sup=math_group
      {
        base
          |> make_sub ~sub ~range:(make_range (Ranged base) (Ranged (snd sub)))
          |> make_sup ~sup ~range:(make_range (Ranged base) (Ranged (snd sup)))
      }
  (* a^p_b: *)
  | base=math_bot; SUPERSCRIPT; sup=math_group; SUBSCRIPT; sub=math_group
      {
        base
          |> make_sub ~sub ~range:(Range.dummy "mathtop")
          |> make_sup ~sup ~range:(make_range (Ranged base) (Ranged (snd sub)))
      }
  (* a': *)
  | base=math_bot; prime=PRIMES
      {
        base
          |> make_sup ~prime ~range:(make_range (Ranged base) (Tok (fst prime)))
      }
  (* a'^p: *)
  | base=math_bot; prime=PRIMES; SUPERSCRIPT; sup=math_group
      {
        base
          |> make_sup ~prime ~sup ~range:(make_range (Ranged base) (Ranged (snd sup)))
      }
  (* a'_b: *)
  | base=math_bot; prime=PRIMES; SUBSCRIPT; sub=math_group
      {
        base
          |> make_sub ~sub ~range:(Range.dummy "mathtop")
          |> make_sup ~prime ~range:(make_range (Ranged base) (Ranged (snd sub)))
      }
  (* a'_b^p: *)
  | base=math_bot; prime=PRIMES; SUBSCRIPT; sub=math_group; SUPERSCRIPT; sup=math_group
      {
        base
          |> make_sub ~sub ~range:(Range.dummy "mathtop")
          |> make_sup ~prime ~sup ~range:(make_range (Ranged base) (Ranged (snd sup)))
      }
  (* a'^p_b: *)
  | base=math_bot; prime=PRIMES; SUPERSCRIPT; sup=math_group; SUBSCRIPT; sub=math_group
      {
        base
          |> make_sub ~sub ~range:(Range.dummy "mathtop")
          |> make_sup ~prime ~sup ~range:(make_range (Ranged base) (Ranged (snd sub)))
      }
;
math_group:
  | tokL=L_MATH_TEXT; utm=math_single; tokR=R_MATH_TEXT
      { (true, make_standard (Tok tokL) (Tok tokR) (extract_main utm)) }
  | utm=math_bot
      { (false, utm) }
;
math_bot:
  | tok=MATHCHARS
      {
        let (rng, s) = tok in
        let uchs = InternalText.to_uchar_list (InternalText.of_utf8 s) in
        (rng, UTMChars(uchs))
      }
  | mcmd=backslash_cmd; args=list(math_cmd_arg)
      {
        let (rng_cs, modnms, csnm) = mcmd in
        let rng_last =
          match List.rev args with
          | []                             -> rng_cs
          | UTCommandArg(_, (rng, _)) :: _ -> rng
        in
        let utast_cmd = (rng_cs, UTContentOf(modnms, csnm)) in
        make_standard (Tok rng_cs) (Tok rng_last) (UTMCommand(utast_cmd, args))
      }
  | long_ident=VAR_IN_TEXT
      { let (rng, modnms, varnm) = long_ident in (rng, UTMEmbed((rng, UTContentOf(modnms, varnm)))) }
;
math_cmd_arg:
  | mnopts=expr_opts; tokL=L_MATH_TEXT; utast=math; tokR=R_MATH_TEXT
      { UTCommandArg(mnopts, make_standard (Tok tokL) (Tok tokR) (extract_main utast)) }
  | mnopts=expr_opts; tokL=L_INLINE_TEXT; utast=inline; tokR=R_INLINE_TEXT
      { UTCommandArg(mnopts, make_standard (Tok tokL) (Tok tokR) (extract_main utast)) }
  | mnopts=expr_opts; tokL=L_BLOCK_TEXT; utast=block; tokR=R_BLOCK_TEXT
      { UTCommandArg(mnopts, make_standard (Tok tokL) (Tok tokR) (extract_main utast)) }
  | utcmdarg=cmd_arg_expr
      { utcmdarg }
;
/*
macroargs:
  | macnargs=list(macronarg); cls=SEMICOLON { (cls, macnargs) }
;
macronarg:
  | L_PAREN; expr=expr_bot; R_PAREN              { UTLateMacroArg(expr) }
  | EXACT_TILDE; L_PAREN; expr=expr_bot; R_PAREN { UTEarlyMacroArg(expr) }
;
*/
cmd_arg_expr:
  | mnopts=expr_opts; tokL=L_PAREN; utast=expr; tokR=R_PAREN
      { UTCommandArg(mnopts, make_standard (Tok tokL) (Tok tokR) (extract_main utast)) }
  | mnopts=expr_opts; tokL=L_PAREN; tokR=R_PAREN
      { UTCommandArg(mnopts, make_standard (Tok tokL) (Tok tokR) UTUnitConstant) }
  | mnopts=expr_opts; utast=expr_bot_record
      { UTCommandArg(mnopts, utast) }
  | mnopts=expr_opts; utast=expr_bot_list
      { UTCommandArg(mnopts, utast) }
;
expr_opts:
  | QUESTION; L_PAREN; mnopts=optterm_nonempty_list(COMMA, expr_opt_entry); R_PAREN
      { mnopts }
  |   { [] }
;
expr_opt_entry:
  | rlabel=LOWER; EXACT_EQ; utast=expr
      { (rlabel, utast) }
;
cmd_args_text:
  | rng=SEMICOLON
      { (rng, []) }
  | sargs=nonempty_list(cmd_arg_text)
      {
        let rng =
          match List.rev sargs with
          | []                             -> assert false
          | UTCommandArg(_, (rng, _)) :: _ -> rng
        in
        (rng, sargs)
      }
;
cmd_arg_text:
  | tokL=L_BLOCK_TEXT; utast=block; tokR=R_BLOCK_TEXT
      { UTCommandArg([], make_standard (Tok tokL) (Tok tokR) (extract_main utast)) }
  | tokL=L_INLINE_TEXT; utast=inline; tokR=R_INLINE_TEXT
      { UTCommandArg([], make_standard (Tok tokL) (Tok tokR) (extract_main utast)) }
;
backslash_cmd:
  | cs=BACKSLASH_CMD
      { let (rng, csnm) = cs in (rng, [], (rng, csnm)) }
  | long_cs=LONG_BACKSLASH_CMD
      { long_cs }
;
plus_cmd:
  | cs=PLUS_CMD
      { let (rng, csnm) = cs in (rng, [], (rng, csnm)) }
  | long_cs=LONG_PLUS_CMD
      { long_cs }
;
