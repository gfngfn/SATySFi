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


  let rec make_cons utastlst =
    match utastlst with
    | [] -> (Range.dummy "make_cons", UTEndOfList)
    | ((rng, utastmain) as utast) :: tail ->
        let utasttail = make_cons tail in
        let (rngtail, _) = utasttail in
          (Range.unite rng rngtail, UTListCons(utast, utasttail))


  let curry_lambda_abstraction (param_units : untyped_parameter_unit list) (utast : untyped_abstract_tree) : untyped_abstract_tree =
    let rng = Range.dummy "curry_lambda_abstraction" in
    utast |> List.fold_right (fun param_unit utast ->
      let UTParameterUnit(opts, utpat) = param_unit in
      (rng, UTFunction(opts, utpat, utast))
    ) param_units


  let rec omit_pre_spaces str =
    let len = String.length str in
      if len = 0 then "" else
        match String.sub str 0 1 with
        | " " -> omit_pre_spaces (String.sub str 1 (len - 1))
        | _   -> str

  let rec omit_post_spaces str =
    let len = String.length str in
      if len = 0 then "" else
        match String.sub str (len - 1) 1 with
        | " "  -> omit_post_spaces (String.sub str 0 (len - 1))
        | "\n" -> String.sub str 0 (len - 1)
        | _    -> str


  let rec omit_spaces (omit_pre : bool) (omit_post : bool) (str_literal_raw : string) : string =
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


  and min_indent_space (str_ltrl : string) =
    min_indent_space_sub str_ltrl 0 ReadingSpace 0 (String.length str_ltrl)


  and min_indent_space_sub (str_ltrl : string) (index : int) (lrstate : literal_reading_state) (spnum : int) (minspnum : int) =
    if index >= (String.length str_ltrl) then
        minspnum
    else
      match lrstate with
      | Normal ->
          ( match str_ltrl.[index] with
            | '\n' -> min_indent_space_sub str_ltrl (index + 1) ReadingSpace 0 minspnum
            | _    -> min_indent_space_sub str_ltrl (index + 1) Normal 0 minspnum
          )
      | ReadingSpace ->
          ( match str_ltrl.[index] with
            | ' '  -> min_indent_space_sub str_ltrl (index + 1) ReadingSpace (spnum + 1) minspnum
            | '\n' -> min_indent_space_sub str_ltrl (index + 1) ReadingSpace 0 minspnum
                (* -- does not take space-only line into account -- *)
            | _    -> min_indent_space_sub str_ltrl (index + 1) Normal 0 (if spnum < minspnum then spnum else minspnum)
          )


  and shave_indent str_ltrl minspnum =
    shave_indent_sub str_ltrl minspnum 0 "" Normal 0


  and shave_indent_sub str_ltrl minspnum index str_constr lrstate spnum =
    if index >= (String.length str_ltrl) then
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

            | '\n' -> shave_indent_sub str_ltrl minspnum (index + 1) (str_constr ^ "\n") ReadingSpace 0
            | ch   -> shave_indent_sub str_ltrl minspnum (index + 1) (str_constr ^ (String.make 1 ch)) Normal 0
          end


  let extract_main (_, utastmain) = utastmain


  let extract_name (_, name) = name


  let binary_operator (utastL : untyped_abstract_tree) (optok : Range.t * var_name) (utastR : untyped_abstract_tree) : untyped_abstract_tree =
    let (rngop, opnm) = optok in
    let rng = make_range (Ranged utastL) (Ranged utastR) in
      (rng, UTApply([], (Range.dummy "binary_operator", UTApply([], (rngop, UTContentOf([], opnm)), utastL)), utastR))

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


  let make_product_pattern (rng : Range.t) (pats : untyped_pattern_tree list) : untyped_pattern_tree =
    match pats with
    | []                      -> assert false
    | pat :: []               -> pat
    | pat1 :: pat2 :: patrest -> (rng, UTPTuple(TupleList.make pat1 pat2 patrest))


  let rec make_list_to_itemize (lst : (Range.t * int * untyped_abstract_tree) list) =
    let contents = make_list_to_itemize_sub (UTItem((Range.dummy "itemize2", UTInputHorz([])), [])) lst 0 in
    (Range.dummy "itemize1", UTItemize(contents))

  and make_list_to_itemize_sub (resitmz : untyped_itemize) (lst : (Range.t * int * untyped_abstract_tree) list) (crrntdp : int) =
    match lst with
    | []                          -> resitmz
    | (rng, depth, utast) :: tail ->
        if depth <= crrntdp + 1 then
          let newresitmz = insert_last [] resitmz 1 depth utast in
            make_list_to_itemize_sub newresitmz tail depth
        else
          raise (ParseErrorDetail(rng, "syntax error: illegal item depth "
            ^ (string_of_int depth) ^ " after " ^ (string_of_int crrntdp)))

  and insert_last (resitmzlst : untyped_itemize list) (itmz : untyped_itemize) (i : int) (depth : int) (utast : untyped_abstract_tree) : untyped_itemize =
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


  let primes (n : int) : Uchar.t list =
    List.init n (fun _ -> Uchar.of_int 0x2032)

  let make_sup ~range ?prime ?sup base =
    let make_primes r n =
      r, UTMChars(primes n)
    in
    match prime, sup with
    | None, None ->
        range, snd base
    | None, Some(b, p) ->
        range, UTMSuperScript(base, b, p)
    | Some(r, n), None ->
        range, UTMSuperScript(base, true, make_primes r n)
    | Some(r, n), Some(b, p) ->
        let ps = make_standard (Tok r) (Ranged p) @@ UTMList [make_primes r n; p] in
        range, UTMSuperScript(base, b, ps)

  let make_sub ~range ~sub:(b, s) base =
    range, UTMSubScript(base, b, s)
%}

%token<Range.t>
  AND AS BLOCK ELSE END FALSE FUN
  IF IN INCLUDE INLINE LET MOD MATCH MATH MODULE MUTABLE OF OPEN
  REC SIG SIGNATURE STRUCT THEN TRUE TYPE VAL WITH

%token<Range.t> BAR WILDCARD COLON ARROW REVERSED_ARROW ENDACTIVE COMMA CONS ACCESS QUESTION
%token<Range.t> LPAREN RPAREN BVERTGRP EVERTGRP BHORZGRP EHORZGRP BMATHGRP EMATHGRP BLIST ELIST BRECORD ERECORD
%token<Range.t> EXACT_MINUS EXACT_TIMES EXACT_AMP EXACT_TILDE EXACT_EQ

%token<Range.t * Types.var_name>
  BINOP_TIMES BINOP_DIVIDES BINOP_PLUS BINOP_MINUS BINOP_HAT BINOP_AMP BINOP_BAR BINOP_GT BINOP_LT BINOP_EQ

%token<Range.t * Types.var_name> UNOP_EXCLAM

%token<Range.t * Types.var_name> LOWER
%token<Range.t * Types.constructor_name> UPPER
%token<Range.t * (Types.module_name list) * Types.var_name> PATH_LOWER

%token <Range.t * Types.ctrlseq_name> HORZCMD
%token <Range.t * Types.ctrlseq_name> HORZMACRO
%token <Range.t * Types.ctrlseq_name> VERTCMD
%token <Range.t * Types.ctrlseq_name> VERTMACRO
%token <Range.t * Types.ctrlseq_name> MATHCMD
%token <Range.t * (Types.module_name list) * Types.ctrlseq_name> HORZCMDWITHMOD
%token <Range.t * (Types.module_name list) * Types.ctrlseq_name> VERTCMDWITHMOD
%token <Range.t * (Types.module_name list) * Types.ctrlseq_name> MATHCMDWITHMOD
%token <Range.t * (Types.module_name list) * Types.ctrlseq_name> VARINHORZ
%token <Range.t * (Types.module_name list) * Types.ctrlseq_name> VARINVERT
%token <Range.t * (Types.module_name list) * Types.var_name> VARINMATH
%token <Range.t * Types.type_variable_name> TYPEVAR
%token <Range.t * Types.row_variable_name> ROWVAR

%token <Range.t * int> INTCONST
%token <Range.t * float> FLOATCONST
%token <Range.t * float * Types.length_unit_name> LENGTHCONST
%token <Range.t * string> CHAR
%token <Range.t * string * bool * bool> LITERAL
%token <Range.t * Types.input_position * string> POSITIONED_LITERAL

%token <Range.t> SPACE BREAK
%token <Range.t * string> MATHCHARS
%token <Range.t * int> PRIMES
%token <Range.t> SUBSCRIPT SUPERSCRIPT
%token <Range.t * Types.module_name> OPENMODULE
%token <Range.t * int> ITEM

%token <Range.t * string> HEADER_REQUIRE HEADER_IMPORT
%token <Range.t> HEADER_STAGE0 HEADER_STAGE1 HEADER_PERSISTENT0

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

%type <Types.untyped_pattern_tree> patas
%type <Types.untyped_pattern_tree> patbot
%type <Types.untyped_abstract_tree> sxsep
%type <Types.untyped_abstract_tree> sxblock
%type <Types.untyped_abstract_tree> vxblock
%type <Types.untyped_input_vert_element> vxbot
/*
%type <Types.untyped_command_argument> narg
*/
%type <Types.untyped_command_argument> sarg
%type <Range.t * Types.var_name> binop
%type <bool * Types.untyped_math> mathgroup
%type <Types.untyped_math> mathbot

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
  | LPAREN; ident=binop; RPAREN
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
  | modtok=UPPER {
      let (rng, modnm) = modtok in
      (rng, UTModVar(modnm))
    }
  | tokL=STRUCT; utbinds=list(bind); tokR=END {
      let rng = make_range (Tok tokL) (Tok tokR) in
      (rng, UTModBinds(utbinds))
    }
(* TODO: support other module syntax *)
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
  | MUTABLE; var=LOWER; REVERSED_ARROW; utast=expr
      { let mutbind = (var, utast) in UTMutable(mutbind) }
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
  | ident_ctx=LOWER; cs=HORZCMD; param_units=list(param_unit); EXACT_EQ; utast=expr
      {
        let (rng_ctx, varnm_ctx) = ident_ctx in
        let rng = make_range (Tok rng_ctx) (Ranged utast) in
        let curried = curry_lambda_abstraction param_units utast in
        (cs, (rng, UTLambdaHorz(rng_ctx, varnm_ctx, curried)))
      }
  | cs=HORZCMD; param_units=list(param_unit); EXACT_EQ; utast=expr
      {
        let rng = make_range (Ranged cs) (Ranged utast) in
        let rng_ctx = Range.dummy "context-of-lightweight-let-inline" in
        let varnm_ctx = "%context" in
        let utast_ctx = (rng_ctx, UTContentOf([], varnm_ctx)) in
        let utast_read = (Range.dummy "read-inline-of-lightweight-let-inline", UTLexHorz(utast_ctx, utast)) in
        let curried = curry_lambda_abstraction param_units utast_read in
        (cs, (rng, UTLambdaHorz(rng_ctx, varnm_ctx, curried)))
      }
;
bind_block:
  | ident_ctx=LOWER; cs=VERTCMD; param_units=list(param_unit); EXACT_EQ; utast=expr
      {
        let (rng_ctx, varnm_ctx) = ident_ctx in
        let rng = make_range (Tok rng_ctx) (Ranged utast) in
        let curried = curry_lambda_abstraction param_units utast in
        (cs, (rng, UTLambdaVert(rng_ctx, varnm_ctx, curried)))
      }
  | cs=VERTCMD; param_units=list(param_unit); EXACT_EQ; utast=expr
      {
        let rng = make_range (Ranged cs) (Ranged utast) in
        let rng_ctx = Range.dummy "context-of-lightweight-let-block" in
        let varnm_ctx = "%context" in
        let utast_ctx = (rng_ctx, UTContentOf([], varnm_ctx)) in
        let utast_read = (Range.dummy "read-block-of-lightweight-let-block", UTLexVert(utast_ctx, utast)) in
        let curried = curry_lambda_abstraction param_units utast_read in
        (cs, (rng, UTLambdaVert(rng_ctx, varnm_ctx, curried)))
      }
;
bind_math:
  | cs=HORZCMD; param_units=list(param_unit); EXACT_EQ; utast=expr
      {
        let rng = make_range (Ranged cs) (Ranged utast) in
        let curried = curry_lambda_abstraction param_units utast in
        (cs, (rng, UTLambdaMath(curried)))
      }
;
bind_type:
  | ds=separated_nonempty_list(AND, bind_type_single)
      { ds }
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
  | sigident=UPPER
      {
        let (rng, signm) = sigident in
        (rng, UTSigVar(signm))
      }
  | tokL=SIG; decls=list(decl); tokR=END
      {
        let rng = make_range (Tok tokL) (Tok tokR) in
        (rng, UTSigDecls(decls))
      }
(* TODO: support other signature syntax *)
;
decl:
  | VAL; ident=bound_identifier; mnquant=quant; COLON; mnty=typ
      { UTDeclValue(ident, mnquant, mnty) }
  | VAL; cs=HORZCMD; mnquant=quant; COLON; mnty=typ
      { UTDeclValue(cs, mnquant, mnty) }
  | VAL; cs=VERTCMD; mnquant=quant; COLON; mnty=typ
      { UTDeclValue(cs, mnquant, mnty) }
  | TYPE; tyident=LOWER; tyvars=list(TYPEVAR); CONS; mnkd=kind
      { UTDeclTypeOpaque(tyident, mnkd) }
  | TYPE; uttypebind=bind_type
      { failwith "TODO: decl, declaration for transparent types" }
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
  | LPAREN; rowvar=ROWVAR; CONS; mnrbkd=kind_row; RPAREN
      { (rowvar, mnrbkd) }
;
param_unit:
  | opts_opt=option(opt_params); utpat=patbot
      {
        let opts = opts_opt |> Option.value ~default:[] in
        UTParameterUnit(opts, utpat)
      }
;
opt_params:
  | QUESTION; LPAREN; opts=optterm_nonempty_list(COMMA, opt_param); RPAREN
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
  | BRECORD; rlabels=optterm_nonempty_list(COMMA, LOWER); ERECORD { rlabels }
;
typ:
  | mnty1=typ_prod; ARROW; mnty2=typ
      { make_standard (Ranged mnty1) (Ranged mnty2) (MFuncType([], mnty1, mnty2)) }
  | mnopts=typ_opt_dom; mnty1=typ_prod; ARROW; mnty2=typ
      {
        let (tokL, mnopts) = mnopts in
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
  | tokL=INLINE; BLIST; mncmdargtys=optterm_list(COMMA, typ_cmd_arg); tokR=ELIST
      { let rng = make_range (Tok tokL) (Tok tokR) in (rng, MHorzCommandType(mncmdargtys)) }
  | tokL=BLOCK; BLIST; mncmdargtys=optterm_list(COMMA, typ_cmd_arg); tokR=ELIST
      { let rng = make_range (Tok tokL) (Tok tokR) in (rng, MVertCommandType(mncmdargtys)) }
  | tokL=MATH; BLIST; mncmdargtys=optterm_list(COMMA, typ_cmd_arg); tokR=ELIST
      { let rng = make_range (Tok tokL) (Tok tokR) in (rng, MMathCommandType(mncmdargtys)) }
  | mnty=typ_bot
      { mnty }
;
typ_bot:
  | tyident=LOWER
      { let (rng, tynm) = tyident in (rng, MTypeName(tynm, [])) }
  | tyvar=TYPEVAR
      { let (rng, tyvarnm) = tyvar in (rng, MTypeParam(tyvarnm)) }
  | tokL=BRECORD; fields=optterm_nonempty_list(COMMA, typ_record_elem); tokR=ERECORD
      { let rng = make_range (Tok tokL) (Tok tokR) in (rng, MRecordType(fields)) }
  | LPAREN; mnty=typ; RPAREN
      { mnty }
;
typ_opt_dom:
  | tokL=QUESTION; LPAREN; mnopts=optterm_nonempty_list(COMMA, typ_opt_dom_entry); RPAREN
      { (tokL, mnopts) }
;
typ_opt_dom_entry:
  | rlabel=LOWER; EXACT_EQ; mnty=typ
      { (rlabel, mnty) }
;
typ_cmd_arg:
  | mnopts_opt=option(typ_opt_dom); mnty=typ_prod
      {
        let mnopts =
          match mnopts_opt with
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
      {
        let (rng, modnm) = modident in
        make_standard (Tok tok) (Ranged utast) (UTOpenIn(rng, modnm, utast))
      }
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
      {
        let (rng, varnm) = ident in
        make_standard (Ranged ident) (Ranged utast) (UTOverwrite(rng, varnm, utast))
      }
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
      { make_standard (Ranged ctor) (Ranged utast2) (UTConstructor(extract_name ctor, utast2)) }
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
  | utast1=expr_app; utast2=expr_un
      {
        let optargs = failwith "TODO: nxapp, optargs" in
        make_standard (Ranged utast1) (Ranged utast2) (UTApply(optargs, utast1, utast2))
      }
  | utast1=expr_app; ctor=UPPER
      {
        let optargs = failwith "TODO: nxapp, optargs" in
        let utast_unit = (Range.dummy "constructor-unitvalue", UTUnitConstant) in
        let (rng, ctornm) = ctor in
        make_standard (Ranged utast1) (Tok rng) (UTApply(optargs, utast1, (rng, UTConstructor(ctornm, utast_unit))))
      }
  | utast=expr_un
      { utast }
;
expr_un:
  | unop=UNOP_EXCLAM; utast2=expr_bot
      {
        let (rng, varnm) = unop in
        make_standard (Tok rng) (Ranged utast2) (UTApply([], (rng, UTContentOf([], varnm)), utast2))
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
      { let (rng, varnm) = ident in (rng, UTContentOf([], varnm)) }
  | long_ident=PATH_LOWER
      { let (rng, modnms, varnm) = long_ident in (rng, UTContentOf(modnms, varnm)) }
  | ic=INTCONST
      { let (rng, n) = ic in (rng, UTIntegerConstant(n)) }
  | fc=FLOATCONST
      { let (rng, r) = fc in (rng, UTFloatConstant(r)) }
  | lc=LENGTHCONST
      { let (rng, r, unitnm) = lc in (rng, UTLengthDescription(r, unitnm)) }
  | rng=TRUE
      { (rng, UTBooleanConstant(true)) }
  | rng=FALSE
      { (rng, UTBooleanConstant(false)) }
  | tok=LITERAL
      {
        let (rng, str, pre, post) = tok in
        make_standard (Tok rng) (Tok rng) (UTStringConstant(omit_spaces pre post str))
      }
  | tok=POSITIONED_LITERAL
      {
        let (rng, ipos, s) = tok in
        make_standard (Tok rng) (Tok rng) (UTPositionedString(ipos, s))
      }
  | tokL=LPAREN; tokR=RPAREN
      { make_standard (Tok tokL) (Tok tokR) UTUnitConstant }
  | LPAREN; utast=expr; RPAREN
      { utast }
  | tokL=LPAREN; utast1=expr; COMMA; utasts=separated_nonempty_list(COMMA, expr); tokR=RPAREN
      {
        match utasts with
        | [] ->
            assert false

        | utast2 :: utast_rest ->
            make_standard (Tok tokL) (Tok tokR) (UTTuple(TupleList.make utast1 utast2 utast_rest))
      }
  | tokL=BLIST; elems=optterm_list(COMMA, expr); tokR=ELIST
      {
        let (_, utast_main) =
          List.fold_right (fun elem tail ->
            make_standard (Ranged elem) (Ranged tail) (UTListCons(elem, tail))
          ) elems (Range.dummy "end-of-list", UTEndOfList)
        in
        make_standard (Tok tokL) (Tok tokR) utast_main
      }
  | tokL=BRECORD; fields=optterm_list(COMMA, record_field); tokR=ERECORD
      { make_standard (Tok tokL) (Tok tokR) (UTRecord(fields)) }
  | tokL=BRECORD; utast=expr_bot; WITH; fields=optterm_nonempty_list(COMMA, record_field); tokR=ERECORD
      {
        let (_, utast_main) =
          fields |> List.fold_left (fun utast1 (rlabel, utast2) ->
            (Range.dummy "update-field", UTUpdateField(utast1, rlabel, utast2))
          ) utast
        in
        make_standard (Tok tokL) (Tok tokR) utast_main
      }

  | opn=BHORZGRP; utast=sxsep; cls=EHORZGRP { make_standard (Tok opn) (Tok cls) (extract_main utast) }
  | opn=BVERTGRP; utast=vxblock; cls=EVERTGRP    { make_standard (Tok opn) (Tok cls) (extract_main utast) }
  | opn=LPAREN; optok=binop; cls=RPAREN          { make_standard (Tok opn) (Tok cls) (UTContentOf([], extract_name optok)) }
  | opn=BMATHGRP; utast=mathblock; cls=EMATHGRP  { make_standard (Tok opn) (Tok cls) (extract_main utast) }
/*
  | opn=OPENMODULE; utast=expr; cls=RPAREN {
      let (rng, mdlnm) = opn in
      make_standard (Tok rng) (Tok cls) (UTOpenIn(rng, mdlnm, utast))
    }
*/
;
record_field:
  | rlabel=LOWER; EXACT_EQ; utast=expr
      { (rlabel, utast) }
;
branch:
  | pat=patas; ARROW; utast=expr
      { UTPatternBranch(pat, utast) }
;
patas:
  | pat=pattr; AS; var=LOWER { make_standard (Ranged pat) (Ranged var) (UTPAsVariable(extract_name var, pat)) }
  | pat=pattr              { pat }
;
pattr:
  | pat1=patbot; CONS; pat2=pattr { make_standard (Ranged pat1) (Ranged pat2) (UTPListCons(pat1, pat2)) }
  | ctor=UPPER; pat=patbot  { make_standard (Ranged ctor) (Ranged pat) (UTPConstructor(extract_name ctor, pat)) }
  | ctor=UPPER              { make_standard (Ranged ctor) (Ranged ctor) (UTPConstructor(extract_name ctor, (Range.dummy "constructor-unit-value", UTPUnitConstant))) }
  | pat=patbot                    { pat }
;
patbot:
  | tok=INTCONST             { make_standard (Ranged tok) (Ranged tok) (UTPIntegerConstant(extract_main tok)) }
  | rng=TRUE                 { make_standard (Tok rng) (Tok rng) (UTPBooleanConstant(true)) }
  | rng=FALSE                { make_standard (Tok rng) (Tok rng) (UTPBooleanConstant(false)) }
  | rng1=LPAREN; rng2=RPAREN { make_standard (Tok rng1) (Tok rng2) UTPUnitConstant }
  | rng=WILDCARD             { make_standard (Tok rng) (Tok rng) UTPWildCard }
  | vartok=bound_identifier  { make_standard (Ranged vartok) (Ranged vartok) (UTPVariable(extract_name vartok)) }
  | lit=LITERAL              { let (rng, str, pre, post) = lit in make_standard (Tok rng) (Tok rng) (UTPStringConstant(omit_spaces pre post str)) }
  | rng1=BLIST; rng2=ELIST            { make_standard (Tok rng1) (Tok rng2) UTPEndOfList }
  | opn=BLIST; pat=patlist; cls=ELIST { make_standard (Tok opn) (Tok cls) (extract_main pat) }
  | opn=LPAREN; pat=patas; cls=RPAREN                       { make_standard (Tok opn) (Tok cls) (extract_main pat) }
  | opn=LPAREN; pat=patas; COMMA; pats=pattuple; cls=RPAREN { let rng = make_range (Tok opn) (Tok cls) in make_product_pattern rng (pat :: pats) }
;
pattuple:
  | ps=separated_nonempty_list(COMMA, patas) { ps }
;
patlist:
  | ps=optterm_nonempty_list(COMMA, patas) {
      List.fold_right (fun pat1 pat2 ->
        make_standard (Ranged pat1) (Ranged pat2) (UTPListCons(pat1, pat2))
      ) ps (Range.dummy "end-of-list-pattern", UTPEndOfList)
    }
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
sxsep:
  | BAR; utastlst=sxlist          { make_cons utastlst }
  | utast=sxblock                 { utast }
  | itmzlst=nonempty_list(sxitem) { make_list_to_itemize itmzlst }
;
sxlist:
  | elems=list(terminated(sxblock, BAR)) { elems }
;
sxitem:
  | item=ITEM; utast=sxblock { let (rng, depth) = item in (rng, depth, utast) }
;
hcmd:
  | tok=HORZCMD        { let (rng, csnm) = tok in (rng, [], csnm) }
  | tok=HORZCMDWITHMOD { tok }
;
mcmd:
  | tok=MATHCMD        { let (rng, csnm) = tok in (rng, [], csnm) }
  | tok=MATHCMDWITHMOD { tok }
;
mathblock:
  | BAR; utmlst=mathlist { utmlst |> List.map (fun utm -> let (rng, _) = utm in (rng, UTMath(utm))) |> make_cons }
  | utm=mathmain         { let (rng, _) = utm in (rng, UTMath(utm)) }
;
mathlist:
  | elems=list(terminated(mathmain, BAR)) { elems }
;
mathmain:
  | utmlst=list(mathtop) {
      let rng =
        match (utmlst, List.rev utmlst) with
        | ([], [])                                -> Range.dummy "empty-math"
        | ((rngfirst, _) :: _, (rnglast, _) :: _) -> Range.unite rngfirst rnglast
        | _                                       -> assert false
      in
      (rng, UTMList(utmlst))
    }
;
mathtop:
  | base=mathbot                                                                     {
    (* a *)
    base
  }
  | base=mathbot;                                         SUPERSCRIPT; sup=mathgroup {
    (* a^p *)
    base
    |> make_sup ~sup ~range:(make_range (Ranged base) (Ranged (snd sup)))
  }
  | base=mathbot;               SUBSCRIPT; sub=mathgroup                             {
    (* a_b *)
    base
    |> make_sub ~sub ~range:(make_range (Ranged base) (Ranged (snd sub)))
  }
  | base=mathbot;               SUBSCRIPT; sub=mathgroup; SUPERSCRIPT; sup=mathgroup {
    (* a_b^p *)
    base
    |> make_sub ~sub ~range:(make_range (Ranged base) (Ranged (snd sub)))
    |> make_sup ~sup ~range:(make_range (Ranged base) (Ranged (snd sup)))
  }
  | base=mathbot;               SUPERSCRIPT; sup=mathgroup; SUBSCRIPT; sub=mathgroup {
    (* a^p_b *)
    base
    |> make_sub ~sub ~range:(Range.dummy "mathtop")
    |> make_sup ~sup ~range:(make_range (Ranged base) (Ranged (snd sub)))
  }
  | base=mathbot; prime=PRIMES                                                       {
    (* a' *)
    base
    |> make_sup ~prime ~range:(make_range (Ranged base) (Tok (fst prime)))
  }
  | base=mathbot; prime=PRIMES;                           SUPERSCRIPT; sup=mathgroup {
    (* a'^p *)
    base
    |> make_sup ~prime ~sup ~range:(make_range (Ranged base) (Ranged (snd sup)))
  }
  | base=mathbot; prime=PRIMES; SUBSCRIPT; sub=mathgroup                             {
    (* a'_b *)
    base
    |> make_sub ~sub ~range:(Range.dummy "mathtop")
    |> make_sup ~prime ~range:(make_range (Ranged base) (Ranged (snd sub)))
  }
  | base=mathbot; prime=PRIMES; SUBSCRIPT; sub=mathgroup; SUPERSCRIPT; sup=mathgroup {
    (* a'_b^p *)
    base
    |> make_sub ~sub ~range:(Range.dummy "mathtop")
    |> make_sup ~prime ~sup ~range:(make_range (Ranged base) (Ranged (snd sup)))
  }
  | base=mathbot; prime=PRIMES; SUPERSCRIPT; sup=mathgroup; SUBSCRIPT; sub=mathgroup {
    (* a'^p_b *)
    base
    |> make_sub ~sub ~range:(Range.dummy "mathtop")
    |> make_sup ~prime ~sup ~range:(make_range (Ranged base) (Ranged (snd sub)))
  }
;
mathgroup:
  | opn=BMATHGRP; utm=mathmain; cls=EMATHGRP {
      (true, make_standard (Tok opn) (Tok cls) (extract_main utm))
    }
  | utm=mathbot {
      (false, utm)
    }
;
mathbot:
  | tok=MATHCHARS {
      let (rng, s) = tok in
      let uchs = InternalText.to_uchar_list (InternalText.of_utf8 s) in
      (rng, UTMChars(uchs))
    }
/*
  | mcmd=mcmd; arglst=list(matharg) {
      let (rngcmd, mdlnmlst, csnm) = mcmd in
      let rnglast =
        match List.rev arglst with
        | []                             -> rngcmd
        | UTCommandArg(_, (rng, _)) :: _ -> rng
      in
      let utastcmd = (rngcmd, UTContentOf(mdlnmlst, csnm)) in
      make_standard (Tok rngcmd) (Tok rnglast) (UTMCommand(utastcmd, arglst))
    }
  | tok=VARINMATH { let (rng, mdlnmlst, varnm) = tok in (rng, UTMEmbed((rng, UTContentOf(mdlnmlst, varnm)))) }
*/
;
/*
matharg:
  | opts=list(mathoptarg); opn=BMATHGRP; utast=mathblock; cls=EMATHGRP {
      UTCommandArg(opts, make_standard (Tok opn) (Tok cls) (extract_main utast))
    }
  | opts=list(mathoptarg); opn=BHORZGRP; utast=sxsep; cls=EHORZGRP {
      UTCommandArg(opts, make_standard (Tok opn) (Tok cls) (extract_main utast))
    }
  | opts=list(mathoptarg); opn=BVERTGRP; utast=vxblock; cls=EVERTGRP {
      UTCommandArg(opts, make_standard (Tok opn) (Tok cls) (extract_main utast))
    }
  | utcmdarg=narg {
      utcmdarg
    }
;
mathoptarg:
  | tok=OPTIONAL; BMATHGRP; utast=mathblock; cls=EMATHGRP {
      let rlabel = failwith "TODO: matharg, rlabel 1" in
      (rlabel, make_standard (Tok tok) (Tok cls) (extract_main utast))
    }
  | tok=OPTIONAL; BHORZGRP; utast=sxsep; cls=EHORZGRP {
      let rlabel = failwith "TODO: matharg, rlabel 2" in
      (rlabel, make_standard (Tok tok) (Tok cls) (extract_main utast))
    }
  | tok=OPTIONAL; BVERTGRP; utast=vxblock; cls=EVERTGRP {
      let rlabel = failwith "TODO: matharg, rlabel 3" in
      (rlabel, make_standard (Tok tok) (Tok cls) (extract_main utast))
    }
;
*/
sxblock:
  | ih=ih { let rng = make_range_from_list ih in (rng, UTInputHorz(ih)) }
;
ih:
  | ihtext=ihtext                     { ihtext :: [] }
  | ihtext=ihtext; ihcmd=ihcmd; ih=ih { ihtext :: ihcmd :: ih }
  | ihcmd=ihcmd; ih=ih                { ihcmd :: ih }
  |                                   { [] }
;
ihcmd:
  | hmacro=HORZMACRO; macargsraw=macroargs {
      let (rngcs, _) = hmacro in
      let (rnglast, macroargs) = macargsraw in
      make_standard (Tok rngcs) (Tok rnglast) (UTInputHorzMacro(hmacro, macroargs))
    }
/*
  | hcmd=hcmd; nargs=list(narg); sargsraw=sargs {
      let (rngcs, mdlnmlst, csnm) = hcmd in
      let utastcmd = (rngcs, UTContentOf(mdlnmlst, csnm)) in
      let (rnglast, sargs) = sargsraw in
      let args = List.append nargs sargs in
      make_standard (Tok rngcs) (Tok rnglast) (UTInputHorzEmbedded(utastcmd, args))
    }
*/
  | opn=BMATHGRP; utast=mathblock; cls=EMATHGRP {
      make_standard (Tok opn) (Tok cls) (UTInputHorzEmbeddedMath(utast))
    }
  | literal=LITERAL {
      let (rng, str, pre, post) = literal in
      make_standard (Tok rng) (Tok rng) (UTInputHorzEmbeddedCodeText(omit_spaces pre post str))
    }
  | vartok=VARINHORZ; cls=ENDACTIVE {
      let (rng, mdlnmlst, varnm) = vartok in
      let utast = (rng, UTContentOf(mdlnmlst, varnm)) in
      make_standard (Tok rng) (Tok cls) (UTInputHorzContent(utast))
    }
;
ihtext:
  | ihcharlst=nonempty_list(ihchar) {
      let rng = make_range_from_list ihcharlst in
      let text = String.concat "" (ihcharlst |> List.map (fun (r, t) -> t)) in
      (rng, UTInputHorzText(text))
    }
;
ihchar:
  | tok=CHAR  { let (rng, ch) = tok in (rng, ch) }
  | rng=SPACE { (rng, " ") }
  | rng=BREAK { (rng, "\n") }
;
macroargs:
  | macnargs=list(macronarg); cls=ENDACTIVE { (cls, macnargs) }
;
macronarg:
  | LPAREN; expr=expr_bot; RPAREN              { UTLateMacroArg(expr) }
  | EXACT_TILDE; LPAREN; expr=expr_bot; RPAREN { UTEarlyMacroArg(expr) }
;
/*
narg:
  | opts=list(noptarg); opn=LPAREN; utast=nxlet; cls=RPAREN {
      UTCommandArg(opts, make_standard (Tok opn) (Tok cls) (extract_main utast))
    }
  | opts=list(noptarg); opn=LPAREN; cls=RPAREN {
      UTCommandArg(opts, make_standard (Tok opn) (Tok cls) UTUnitConstant)
    }
  | opts=list(noptarg); utast=nxrecordsynt {
      UTCommandArg(opts, utast)
    }
  | opts=list(noptarg); utast=nxlistsynt {
      UTCommandArg(opts, utast)
    }
;
noptarg:
  | opn=OPTIONAL; LPAREN; utast=nxlet; cls=RPAREN {
      let rlabel = failwith "TODO: narg, rlabel 1" in
      (rlabel, make_standard (Tok opn) (Tok cls) (extract_main utast))
    }
  | opn=OPTIONAL; LPAREN; cls=RPAREN {
      let rlabel = failwith "TODO: narg, rlabel 2" in
      (rlabel, make_standard (Tok opn) (Tok cls) UTUnitConstant)
    }
  | opn=OPTIONAL; utast=nxrecordsynt {
      let rlabel = failwith "TODO: narg, rlabel 3" in
      (rlabel, make_standard (Tok opn) (Ranged utast) (extract_main utast))
    }
  | opn=OPTIONAL; utast=nxlistsynt {
      let rlabel = failwith "TODO: narg, rlabel 4" in
      (rlabel, make_standard (Tok opn) (Ranged utast) (extract_main utast))
    }
;
*/
sargs:
  | rng=ENDACTIVE             { (rng, []) }
  | sargs=nonempty_list(sarg) {
      let rng =
        match List.rev sargs with
        | []                             -> assert false
        | UTCommandArg(_, (rng, _)) :: _ -> rng
      in
      (rng, sargs)
    }
;

sarg:
  | opn=BVERTGRP; utast=vxblock; cls=EVERTGRP {
      UTCommandArg([], make_standard (Tok opn) (Tok cls) (extract_main utast))
    }
  | opn=BHORZGRP; utast=sxsep; cls=EHORZGRP {
      UTCommandArg([], make_standard (Tok opn) (Tok cls) (extract_main utast))
    }
;
vcmd:
  | tok=VERTCMD        { let (rng, csnm) = tok in (rng, [], csnm) }
  | tok=VERTCMDWITHMOD { tok }
;
vxblock:
  | ivlst=list(vxbot) { (make_range_from_list ivlst, UTInputVert(ivlst)) }
;
vxbot:
/*
  | vcmd=vcmd; nargs=list(narg); sargsraw=sargs {
      let (rngcs, mdlnmlst, csnm) = vcmd in
      let (rnglast, sargs) = sargsraw in
      let args = List.append nargs sargs in
      make_standard (Tok rngcs) (Tok rnglast) (UTInputVertEmbedded((rngcs, UTContentOf(mdlnmlst, csnm)), args))
    }
*/
  | vartok=VARINVERT; cls=ENDACTIVE {
      let (rng, mdlnmlst, varnm) = vartok in
      make_standard (Tok rng) (Tok cls) (UTInputVertContent((rng, UTContentOf(mdlnmlst, varnm))))
    }
  | vmacro=VERTMACRO; macargsraw=macroargs {
      let (rngcs, _) = vmacro in
      let (rnglast, macargs) = macargsraw in
      make_standard (Tok rngcs) (Tok rnglast) (UTInputVertMacro(vmacro, macargs))
  }
;
