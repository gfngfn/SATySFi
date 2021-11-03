%{

  exception IllegalArgumentLength of Range.t * int * int


  let report_bug_parser msg =
    failwith msg;


  open Types

  type literal_reading_state = Normal | ReadingSpace
  type 'a range_kind =
    | Tok        of Range.t
    | Ranged     of (Range.t * 'a)
    | RangedList of (Range.t * 'a) list


  let make_range_from_list (rangedlst : (Range.t * 'a) list) =
    match (rangedlst, List.rev rangedlst) with
    | ([], [])                                -> Range.dummy "empty-input-horz"
    | ((rngfirst, _) :: _, (rnglast, _) :: _) -> Range.unite rngfirst rnglast
    | _                                       -> assert false


  let make_range sttx endx =
    let extract x =
      match x with
      | Tok(rng)          -> rng
      | Ranged((rng, _))  -> rng
      | RangedList(ihlst) -> make_range_from_list ihlst
    in
      Range.unite (extract sttx) (extract endx)


  let rec make_cons utastlst =
    match utastlst with
    | [] -> (Range.dummy "make_cons", UTEndOfList)
    | ((rng, utastmain) as utast) :: tail ->
        let utasttail = make_cons tail in
        let (rngtail, _) = utasttail in
          (Range.unite rng rngtail, UTListCons(utast, utasttail))


  let rec curry_lambda_abstract (optargacc : (label ranged * var_name) Alist.t) (rng : Range.t) (utarglst : untyped_argument list) (utastdef : untyped_abstract_tree) =
    match utarglst with
    | [] ->
        utastdef

    | UTPatternArgument(argpat) :: utargtail ->
        (rng, UTFunction(Alist.to_list optargacc, argpat, curry_lambda_abstract Alist.empty rng utargtail utastdef))

    | UTOptionalArgument(rngvar, varnm) :: utargtail ->
        failwith "TODO: curry_lambda_abstract"
(*
        curry_lambda_abstract (Alist.extend optargacc (rngvar, varnm)) rng utargtail utastdef
*)


  let curry_lambda_abstract_pattern (rng : Range.t) (argpatlst : untyped_pattern_tree list) =
    curry_lambda_abstract Alist.empty rng (argpatlst |> List.map (fun argpat -> UTPatternArgument(argpat)))


  let rec stringify_literal ltrl =
    let (_, ltrlmain) = ltrl in
      match ltrlmain with
      | UTConcat(utastf, utastl) -> (stringify_literal utastf) ^ (stringify_literal utastl)
      | UTStringConstant(s)      -> s
      | UTStringEmpty            -> ""
      | _                        -> report_bug_parser ("stringify_literal; " ^ (Display.string_of_utast ltrl))

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


  let make_standard sttknd endknd main =
    let rng = make_range sttknd endknd in (rng, main)


  let make_letrec_binding (mntyopt : manual_type option) (vartok : Range.t * var_name) (argpatlst : untyped_pattern_tree list) (utastdef : untyped_abstract_tree) : untyped_letrec_binding =
    let (varrng, varnm) = vartok in
    let curried = curry_lambda_abstract_pattern varrng argpatlst utastdef in
    UTLetRecBinding(mntyopt, varrng, varnm, curried)


  let get_range_of_arguments (patlst : untyped_pattern_tree list) : Range.t =
    let rngfirst =
      match patlst with
      | (rngpat, _) :: _ -> rngpat
      | _                -> assert false
    in
    let rnglast =
      match List.rev patlst with
      | (rngpat, _) :: _  -> rngpat
      | []                -> assert false
    in
      make_range (Tok rngfirst) (Tok rnglast)


  let get_range_of_pattern_branch_list (recpatbrs : untyped_letrec_pattern_branch list) : Range.t =
    let rngfirst =
      match recpatbrs with
      | UTLetRecPatternBranch((rngpat, _) :: _, _) :: _ -> rngpat
      | UTLetRecPatternBranch([], _) :: _               -> Range.dummy "get_range_of_pattern_branch_list"
      | []                                              -> assert false
    in
    let rnglast =
      match List.rev recpatbrs with
      | UTLetRecPatternBranch(_, (rngutast, _)) :: _ -> rngutast
      | []                                           -> assert false
    in
      make_range (Tok rngfirst) (Tok rnglast)


  let make_product_pattern (rng : Range.t) (pats : untyped_pattern_tree list) : untyped_pattern_tree =
    match pats with
    | []                      -> assert false
    | pat :: []               -> pat
    | pat1 :: pat2 :: patrest -> (rng, UTPTuple(TupleList.make pat1 pat2 patrest))


  let unite_into_pattern_branch_list (recpatbrs : untyped_letrec_pattern_branch list) : untyped_pattern_branch list * int =
    let (acc, numopt) =
      recpatbrs |> List.fold_left (fun (acc, numprevopt) recpatbr ->
        match recpatbr with
        | UTLetRecPatternBranch(pats, utastdef) ->
            let rngargs = get_range_of_arguments pats in
            let num = List.length pats in
            let numopt =
              match numprevopt with
              | None          -> Some(num)
              | Some(numprev) -> if numprev = num then numprevopt else raise (IllegalArgumentLength(rngargs, num, numprev))
            in
            let patprod = make_product_pattern rngargs pats in
              (Alist.extend acc (UTPatternBranch(patprod, utastdef)), numopt)
      ) (Alist.empty, None)
    in
      match numopt with
      | None            -> assert false  (* -- 'recpatbrs' is not '[]' -- *)
      | Some(numofargs) -> (Alist.to_list acc, numofargs)


  let make_product_expression (rng : Range.t) (utasts : untyped_abstract_tree list) : untyped_abstract_tree =
    match utasts with
    | []                            -> assert false
    | utast :: []                   -> utast
    | utast1 :: utast2 :: utastrest -> (rng, UTTuple(TupleList.make utast1 utast2 utastrest))


  let make_function_for_parallel (rngfull : Range.t) (numofargs : int) (patbrs : untyped_pattern_branch list) =

    let numbered_var_name i = "%pattup" ^ (string_of_int i) in

    let rec aux acc i =
      if i = numofargs then
        let utastprod = make_product_expression (Range.dummy "make_product_expression") (Alist.to_list acc) in
          (rngfull, UTPatternMatch(utastprod, patbrs))
      else
        let varnm = numbered_var_name i in
        let accnew = Alist.extend acc (Range.dummy "make_function_for_parallel:2", UTContentOf([], varnm)) in
        let patvar = (Range.dummy "make_function_for_parallel:3", UTPVariable(varnm)) in
          (rngfull, UTFunction([], patvar, aux accnew (i + 1)))
    in
      aux Alist.empty 0


  let rec make_letrec_binding_from_pattern (mntyopt : manual_type option) (vartok : Range.t * var_name) (recpatbrs : untyped_letrec_pattern_branch list) : untyped_letrec_binding =
    let (varrng, varnm) = vartok in
    let (patbrs, numofargs) = unite_into_pattern_branch_list recpatbrs in
    let rngfull = get_range_of_pattern_branch_list recpatbrs in
    let abs = make_function_for_parallel rngfull numofargs patbrs in
    UTLetRecBinding(mntyopt, varrng, varnm, abs)


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
  AND AS BLOCK COMMAND ELSE END FALSE FUN
  IF IN INLINE LET MOD MATCH MATH MODULE MUTABLE OF OPEN
  REC SIG SIGNATURE STRUCT THEN TRUE TYPE VAL WITH

%token<Range.t> BAR WILDCARD COLON ARROW REVERSED_ARROW ENDACTIVE COMMA CONS ACCESS
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
%type <Types.stage * Types.header_element list * Types.untyped_source_file> main
%type <Types.untyped_binding> bind
%type <Types.untyped_letrec_binding list> bind_value_rec
%type <Types.untyped_let_binding> bind_value_nonrec
%type <Types.untyped_let_binding> bind_inline
%type <Types.untyped_let_binding> bind_block
%type <Types.untyped_type_binding list> bind_type
%type <Types.untyped_type_binding> bind_type_single
%type <Types.untyped_declaration> decl
%type <Types.untyped_abstract_tree> nxlet
%type <Types.untyped_abstract_tree> nxletsub
%type <Types.untyped_abstract_tree> nxif
%type <Types.untyped_abstract_tree> nxop
%type <Types.untyped_abstract_tree> nxapp
%type <Types.untyped_abstract_tree> nxbot
%type <Types.untyped_abstract_tree list> tuple
%type <Range.t * Types.untyped_pattern_branch list> pats
%type <Types.untyped_pattern_tree> patas
%type <Types.untyped_pattern_tree> patbot
%type <Types.untyped_abstract_tree> nxlist
%type <Types.untyped_abstract_tree> sxsep
%type <Types.untyped_abstract_tree> sxblock
%type <Types.untyped_abstract_tree> vxblock
%type <Types.untyped_input_vert_element> vxbot
/*
%type <Types.untyped_command_argument> narg
*/
%type <Types.untyped_command_argument> sarg
%type <Types.untyped_pattern_tree list> argpats
%type <Range.t * Types.var_name> binop
%type <Types.manual_type option * untyped_pattern_tree list> recdecargpart
%type <Types.manual_type option * untyped_argument list> nonrecdecargpart
%type <Range.t * Types.manual_type list * Types.type_name> txapp
%type <Range.t * Types.type_name> txbot
%type <Types.manual_type> txapppre
%type <bool * Types.untyped_math> mathgroup
%type <Types.untyped_math> mathbot
%type <Types.manual_kind> kxtop
%type <Types.manual_row_base_kind> kxrow
%type <Types.type_variable_name Types.ranged> tyquant
%type <Types.row_variable_name ranged * Types.manual_row_base_kind> rowquant

%%

optterm_list(sep, X):
  |   { [] }
  | x=X; sep?
      { [x] }
  | x=X; sep; xs=optterm_nonempty_list(sep, X)
      { x :: xs }

optterm_nonempty_list(sep, X):
  | x=X; sep?
      { [x] }
  | x=X; sep; xs=optterm_nonempty_list(sep, X)
      { x :: xs }

main:
  | stage=stage; header=list(headerelem); lib=main_lib; EOI
      { (stage, header, UTLibraryFile(lib)) }
  | stage=stage; header=list(headerelem); utast=nxif; EOI
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
  | MUTABLE; var=LOWER; REVERSED_ARROW; utast=nxlet
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
bind_inline:
  | ident_ctx=LOWER; cs=HORZCMD; cmdargs=list(arg); EXACT_EQ; utast=nxlet
      {
        let (rng_ctx, varnm_ctx) = ident_ctx in
        let (rng_cs, hcmd) = cs in
        let rng = make_range (Tok rng_ctx) (Ranged utast) in
        let curried = curry_lambda_abstract Alist.empty rng_cs cmdargs utast in
        (None, (rng_cs, UTPVariable(hcmd)), (rng, UTLambdaHorz(rng_ctx, varnm_ctx, curried)))
      }
/*
  | hcmdtok=HORZCMD; argpatlst=argpats; EXACT_EQ; utast=nxlet {
      let (rngcs, hcmd) = hcmdtok in
      let rng = make_range (Tok rngcs) (Ranged utast) in
      let rngctxvar = Range.dummy "context-of-lightweight-let-inline" in
      let ctxvarnm = "%context" in
      let utctx = (rngctxvar, UTContentOf([], ctxvarnm)) in
      let utastread = (Range.dummy "read-inline-of-lightweight-let-inline", UTLexHorz(utctx, utast)) in
      let curried = curry_lambda_abstract_pattern rngcs argpatlst utastread in
      (None, (rngcs, UTPVariable(hcmd)), (rng, UTLambdaHorz(rngctxvar, ctxvarnm, curried)))
    }
*/
;
bind_block:
  | ident_ctx=LOWER; cs=VERTCMD; cmdargs=list(arg); EXACT_EQ; utast=nxlet
      {
        let (rng_ctx, varnm_ctx) = ident_ctx in
        let (rng_cs, vcmd) = cs in
        let rng = make_range (Tok rng_ctx) (Ranged utast) in
        let curried = curry_lambda_abstract Alist.empty rng_cs cmdargs utast in
        (None, (rng_cs, UTPVariable(vcmd)), (rng, UTLambdaVert(rng_ctx, varnm_ctx, curried)))
      }
/*
  | vcmdtok=VERTCMD; argpatlst=argpats; EXACT_EQ; utast=nxlet {
      let (rngcs, vcmd) = vcmdtok in
      let rng = make_range (Tok rngcs) (Ranged utast) in
      let rngctxvar = Range.dummy "context-of-lightweight-let-block" in
      let ctxvarnm = "%context" in
      let utctx = (rngctxvar, UTContentOf([], ctxvarnm)) in
      let utastread = (Range.dummy "read-block-of-lightweight-let-block", UTLexVert(utctx, utast)) in
      let curried = curry_lambda_abstract_pattern rngcs argpatlst utastread in
      (None, (rngcs, UTPVariable(vcmd)), (rng, UTLambdaVert(rngctxvar, ctxvarnm, curried)))
    }
*/
;
bind_math:
  | cs=HORZCMD; cmdargs=list(arg); EXACT_EQ; utast=nxlet
      {
        let (rng_cs, mcmd) = cs in
        let rng = make_range (Tok rng_cs) (Ranged utast) in
        let curried = curry_lambda_abstract Alist.empty rng_cs cmdargs utast in
        (None, (rng_cs, UTPVariable(mcmd)), (rng, UTLambdaMath(curried)))
      }
;
bind_type:
  | ds=separated_nonempty_list(AND, bind_type_single) { ds }
;
bind_type_single:
  | tyident=LOWER; tyvars=list(TYPEVAR); EXACT_EQ; BAR?; ctors=variants {
      (tyident, tyvars, UTBindVariant(ctors))
    }
  | tyident=LOWER; tyvars=list(TYPEVAR); EXACT_EQ; mty=txfunc {
      (tyident, tyvars, UTBindSynonym(mty))
    }
;
variants:
  | vs=separated_nonempty_list(BAR, variant) { vs }
;
variant:
  | ctor=UPPER; OF; ty=txfunc { UTConstructorBranch(ctor, ty) }
  | ctor=UPPER                { UTConstructorBranch(ctor, (Range.dummy "dec-constructor-unit1", MTypeName("unit", []))) }
(* TODO: Fix ad-hoc insertion of the unit type *)
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
  | VAL; ident=LOWER; tyquants=list(tyquant); rowquants=list(rowquant); COLON; mnty=txfunc
      { UTDeclValue(ident, tyquants, rowquants, mnty) }
  | VAL; LPAREN; ident=binop; RPAREN; tyquants=list(tyquant); rowquants=list(rowquant); COLON; mnty=txfunc
      { UTDeclValue(ident, tyquants, rowquants, mnty) }
  | VAL; cs=HORZCMD; tyquants=list(tyquant); rowquants=list(rowquant); COLON; mnty=txfunc
      { UTDeclValue(cs, tyquants, rowquants, mnty) }
  | VAL; cs=VERTCMD; tyquants=list(tyquant); rowquants=list(rowquant); COLON; mnty=txfunc
      { UTDeclValue(cs, tyquants, rowquants, mnty) }
  | TYPE; tyident=LOWER; tyvars=list(TYPEVAR); CONS; mnkd=kxtop
      { UTDeclTypeOpaque(tyident, mnkd) }
  | MODULE; modident=UPPER; COLON; utsig=sigexpr
      { UTDeclModule(modident, utsig) }
  | SIGNATURE; sigident=UPPER; EXACT_EQ; utsig=sigexpr
      { UTDeclSignature(sigident, utsig) }
(* TODO: support other declaration syntax *)
;
tyquant:
  | tyvar=TYPEVAR { tyvar }
;
rowquant:
  | LPAREN; rowvar=ROWVAR; CONS; mnrbkd=kxrow; RPAREN { (rowvar, mnrbkd) }
;
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
nonrecdecargpart:
  | COLON; mty=txfunc                                   { (Some(mty), []) }
/*
  | COLON; mty=txfunc; BAR; utarglst=nonempty_list(arg) { (Some(mty), utarglst) }
  | BAR; utarglst=nonempty_list(arg)                    { (None, utarglst) }
  | utarglst=list(arg)                                  { (None, utarglst) }
*/
;
recdecargpart:
  | COLON; mty=txfunc                                       { (Some(mty), []) }
  | COLON; mty=txfunc; BAR; argpatlst=nonempty_list(patbot) { (Some(mty), argpatlst) }
  | BAR; argpatlst=nonempty_list(patbot)                    { (None, argpatlst) }
  | argpatlst=argpats                                       { (None, argpatlst) }
;
arg:
  | pattr=patbot              { UTPatternArgument(pattr) }
/*
  | OPTIONAL; vartok=defedvar { let (rng, varnm) = vartok in UTOptionalArgument(rng, varnm) }
*/
;
%inline defedvar:
  | ident=LOWER                 { ident }
  | LPAREN; ident=binop; RPAREN { ident }
;
nxrecdecsub:
  | AND; dec=bind_value_rec { dec }
  |                         { [] }
;
bind_value_rec:
  | ident=defedvar; argpart=recdecargpart; EXACT_EQ; utast=nxlet; dec=nxrecdecsub {
      let (mnty_opt, argpats) = argpart in
      let is_all_var =
        argpats |> List.for_all (function
        | (_, UTPVariable(_)) -> true
        | (_, UTPWildCard)    -> true
        | _                   -> false
        )
      in
      if is_all_var then
        make_letrec_binding mnty_opt ident argpats utast :: dec
      else
        make_letrec_binding_from_pattern mnty_opt ident (UTLetRecPatternBranch(argpats, utast) :: []) :: dec
    }
/*
  | vartok=defedvar; argpart=recdecargpart; EXACT_EQ; utastdef=nxlet; BAR; tail=nxrecdecpar; dec=nxrecdecsub {
      let (mntyopt, argpatlst) = argpart in
      make_letrec_binding_from_pattern mntyopt vartok (UTLetRecPatternBranch(argpatlst, utastdef) :: tail) dec
    }
*/
;
nxrecdecpar:
  | patlst=argpats; EXACT_EQ; utast=nxlet; BAR; tail=nxrecdecpar { UTLetRecPatternBranch(patlst, utast) :: tail }
  | patlst=argpats; EXACT_EQ; utast=nxlet                        { UTLetRecPatternBranch(patlst, utast) :: [] }
;
bind_value_nonrec:
  | pat=patbot; argpart=nonrecdecargpart; EXACT_EQ; utast1=nxlet {
      let (mntyopt, utarglst) = argpart in
      let curried = curry_lambda_abstract Alist.empty (get_range pat) utarglst utast1 in
      (mntyopt, pat, curried)
    }
;
kxtop:
  | bkd=kxbase; ARROW kd=kxtop {
      let MKind(bkds_dom, bkd_cod) = kd in
      MKind(bkd :: bkds_dom, bkd_cod)
    }
  | bkd=kxbase {
      MKind([], bkd)
    }
;
kxbase:
  | tok=LOWER { MKindName(tok) }
;
kxrow:
  | BRECORD; rlabel=LOWER; rlabels=list(COMMA; rlabel=LOWER { rlabel }) ERECORD { rlabel :: rlabels }
;
nxlet:
  | tokL=MATCH; utast=nxlet; WITH; BAR?; pats=pats; tokR=END {
      let (_, pmcons) = pats in
      make_standard (Tok tokL) (Tok tokR) (UTPatternMatch(utast, pmcons))
    }
  | letsub=nxletsub { letsub }
;
nxletsub:
  | tok=LET; valbind=bind_value; IN; utast2=nxlet
      { make_standard (Tok tok) (Ranged utast2) (UTLetIn(valbind, utast2)) }
  | tok=LET; OPEN; modident=UPPER; IN; utast=nxlet
      {
        let (rng, modnm) = modident in
        make_standard (Tok tok) (Ranged utast) (UTOpenIn(rng, modnm, utast))
      }
  | utast=nxif
      { utast }
;
nxif:
  | tok=IF; utast0=nxlet; THEN; utast1=nxlet; ELSE; utast2=nxlet
      { make_standard (Tok tok) (Ranged utast2) (UTIfThenElse(utast0, utast1, utast2)) }
  | utast=nxlambda
      { utast }
;
nxlambda:
  | ident=LOWER; REVERSED_ARROW; utast=nxop {
      let (rngvar, varnm) = ident in
      make_standard (Tok rngvar) (Ranged utast) (UTOverwrite(rngvar, varnm, utast))
    }
  | top=FUN; argpatlst=argpats; ARROW; utast=nxop {
      let rng = make_range (Tok top) (Ranged utast) in
      curry_lambda_abstract_pattern rng argpatlst utast
    }
  | utast=nxop { utast }
;
argpats:
  | argpatlst=list(patbot) { argpatlst }
;
nxop:
  | utastL=nxop; op=BINOP_BAR;     utastR=nxop
  | utastL=nxop; op=BINOP_AMP;     utastR=nxop
  | utastL=nxop; op=BINOP_EQ;      utastR=nxop
  | utastL=nxop; op=BINOP_GT;      utastR=nxop
  | utastL=nxop; op=BINOP_LT;      utastR=nxop
  | utastL=nxop; op=BINOP_HAT;     utastR=nxop
  | utastL=nxop; op=BINOP_PLUS;    utastR=nxop
  | utastL=nxop; op=BINOP_MINUS;   utastR=nxop
  | utastL=nxop; op=BINOP_TIMES;   utastR=nxop
  | utastL=nxop; op=BINOP_DIVIDES; utastR=nxop
      { binary_operator utastL op utastR }
  | utastL=nxop; rng=CONS;         utastR=nxop
      { binary_operator utastL (rng, "::") utastR }
  | utastL=nxop; rng=EXACT_MINUS;  utastR=nxop
      { binary_operator utastL (rng, "-") utastR }
  | utastL=nxop; rng=EXACT_TIMES;  utastR=nxop
      { binary_operator utastL (rng, "*") utastR }
  | utastL=nxop; rng=MOD;          utastR=nxop
      { binary_operator utastL (rng, "mod") utastR }
  | tok=EXACT_MINUS; utast2=nxapp
      { make_uminus tok utast2 }
  | constr=UPPER; utast2=nxbot
      {
        make_standard
         (Ranged constr)
         (Ranged utast2)
         (UTConstructor(extract_name constr, utast2))
      }
  | constr=UPPER
      {
        let (rng, constrnm) = constr in
        (rng, UTConstructor(constrnm, (Range.dummy "constructor-unitvalue", UTUnitConstant)))
      }
  | utast=nxapp
      { utast }
;
nxapp:
  | utast1=nxapp; utast2=nxunsub {
      let optargs = failwith "TODO: nxapp, optargs" in
      make_standard (Ranged utast1) (Ranged utast2) (UTApply(optargs, utast1, utast2))
    }
  | utast1=nxapp; constr=UPPER {
      let optargs = failwith "TODO: nxapp, optargs" in
      let (rng, constrnm) = constr in
      make_standard (Ranged utast1) (Tok rng) (UTApply(optargs, utast1, (rng, UTConstructor(constrnm, (Range.dummy "constructor-unitvalue", UTUnitConstant)))))
    }
  | pre=COMMAND; hcmd=hcmd {
      let (rng, mdlnmlst, csnm) = hcmd in
      make_standard (Tok pre) (Tok rng) (UTContentOf(mdlnmlst, csnm))
    }
  | utast=nxunsub                          { utast }
;
nxunsub:
  | unop=UNOP_EXCLAM; utast2=nxbot { let (rng, varnm) = unop in make_standard (Tok rng) (Ranged utast2) (UTApply([], (rng, UTContentOf([], varnm)), utast2)) }
  | tok=EXACT_AMP; utast2=nxbot    { make_standard (Tok tok) (Ranged utast2) (UTNext(utast2)) }
  | tok=EXACT_TILDE; utast2=nxbot  { make_standard (Tok tok) (Ranged utast2) (UTPrev(utast2)) }
  | utast=nxbot                    { utast }
;
nxbot:
  | utast=nxbot; ACCESS; rlabel=LOWER { make_standard (Ranged utast) (Ranged rlabel) (UTAccessField(utast, rlabel)) }
  | var=LOWER                      { let (rng, varnm) = var in (rng, UTContentOf([], varnm)) }
  | vwm=PATH_LOWER               { let (rng, mdlnmlst, varnm) = vwm in (rng, UTContentOf(mdlnmlst, varnm)) }
  | ic=INTCONST                  { make_standard (Ranged ic) (Ranged ic)  (UTIntegerConstant(extract_main ic)) }
  | fc=FLOATCONST                { make_standard (Ranged fc) (Ranged fc) (UTFloatConstant(extract_main fc)) }
  | lc=LENGTHCONST               { let (rng, flt, unitnm) = lc in make_standard (Tok rng) (Tok rng) (UTLengthDescription(flt, unitnm)) }
  | tok=TRUE                     { make_standard (Tok tok) (Tok tok) (UTBooleanConstant(true)) }
  | tok=FALSE                    { make_standard (Tok tok) (Tok tok) (UTBooleanConstant(false)) }
  | opn=LPAREN; cls=RPAREN       { make_standard (Tok opn) (Tok cls) UTUnitConstant }
  | opn=LPAREN; utast=nxlet; cls=RPAREN                   { make_standard (Tok opn) (Tok cls) (extract_main utast) }
  | opn=LPAREN; utast=nxlet; COMMA; tup=tuple; cls=RPAREN { let rng = make_range (Tok opn) (Tok cls) in make_product_expression rng (utast :: tup) }
  | opn=BHORZGRP; utast=sxsep; cls=EHORZGRP      { make_standard (Tok opn) (Tok cls) (extract_main utast) }
  | opn=BVERTGRP; utast=vxblock; cls=EVERTGRP    { make_standard (Tok opn) (Tok cls) (extract_main utast) }
  | tok=LITERAL                                  { let (rng, str, pre, post) = tok in make_standard (Tok rng) (Tok rng) (UTStringConstant(omit_spaces pre post str)) }
  | tok=POSITIONED_LITERAL                       { let (rng, ipos, s) = tok in make_standard (Tok rng) (Tok rng) (UTPositionedString(ipos, s)) }
  | utast=nxlistsynt                             { utast }
  | opn=LPAREN; optok=binop; cls=RPAREN          { make_standard (Tok opn) (Tok cls) (UTContentOf([], extract_name optok)) }
  | utast=nxrecordsynt                           { utast }
  | opn=BMATHGRP; utast=mathblock; cls=EMATHGRP  { make_standard (Tok opn) (Tok cls) (extract_main utast) }
  | opn=OPENMODULE; utast=nxlet; cls=RPAREN {
      let (rng, mdlnm) = opn in
      make_standard (Tok rng) (Tok cls) (UTOpenIn(rng, mdlnm, utast))
    }
;
nxlistsynt:
  | opn=BLIST; cls=ELIST               { make_standard (Tok opn) (Tok cls) UTEndOfList }
  | opn=BLIST; utast=nxlist; cls=ELIST { make_standard (Tok opn) (Tok cls) (extract_main utast) }
;
nxrecordsynt:
  | opn=BRECORD; cls=ERECORD               { make_standard (Tok opn) (Tok cls) (UTRecord([])) }
  | opn=BRECORD; fields=nxrecord; cls=ERECORD { make_standard (Tok opn) (Tok cls) (UTRecord(fields)) }
  | opn=BRECORD; utast=nxbot; WITH; fields=nxrecord; cls=ERECORD {
      let (_, utastmain) =
        fields |> List.fold_left (fun utast1 (rlabel, utast2) ->
          (Range.dummy "update-field", UTUpdateField(utast1, rlabel, utast2))
        ) utast
      in
      make_standard (Tok opn) (Tok cls) utastmain
    }
;
nxrecord:
  | x=optterm_nonempty_list(COMMA, nxrecord_field) { x }
;
nxrecord_field:
  | rlabel=LOWER; EXACT_EQ; utast=nxlet { (rlabel, utast) }
;
nxlist:
  | elems=optterm_nonempty_list(COMMA, nxlet) {
    List.fold_right (fun elem tail ->
      make_standard (Ranged elem) (Ranged tail) (UTListCons(elem, tail)))
      elems (Range.dummy "end-of-list", UTEndOfList)
  }
;
txfunc:
/*
  | QUESTION; LPAREN; mnopts=list(txfuncopt); RPAREN; mntydom=txprod; ARROW; mntycod=txfunc {
      (* TODO: reconsider the concrete syntax *)
      make_standard (Ranged mntydom) (Ranged mntycod) (MFuncType(mnopts, mntydom, mntycod))
    }
*/
  | mnty=txprod { mnty }
;
/*
txfuncopt:
  | OPTIONAL; mnty=txprod {
      let rlabel = failwith "TODO: txfuncopt, rlabel" in
      (rlabel, mnty)
    }
;
*/
txprod:
  | mntys=separated_nonempty_list(EXACT_TIMES, txapppre) {
      match (mntys, List.rev mntys) with
      | ([mnty], [_]) ->
          mnty

      | (mnty1 :: mnty2 :: mntys_rest, mnty_last :: _) ->
          make_standard (Ranged mnty1) (Ranged mnty_last) (MProductType(TupleList.make mnty1 mnty2 mntys_rest))

      | (_, _) ->
          assert false
  }
;
txapppre:
  | tyapp=txapp {
      let (rng, mntyargs, tynm) = tyapp in
      (rng, MTypeName(tynm, mntyargs))
    }
/*
  | opn=BLIST; mntylst=txlist; ELIST; last=HORZCMDTYPE {
      let rng = make_range (Tok opn) (Tok last) in
      (rng, MHorzCommandType(mntylst))
    }
  | opn=BLIST; mntylst=txlist; ELIST; last=VERTCMDTYPE {
      let rng = make_range (Tok opn) (Tok last) in
      (rng, MVertCommandType(mntylst))
    }
  | opn=BLIST; mntylst=txlist; ELIST; last=MATHCMDTYPE {
      let rng = make_range (Tok opn) (Tok last) in
      (rng, MMathCommandType(mntylst))
    }
*/
  | LPAREN; mnty=txfunc; RPAREN { mnty }
  | opn=BRECORD; kvs=txrecord; cls=ERECORD {
      let rng = make_range (Tok opn) (Tok cls) in
      (rng, MRecordType(kvs))
    }
  | tyvar=TYPEVAR {
      let (rng, tyargnm) = tyvar in (rng, MTypeParam(tyargnm))
    }
;
txapp:
  | tybot=txbot; tyapp=txapp {
      let (rng1, tynm) = tybot in
      let mnty = (rng1, MTypeName(tynm, [])) in
      let (rng2, mntytail, tyconstr) = tyapp in
      let rng = make_range (Ranged mnty) (Tok rng2) in
      (rng, mnty :: mntytail, tyconstr)
    }
  | LPAREN; mnty=txfunc; RPAREN; tyapp=txapp {
      let (rng2, lst, tyconstr) = tyapp in
      let rng = make_range (Ranged mnty) (Tok rng2) in
      (rng, mnty :: lst, tyconstr)
    }
  | tyvar=TYPEVAR; tyapp=txapp {
      let (rngtyarg, tyargnm) = tyvar in
      let (rng2, lst, tyconstr) = tyapp in
      let rng = make_range (Tok rngtyarg) (Tok rng2) in
      (rng, (rngtyarg, MTypeParam(tyargnm)) :: lst, tyconstr)
    }
  | tybot=txbot { let (rng, tynm) = tybot in (rng, [], tynm) }
;
txbot:
  | tytok=LOWER        { let (rng, tynm) = tytok in (rng, tynm) }
(*
  | tytok=LOWERWITHMOD { tytok }
*)
;
/*
txlist:
  | ts=optterm_list(COMMA, txlist_elem) { ts }
;
txlist_elem:
  | opts=list(txlist_opt); mnty=txfunc { MArgType(opts, mnty) }
;
txlist_opt:
  | OPTIONAL; mnty=txapppre; COMMA { let rlabel = failwith "TODO: txlist_opt, rlabel" in (rlabel, mnty) }
;
*/
txrecord:
  | fs=optterm_nonempty_list(COMMA, txrecord_elem) { fs }
;
txrecord_elem:
  | rlabel=LOWER; COLON; mnty=txfunc { (rlabel, mnty) }
tuple:
  | x=separated_nonempty_list(COMMA, nxlet) { x }
;
pats:
  | brs=separated_nonempty_list(BAR, pat) {
      match brs with
      | (rnglast, _) :: _ -> (rnglast, List.map snd brs)
      | _                 -> assert false
    }
;
pat:
  | pat=patas; ARROW; utast=nxletsub; {
      let (rnglast, _) = utast in
      (rnglast, UTPatternBranch(pat, utast))
    }
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
  | vartok=defedvar          { make_standard (Ranged vartok) (Ranged vartok) (UTPVariable(extract_name vartok)) }
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
  | LPAREN; expr=nxbot; RPAREN              { UTLateMacroArg(expr) }
  | EXACT_TILDE; LPAREN; expr=nxbot; RPAREN { UTEarlyMacroArg(expr) }
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
