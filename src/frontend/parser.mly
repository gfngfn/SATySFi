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


  let end_header : untyped_abstract_tree = (Range.dummy "end_header", UTFinishHeaderFile)

  let end_struct (rng : Range.t) : untyped_abstract_tree = (rng, UTFinishStruct)


  let rec curry_lambda_abstract (rng : Range.t) (utarglst : untyped_argument list) (utastdef : untyped_abstract_tree) =
    match utarglst with
    | [] ->
        utastdef

    | UTPatternArgument(argpat) :: utargtail ->
        (rng, UTFunction([UTPatternBranch(argpat, curry_lambda_abstract rng utargtail utastdef)]))

    | UTOptionalArgument(rngvar, varnm) :: utargtail ->
        (rng, UTLambdaOptional(rngvar, varnm, curry_lambda_abstract rng utargtail utastdef))


  let curry_lambda_abstract_pattern (rng : Range.t) (argpatlst : untyped_pattern_tree list) =
    curry_lambda_abstract rng (argpatlst |> List.map (fun argpat -> UTPatternArgument(argpat)))


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


  let rec omit_spaces (str_literal_raw : string) =
    let str_literal = omit_post_spaces (omit_pre_spaces str_literal_raw) in
      let min_indent = min_indent_space str_literal in
        let str_shaved = shave_indent str_literal min_indent in
        let len_shaved = String.length str_shaved in
          if len_shaved >= 1 && str_shaved.[len_shaved - 1] = '\n' then
            let str_no_last_break = String.sub str_shaved 0 (len_shaved - 1) in
              UTStringConstant(str_no_last_break)
          else
            UTStringConstant(str_shaved)


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
      (rng, UTApply((Range.dummy "binary_operator", UTApply((rngop, UTContentOf([], opnm)), utastL)), utastR))


  let make_standard sttknd endknd main =
    let rng = make_range sttknd endknd in (rng, main)


  let make_letrec_expression (lettok : Range.t) (utrecbinds : untyped_letrec_binding list) (utastaft : untyped_abstract_tree) =
    make_standard (Tok lettok) (Ranged utastaft) (UTLetRecIn(utrecbinds, utastaft))


  let make_let_expression (lettok : Range.t) ((mntyopt, vartok, utast1) : manual_type option * (Range.t * var_name) * untyped_abstract_tree) (utast2 : untyped_abstract_tree) =
    let (varrng, varnm) = vartok in
    make_standard (Tok lettok) (Ranged utast2) (UTLetNonRecIn(mntyopt, (varrng, UTPVariable(varnm)), utast1, utast2))


  let make_let_expression_of_pattern (lettok : Range.t) ((mntyopt, pat, utarglst, utast1) : untyped_let_binding) (utast2 : untyped_abstract_tree) =
    let curried = curry_lambda_abstract (get_range pat) utarglst utast1 in
    make_standard (Tok lettok) (Ranged utast2) (UTLetNonRecIn(mntyopt, pat, curried, utast2))


  let make_let_mutable_expression
      (letmuttok : Range.t) (vartok : Range.t * var_name)
      (utastdef : untyped_abstract_tree) (utastaft : untyped_abstract_tree)
  : untyped_abstract_tree
  =
    let (varrng, varnm) = vartok in
      make_standard (Tok letmuttok) (Ranged utastaft) (UTLetMutableIn(varrng, varnm, utastdef, utastaft))


  let make_variant_declaration (firsttk : Range.t) (varntdecs : untyped_mutual_variant_cons) (utastaft : untyped_abstract_tree) : untyped_abstract_tree =
    make_standard (Tok firsttk) (Ranged utastaft) (UTDeclareVariantIn(varntdecs, utastaft))


  let make_letrec_binding
      (mntyopt : manual_type option)
      (vartok : Range.t * var_name) (argpatlst : untyped_pattern_tree list) (utastdef : untyped_abstract_tree)
      (tailcons : untyped_letrec_binding list)
  : untyped_letrec_binding list
  =
    let (varrng, varnm) = vartok in
    let curried = curry_lambda_abstract_pattern varrng argpatlst utastdef in
      (UTLetRecBinding(mntyopt, varnm, curried)) :: tailcons


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


  let make_product_pattern (rngfull : Range.t) (patlst : untyped_pattern_tree list) : untyped_pattern_tree =
    let rec aux patlst =
      match patlst with
      | []             -> (Range.dummy "make_product_pattern:1", UTPEndOfTuple)
      | pat :: pattail -> (Range.dummy "make_product_pattern:2", UTPTupleCons(pat, aux pattail))
    in
    let (_, patprodmain) = aux patlst in
      (rngfull, patprodmain)


  let unite_into_pattern_branch_list (recpatbrs : untyped_letrec_pattern_branch list) : untyped_pattern_branch list * int =
    let (acc, numopt) =
      recpatbrs |> List.fold_left (fun (acc, numprevopt) recpatbr ->
        match recpatbr with
        | UTLetRecPatternBranch(patlst, utastdef) ->
            let rngargs = get_range_of_arguments patlst in
            let num = List.length patlst in
            let numopt =
              match numprevopt with
              | None          -> Some(num)
              | Some(numprev) -> if numprev = num then numprevopt else raise (IllegalArgumentLength(rngargs, num, numprev))
            in
            let patprod = make_product_pattern rngargs patlst in
              (Alist.extend acc (UTPatternBranch(patprod, utastdef)), numopt)
      ) (Alist.empty, None)
    in
      match numopt with
      | None            -> assert false  (* -- 'recpatbrs' is not '[]' -- *)
      | Some(numofargs) -> (Alist.to_list acc, numofargs)


  let make_product_expression (utastlst : untyped_abstract_tree list) : untyped_abstract_tree =
    let rngdummy = Range.dummy "make_product_expression" in
    let rec aux utastprod utastlstrev =
      match utastlstrev with
      | []            -> utastprod
      | utast :: rest -> aux (rngdummy, UTTupleCons(utast, utastprod)) rest
    in
      aux (rngdummy, UTEndOfTuple) (List.rev utastlst)


  let make_function_for_parallel (rngfull : Range.t) (numofargs : int) (patbrs : untyped_pattern_branch list) =

    let numbered_var_name i = "%pattup" ^ (string_of_int i) in

    let rec aux acc i =
      if i = numofargs then
        let utastprod = make_product_expression (Alist.to_list acc) in
          (Range.dummy "make_function_for_parallel:1", UTPatternMatch(utastprod, patbrs))
      else
        let varnm = numbered_var_name i in
        let accnew = Alist.extend acc (Range.dummy "make_function_for_parallel:2", UTContentOf([], varnm)) in
        let patvar = (Range.dummy "make_function_for_parallel:3", UTPVariable(varnm)) in
          (Range.dummy "make_function_for_parallel:4", UTFunction([UTPatternBranch(patvar, aux accnew (i + 1))]))
    in
      aux Alist.empty 0


  let rec make_letrec_binding_from_pattern
      (mntyopt : manual_type option)
      (vartok : Range.t * var_name) (recpatbrs : untyped_letrec_pattern_branch list)
      (tailcons : untyped_letrec_binding list)
  : untyped_letrec_binding list
  =
    let (_, varnm) = vartok in
    let (patbrs, numofargs) = unite_into_pattern_branch_list recpatbrs in
    let rngfull = get_range_of_pattern_branch_list recpatbrs in
    let abs = make_function_for_parallel rngfull numofargs patbrs in
      (UTLetRecBinding(mntyopt, varnm, abs)) :: tailcons


  let kind_type_arguments (uktyargs : untyped_unkinded_type_argument list) (constrntcons : constraints) : untyped_type_argument list =
    uktyargs |> List.map (fun (rng, tyvarnm) ->
      match List.assoc_opt tyvarnm constrntcons with
      | Some(mkd) -> (rng, tyvarnm, mkd)
      | None      -> (rng, tyvarnm, MUniversalKind)
    )


  let make_mutual_variant_cons (uktyargs : untyped_unkinded_type_argument list) (tynmtk : Range.t * type_name) (constrdecs : untyped_constructor_dec list) (constrnts : constraints) (tailcons : untyped_mutual_variant_cons) =
    let tynm = extract_name tynmtk in
    let tynmrng = get_range tynmtk in
    let tyargcons = kind_type_arguments uktyargs constrnts in
      UTMutualVariantCons(tyargcons, tynmrng, tynm, constrdecs, tailcons)


  let make_mutual_synonym_cons (uktyargs : untyped_unkinded_type_argument list) (tynmtk : Range.t * type_name) (mnty : manual_type) (constrnts : constraints) (tailcons : untyped_mutual_variant_cons) =
    let tynm = extract_name tynmtk in
    let tynmrng = get_range tynmtk in
    let tyargcons = kind_type_arguments uktyargs constrnts in
      UTMutualSynonymCons(tyargcons, tynmrng, tynm, mnty, tailcons)

  let make_module
      (firsttk : Range.t) (mdlnmtk : Range.t * module_name) (msigopt : (manual_signature_content list) option)
      (utastdef : untyped_abstract_tree) (utastaft : untyped_abstract_tree)
  : untyped_abstract_tree
  =
    let mdlrng = make_range (Tok firsttk) (Ranged utastdef) in
    let mdlnm = extract_name mdlnmtk in
      make_standard (Tok firsttk) (Ranged utastaft) (UTModule(mdlrng, mdlnm, msigopt, utastdef, utastaft))


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
          raise (ParseErrorDetail("syntax error: illegal item depth "
            ^ (string_of_int depth) ^ " after " ^ (string_of_int crrntdp) ^ "\n"
            ^ "    " ^ (Range.to_string rng)))

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


  let report_error rngknd (tok : string) =
    match rngknd with
    | Tok(rng) ->
          raise (ParseErrorDetail(
            "syntax error:\n"
            ^ "    unexpected token after '" ^ tok ^ "'\n"
            ^ "    " ^ (Range.to_string rng)))
    | Ranged((rng, nm)) ->
          raise (ParseErrorDetail(
            "syntax error:\n"
            ^ "    unexpected token after '" ^ nm ^ "'\n"
            ^ "    " ^ (Range.to_string rng)))
    | _ -> assert false

%}

%token <Range.t * Types.var_name> VAR
%token <Range.t * Types.ctrlseq_name> HORZCMD
%token <Range.t * Types.ctrlseq_name> VERTCMD
%token <Range.t * Types.ctrlseq_name> MATHCMD
%token <Range.t * (Types.module_name list) * Types.var_name> VARWITHMOD
%token <Range.t * (Types.module_name list) * Types.ctrlseq_name> HORZCMDWITHMOD
%token <Range.t * (Types.module_name list) * Types.ctrlseq_name> VERTCMDWITHMOD
%token <Range.t * (Types.module_name list) * Types.ctrlseq_name> MATHCMDWITHMOD
%token <Range.t * (Types.module_name list) * Types.ctrlseq_name> VARINHORZ
%token <Range.t * (Types.module_name list) * Types.ctrlseq_name> VARINVERT
%token <Range.t * (Types.module_name list) * Types.var_name> VARINMATH
%token <Range.t * Types.var_name> TYPEVAR
%token <Range.t * Types.constructor_name> CONSTRUCTOR
%token <Range.t * int> INTCONST
%token <Range.t * float> FLOATCONST
%token <Range.t * float * Types.length_unit_name> LENGTHCONST
%token <Range.t * string> CHAR
%token <Range.t> SPACE BREAK
%token <Range.t * string> MATHCHAR
%token <Range.t> SUBSCRIPT SUPERSCRIPT
%token <Range.t> LAMBDA ARROW COMMAND
%token <Range.t> LETREC LETNONREC DEFEQ LETAND IN
%token <Range.t> MODULE STRUCT END DIRECT DOT SIG VAL CONSTRAINT
%token <Range.t> TYPE OF MATCH WITH BAR WILDCARD WHEN AS COLON
%token <Range.t> LETMUTABLE OVERWRITEEQ
%token <Range.t> LETHORZ LETVERT LETMATH
%token <Range.t> DEREF
%token <Range.t> IF THEN ELSE
%token <Range.t * Types.var_name> BINOP_TIMES BINOP_DIVIDES BINOP_PLUS BINOP_MINUS
%token <Range.t * Types.var_name> BINOP_HAT BINOP_AMP BINOP_BAR BINOP_GT BINOP_LT BINOP_EQ
%token <Range.t> EXACT_MINUS EXACT_TIMES MOD BEFORE LNOT
%token <Range.t> LPAREN RPAREN
%token <Range.t> BVERTGRP EVERTGRP
%token <Range.t> BHORZGRP EHORZGRP
%token <Range.t> BMATHGRP EMATHGRP
%token <Range.t> OPENQT CLOSEQT
%token <Range.t> OPENVERT CLOSEVERT
%token <Range.t> OPENHORZ CLOSEHORZ
%token <Range.t> OPENPROG CLOSEPROG
%token <Range.t> OPENMATH CLOSEMATH
%token <Range.t> BPATH EPATH PATHLINE PATHCURVE CONTROLS CYCLE
%token <Range.t> TRUE FALSE
%token <Range.t> SEP ENDACTIVE COMMA
%token <Range.t> BLIST LISTPUNCT ELIST CONS BRECORD ERECORD ACCESS
%token <Range.t> OPENPROG_AND_BRECORD CLOSEPROG_AND_ERECORD OPENPROG_AND_BLIST CLOSEPROG_AND_ELIST
%token <Range.t> WHILE DO
%token <Range.t> HORZCMDTYPE VERTCMDTYPE MATHCMDTYPE
%token <Range.t> OPTIONAL OMISSION OPTIONALTYPE
(*
%token <Range.t> NEWGLOBALHASH OVERWRITEGLOBALHASH RENEWGLOBALHASH
*)
%token <Range.t * int> ITEM
%token <Range.t> HEADER_REQUIRE HEADER_IMPORT
%token <Range.t * string> HEADER_CONTENT
%token EOI

(*
%nonassoc LET DEFEQ IN LETAND LETMUTABLE OVERWRITEEQ
%nonassoc MATCH WITH
%nonassoc IF THEN ELSE
*)
(*
%left OVERWRITEGLOBALHASH
*)

%left BEFORE
(*
%nonassoc WHILE
*)
%left BINOP_BAR
%left BINOP_AMP
(*
%nonassoc LNOT
*)
%left BINOP_EQ
%left BINOP_GT BINOP_LT
%right CONS
%left BINOP_PLUS
%left BINOP_MINUS EXACT_MINUS
%left BINOP_TIMES EXACT_TIMES
%right MOD BINOP_DIVIDES
(*
%nonassoc VAR
%nonassoc LPAREN RPAREN
*)

%start main
%type <Types.header_element list * Types.untyped_abstract_tree> main
%type <Types.untyped_abstract_tree> nxlet
%type <Types.untyped_abstract_tree> nxletsub
%type <Types.untyped_letrec_binding list> nxrecdec
%type <Types.untyped_let_binding> nxnonrecdec
%type <Types.untyped_abstract_tree> nxbfr
%type <Types.untyped_abstract_tree> nxwhl
%type <Types.untyped_abstract_tree> nxif
%type <Types.untyped_abstract_tree> nxlor
%type <Types.untyped_abstract_tree> nxland
%type <Types.untyped_abstract_tree> nxcomp
%type <Types.untyped_abstract_tree> nxconcat
%type <Types.untyped_abstract_tree> nxlplus
%type <Types.untyped_abstract_tree> nxltimes
%type <Types.untyped_abstract_tree> nxrplus
%type <Types.untyped_abstract_tree> nxrtimes
%type <Types.untyped_abstract_tree> nxun
%type <Types.untyped_abstract_tree> nxapp
%type <Types.untyped_abstract_tree> nxbot
%type <Types.untyped_abstract_tree> tuple
%type <Range.t * Types.untyped_pattern_branch list> pats
%type <Types.untyped_pattern_tree> patas
%type <Types.untyped_pattern_tree> patbot
%type <Types.untyped_abstract_tree> nxlist
%type <Types.untyped_abstract_tree> sxsep
%type <Types.untyped_abstract_tree> sxblock
%type <Types.untyped_abstract_tree> vxblock
%type <Types.untyped_input_vert_element> vxbot
%type <Types.var_name * Types.manual_kind> constrnt
%type <Types.constraints> constrnts
(*
%type <Types.untyped_abstract_tree> sxclsnm
%type <Types.untyped_abstract_tree> sxidnm
*)
%type <Types.untyped_command_argument> narg
%type <Types.untyped_command_argument> sarg
%type <Types.untyped_pattern_tree list> argpats
%type <Range.t * Types.var_name> binop
%type <Types.untyped_unkinded_type_argument list> xpltyvars
%type <Types.manual_type option * untyped_pattern_tree list> recdecargpart
%type <Types.manual_type option * untyped_argument list> nonrecdecargpart
%type <Range.t * Types.type_name> txbot

%%


main:
  | header=list(headerelem); utast=nxtoplevel    { (header, utast) }
  | header=list(headerelem); utast=vxblock; EOI  { (header, utast) }
  | header=list(headerelem); utast=nxwhl; EOI    { (header, utast) }
;
headerelem:
  | HEADER_REQUIRE; content=HEADER_CONTENT { let (_, s) = content in HeaderRequire(s) }
  | HEADER_IMPORT; content=HEADER_CONTENT  { let (_, s) = content in HeaderImport(s) }
;
nxtoplevel:
  | top=LETREC; recdec=nxrecdec; subseq=nxtopsubseq                          { make_letrec_expression top recdec subseq }
  | top=LETNONREC; nonrecdec=nxnonrecdec; subseq=nxtopsubseq                 { make_let_expression_of_pattern top nonrecdec subseq }
  | top=LETMUTABLE; vartok=VAR; OVERWRITEEQ; utast=nxlet; subseq=nxtopsubseq { make_let_mutable_expression top vartok utast subseq }
  | top=LETHORZ; dec=nxhorzdec; subseq=nxtopsubseq                           { make_let_expression top dec subseq }
  | top=LETVERT; dec=nxvertdec; subseq=nxtopsubseq                           { make_let_expression top dec subseq }
  | top=LETMATH; dec=nxmathdec; subseq=nxtopsubseq                           { make_let_expression top dec subseq }
  | top=TYPE; variantdec=nxvariantdec; subseq=nxtopsubseq                    { make_variant_declaration top variantdec subseq }
  | top=MODULE; mdlnmtok=CONSTRUCTOR; sigopt=nxsigopt;
      DEFEQ; STRUCT; strct=nxstruct; subseq=nxtopsubseq                      { make_module top mdlnmtok sigopt strct subseq }
;
nxtopsubseq:
  | utast=nxtoplevel     { utast }
  | EOI                  { end_header }
  | IN; utast=nxlet; EOI { utast }
;
nxsigopt:
  |                                     { None }
  | COLON; SIG; sg=list(nxsigelem); END { Some(sg) }
;
nxsigelem:
  | TYPE; tyvarlst=xpltyvars; tytok=VAR; clst=constrnts         { let (_, tynm) = tytok in (SigType(kind_type_arguments tyvarlst clst, tynm)) }
  | VAL; vartok=VAR; COLON; mnty=txfunc; clst=constrnts         { let (_, varnm) = vartok in (SigValue(varnm, mnty, clst)) }
  | VAL; hcmdtok=HORZCMD; COLON; mnty=txfunc; clst=constrnts    { let (_, csnm) = hcmdtok in (SigValue(csnm, mnty, clst)) }
  | VAL; vcmdtok=VERTCMD; COLON; mnty=txfunc; clst=constrnts    { let (_, csnm) = vcmdtok in (SigValue(csnm, mnty, clst)) }
  | DIRECT; hcmdtok=HORZCMD; COLON; mnty=txfunc; clst=constrnts { let (_, csnm) = hcmdtok in (SigDirect(csnm, mnty, clst)) }
  | DIRECT; vcmdtok=VERTCMD; COLON; mnty=txfunc; clst=constrnts { let (_, csnm) = vcmdtok in (SigDirect(csnm, mnty, clst)) }
;
constrnts:
  | clst=list(constrnt) { clst }
;
constrnt:
  | CONSTRAINT; tyvar=TYPEVAR; CONS; mnkd=kxtop { let (_, tyvarnm) = tyvar in (tyvarnm, mnkd) }
;
nxstruct:
  | endtok=END                                                       { end_struct endtok }
  | top=LETREC; recdec=nxrecdec; tail=nxstruct                       { make_letrec_expression top recdec tail }
  | top=LETNONREC; nonrecdec=nxnonrecdec; tail=nxstruct              { make_let_expression_of_pattern top nonrecdec tail }
  | top=LETMUTABLE; var=VAR; OVERWRITEEQ; utast=nxlet; tail=nxstruct { make_let_mutable_expression top var utast tail }
  | top=LETHORZ; dec=nxhorzdec; tail=nxstruct                        { make_let_expression top dec tail }
  | top=LETVERT; dec=nxvertdec; tail=nxstruct                        { make_let_expression top dec tail }
  | top=LETMATH; dec=nxmathdec; tail=nxstruct                        { make_let_expression top dec tail }
  | top=TYPE; varntdec=nxvariantdec; tail=nxstruct                   { make_variant_declaration top varntdec tail }
  | top=MODULE; tok=CONSTRUCTOR; sigopt=nxsigopt;
      DEFEQ; STRUCT; strct=nxstruct; tail=nxstruct                   { make_module top tok sigopt strct tail }
;
nxhorzdec:
  | ctxvartok=VAR; hcmdtok=HORZCMD; cmdarglst=list(arg); DEFEQ; utast=nxlet {
      let (rngcs, _) = hcmdtok in
      let (rngctxvar, ctxvarnm) = ctxvartok in
      let rng = make_range (Tok rngctxvar) (Ranged utast) in
      let curried = curry_lambda_abstract rngcs cmdarglst utast in
        (None, hcmdtok, (rng, UTLambdaHorz(rngctxvar, ctxvarnm, curried)))
      }
  | hcmdtok=HORZCMD; argpatlst=argpats; DEFEQ; utast=nxlet {
      let (rngcs, _) = hcmdtok in
      let rng = make_range (Tok rngcs) (Ranged utast) in
      let rngctxvar = Range.dummy "context-of-lightweight-let-inline" in
      let ctxvarnm = "%context" in
      let utctx = (rngctxvar, UTContentOf([], ctxvarnm)) in
      let utastread = (Range.dummy "read-inline-of-lightweight-let-inline", UTLexHorz(utctx, utast)) in
      let curried = curry_lambda_abstract_pattern rngcs argpatlst utastread in
        (None, hcmdtok, (rng, UTLambdaHorz(rngctxvar, ctxvarnm, curried)))
      }
;
nxvertdec:
  | ctxvartok=VAR; vcmdtok=VERTCMD; cmdarglst=list(arg); DEFEQ; utast=nxlet {
      let (rngcs, _) = vcmdtok in
      let (rngctxvar, ctxvarnm) = ctxvartok in
      let rng = make_range (Tok rngctxvar) (Ranged utast) in
      let curried = curry_lambda_abstract rngcs cmdarglst utast in
        (None, vcmdtok, (rng, UTLambdaVert(rngctxvar, ctxvarnm, curried)))
      }
  | vcmdtok=VERTCMD; argpatlst=argpats; DEFEQ; utast=nxlet {
      let (rngcs, _) = vcmdtok in
      let rng = make_range (Tok rngcs) (Ranged utast) in
      let rngctxvar = Range.dummy "context-of-lightweight-let-block" in
      let ctxvarnm = "%context" in
      let utctx = (rngctxvar, UTContentOf([], ctxvarnm)) in
      let utastread = (Range.dummy "read-block-of-lightweight-let-block", UTLexVert(utctx, utast)) in
      let curried = curry_lambda_abstract_pattern rngcs argpatlst utastread in
        (None, vcmdtok, (rng, UTLambdaVert(rngctxvar, ctxvarnm, curried)))
      }
;
nxmathdec:
  | mcmdtok=HORZCMD; argpatlst=argpats; DEFEQ; utast=nxlet {
      let (rngcs, _) = mcmdtok in
      let rng = make_range (Tok rngcs) (Ranged utast) in
      let curried = curry_lambda_abstract_pattern rngcs argpatlst utast in
        (None, mcmdtok, (rng, UTLambdaMath(curried)))
      }
;
nonrecdecargpart:
  | COLON; mty=txfunc                                   { (Some(mty), []) }
  | COLON; mty=txfunc; BAR; utarglst=nonempty_list(arg) { (Some(mty), utarglst) }
  | BAR; utarglst=nonempty_list(arg)                    { (None, utarglst) }
  | utarglst=list(arg)                                  { (None, utarglst) }
;
recdecargpart:
  | COLON; mty=txfunc                                       { (Some(mty), []) }
  | COLON; mty=txfunc; BAR; argpatlst=nonempty_list(patbot) { (Some(mty), argpatlst) }
  | BAR; argpatlst=nonempty_list(patbot)                    { (None, argpatlst) }
  | argpatlst=argpats                                       { (None, argpatlst) }
;
arg:
  | pattr=patbot              { UTPatternArgument(pattr) }
  | OPTIONAL; vartok=defedvar { let (rng, varnm) = vartok in UTOptionalArgument(rng, varnm) }
;
%inline defedvar:
  | vartok=VAR                  { vartok }
  | LPAREN; optok=binop; RPAREN { optok }
;
nxrecdecsub:
  | LETAND; dec=nxrecdec { dec }
  |                      { [] }
;
nxrecdec:
  | vartok=defedvar; argpart=recdecargpart; DEFEQ; utastdef=nxlet; dec=nxrecdecsub {
        let (mntyopt, argpatlst) = argpart in
          make_letrec_binding mntyopt vartok argpatlst utastdef dec
      }
  | vartok=defedvar; argpart=recdecargpart; DEFEQ; utastdef=nxlet; BAR; tail=nxrecdecpar; dec=nxrecdecsub {
        let (mntyopt, argpatlst) = argpart in
          make_letrec_binding_from_pattern mntyopt vartok (UTLetRecPatternBranch(argpatlst, utastdef) :: tail) dec
      }
;
nxrecdecpar:
  | patlst=argpats; DEFEQ; utast=nxlet; BAR; tail=nxrecdecpar { UTLetRecPatternBranch(patlst, utast) :: tail }
  | patlst=argpats; DEFEQ; utast=nxlet                        { UTLetRecPatternBranch(patlst, utast) :: [] }
;
nxnonrecdec:
  | pat=patbot; argpart=nonrecdecargpart; DEFEQ; utastdef=nxlet {
        let (mntyopt, utarglst) = argpart in
          (mntyopt, pat, utarglst, utastdef)
      }
;
nxvariantdec: /* -> untyped_mutual_variant_cons */
  | xpltyvars VAR DEFEQ variants constrnts LETAND nxvariantdec     { make_mutual_variant_cons $1 $2 $4 $5 $7 }
  | xpltyvars VAR DEFEQ variants constrnts                         { make_mutual_variant_cons $1 $2 $4 $5 UTEndOfMutualVariant }
  | xpltyvars VAR DEFEQ BAR variants constrnts LETAND nxvariantdec { make_mutual_variant_cons $1 $2 $5 $6 $8 }
  | xpltyvars VAR DEFEQ BAR variants constrnts                     { make_mutual_variant_cons $1 $2 $5 $6 UTEndOfMutualVariant }
  | xpltyvars VAR DEFEQ txfunc constrnts LETAND nxvariantdec       { make_mutual_synonym_cons $1 $2 $4 $5 $7 }
  | xpltyvars VAR DEFEQ txfunc constrnts                           { make_mutual_synonym_cons $1 $2 $4 $5 UTEndOfMutualVariant }
;
xpltyvars:
  | tyvarlst=list(TYPEVAR) { tyvarlst }
;
kxtop:
  | BRECORD; tyrcd=txrecord; ERECORD { MRecordKind(Assoc.of_list tyrcd) }
;
nxlet:
  | tok=MATCH; utast=nxlet; WITH; option(BAR { () }); pats=pats {
        let (lastrng, pmcons) = pats in make_standard (Tok tok) (Tok lastrng) (UTPatternMatch(utast, pmcons))
      }
  | nxletsub { $1 }
;
nxletsub:
  | tok=LETREC; recdec=nxrecdec; IN; utast=nxlet                         { make_letrec_expression tok recdec utast }
  | tok=LETNONREC; nonrecdec=nxnonrecdec; IN; utast=nxlet                { make_let_expression_of_pattern tok nonrecdec utast }
  | tok=LETMUTABLE; var=VAR; OVERWRITEEQ; utast1=nxlet; IN; utast2=nxlet { make_let_mutable_expression tok var utast1 utast2 }
  | tok=LETMATH; dec=nxmathdec; IN; utast=nxlet                          { make_let_expression tok dec utast }
  | utast=nxwhl { utast }
;
nxwhl:
  | tok=WHILE utast1=nxlet; DO; utast2=nxwhl {
        make_standard (Tok tok) (Ranged utast2) (UTWhileDo(utast1, utast2))
      }
  | utast=nxif { utast }
nxif:
  | tok=IF; utast0=nxlet; THEN; utast1=nxlet; ELSE; utast2=nxlet {
        make_standard (Tok tok) (Ranged utast2) (UTIfThenElse(utast0, utast1, utast2))
      }
  | utast=nxbfr { utast }
;
nxbfr:
  | utast1=nxlambda; BEFORE; utast2=nxbfr { make_standard (Ranged utast1) (Ranged utast2) (UTSequential(utast1, utast2)) }
  | utast=nxlambda { utast }
;
nxlambda:
  | vartok=VAR; OVERWRITEEQ; utast=nxlor {
        let (rngvar, varnm) = vartok in
          make_standard (Tok rngvar) (Ranged utast) (UTOverwrite(rngvar, varnm, utast))
      }
  | top=LAMBDA; argpatlst=argpats; ARROW; utast=nxlor {
        let rng = make_range (Tok top) (Ranged utast) in curry_lambda_abstract_pattern rng argpatlst utast
      }
  | utast=nxlor { utast }
;
argpats: /* -> argument_variable_cons */
  | argpatlst=list(patbot) { argpatlst }
;
nxlor:
  | nxlor BINOP_BAR nxland { binary_operator $1 $2 $3 }
(*
  | nxlor PATHLINE nxland  { binary_operator $1 ($2, "--") $3 }
*)
  | nxland                 { $1 }
;
nxland:
  | nxland BINOP_AMP nxcomp  { binary_operator $1 $2 $3 }
  | nxcomp                   { $1 }
;
nxcomp:
  | nxconcat BINOP_EQ nxcomp { binary_operator $1 $2 $3 }
  | nxconcat BINOP_GT nxcomp { binary_operator $1 $2 $3 }
  | nxconcat BINOP_LT nxcomp { binary_operator $1 $2 $3 }
  | nxconcat                 { $1 }
;
nxconcat:
  | nxlplus BINOP_HAT nxconcat { binary_operator $1 $2 $3 }
  | nxlplus CONS nxconcat      { binary_operator $1 ($2, "::") $3 }
  | nxlplus                    { $1 }
;
nxlplus:
  | nxlminus BINOP_PLUS nxrplus { binary_operator $1 $2 $3 }
  | nxlminus                    { $1 }
;
nxlminus:
  | utastL=nxlplus; op=BINOP_MINUS;  utastR=nxrtimes { binary_operator utastL op utastR }
  | utastL=nxlplus; rng=EXACT_MINUS; utastR=nxrtimes { binary_operator utastL (rng, "-") utastR }
  | utast=nxltimes                                   { utast }
;
nxrplus:
  | utastL=nxrminus; op=BINOP_PLUS; utastR=nxrplus { binary_operator utastL op utastR }
  | utast=nxrminus                                 { utast }
;
nxrminus:
  | utastL=nxrplus; op=BINOP_MINUS; utastR=nxrtimes  { binary_operator utastL op utastR }
  | utastL=nxrplus; rng=EXACT_MINUS; utastR=nxrtimes { binary_operator utastL (rng, "-") utastR }
  | utast=nxrtimes                                   { utast }
;
nxltimes:
  | utastL=nxun; op=BINOP_TIMES;   utastR=nxrtimes { binary_operator utastL op utastR }
  | utastL=nxun; rng=EXACT_TIMES;  utastR=nxrtimes { binary_operator utastL (rng, "*") utastR }
  | utastL=nxun; op=BINOP_DIVIDES; utastR=nxrtimes { binary_operator utastL op utastR }
  | utastL=nxun; rng=MOD;          utastR=nxrtimes { binary_operator utastL (rng, "mod") utastR }
  | utast=nxun                                     { utast }
;
nxrtimes:
  | utastL=nxapp; op=BINOP_TIMES;   utastR=nxrtimes { binary_operator utastL op utastR }
  | utastL=nxapp; rng=EXACT_TIMES;  utastR=nxrtimes { binary_operator utastL (rng, "*") utastR }
  | utastL=nxapp; op=BINOP_DIVIDES; utastR=nxrtimes { binary_operator utastL op utastR }
  | utastL=nxapp; rng=MOD;          utastR=nxrtimes { binary_operator utastL (rng, "mod") utastR }
  | utast=nxapp                                     { utast }
;
nxun:
  | tok=EXACT_MINUS; utast2=nxapp    { binary_operator (Range.dummy "zero-of-unary-minus", UTIntegerConstant(0)) (tok, "-") utast2 }
  | tok=LNOT; utast2=nxapp           { make_standard (Tok tok) (Ranged utast2) (UTApply((tok, UTContentOf([], "not")), utast2)) }
  | constr=CONSTRUCTOR; utast2=nxbot { make_standard (Ranged constr) (Ranged utast2) (UTConstructor(extract_name constr, utast2)) }
  | constr=CONSTRUCTOR               { let (rng, constrnm) = constr in (rng, UTConstructor(constrnm, (Range.dummy "constructor-unitvalue", UTUnitConstant))) }
  | utast=nxapp                      { utast }
;
nxapp:
  | utast1=nxapp; utast2=nxbot { make_standard (Ranged utast1) (Ranged utast2) (UTApply(utast1, utast2)) }
  | utast1=nxapp; constr=CONSTRUCTOR {
      let (rng, constrnm) = constr in
        make_standard (Ranged utast1) (Tok rng)
          (UTApply(utast1, (rng, UTConstructor(constrnm, (Range.dummy "constructor-unitvalue", UTUnitConstant))))) }
  | tok=DEREF; utast2=nxbot { make_standard (Tok tok) (Ranged utast2) (UTApply((tok, UTContentOf([], "!")), utast2)) }
  | pre=COMMAND; hcmd=hcmd {
      let (rng, mdlnmlst, csnm) = hcmd in
        make_standard (Tok pre) (Tok rng) (UTContentOf(mdlnmlst, csnm)) }
  | utast1=nxapp; OPTIONAL; utast2=nxbot { make_standard (Ranged utast1) (Ranged utast2) (UTApplyOptional(utast1, utast2)) }
  | utast1=nxapp; tok=OMISSION { make_standard (Ranged utast1) (Tok tok) (UTApplyOmission(utast1)) }
  | utast=nxbot { utast }
;
nxbot:
  | utast=nxbot; ACCESS; var=VAR { make_standard (Ranged utast) (Ranged var) (UTAccessField(utast, extract_name var)) }
  | var=VAR                      { let (rng, varnm) = var in (rng, UTContentOf([], varnm)) }
  | vwm=VARWITHMOD               { let (rng, mdlnmlst, varnm) = vwm in (rng, UTContentOf(mdlnmlst, varnm)) }
  | ic=INTCONST                  { make_standard (Ranged ic) (Ranged ic)  (UTIntegerConstant(extract_main ic)) }
  | fc=FLOATCONST                { make_standard (Ranged fc) (Ranged fc) (UTFloatConstant(extract_main fc)) }
  | lc=LENGTHCONST               { let (rng, flt, unitnm) = lc in make_standard (Tok rng) (Tok rng) (UTLengthDescription(flt, unitnm)) }
  | tok=TRUE                                              { make_standard (Tok tok) (Tok tok) (UTBooleanConstant(true)) }
  | tok=FALSE                                             { make_standard (Tok tok) (Tok tok) (UTBooleanConstant(false)) }
  | opn=LPAREN; cls=RPAREN                                { make_standard (Tok opn) (Tok cls) UTUnitConstant }
  | opn=LPAREN; utast=nxlet; cls=RPAREN                   { make_standard (Tok opn) (Tok cls) (extract_main utast) }
  | opn=LPAREN; utast=nxlet; COMMA; tup=tuple; cls=RPAREN { make_standard (Tok opn) (Tok cls) (UTTupleCons(utast, tup)) }
  | opn=OPENHORZ; utast=sxsep; cls=CLOSEHORZ     { make_standard (Tok opn) (Tok cls) (extract_main utast) }
  | opn=OPENVERT; utast=vxblock; cls=CLOSEVERT   { make_standard (Tok opn) (Tok cls) (extract_main utast) }
  | opn=OPENQT; strlst=list(str); cls=CLOSEQT    { make_standard (Tok opn) (Tok cls) (omit_spaces (String.concat "" strlst)) }
  | opn=BLIST; cls=ELIST                         { make_standard (Tok opn) (Tok cls) UTEndOfList }
  | opn=BLIST; utast=nxlist; cls=ELIST           { make_standard (Tok opn) (Tok cls) (extract_main utast) }
  | opn=LPAREN; optok=binop; cls=RPAREN          { make_standard (Tok opn) (Tok cls) (UTContentOf([], extract_name optok)) }
  | opn=BRECORD; cls=ERECORD                     { make_standard (Tok opn) (Tok cls) (UTRecord([])) }
  | opn=BRECORD; rcd=nxrecord; cls=ERECORD       { make_standard (Tok opn) (Tok cls) (UTRecord(rcd)) }
  | opn=BPATH; path=path; cls=EPATH              { make_standard (Tok opn) (Tok cls) path }
  | opn=OPENMATH; utast=mathblock; cls=CLOSEMATH { make_standard (Tok opn) (Tok cls) (extract_main utast) }
;
path: (* untyped_abstract_tree_main *)
  | ast=nxbot; sub=pathsub { let (pathcomplst, utcycleopt) = sub in UTPath(ast, pathcomplst, utcycleopt) }
;
pathsub: (* (untyped_abstract_tree untyped_path_component) list * (unit untyped_path_component) option *)
  | pc=pathcomp; sub=pathsub          { let (tail, utcycleopt) = sub in (pc :: tail, utcycleopt) }
  | utcycleopt=option(pathcompcycle); { ([], utcycleopt) }
;
pathcomp:
  | PATHLINE; ast=nxbot                                                       { UTPathLineTo(ast) }
  | PATHCURVE; CONTROLS; ast1=nxbot; LETAND; ast2=nxbot; PATHCURVE; ast=nxbot { UTPathCubicBezierTo(ast1, ast2, ast) }
;
pathcompcycle:
  | PATHLINE; CYCLE                                                       { UTPathLineTo(()) }
  | PATHCURVE; CONTROLS; ast1=nxbot; LETAND; ast2=nxbot; PATHCURVE; CYCLE { UTPathCubicBezierTo(ast1, ast2, ()) }
;
nxrecord:
  | VAR DEFEQ nxlet                    { (extract_name $1, $3) :: [] }
  | VAR DEFEQ nxlet LISTPUNCT          { (extract_name $1, $3) :: [] }
  | VAR DEFEQ nxlet LISTPUNCT nxrecord { (extract_name $1, $3) :: $5 }
;
nxlist:
  | nxlet LISTPUNCT nxlist { make_standard (Ranged $1) (Ranged $3) (UTListCons($1, $3)) }
  | nxlet LISTPUNCT        { make_standard (Ranged $1) (Tok $2) (UTListCons($1, (Range.dummy "end-of-list", UTEndOfList))) }
  | nxlet                  { make_standard (Ranged $1) (Ranged $1) (UTListCons($1, (Range.dummy "end-of-list", UTEndOfList))) }
;
variants: /* -> untyped_variant_cons */
  | CONSTRUCTOR OF txfunc BAR variants  { let (rng, constrnm) = $1 in (rng, constrnm, $3) :: $5 }
  | CONSTRUCTOR OF txfunc               { let (rng, constrnm) = $1 in (rng, constrnm, $3) :: [] }
  | CONSTRUCTOR BAR variants            { let (rng, constrnm) = $1 in (rng, constrnm, (Range.dummy "dec-constructor-unit1", MTypeName([], "unit"))) :: $3 }
  | CONSTRUCTOR                         { let (rng, constrnm) = $1 in (rng, constrnm, (Range.dummy "dec-constructor-unit2", MTypeName([], "unit"))) :: [] }
;
txfunc: /* -> manual_type */
  | mntydom=txprod; ARROW; mntycod=txfunc {
        make_standard (Ranged mntydom) (Ranged mntycod) (MFuncType(mntydom, mntycod))
      }
  | mntydom=txprod; OPTIONALTYPE; ARROW; mntycod=txfunc {
        make_standard (Ranged mntydom) (Ranged mntycod) (MOptFuncType(mntydom, mntycod))
      }
  | mnty=txprod { mnty }
;
txprod:
  | mnty=txapppre; EXACT_TIMES; mntyprod=txprodsub {
        let (rng1, _) = mnty in
        let (rng2, mntylst) = mntyprod in
          make_standard (Tok rng1) (Tok rng2) (MProductType(mnty :: mntylst))
      }

  | mnty=txapppre { mnty }
;
txprodsub: /* -> Range.t * manual_type list */
  | mnty=txapppre; EXACT_TIMES; mntyprod=txprodsub {
        let (rng2, mntylst) = mntyprod in
          (rng2, mnty :: mntylst)
      }
  | mnty=txapppre {
        let (rng2, _) = mnty in
          (rng2, mnty :: [])
      }
;
txapppre: /* -> manual_type */
  | tyapp=txapp {
        let (rng, lst, tynm) = tyapp in
          (rng, MTypeName(lst, tynm))
      }
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
  | LPAREN; mnty=txfunc; RPAREN { mnty }
  | opn=BRECORD; lst=txrecord; cls=ERECORD {
        let asc = Assoc.of_list lst in
        let rng = make_range (Tok opn) (Tok cls) in
          (rng, MRecordType(asc))
      }
  | tyvar=TYPEVAR {
        let (rng, tyargnm) = tyvar in (rng, MTypeParam(tyargnm))
      }
;
txapp: /* Range.t * manual_type list * type_name */
  | tybot=txbot; tyapp=txapp {
        let (rng1, tynm) = tybot in
        let mnty = (rng1, MTypeName([], tynm)) in
        let (rng2, lst, tynm) = tyapp in
        let rng = make_range (Ranged mnty) (Tok rng2) in
          (rng, mnty :: lst, tynm)
      }
  | LPAREN; mnty=txfunc; RPAREN; tyapp=txapp {
        let (rng2, lst, tynm) = tyapp in
        let rng = make_range (Ranged mnty) (Tok rng2) in
          (rng, mnty :: lst, tynm)
      }
  | tyvar=TYPEVAR; tyapp=txapp {
        let (rngtyarg, tyargnm) = tyvar in
        let (rng2, lst, tynm) = tyapp in
        let rng = make_range (Tok rngtyarg) (Tok rng2) in
          (rng, (rngtyarg, MTypeParam(tyargnm)) :: lst, tynm)
      }
  | tybot=txbot { let (rng, tynm) = tybot in (rng, [], tynm) }
;
txbot:
  | tytok=VAR { tytok }
  | mdltok=CONSTRUCTOR; DOT; tytok=VAR {
      (* temporary; currently only one module name can be appended *)
        let (rng1, mdlnm) = mdltok in
        let (rng2, tynm)  = tytok in
        let rng = make_range (Tok rng1) (Tok rng2) in
          (rng, mdlnm ^ "." ^ tynm)
      }
;
txlist:
  | mnty=txfunc; LISTPUNCT; tail=txlist                 { MMandatoryArgumentType(mnty) :: tail }
  | mnty=txfunc                                         { MMandatoryArgumentType(mnty) :: [] }
  | mnty=txapppre; OPTIONALTYPE; LISTPUNCT; tail=txlist { MOptionalArgumentType(mnty) :: tail }
  | mnty=txapppre; OPTIONALTYPE                         { MOptionalArgumentType(mnty) :: [] }
  |                                                     { [] }
;
txrecord: /* -> (field_name * manual_type) list */
  | fldtok=VAR; COLON; mnty=txfunc; LISTPUNCT; tail=txrecord { let (_, fldnm) = fldtok in (fldnm, mnty) :: tail }
  | fldtok=VAR; COLON; mnty=txfunc; LISTPUNCT                { let (_, fldnm) = fldtok in (fldnm, mnty) :: [] }
  | fldtok=VAR; COLON; mnty=txfunc                           { let (_, fldnm) = fldtok in (fldnm, mnty) :: [] }
;
tuple: /* -> untyped_tuple_cons */
  | nxlet             { make_standard (Ranged $1) (Ranged $1) (UTTupleCons($1, (Range.dummy "end-of-tuple'", UTEndOfTuple))) }
  | nxlet COMMA tuple { make_standard (Ranged $1) (Ranged $3) (UTTupleCons($1, $3)) }
;
pats: /* -> code_range * untyped_pattern_branch list */
  | pat=patas; ARROW; utast=nxletsub {
        let (rnglast, _) = utast in
          (rnglast, UTPatternBranch(pat, utast) :: [])
      }
  | pat=patas; ARROW; utast=nxletsub; BAR; tail=pats {
        let (rnglast, patbrs) = tail in
          (rnglast, UTPatternBranch(pat, utast) :: patbrs)
      }
  | pat=patas; WHEN; utastcond=nxletsub; ARROW; utast=nxletsub {
        let (rnglast, _) = utast in
          (rnglast, UTPatternBranchWhen(pat, utastcond, utast) :: []) }
  | pat=patas; WHEN; utastcond=nxletsub; ARROW; utast=nxletsub; BAR; tail=pats {
        let (rnglast, patbrs) = tail in
          (rnglast, UTPatternBranchWhen(pat, utastcond, utast) :: patbrs) }
;
patas:
  | pattr AS VAR       { make_standard (Ranged $1) (Ranged $3) (UTPAsVariable(extract_name $3, $1)) }
  | pattr              { $1 }
;
pattr: /* -> Types.untyped_pattern_tree */
  | patbot CONS pattr  { make_standard (Ranged $1) (Ranged $3) (UTPListCons($1, $3)) }
  | CONSTRUCTOR patbot { make_standard (Ranged $1) (Ranged $2) (UTPConstructor(extract_name $1, $2)) }
  | CONSTRUCTOR        { make_standard (Ranged $1) (Ranged $1) (UTPConstructor(extract_name $1, (Range.dummy "constructor-unit-value", UTPUnitConstant))) }
  | patbot             { $1 }
;
patbot: /* -> Types.untyped_pattern_tree */
  | INTCONST           { make_standard (Ranged $1) (Ranged $1) (UTPIntegerConstant(extract_main $1)) }
  | TRUE               { make_standard (Tok $1) (Tok $1) (UTPBooleanConstant(true)) }
  | FALSE              { make_standard (Tok $1) (Tok $1) (UTPBooleanConstant(false)) }
  | LPAREN RPAREN      { make_standard (Tok $1) (Tok $2) UTPUnitConstant }
  | WILDCARD           { make_standard (Tok $1) (Tok $1) UTPWildCard }
  | vartok=defedvar    { make_standard (Ranged vartok) (Ranged vartok) (UTPVariable(extract_name vartok)) }
  | LPAREN patas RPAREN                { make_standard (Tok $1) (Tok $3) (extract_main $2) }
  | LPAREN patas COMMA pattuple RPAREN { make_standard (Tok $1) (Tok $5) (UTPTupleCons($2, $4)) }
  | BLIST ELIST                        { make_standard (Tok $1) (Tok $2) UTPEndOfList }
  | opn=OPENQT; strlst=list(str); cls=CLOSEQT {
        let rng = make_range (Tok opn) (Tok cls) in (rng, UTPStringConstant(rng, omit_spaces (String.concat "" strlst))) }
;
pattuple: /* -> untyped_pattern_tree */
  | patas                { make_standard (Ranged $1) (Ranged $1) (UTPTupleCons($1, (Range.dummy "end-of-tuple-pattern", UTPEndOfTuple))) }
  | patas COMMA pattuple { make_standard (Ranged $1) (Ranged $3) (UTPTupleCons($1, $3)) }
;
binop:
  | BINOP_TIMES
  | BINOP_DIVIDES
  | BINOP_HAT
  | BINOP_EQ
  | BINOP_GT
  | BINOP_LT
  | BINOP_AMP
  | BINOP_BAR
  | BINOP_PLUS
  | BINOP_MINUS { $1 }
  | EXACT_TIMES { ($1, "*") }
  | EXACT_MINUS { ($1, "-") }
  | MOD         { ($1, "mod") }
  | BEFORE      { ($1, "before") }
  | LNOT        { ($1, "not") }
;
sxsep:
  | SEP; utastlst=sxlist          { make_cons utastlst }
  | utast=sxblock                 { utast }
  | itmzlst=nonempty_list(sxitem) { make_list_to_itemize itmzlst }
;
sxlist:
  | utast=sxblock; SEP; utasttail=sxlist { utast :: utasttail }
  |                                      { [] }
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
  | utm=mathlist { let (rng, _) = utm in (rng, UTMath(utm)) }
;
mathlist:
  | utmlst=list(mathsuper) {
        let rng =
          match (utmlst, List.rev utmlst) with
          | ([], [])                                -> Range.dummy "empty-math"
          | ((rngfirst, _) :: _, (rnglast, _) :: _) -> Range.unite rngfirst rnglast
          | _                                       -> assert false
        in
          (rng, UTMList(utmlst))
      }
;
mathsuper:
  | utm1=mathsub; SUPERSCRIPT; utm2=mathgroup {
        make_standard (Ranged utm1) (Ranged utm2) (UTMSuperScript(utm1, utm2))
      }
  | utm=mathsub { utm }
;
mathsub:
  | utm1=mathbot; SUBSCRIPT; utm2=mathgroup {
        make_standard (Ranged utm1) (Ranged utm2) (UTMSubScript(utm1, utm2))
      }
  | utm=mathbot { utm }
;
mathgroup:
  | opn=BMATHGRP; utm=mathlist; cls=EMATHGRP { make_standard (Tok opn) (Tok cls) (extract_main utm) }
  | utm=mathbot                              { utm }
;
mathbot:
  | tok=MATHCHAR                    { let (rng, char) = tok in (rng, UTMChar(char)) }
  | mcmd=mcmd; arglst=list(matharg) {
        let (rngcmd, mdlnmlst, csnm) = mcmd in
        let rnglast =
          match List.rev arglst with
          | []                                 -> rngcmd
          | UTMandatoryArgument((rng, _)) :: _ -> rng
          | UTOptionalArgument((rng, _)) :: _  -> rng
          | UTOmission(rng) :: _               -> rng
        in
        let utastcmd = (rngcmd, UTContentOf(mdlnmlst, csnm)) in
          make_standard (Tok rngcmd) (Tok rnglast) (UTMCommand(utastcmd, arglst))
      }
  | tok=VARINMATH { let (rng, mdlnmlst, varnm) = tok in (rng, UTMEmbed((rng, UTContentOf(mdlnmlst, varnm)))) }
;
matharg:
  | opn=BMATHGRP; utast=mathblock; cls=EMATHGRP { let (_, utastmain) = utast in UTMandatoryArgument(make_standard (Tok opn) (Tok cls) utastmain) }
  | opn=BHORZGRP; utast=sxsep; cls=EHORZGRP     { let (_, utastmain) = utast in UTMandatoryArgument(make_standard (Tok opn) (Tok cls) utastmain) }
  | opn=BVERTGRP; utast=vxblock; cls=EVERTGRP   { let (_, utastmain) = utast in UTMandatoryArgument(make_standard (Tok opn) (Tok cls) utastmain) }
  | utcmdarg=narg                               { utcmdarg }
;
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
  | hcmd=hcmd; nargs=nargs; sargsraw=sargs {
        let (rngcs, mdlnmlst, csnm) = hcmd in
        let utastcmd = (rngcs, UTContentOf(mdlnmlst, csnm)) in
        let (rnglast, sargs) = sargsraw in
        let args = List.append nargs sargs in
          make_standard (Tok rngcs) (Tok rnglast) (UTInputHorzEmbedded(utastcmd, args))
      }
  | opn=OPENMATH; utast=mathblock; cls=CLOSEMATH {
        make_standard (Tok opn) (Tok cls) (UTInputHorzEmbeddedMath(utast))
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
  | CHAR  { let (rng, ch) = $1 in (rng, ch) }
  | SPACE { let rng = $1 in (rng, " ") }
  | BREAK { let rng = $1 in (rng, "\n") }
;
nargs:
  | nargs=list(narg) { nargs }
;
narg:
  | opn=OPENPROG; utast=nxlet; cls=CLOSEPROG {
        UTMandatoryArgument(make_standard (Tok opn) (Tok cls) (extract_main utast))
      }
  | opn=OPENPROG; cls=CLOSEPROG {
        UTMandatoryArgument(make_standard (Tok opn) (Tok cls) UTUnitConstant)
      }
  | opn=OPENPROG_AND_BRECORD; rcd=nxrecord; cls=CLOSEPROG_AND_ERECORD {
        UTMandatoryArgument(make_standard (Tok opn) (Tok cls) (UTRecord(rcd)))
      }
  | opn=OPENPROG_AND_BLIST; utast=nxlist; cls=CLOSEPROG_AND_ELIST {
        UTMandatoryArgument(make_standard (Tok opn) (Tok cls) (extract_main utast))
      }
  | opn=OPTIONAL; OPENPROG; utast=nxlet; cls=CLOSEPROG {
        UTOptionalArgument(make_standard (Tok opn) (Tok cls) (extract_main utast))
      }
  | opn=OPTIONAL; OPENPROG; cls=CLOSEPROG {
        UTOptionalArgument(make_standard (Tok opn) (Tok cls) UTUnitConstant)
      }
  | opn=OPTIONAL; OPENPROG_AND_BRECORD; rcd=nxrecord; cls=CLOSEPROG_AND_ERECORD {
        UTOptionalArgument(make_standard (Tok opn) (Tok cls) (UTRecord(rcd)))
      }
  | opn=OPTIONAL; OPENPROG_AND_BLIST; utast=nxlist; cls=CLOSEPROG_AND_ELIST {
        UTOptionalArgument(make_standard (Tok opn) (Tok cls) (extract_main utast))
      }
  | rng=OMISSION { UTOmission(rng) }
;
str:
  | chartok=CHAR { let (rng, c) = chartok in c }
  | BREAK        { "\n" }
  | SPACE        { " " }
;
sargs:
  | rng=ENDACTIVE             { (rng, []) }
  | sargs=nonempty_list(sarg) {
      let rng =
        match List.rev sargs with
        | []                                 -> assert false
        | UTMandatoryArgument((rng, _)) :: _ -> rng
        | UTOptionalArgument((rng, _)) :: _  -> rng
        | UTOmission(rng) :: _               -> rng
      in
        (rng, sargs)
    }
;

sarg: /* -> Types.untyped_argument_cons */
  | opn=BVERTGRP; utast=vxblock; cls=EVERTGRP { UTMandatoryArgument(make_standard (Tok opn) (Tok cls) (extract_main utast)) }
  | opn=BHORZGRP; utast=sxsep; cls=EHORZGRP   { UTMandatoryArgument(make_standard (Tok opn) (Tok cls) (extract_main utast)) }
;
vcmd:
  | tok=VERTCMD        { let (rng, csnm) = tok in (rng, [], csnm) }
  | tok=VERTCMDWITHMOD { tok }
;
vxblock:
  | ivlst=list(vxbot) { (make_range_from_list ivlst, UTInputVert(ivlst)) }
;
vxbot:
  | vcmd=vcmd; nargs=nargs; sargsraw=sargs {
        let (rngcs, mdlnmlst, csnm) = vcmd in
        let (rnglast, sargs) = sargsraw in
        let args = List.append nargs sargs in
          make_standard (Tok rngcs) (Tok rnglast) (UTInputVertEmbedded((rngcs, UTContentOf([], csnm)), args))
      }
  | vartok=VARINVERT; cls=ENDACTIVE {
        let (rng, mdlnmlst, varnm) = vartok in
          make_standard (Tok rng) (Tok cls) (UTInputVertContent((rng, UTContentOf(mdlnmlst, varnm))))
      }
;
