%{

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


  let rec append_argument_list (arglsta : untyped_argument_cons) (arglstb : untyped_argument_cons) =
    List.append arglsta arglstb

(*
  let class_and_id_region (utast : untyped_abstract_tree) =
    (Range.dummy "class_and_id_region", UTClassAndIDRegion(utast))

  let convert_into_apply (csutast : untyped_abstract_tree) (clsnmutast : untyped_abstract_tree)
                               (idnmutast : untyped_abstract_tree) (argcons : untyped_argument_cons) =
    let (csrng, _) = csutast in
    let rec iter argcons utastconstr =
      match argcons with
      | []                          -> utastconstr
      | (argrng, argmain) :: actail -> iter actail (Range.unite csrng argrng, UTApply(utastconstr, (argrng, argmain)))
    in
      iter argcons (Range.dummy "convert_into_apply", UTApplyClassAndID(clsnmutast, idnmutast, csutast))


  let class_name_to_abstract_tree (clsnm : class_name) =
    UTConstructor("Just", (Range.dummy "class_name_to", UTStringConstant((String.sub clsnm 1 ((String.length clsnm) - 1)))))


  let id_name_to_abstract_tree (idnm : id_name) =
    UTConstructor("Just", (Range.dummy "id_name_to", UTStringConstant((String.sub idnm 1 ((String.length idnm) - 1)))))
*)

  let rec curry_lambda_abstract (rng : Range.t) (argvarcons : untyped_argument_variable_cons) (utastdef : untyped_abstract_tree) =
    match argvarcons with
    | []                                     -> utastdef
    | (varrng, UTPVariable(varnm)) :: avtail ->
        (rng, UTLambdaAbstract(varrng, varnm, curry_lambda_abstract rng avtail utastdef))
    | (varrng, UTPWildCard) :: avtail        ->
        (rng, UTLambdaAbstract(varrng, "%wild", curry_lambda_abstract rng avtail utastdef))
    | (varrng, argpatas) :: avtail           ->
        let afterabs     = curry_lambda_abstract rng avtail utastdef in
        let dummyutast   = (varrng, UTContentOf([], "%patarg")) in
        let dummypatcons = UTPatternMatchCons((varrng, argpatas), afterabs, UTEndOfPatternMatch) in
          (rng, UTLambdaAbstract(varrng, "%patarg", (varrng, UTPatternMatch(dummyutast, dummypatcons))))


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
                (* does not take space-only line into account *)
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


  let make_let_expression (lettok : Range.t) (decs : untyped_mutual_let_cons) (utastaft : untyped_abstract_tree) =
    make_standard (Tok lettok) (Ranged utastaft) (UTLetIn(decs, utastaft))


  let make_let_mutable_expression
      (letmuttok : Range.t) (vartok : Range.t * var_name)
      (utastdef : untyped_abstract_tree) (utastaft : untyped_abstract_tree)
  : untyped_abstract_tree
  =
    let (varrng, varnm) = vartok in
      make_standard (Tok letmuttok) (Ranged utastaft) (UTLetMutableIn(varrng, varnm, utastdef, utastaft))


  let make_variant_declaration (firsttk : Range.t) (varntdecs : untyped_mutual_variant_cons) (utastaft : untyped_abstract_tree) : untyped_abstract_tree =
    make_standard (Tok firsttk) (Ranged utastaft) (UTDeclareVariantIn(varntdecs, utastaft))


  let make_mutual_let_cons
      (mntyopt : manual_type option)
      (vartok : Range.t * var_name) (argcons : untyped_argument_variable_cons) (utastdef : untyped_abstract_tree)
      (tailcons : untyped_mutual_let_cons)
  : untyped_mutual_let_cons
  =
    let (varrng, varnm) = vartok in
    let curried = curry_lambda_abstract varrng argcons utastdef in
      (mntyopt, varnm, curried) :: tailcons


  let rec make_mutual_let_cons_par
      (mntyopt : manual_type option)
      (vartok : Range.t * var_name) (argletpatcons : untyped_let_pattern_cons)
      (tailcons : untyped_mutual_let_cons)
  : untyped_mutual_let_cons
  =
    let (_, varnm) = vartok in
    let pmcons  = make_pattern_match_cons_of_argument_pattern_cons argletpatcons in
    let fullrng = get_range_of_let_pattern_cons argletpatcons in
    let abs     = make_lambda_abstract_for_parallel fullrng argletpatcons pmcons in
      (mntyopt, varnm, abs) :: tailcons


  and get_range_of_let_pattern_cons (argletpatcons : untyped_let_pattern_cons) : Range.t =
    let get_first_range argletpatcons =
      match argletpatcons with
      | UTLetPatternCons((argpatrng, _) :: _, _, _) -> argpatrng
      | _                                           -> assert false
    in
    let rec get_last_range argletpatcons =
      match argletpatcons with
      | UTEndOfLetPattern                                             -> assert false
      | UTLetPatternCons(argpatcons, (lastrng, _), UTEndOfLetPattern) -> lastrng
      | UTLetPatternCons(_, _, tailcons)                              -> get_last_range tailcons
    in
      make_range (Tok (get_first_range argletpatcons)) (Tok (get_last_range argletpatcons))


  and make_pattern_match_cons_of_argument_pattern_cons (argletpatcons : untyped_let_pattern_cons) : untyped_pattern_match_cons =
    match argletpatcons with
    | UTEndOfLetPattern                                         -> UTEndOfPatternMatch
    | UTLetPatternCons(argpatcons, utastdef, argletpattailcons) ->
        let tailpmcons = make_pattern_match_cons_of_argument_pattern_cons argletpattailcons in
        let prodpatrng = get_range_of_argument_variable_cons argpatcons in
        let prodpat    = make_product_pattern_of_argument_cons prodpatrng argpatcons in
          UTPatternMatchCons(prodpat, utastdef, tailpmcons)

  and get_range_of_argument_variable_cons (argpatcons : untyped_argument_variable_cons) : Range.t =
    let first_range =
      match argpatcons with
      | (fstrng, _) :: _ -> fstrng
      | _                -> assert false
    in
    let rec get_last_range apcons =
      match apcons with
      | []                  -> assert false
      | (lastrng, _) :: []  -> lastrng
      | _ :: tailargpatcons -> get_last_range tailargpatcons
    in
      make_range (Tok first_range) (Tok (get_last_range argpatcons))


  and make_product_pattern_of_argument_cons (prodpatrng : Range.t) (argpatcons : untyped_argument_variable_cons) : untyped_pattern_tree =
    let rec aux argpatcons =
      match argpatcons with
      | []                 -> (Range.dummy "endofargvar", UTPEndOfTuple)
      | argpat :: tailcons -> (Range.dummy "argvarcons", UTPTupleCons(argpat, aux tailcons))
    in
      let (_, prodpatmain) = aux argpatcons in (prodpatrng, prodpatmain)


  and make_lambda_abstract_for_parallel
      (fullrng : Range.t) (argletpatcons : untyped_let_pattern_cons)
      (pmcons : untyped_pattern_match_cons)
  =
    match argletpatcons with
    | UTEndOfLetPattern                  -> assert false
    | UTLetPatternCons(argpatcons, _, _) ->
        make_lambda_abstract_for_parallel_sub fullrng (fun u -> u) 0 argpatcons pmcons


  and make_lambda_abstract_for_parallel_sub
      (fullrng : Range.t) (k : untyped_abstract_tree -> untyped_abstract_tree)
      (i : int) (argpatcons : untyped_argument_variable_cons)
      (pmcons : untyped_pattern_match_cons)
  : untyped_abstract_tree
  =
    match argpatcons with
    | []                   -> (fullrng, UTPatternMatch(k (Range.dummy "endoftuple", UTEndOfTuple), pmcons))
    | (rng, _) :: tailcons ->
(*        let knew = (fun u -> k (dummy_range, UTTupleCons((rng, UTContentOf(numbered_var_name i)), u))) in *)
(*        let knew = (fun u -> k (dummy_range, UTTupleCons(((3000, 0, 0, 0), UTContentOf(numbered_var_name i)), u))) in (* for test *) *)
        let knew = (fun u -> k (Range.dummy "knew1", UTTupleCons((Range.dummy "knew2", UTContentOf([], numbered_var_name i)), u))) in (* for test *)
        let after = make_lambda_abstract_for_parallel_sub fullrng knew (i + 1) tailcons pmcons in
          (Range.dummy "pattup1", UTLambdaAbstract(Range.dummy "pattup2", numbered_var_name i, after))

  and numbered_var_name i = "%pattup" ^ (string_of_int i)


  let kind_type_argument_cons (uktyargcons : untyped_unkinded_type_argument_cons) (constrntcons : constraint_cons) : untyped_type_argument_cons =
    uktyargcons |> List.map (fun (rng, tyvarnm) ->
      try
        let mkd = List.assoc tyvarnm constrntcons in (rng, tyvarnm, mkd)
      with
      | Not_found -> (rng, tyvarnm, MUniversalKind)
    )


  let make_mutual_variant_cons (uktyargcons : untyped_unkinded_type_argument_cons) (tynmtk : Range.t * type_name) (constrdecs : untyped_variant_cons) (constrntcons : constraint_cons) (tailcons : untyped_mutual_variant_cons) =
    let tynm = extract_name tynmtk in
    let tynmrng = get_range tynmtk in
    let tyargcons = kind_type_argument_cons uktyargcons constrntcons in
      UTMutualVariantCons(tyargcons, tynmrng, tynm, constrdecs, tailcons)

  let make_mutual_synonym_cons (uktyargcons : untyped_unkinded_type_argument_cons) (tynmtk : Range.t * type_name) (mnty : manual_type) (constrntcons : constraint_cons) (tailcons : untyped_mutual_variant_cons) =
    let tynm = extract_name tynmtk in
    let tynmrng = get_range tynmtk in
    let tyargcons = kind_type_argument_cons uktyargcons constrntcons in
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

  (* range_kind -> string -> 'a *)
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
(*
%token <Range.t * Types.var_name> VARINSTR
*)
%token <Range.t * Types.var_name> VARINMATH
%token <Range.t * (Types.module_name list) * Types.var_name> VARINMATHWITHMOD
%token <Range.t * Types.var_name> TYPEVAR
%token <Range.t * Types.constructor_name> CONSTRUCTOR
%token <Range.t * int> INTCONST
%token <Range.t * float> FLOATCONST
%token <Range.t * float * Types.length_unit_name> LENGTHCONST
%token <Range.t * string> CHAR
%token <Range.t> SPACE BREAK
%token <Range.t * string> MATHCHAR
%token <Range.t> SUBSCRIPT SUPERSCRIPT
(*
%token <Range.t * Types.id_name>      IDNAME
%token <Range.t * Types.class_name>   CLASSNAME
*)
%token <Range.t> LAMBDA ARROW
%token <Range.t> LET DEFEQ LETAND IN
%token <Range.t> MODULE STRUCT END DIRECT DOT SIG VAL CONSTRAINT
%token <Range.t> TYPE OF MATCH WITH BAR WILDCARD WHEN AS COLON
%token <Range.t> LETMUTABLE OVERWRITEEQ
%token <Range.t> LETHORZ LETVERT LETMATH LETVERTDETAILED
%token <Range.t> REFNOW (* REFFINAL *)
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
%token <Range.t> UNITVALUE WHILE DO
%token <Range.t> HORZCMDTYPE VERTCMDTYPE MATHCMDTYPE
(*
%token <Range.t> NEWGLOBALHASH OVERWRITEGLOBALHASH RENEWGLOBALHASH
*)
%token <Range.t * int> ITEM
%token EOI
%token IGNORED

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
%type <Types.untyped_abstract_tree> main
%type <Types.untyped_abstract_tree> nxlet
%type <Types.untyped_abstract_tree> nxletsub
%type <Types.untyped_mutual_let_cons> nxdec
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
%type <Range.t * Types.untyped_pattern_match_cons> pats
%type <Types.untyped_pattern_tree> patas
%type <Types.untyped_pattern_tree> patbot
%type <Types.untyped_abstract_tree> nxlist
%type <Types.untyped_abstract_tree> sxsep
%type <Types.untyped_abstract_tree> sxblock
%type <Types.untyped_abstract_tree> vxblock
%type <Types.untyped_input_vert_element> vxbot
%type <Types.var_name * Types.manual_kind> constrnt
%type <Types.constraint_cons> constrnts
(*
%type <Types.untyped_abstract_tree> sxclsnm
%type <Types.untyped_abstract_tree> sxidnm
*)
%type <Types.untyped_abstract_tree> narg
%type <Types.untyped_abstract_tree> sarg
%type <Types.untyped_argument_variable_cons> argvar
%type <Range.t * Types.var_name> binop
%type <Types.untyped_unkinded_type_argument_cons> xpltyvars

%%


main:
  | utast=nxtoplevel    { utast }
  | utast=vxblock; EOI  { utast }
  | utast=nxwhl; EOI    { utast }
;
nxtoplevel:
  | top=LET; dec=nxdec; subseq=nxtopsubseq                                   { make_let_expression top dec subseq }
  | top=LETMUTABLE; vartok=VAR; OVERWRITEEQ; utast=nxlet; subseq=nxtopsubseq { make_let_mutable_expression top vartok utast subseq }
  | top=LETHORZ; dec=nxhorzdec; subseq=nxtopsubseq                           { make_let_expression top dec subseq }
  | top=LETVERT; dec=nxvertdec; subseq=nxtopsubseq                           { make_let_expression top dec subseq }
  | top=LETMATH; dec=nxmathdec; subseq=nxtopsubseq                           { make_let_expression top dec subseq }
  | top=LETVERTDETAILED; dec=nxvertdetaileddec; subseq=nxtopsubseq           { make_let_expression top dec subseq }
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
  | TYPE; tyvarlst=xpltyvars; tytok=VAR; clst=constrnts         { let (_, tynm) = tytok in (SigType(kind_type_argument_cons tyvarlst clst, tynm)) }
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
  | END                                                            { (end_struct $1) }
  | LET nxdec nxstruct                                             { make_let_expression $1 $2 $3 }
  | LETMUTABLE VAR OVERWRITEEQ nxlet nxstruct                      { make_let_mutable_expression $1 $2 $4 $5 }
  | TYPE nxvariantdec nxstruct                                     { make_variant_declaration $1 $2 $3 }
  | MODULE CONSTRUCTOR nxsigopt DEFEQ STRUCT nxstruct nxstruct     { make_module $1 $2 $3 $6 $7 }
;
nxhorzdec:
  | ctxvartok=VAR; hcmdtok=HORZCMD; argvarlst=argvar; DEFEQ; utast=nxlet {
      let (rngcs, csnm) = hcmdtok in
      let (rngctxvar, ctxvarnm) = ctxvartok in
      let rng = make_range (Tok rngctxvar) (Ranged utast) in
      let curried = curry_lambda_abstract rngcs argvarlst utast in
        (None, csnm, (rng, UTLambdaHorz(rngctxvar, ctxvarnm, curried))) :: []
      }
;
nxvertdec:
  | ctxvartok=VAR; vcmdtok=VERTCMD; argvarlst=argvar; DEFEQ; utast=nxlet {
      let (rngcs, csnm) = vcmdtok in
      let (rngctxvar, ctxvarnm) = ctxvartok in
      let rng = make_range (Tok rngctxvar) (Ranged utast) in
      let curried = curry_lambda_abstract rngcs argvarlst utast in
        (None, csnm, (rng, UTLambdaVert(rngctxvar, ctxvarnm, curried))) :: []
      }
;
nxmathdec:
  | mcmdtok=HORZCMD; argvarlst=argvar; DEFEQ; utast=nxlet {
      let (rngcs, csnm) = mcmdtok in
      let rng = make_range (Tok rngcs) (Ranged utast) in
      let curried = curry_lambda_abstract rngcs argvarlst utast in
        (None, csnm, (rng, UTLambdaMath(curried))) :: []
      }
;
nxvertdetaileddec:
  | ctxvartok=VAR; vcmdtok=VERTCMD; argvarlst=argvar; DEFEQ; utast=nxlet {
      let (rngcs, csnm) = vcmdtok in
      let (rngctxvar, ctxvarnm) = ctxvartok in
      let rng = make_range (Tok rngctxvar) (Ranged utast) in
      let curried = curry_lambda_abstract rngcs argvarlst utast in
        (None, csnm, (rng, UTLambdaVertDetailed(rngctxvar, ctxvarnm, curried))) :: []
      }
;
nxdecargpart:
  | COLON; mty=txfunc                                       { (Some(mty), []) }
  | COLON; mty=txfunc; BAR; argvarlst=nonempty_list(patbot) { (Some(mty), argvarlst) }
  | BAR; argvarlst=nonempty_list(patbot)                    { (None, argvarlst) }
  | argvarlst=argvar                                        { (None, argvarlst) }
;
nxdecsub:
  | LETAND; dec=nxdec { dec }
  |                   { [] }
;
%inline defedvar:
  | vartok=VAR  { vartok }
  | LPAREN; optok=binop; RPAREN { optok }
;
nxdec: /* -> untyped_mutual_let_cons */
  | vartok=defedvar; argpart=nxdecargpart; DEFEQ; utastdef=nxlet; dec=nxdecsub {
        let (mtyopt, argvarlst) = argpart in
          make_mutual_let_cons mtyopt vartok argvarlst utastdef dec
      }
  | vartok=defedvar; argpart=nxdecargpart; DEFEQ; utastdef=nxlet; BAR; decpar=nxdecpar; dec=nxdecsub {
        let (mtyopt, argvarlst) = argpart in
          make_mutual_let_cons_par mtyopt vartok (UTLetPatternCons(argvarlst, utastdef, decpar)) dec
      }
;
nxdecpar:
  | argvar DEFEQ nxlet BAR nxdecpar { UTLetPatternCons($1, $3, $5) }
  | argvar DEFEQ nxlet              { UTLetPatternCons($1, $3, UTEndOfLetPattern) }
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
  | tok=LET; dec=nxdec; IN; utast=nxlet { make_let_expression tok dec utast }
  | tok=LET; pat=patbotwithoutvar; DEFEQ; utast1=nxlet; IN; utast2=nxlet {
        make_standard (Tok tok) (Ranged utast2) (UTPatternMatch(utast1, UTPatternMatchCons(pat, utast2, UTEndOfPatternMatch)))
      }
  | tok=LETMUTABLE; var=VAR; OVERWRITEEQ; utast1=nxlet; IN; utast2=nxlet {
        make_let_mutable_expression tok var utast1 utast2
      }
  | tok=LETMATH; dec=nxmathdec; IN; utast=nxlet { make_let_expression tok dec utast }
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
  | VAR OVERWRITEEQ nxlor {
        let (varrng, varnm) = $1 in
          make_standard (Ranged $1) (Ranged $3) (UTOverwrite(varrng, varnm, $3)) }
(*
  | NEWGLOBALHASH nxlet OVERWRITEGLOBALHASH nxlor {
        make_standard (Tok $1) (Ranged $4) (UTDeclareGlobalHash($2, $4)) }
  | RENEWGLOBALHASH nxlet OVERWRITEGLOBALHASH nxlor {
        make_standard (Tok $1) (Ranged $4) (UTOverwriteGlobalHash($2, $4)) }
*)
  | LAMBDA argvar ARROW nxlor {
        let rng = make_range (Tok $1) (Ranged $4) in curry_lambda_abstract rng $2 $4 }
  | nxlor { $1 }
;
argvar: /* -> argument_variable_cons */
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
  | nxlplus BINOP_MINUS nxrtimes { binary_operator $1 $2 $3 }
  | nxlplus EXACT_MINUS nxrtimes { binary_operator $1 ($2, "-") $3 }
  | nxltimes                     { $1 }
;
nxrplus:
  | nxrminus BINOP_PLUS nxrplus { binary_operator $1 $2 $3 }
  | nxrminus                    { $1 }
;
nxrminus:
  | nxrplus BINOP_MINUS nxrtimes  { binary_operator $1 $2 $3 }
  | nxrtimes                      { $1 }
;
nxltimes:
  | nxun BINOP_TIMES nxrtimes    { binary_operator $1 $2 $3 }
  | nxun EXACT_TIMES nxrtimes    { binary_operator $1 ($2, "*") $3 }
  | nxltimes BINOP_DIVIDES nxapp { binary_operator $1 $2 $3 }
  | nxltimes MOD nxapp           { binary_operator $1 ($2, "mod") $3 }
  | nxun                         { $1 }
;
nxrtimes:
  | nxapp EXACT_TIMES nxrtimes   { binary_operator $1 ($2, "*") $3 }
  | nxapp BINOP_TIMES nxrtimes   { binary_operator $1 $2 $3 }
  | nxrtimes BINOP_DIVIDES nxapp { binary_operator $1 $2 $3 }
  | nxrtimes MOD nxapp           { binary_operator $1 ($2, "mod") $3 }
  | nxapp                        { $1 }
;
nxun:
  | EXACT_MINUS nxapp { binary_operator (Range.dummy "zero-of-unary-minus", UTIntegerConstant(0)) ($1, "-") $2 }
  | LNOT nxapp        { make_standard (Tok $1) (Ranged $2) (UTApply(($1, UTContentOf([], "not")), $2)) }
  | CONSTRUCTOR nxbot { make_standard (Ranged $1) (Ranged $2) (UTConstructor(extract_name $1, $2)) }
  | CONSTRUCTOR       { make_standard (Ranged $1) (Ranged $1)
                          (UTConstructor(extract_name $1, (Range.dummy "constructor-unitvalue", UTUnitConstant))) }
  | nxapp             { $1 }
;
nxapp:
  | nxapp nxbot    { make_standard (Ranged $1) (Ranged $2) (UTApply($1, $2)) }
  | nxapp CONSTRUCTOR {
      let (rng, constrnm) = $2 in
        make_standard (Ranged $1) (Ranged $2)
          (UTApply($1, (rng, UTConstructor(constrnm, (Range.dummy "constructor-unitvalue", UTUnitConstant))))) }
  | REFNOW nxbot   { make_standard (Tok $1) (Ranged $2) (UTApply(($1, UTContentOf([], "!")), $2)) }
(*
  | REFFINAL nxbot { make_standard (Tok $1) (Ranged $2) (UTReferenceFinal($2)) }
*)
  | nxbot          { $1 }
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
  | tok=UNITVALUE                                         { make_standard (Tok tok) (Tok tok) UTUnitConstant }
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
        let rng = make_range (Ranged mntydom) (Ranged mntycod) in (rng, MFuncType(mntydom, mntycod))
      }
  | mnty=txprod { mnty }
;
txprod: /* -> manual_type */
  | txapppre EXACT_TIMES txprod {
        let rng = make_range (Ranged $1) (Ranged $3) in
          match $3 with
          | (_, MProductType(tylist)) -> (rng, MProductType($1 :: tylist))
          | other                     -> (rng, MProductType([$1; $3]))
      }
  | txapppre { $1 }
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
txbot: /* -> Range.t * type_name */
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
  | mnty=txfunc; SEP; tail=txlist { mnty :: tail }
  | mnty=txfunc                   { mnty :: [] }
  |                               { [] }
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
pats: /* -> code_range * untyped_patter_match_cons */
  | patas ARROW nxletsub {
        let (lastrng, _) = $3 in
          (lastrng, UTPatternMatchCons($1, $3, UTEndOfPatternMatch)) }
  | patas ARROW nxletsub BAR pats {
        let (lastrng, pmcons) = $5 in
          (lastrng, UTPatternMatchCons($1, $3, pmcons)) }
  | patas WHEN nxletsub ARROW nxletsub {
        let (lastrng, _) = $5 in
          (lastrng, UTPatternMatchConsWhen($1, $3, $5, UTEndOfPatternMatch)) }
  | patas WHEN nxletsub ARROW nxletsub BAR pats {
        let (lastrng, pmcons) = $7 in
          (lastrng, UTPatternMatchConsWhen($1, $3, $5, pmcons)) }
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
  | UNITVALUE          { make_standard (Tok $1) (Tok $1) UTPUnitConstant }
  | WILDCARD           { make_standard (Tok $1) (Tok $1) UTPWildCard }
  | VAR                { make_standard (Ranged $1) (Ranged $1) (UTPVariable(extract_name $1)) }
  | LPAREN patas RPAREN                { make_standard (Tok $1) (Tok $3) (extract_main $2) }
  | LPAREN patas COMMA pattuple RPAREN { make_standard (Tok $1) (Tok $5) (UTPTupleCons($2, $4)) }
  | BLIST ELIST                        { make_standard (Tok $1) (Tok $2) UTPEndOfList }
  | opn=OPENQT; strlst=list(str); cls=CLOSEQT {
        let rng = make_range (Tok opn) (Tok cls) in (rng, UTPStringConstant(rng, omit_spaces (String.concat "" strlst))) }
;
patbotwithoutvar: /* -> Types.untyped_pattern_tree */
  | INTCONST           { make_standard (Ranged $1) (Ranged $1) (UTPIntegerConstant(extract_main $1)) }
  | TRUE               { make_standard (Tok $1) (Tok $1) (UTPBooleanConstant(true)) }
  | FALSE              { make_standard (Tok $1) (Tok $1) (UTPBooleanConstant(false)) }
  | UNITVALUE          { make_standard (Tok $1) (Tok $1) UTPUnitConstant }
  | WILDCARD           { make_standard (Tok $1) (Tok $1) UTPWildCard }
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
          | []                -> rngcmd
          | (rnglast, _) :: _ -> rnglast
        in
        let utastcmd = (rngcmd, UTContentOf(mdlnmlst, csnm)) in
          make_standard (Tok rngcmd) (Tok rnglast) (UTMCommand(utastcmd, arglst))
      }
  | tok=VARINMATH        { let (rng, varnm) = tok in (rng, UTMEmbed((rng, UTContentOf([], varnm)))) }
  | tok=VARINMATHWITHMOD { let (rng, mdlnmlst, varnm) = tok in (rng, UTMEmbed((rng, UTContentOf(mdlnmlst, varnm)))) }
;
matharg:
  | opn=BMATHGRP; utast=mathblock; cls=EMATHGRP { let (_, utastmain) = utast in make_standard (Tok opn) (Tok cls) utastmain }
  | opn=BHORZGRP; utast=sxsep; cls=EHORZGRP     { let (_, utastmain) = utast in make_standard (Tok opn) (Tok cls) utastmain }
  | opn=BVERTGRP; utast=vxblock; cls=EVERTGRP   { let (_, utastmain) = utast in make_standard (Tok opn) (Tok cls) utastmain }
  | utast=narg                                  { utast }
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
  | hcmd=hcmd; nargs=nargs; sargs=sargs {
        let (rngcs, mdlnmlst, csnm) = hcmd in
        let utastcmd = (rngcs, UTContentOf(mdlnmlst, csnm)) in
        let args = List.append nargs sargs in
        let rngargs = make_range_from_list args in
          make_standard (Tok rngcs) (Tok rngargs) (UTInputHorzEmbedded(utastcmd, args))
      }
  | opn=OPENMATH; utast=mathblock; cls=CLOSEMATH {
        let utastcmd = (Range.dummy "inline-math", UTContentOf([], "\\math")) in  (* -- inline command '\\math' is inserted -- *)
          make_standard (Tok opn) (Tok cls) (UTInputHorzEmbedded(utastcmd, [utast]))
      }
(*
  | VARINSTR ENDACTIVE { make_standard (Ranged $1) (Tok $2) (UTContentOf([], extract_name $1)) }
*)
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
(*
sxclsnm:
  | CLASSNAME { make_standard (Ranged $1) (Ranged $1) (class_name_to_abstract_tree (extract_name $1)) }
  |           { (Range.dummy "no-class-name1", UTConstructor("Nothing", (Range.dummy "no-class-name2", UTUnitConstant))) }
sxidnm:
  | IDNAME    { make_standard (Ranged $1) (Ranged $1) (id_name_to_abstract_tree (extract_name $1)) }
  |           { (Range.dummy "no-id-name1", UTConstructor("Nothing", (Range.dummy "no-id-name2", UTUnitConstant))) }
;
*)
nargs:
  | nargs=list(narg) { nargs }
;
narg: /* -> untyped_abstract_tree */
  | opn=OPENPROG; utast=nxlet; cls=CLOSEPROG {
        let rng = make_range (Tok opn) (Tok cls) in (rng, extract_main utast)
      }
  | opn=OPENPROG; cls=CLOSEPROG {
        let rng = make_range (Tok opn) (Tok cls) in (rng, UTUnitConstant)
      }
  | opn=OPENPROG_AND_BRECORD; rcd=nxrecord; cls=CLOSEPROG_AND_ERECORD {
        let rng = make_range (Tok opn) (Tok cls) in (rng, UTRecord(rcd))
      }
  | opn=OPENPROG_AND_BLIST; utast=nxlist; cls=CLOSEPROG_AND_ELIST {
        let rng = make_range (Tok opn) (Tok cls) in (rng, extract_main utast)
      }
;
str:
  | chartok=CHAR { let (rng, c) = chartok in c }
  | BREAK        { "\n" }
  | SPACE        { " " }
;
sargs:
  | ENDACTIVE                 { [] }
  | sargs=nonempty_list(sarg) { sargs }
;

sarg: /* -> Types.untyped_argument_cons */
  | opn=BVERTGRP; utast=vxblock; cls=EVERTGRP { make_standard (Tok opn) (Tok cls) (extract_main utast) }
  | opn=BHORZGRP; utast=sxsep; cls=EHORZGRP   { make_standard (Tok opn) (Tok cls) (extract_main utast) }
  | opn=OPENQT; strlst=list(str); cls=CLOSEQT { make_standard (Tok opn) (Tok cls) (omit_spaces (String.concat "" strlst)) }
;
vcmd:
  | tok=VERTCMD        { let (rng, csnm) = tok in (rng, [], csnm) }
  | tok=VERTCMDWITHMOD { tok }
;
vxblock:
  | ivlst=list(vxbot) { (make_range_from_list ivlst, UTInputVert(ivlst)) }
;
vxbot:
  | vcmd=vcmd; nargs=nargs; sargs=sargs {
        let (rngcs, mdlnmlst, csnm) = vcmd in
        let args = List.append nargs sargs in
          make_standard (Tok rngcs) (RangedList args) (UTInputVertEmbedded((rngcs, UTContentOf([], csnm)), args))
      }
;

