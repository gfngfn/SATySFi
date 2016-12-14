%{
  open Types

  type literal_reading_state = Normal | ReadingSpace
  type range_kind =
    | Tok       of Range.t
    | TokArg    of (Range.t * string)
    | Untyped   of untyped_abstract_tree
    | UnMdl     of untyped_module_tree
    | Pat       of untyped_pattern_tree
    | Rng       of Range.t
    | TypeStr   of type_struct
    | VarntCons of untyped_variant_cons


  let make_range (sttx : range_kind) (endx : range_kind) =
    let extract x =
      match x with
      | Tok(rng)          -> rng
      | TokArg(rng, _)    -> rng
      | Untyped(rng, _)   -> rng
      | UnMdl(rng, _)     -> rng
      | Pat(rng, _)       -> rng
      | Rng(rng)          -> rng
      | VarntCons(rng, _) -> rng
      | TypeStr((rng, _)) -> rng
    in
      Range.unite (extract sttx) (extract endx)


  let end_header : untyped_abstract_tree = (Range.dummy "end_header", UTFinishHeaderFile)

  let end_struct : untyped_module_tree   = (Range.dummy "end_struct", UTMFinishModule)


  let rec append_argument_list (arglsta : untyped_argument_cons) (arglstb : untyped_argument_cons) =
    match arglsta with
    | UTEndOfArgument              -> arglstb
    | UTArgumentCons(arg, arglstl) -> UTArgumentCons(arg, (append_argument_list arglstl arglstb))


  let class_and_id_region (utast : untyped_abstract_tree) =
    (Range.dummy "class_and_id_region", UTClassAndIDRegion(utast))


  let convert_into_apply (csutast : untyped_abstract_tree) (clsnmutast : untyped_abstract_tree)
                               (idnmutast : untyped_abstract_tree) (argcons : untyped_argument_cons) =
    let (csrng, _) = csutast in
    let rec iter argcons utastconstr =
      match argcons with
      | UTEndOfArgument                           -> utastconstr
      | UTArgumentCons((argrng, argmain), actail) -> iter actail (Range.unite csrng argrng, UTApply(utastconstr, (argrng, argmain)))
    in
      iter argcons (Range.dummy "convert_into_apply", UTApplyClassAndID(clsnmutast, idnmutast, csutast))


  let class_name_to_abstract_tree (clsnm : class_name) =
    UTConstructor("Just", (Range.dummy "class_name_to", UTStringConstant((String.sub clsnm 1 ((String.length clsnm) - 1)))))


  let id_name_to_abstract_tree (idnm : id_name) =
    UTConstructor("Just", (Range.dummy "id_name_to", UTStringConstant((String.sub idnm 1 ((String.length idnm) - 1)))))


  let rec curry_lambda_abstract (rng : Range.t) (argvarcons : untyped_argument_variable_cons) (utastdef : untyped_abstract_tree) =
    match argvarcons with
    | UTEndOfArgumentVariable                                      -> utastdef
    | UTArgumentVariableCons((varrng, UTPVariable(varnm)), avtail) ->
        (rng, UTLambdaAbstract(varrng, varnm, curry_lambda_abstract rng avtail utastdef))
    | UTArgumentVariableCons((varrng, UTPWildCard), avtail)        ->
        (rng, UTLambdaAbstract(varrng, "%wild", curry_lambda_abstract rng avtail utastdef))
    | UTArgumentVariableCons((varrng, argpatas), avtail)        ->
        let afterabs     = curry_lambda_abstract rng avtail utastdef in
        let dummyutast   = (varrng, UTContentOf("%patarg")) in
        let dummypatcons = UTPatternMatchCons((varrng, argpatas), afterabs, UTEndOfPatternMatch) in
          (rng, UTLambdaAbstract(varrng, "%patarg", (varrng, UTPatternMatch(dummyutast, dummypatcons))))


  let rec stringify_literal ltrl =
    let (_, ltrlmain) = ltrl in
      match ltrlmain with
      | UTConcat(utastf, utastl) -> (stringify_literal utastf) ^ (stringify_literal utastl)
      | UTStringConstant(s)      -> s
      | UTStringEmpty            -> ""
      | _                        -> assert false

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


  let rec omit_spaces (ltrl : untyped_abstract_tree) =
    let str_ltrl = omit_post_spaces (omit_pre_spaces (stringify_literal ltrl)) in
      let min_indent = min_indent_space str_ltrl in
        let str_shaved = shave_indent str_ltrl min_indent in
        let len_shaved = String.length str_shaved in
          if len_shaved >= 1 && str_shaved.[len_shaved - 1] = '\n' then
            let str_no_last_break = String.sub str_shaved 0 (len_shaved - 1) in
              UTConcat(
                (Range.dummy "omit_spaces1", UTStringConstant(str_no_last_break)),
                (Range.dummy "omit_spaces2", UTBreakAndIndent)
              )
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

  (* 'a * 'b -> 'b *)
  let extract_main (_, utastmain) = utastmain


  (* token_position * string -> string *)
  let extract_name (_, name) = name


  let binary_operator (opname : var_name) (lft : untyped_abstract_tree) (oprng : Range.t) (rgt : untyped_abstract_tree) =
    let rng = make_range (Untyped lft) (Untyped rgt) in
      (rng, UTApply((Range.dummy "binary_operator", UTApply((oprng, UTContentOf(opname)), lft)), rgt))


  let make_standard (sttknd : range_kind) (endknd : range_kind) main =
    let rng = make_range sttknd endknd in (rng, main)


  let make_let_expression (lettk : Range.t) (decs : untyped_mutual_let_cons) (utastaft : untyped_abstract_tree) =
    make_standard (Tok lettk) (Untyped utastaft) (UTLetIn(decs, utastaft))

  (* code_range -> (code_range * var_name) -> untyped_abstract_tree
      -> untyped_abstract_tree -> untyped_abstract_tree -> untyped_abstract_tree *)
  let make_let_mutable_expression letmuttk vartk utastdef utastaft =
    let (varrng, varnm) = vartk in
      make_standard (Tok letmuttk) (Untyped utastaft) (UTLetMutableIn(varrng, varnm, utastdef, utastaft))

  (* code_range -> untyped_mutual_variant_cons -> untyped_abstract_tree -> untyped_abstract_tree *)
  let make_variant_declaration firsttk varntdecs utastaft =
    make_standard (Tok firsttk) (Untyped utastaft) (UTDeclareVariantIn(varntdecs, utastaft))

  (* type_struct option -> (code_range * var_name) -> untyped_argument_variable_cons ->
      untyed_abstract_tree -> untyped_mutual_let_cons -> untyped_mutual_let_cons *)
  let make_mutual_let_cons (tyopt : type_struct option) vartk argcons utastdef tailcons =
    let (varrng, varnm) = vartk in
    let curried = curry_lambda_abstract varrng argcons utastdef in
      UTMutualLetCons(tyopt, varnm, curried, tailcons)


  (* type_struct option -> (code_range * var_name) -> untyped_let_pattern_cons ->
      untyped_mutual_let_cons -> untyped_mutual_let_cons *)
  let rec make_mutual_let_cons_par (tyopt : type_struct option) vartk (argletpatcons : untyped_let_pattern_cons) (tailcons : untyped_mutual_let_cons) =
    let (_, varnm) = vartk in
    let pmcons  = make_pattern_match_cons_of_argument_pattern_cons argletpatcons in
    let fullrng = get_range_of_let_pattern_cons argletpatcons in
    let abs     = make_lambda_abstract_for_parallel fullrng argletpatcons pmcons in
      UTMutualLetCons(tyopt, varnm, abs, tailcons)

  (* untyped_let_pattern_cons -> code_range *)
  and get_range_of_let_pattern_cons argletpatcons =
    let get_first_range argletpatcons =
      match argletpatcons with
      | UTLetPatternCons(UTArgumentVariableCons((argpatrng, _), _), _, _) -> argpatrng
      | _                                                                 -> assert false
    in
    let rec get_last_range argletpatcons =
      match argletpatcons with
      | UTEndOfLetPattern                                             -> assert false
      | UTLetPatternCons(argpatcons, (lastrng, _), UTEndOfLetPattern) -> lastrng
      | UTLetPatternCons(_, _, tailcons)                              -> get_last_range tailcons
    in
      make_range (Rng (get_first_range argletpatcons)) (Rng (get_last_range argletpatcons))

  (* code_range -> untyped_let_pattern_cons -> untyped_pattern_match_cons * code_range *)
  and make_pattern_match_cons_of_argument_pattern_cons (argletpatcons : untyped_let_pattern_cons) =
    match argletpatcons with
    | UTEndOfLetPattern                                         -> UTEndOfPatternMatch
    | UTLetPatternCons(argpatcons, utastdef, argletpattailcons) ->
        let tailpmcons = make_pattern_match_cons_of_argument_pattern_cons argletpattailcons in
        let prodpatrng = get_range_of_argument_variable_cons argpatcons in
        let prodpat    = make_product_pattern_of_argument_cons prodpatrng argpatcons in
          UTPatternMatchCons(prodpat, utastdef, tailpmcons)

  and get_range_of_argument_variable_cons argpatcons =
    let get_first_range argpatcons =
      match argpatcons with
      | UTArgumentVariableCons((fstrng, _), _) -> fstrng
      | _                                      -> assert false
    in
    let rec get_last_range argpatcons =
      match argpatcons with
      | UTEndOfArgumentVariable                                       -> assert false
      | UTArgumentVariableCons((lastrng, _), UTEndOfArgumentVariable) -> lastrng
      | UTArgumentVariableCons(_, tailargpatcons)                     -> get_last_range tailargpatcons
    in
      make_range (Rng (get_first_range argpatcons)) (Rng (get_last_range argpatcons))


  (* untyped_argument_variable_cons -> untyped_pattern_tree *)
  and make_product_pattern_of_argument_cons prodpatrng (argpatcons : untyped_argument_variable_cons) =
    let rec subfunc argpatcons =
      match argpatcons with
      | UTEndOfArgumentVariable                  -> (Range.dummy "endofargvar", UTPEndOfTuple)
      | UTArgumentVariableCons(argpat, tailcons) -> (Range.dummy "argvarcons", UTPTupleCons(argpat, subfunc tailcons))
    in
      let (_, prodpatmain) = subfunc argpatcons in (prodpatrng, prodpatmain)

  (* untyped_let_pattern_cons -> untyped_pattern_match_cons -> untyped_abstract_tree *)
  and make_lambda_abstract_for_parallel (fullrng : Range.t) (argletpatcons : untyped_let_pattern_cons)
                                          (pmcons : untyped_pattern_match_cons) =
    match argletpatcons with
    | UTEndOfLetPattern                  -> assert false
    | UTLetPatternCons(argpatcons, _, _) ->
        make_lambda_abstract_for_parallel_sub fullrng (fun u -> u) 0 argpatcons pmcons

  (* code_range -> int -> untyped_argument_variable_cons -> untyped_pattern_match_cons -> untyped_abstract_tree *)
  and make_lambda_abstract_for_parallel_sub (fullrng : Range.t) (k : untyped_abstract_tree -> untyped_abstract_tree)
                                              (i : int) (argpatcons : untyped_argument_variable_cons)
                                                (pmcons : untyped_pattern_match_cons) =
    match argpatcons with
    | UTEndOfArgumentVariable                    -> (fullrng, UTPatternMatch(k (Range.dummy "endoftuple", UTEndOfTuple), pmcons))
    | UTArgumentVariableCons((rng, _), tailcons) ->
(*        let knew = (fun u -> k (dummy_range, UTTupleCons((rng, UTContentOf(numbered_var_name i)), u))) in *)
(*        let knew = (fun u -> k (dummy_range, UTTupleCons(((3000, 0, 0, 0), UTContentOf(numbered_var_name i)), u))) in (* for test *) *)
        let knew = (fun u -> k (Range.dummy "knew1", UTTupleCons((Range.dummy "knew2", UTContentOf(numbered_var_name i)), u))) in (* for test *)
        let after = make_lambda_abstract_for_parallel_sub fullrng knew (i + 1) tailcons pmcons in
          (Range.dummy "pattup1", UTLambdaAbstract(Range.dummy "pattup2", numbered_var_name i, after))

  and numbered_var_name i = "%pattup" ^ (string_of_int i)


  let make_mutual_variant_cons tyargcons typenmtk constrdecs tailcons =
    let typenm = extract_name typenmtk in
      UTMutualVariantCons(tyargcons, typenm, constrdecs, tailcons)

  let make_mutual_synonym_cons tyargcons typenmtk tystr tailcons =
    let typenm = extract_name typenmtk in
      UTMutualSynonymCons(tyargcons, typenm, tystr, tailcons)

  let make_module firsttk mdlnmtk utastdef utastaft =
    let mdlnm = extract_name mdlnmtk in
      make_standard (Tok firsttk) (Untyped utastaft) (UTModule(mdlnm, utastdef, utastaft))

  let make_direct_let_expression lettk decs utmdlaft =
      make_standard (Tok lettk) (UnMdl utmdlaft) (UTMDirectLetIn(decs, utmdlaft))

  let make_public_let_expression lettk decs utmdlaft =
      make_standard (Tok lettk) (UnMdl utmdlaft) (UTMPublicLetIn(decs, utmdlaft))

  let make_private_let_expression lettk decs utmdlaft =
      make_standard (Tok lettk) (UnMdl utmdlaft) (UTMPrivateLetIn(decs, utmdlaft))

  let make_public_let_mutable_expression letmuttk vartk utastdef utmdlaft =
    let (varrng, varnm) = vartk in
      make_standard (Tok letmuttk) (UnMdl utmdlaft) (UTMPublicLetMutableIn(varrng, varnm, utastdef, utmdlaft))

  let make_private_let_mutable_expression letmuttk vartk utastdef utmdlaft =
    let (varrng, varnm) = vartk in
      make_standard (Tok letmuttk) (UnMdl utmdlaft) (UTMPrivateLetMutableIn(varrng, varnm, utastdef, utmdlaft))

  let make_public_variant_declaration firsttk varntdecs utmdlaft =
    make_standard (Tok firsttk) (UnMdl utmdlaft) (UTMPublicDeclareVariantIn(varntdecs, utmdlaft))

  let make_private_variant_declaration firsttk varntdecs utmdlaft =
    make_standard (Tok firsttk) (UnMdl utmdlaft) (UTMPrivateDeclareVariantIn(varntdecs, utmdlaft))


  let rec make_list_to_itemize (lst : (Range.t * int * untyped_abstract_tree) list) =
    (Range.dummy "itemize1", UTItemize(make_list_to_itemize_sub (UTItem((Range.dummy "itemize2", UTStringEmpty), [])) lst 0))

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
  let report_error rngknd tok =
    match rngknd with
    | Tok(tp) ->
        let rng = tp in
          raise (ParseErrorDetail(
            "syntax error:\n"
            ^ "    unexpected token after '" ^ tok ^ "'\n"
            ^ "    " ^ (Range.to_string rng)))
    | TokArg(tp, nm) ->
        let rng = tp in
          raise (ParseErrorDetail(
            "syntax error:\n"
            ^ "    unexpected token after '" ^ nm ^ "'\n"
            ^ "    " ^ (Range.to_string rng)))
    | _ -> raise (ParseErrorDetail("something is wrong"))

%}

%token <Range.t * Types.var_name> VAR
%token <Range.t * Types.var_name> VARINSTR
%token <Range.t * Types.var_name> TYPEVAR
%token <Range.t * Types.constructor_name> CONSTRUCTOR
%token <Range.t * string> NUMCONST CHAR
%token <Range.t * Types.ctrlseq_name> CTRLSEQ
%token <Range.t * Types.id_name>      IDNAME
%token <Range.t * Types.class_name>   CLASSNAME
%token <Range.t> SPACE BREAK
%token <Range.t> LAMBDA ARROW
%token <Range.t> LET DEFEQ LETAND IN
%token <Range.t> MODULE STRUCT ENDSTRUCT PUBLIC PRIVATE DIRECT DOT
%token <Range.t> VARIANT OF MATCH WITH BAR WILDCARD WHEN AS COLON
%token <Range.t> LETMUTABLE OVERWRITEEQ LETLAZY
%token <Range.t> REFNOW REFFINAL
%token <Range.t> IF THEN ELSE
%token <Range.t> TIMES DIVIDES MOD PLUS MINUS EQ NEQ GEQ LEQ GT LT LNOT LAND LOR CONCAT
%token <Range.t> LPAREN RPAREN
%token <Range.t> BGRP EGRP
%token <Range.t> OPENQT CLOSEQT
%token <Range.t> OPENSTR CLOSESTR
%token <Range.t> OPENNUM CLOSENUM
%token <Range.t> TRUE FALSE
%token <Range.t> SEP END COMMA
%token <Range.t> BLIST LISTPUNCT ELIST CONS BRECORD ERECORD ACCESS
%token <Range.t> BEFORE UNITVALUE WHILE DO
%token <Range.t> NEWGLOBALHASH OVERWRITEGLOBALHASH RENEWGLOBALHASH
%token <Range.t * int> ITEM
%token EOI
%token IGNORED

%nonassoc LET DEFEQ IN LETAND LETMUTABLE OVERWRITEEQ
%nonassoc MATCH WITH
%nonassoc IF THEN ELSE
%left OVERWRITEGLOBALHASH
%left BEFORE
%nonassoc WHILE
%left LOR
%left LAND
%nonassoc LNOT
%left EQ NEQ
%left GEQ LEQ GT LT
%right CONS
%left PLUS
%left MINUS
%left TIMES
%right MOD DIVIDES
%nonassoc VAR
%nonassoc LPAREN RPAREN

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
%type <Types.untyped_abstract_tree> sxsepsub
%type <Types.untyped_abstract_tree> sxblock
%type <Types.untyped_abstract_tree> sxbot
%type <Types.untyped_abstract_tree> sxclsnm
%type <Types.untyped_abstract_tree> sxidnm
%type <Types.untyped_argument_cons> narg
%type <Types.untyped_argument_cons> sarg
%type <Types.untyped_argument_cons> sargsub
%type <Types.untyped_argument_variable_cons> argvar
%type <string> binop

%%


main:
  | nxtoplevel  { $1 }
  | sxblock EOI { $1 }
;
nxtoplevel:
/* ---- toplevel style ---- */
  | LET nxdec nxtoplevel                                { make_let_expression $1 $2 $3 }
  | LET nxdec EOI                                       { make_let_expression $1 $2 end_header }
  | LETMUTABLE VAR OVERWRITEEQ nxlet nxtoplevel         { make_let_mutable_expression $1 $2 $4 $5 }
  | LETMUTABLE VAR OVERWRITEEQ nxlet EOI                { make_let_mutable_expression $1 $2 $4 end_header }
  | VARIANT nxvariantdec nxtoplevel                     { make_variant_declaration $1 $2 $3 }
  | VARIANT nxvariantdec EOI                            { make_variant_declaration $1 $2 end_header }
  | LETLAZY nxlazydec nxtoplevel                        { make_let_expression $1 $2 $3 }
  | LETLAZY nxlazydec EOI                               { make_let_expression $1 $2 end_header }
  | MODULE CONSTRUCTOR DEFEQ STRUCT nxstruct nxtoplevel { make_module $1 $2 $5 $6 }
  | MODULE CONSTRUCTOR DEFEQ STRUCT nxstruct EOI        { make_module $1 $2 $5 end_header }
/* ---- transition to expression style ---- */
  | LET nxdec IN nxlet EOI                                { make_let_expression $1 $2 $4 }
  | LETMUTABLE VAR OVERWRITEEQ nxlet IN nxlet EOI         { make_let_mutable_expression $1 $2 $4 $6 }
  | VARIANT nxvariantdec IN nxlet EOI                     { make_variant_declaration $1 $2 $4 }
  | LETLAZY nxlazydec IN nxlet EOI                        { make_let_expression $1 $2 $4 }
  | MODULE CONSTRUCTOR DEFEQ STRUCT nxstruct IN nxlet EOI { make_module $1 $2 $5 $7 }
/* ---- for syntax error log -- */
  | LET error                                 { report_error (Tok $1) "let" }
  | LET nxdec IN error                        { report_error (Tok $3) "in" }
  | LETMUTABLE error                          { report_error (Tok $1) "let-mutable"}
  | LETMUTABLE VAR error                      { report_error (TokArg $2) "" }
  | LETMUTABLE VAR OVERWRITEEQ error          { report_error (Tok $3) "<-" }
  | LETMUTABLE VAR OVERWRITEEQ nxlet IN error { report_error (Tok $5) "in" }
  | VARIANT error                             { report_error (Tok $1) "variant" }
  | MODULE error                              { report_error (Tok $1) "module" }
  | MODULE CONSTRUCTOR DEFEQ error            { report_error (Tok $3) "=" }
  | MODULE CONSTRUCTOR DEFEQ STRUCT error     { report_error (Tok $4) "struct" }
;
nxstruct: /* -> untyped_module_tree */
  | DIRECT LET nxdirectdec nxstruct                    { make_direct_let_expression $1 $3 $4 }
  | DIRECT LET nxdirectdec ENDSTRUCT                   { make_direct_let_expression $1 $3 end_struct }
  | DIRECT LETLAZY nxdirectlazydec nxstruct            { make_direct_let_expression $1 $3 $4 }
  | DIRECT LETLAZY nxdirectlazydec ENDSTRUCT           { make_direct_let_expression $1 $3 end_struct }
  | PUBLIC LET nxpubdec nxstruct                       { make_public_let_expression $1 $3 $4 }
  | PUBLIC LET nxpubdec ENDSTRUCT                      { make_public_let_expression $1 $3 end_struct }
  | PUBLIC LETLAZY nxpublazydec nxstruct               { make_public_let_expression $1 $3 $4 }
  | PUBLIC LETLAZY nxpublazydec ENDSTRUCT              { make_public_let_expression $1 $3 end_struct }
  | PUBLIC LETMUTABLE VAR OVERWRITEEQ nxlet nxstruct   { make_public_let_mutable_expression $1 $3 $5 $6 }
  | PUBLIC LETMUTABLE VAR OVERWRITEEQ nxlet ENDSTRUCT  { make_public_let_mutable_expression $1 $3 $5 end_struct }
  | PUBLIC VARIANT nxvariantdec nxstruct               { make_public_variant_declaration $1 $3 $4 }
  | PUBLIC VARIANT nxvariantdec ENDSTRUCT              { make_public_variant_declaration $1 $3 end_struct }
  | PRIVATE LET nxdec nxstruct                         { make_private_let_expression $1 $3 $4 }
  | PRIVATE LET nxdec ENDSTRUCT                        { make_private_let_expression $1 $3 end_struct }
  | PRIVATE LETLAZY nxlazydec nxstruct                 { make_private_let_expression $1 $3 $4 }
  | PRIVATE LETLAZY nxlazydec ENDSTRUCT                { make_private_let_expression $1 $3 end_struct }
  | PRIVATE LETMUTABLE VAR OVERWRITEEQ nxlet nxstruct  { make_private_let_mutable_expression $1 $3 $5 $6 }
  | PRIVATE LETMUTABLE VAR OVERWRITEEQ nxlet ENDSTRUCT { make_private_let_mutable_expression $1 $3 $5 end_struct }
  | PRIVATE VARIANT nxvariantdec nxstruct              { make_private_variant_declaration $1 $3 $4 }
  | PRIVATE VARIANT nxvariantdec ENDSTRUCT             { make_private_variant_declaration $1 $3 end_struct }
/* -- for syntax error log -- */
  | DIRECT error             { report_error (Tok $1) "direct" }
  | DIRECT LET error         { report_error (Tok $2) "let" }
  | PUBLIC error             { report_error (Tok $1) "private" }
  | PUBLIC LET error         { report_error (Tok $2) "let" }
  | PUBLIC LETMUTABLE error  { report_error (Tok $2) "let" }
  | PUBLIC VARIANT error     { report_error (Tok $2) "variant" }
  | PRIVATE error            { report_error (Tok $1) "private" }
  | PRIVATE LET error        { report_error (Tok $2) "let" }
  | PRIVATE LETMUTABLE error { report_error (Tok $2) "let" }
  | PRIVATE VARIANT error    { report_error (Tok $2) "variant" }
/* -- -- */
;
nxdec: /* -> untyped_mutual_let_cons */
  | VAR COLON txfunc DEFEQ nxlet LETAND nxdec        { make_mutual_let_cons (Some $3) $1 UTEndOfArgumentVariable $5 $7 }

  | VAR COLON txfunc DEFEQ nxlet                     { make_mutual_let_cons (Some $3) $1 UTEndOfArgumentVariable $5 UTEndOfMutualLet }

  | VAR argvar DEFEQ nxlet LETAND nxdec              { make_mutual_let_cons None $1 $2 $4 $6 }
  | VAR COLON txfunc BAR
        argvar DEFEQ nxlet LETAND nxdec              { make_mutual_let_cons (Some $3) $1 $5 $7 $9 }

  | VAR argvar DEFEQ nxlet                           { make_mutual_let_cons None $1 $2 $4 UTEndOfMutualLet }
  | VAR COLON txfunc BAR
        argvar DEFEQ nxlet                           { make_mutual_let_cons (Some $3) $1 $5 $7 UTEndOfMutualLet }

  | VAR argvar DEFEQ nxlet BAR nxdecpar LETAND nxdec { make_mutual_let_cons_par None $1 (UTLetPatternCons($2, $4, $6)) $8 }
  | VAR COLON txfunc BAR
        argvar DEFEQ nxlet BAR nxdecpar LETAND nxdec { make_mutual_let_cons_par (Some $3) $1 (UTLetPatternCons($5, $7, $9)) $11 }

  | VAR argvar DEFEQ nxlet BAR nxdecpar              { make_mutual_let_cons_par None $1 (UTLetPatternCons($2, $4, $6)) UTEndOfMutualLet }
  | VAR COLON txfunc BAR
        argvar DEFEQ nxlet BAR nxdecpar              { make_mutual_let_cons_par (Some $3) $1 (UTLetPatternCons($5, $7, $9)) UTEndOfMutualLet }

  | CTRLSEQ COLON txfunc DEFEQ nxlet LETAND nxdec        { make_mutual_let_cons (Some $3) $1 UTEndOfArgumentVariable (class_and_id_region $5) $7 }

  | CTRLSEQ COLON txfunc DEFEQ nxlet                     { make_mutual_let_cons (Some $3) $1 UTEndOfArgumentVariable (class_and_id_region $5) UTEndOfMutualLet }

  | CTRLSEQ argvar DEFEQ nxlet LETAND nxdec              { make_mutual_let_cons None $1 $2 (class_and_id_region $4) $6 }
  | CTRLSEQ COLON txfunc BAR
            argvar DEFEQ nxlet LETAND nxdec              { make_mutual_let_cons (Some $3) $1 $5 (class_and_id_region $7) $9 }

  | CTRLSEQ argvar DEFEQ nxlet                           { make_mutual_let_cons None $1 $2 (class_and_id_region $4) UTEndOfMutualLet }
  | CTRLSEQ COLON txfunc BAR
            argvar DEFEQ nxlet                           { make_mutual_let_cons (Some $3) $1 $5 (class_and_id_region $7) UTEndOfMutualLet }

  | CTRLSEQ argvar DEFEQ nxlet BAR nxdecpar LETAND nxdec { make_mutual_let_cons_par None $1 (UTLetPatternCons($2, class_and_id_region $4, $6)) $8 }
  | CTRLSEQ COLON txfunc BAR
            argvar DEFEQ nxlet BAR nxdecpar LETAND nxdec { make_mutual_let_cons_par (Some $3) $1 (UTLetPatternCons($5, class_and_id_region $7, $9)) $11 }

  | CTRLSEQ argvar DEFEQ nxlet BAR nxdecpar              { make_mutual_let_cons_par None $1 (UTLetPatternCons($2, class_and_id_region $4, $6)) UTEndOfMutualLet }
  | CTRLSEQ COLON txfunc BAR
            argvar DEFEQ nxlet BAR nxdecpar              { make_mutual_let_cons_par (Some $3) $1 (UTLetPatternCons($5, class_and_id_region $7, $9)) UTEndOfMutualLet }
/* -- for syntax error log -- */
  | VAR error                                        { report_error (TokArg $1) "" }
  | VAR COLON error                                  { report_error (Tok $2) ":" }
  | VAR COLON txfunc DEFEQ error                     { report_error (Tok $4) "=" }
  | VAR COLON txfunc BAR
        error                                        { report_error (Tok $4) "|" }
  | VAR COLON txfunc BAR
        argvar DEFEQ error                           { report_error (Tok $6) "=" }
  | VAR COLON txfunc BAR
        argvar DEFEQ nxlet BAR error                 { report_error (Tok $8) "|" }
  | VAR COLON txfunc BAR
        argvar DEFEQ nxlet LETAND error              { report_error (Tok $8) "and" }
  | VAR COLON txfunc BAR
        argvar DEFEQ nxlet BAR nxdecpar LETAND error { report_error (Tok $10) "and" }
  | VAR argvar DEFEQ error                           { report_error (Tok $3) "=" }
  | VAR argvar DEFEQ nxlet BAR error                 { report_error (Tok $5) "|" }
  | VAR argvar DEFEQ nxlet LETAND error              { report_error (Tok $5) "and" }
  | VAR argvar DEFEQ nxlet BAR nxdecpar LETAND error { report_error (Tok $7) "and" }
  | CTRLSEQ error                                    { report_error (TokArg $1) "" }
  | CTRLSEQ argvar DEFEQ error                       { report_error (Tok $3) "=" }
  | CTRLSEQ argvar DEFEQ nxlet BAR error             { report_error (Tok $5) "|" }
  | CTRLSEQ argvar DEFEQ nxlet LETAND error          { report_error (Tok $5) "and" }
/* -- -- */
;
nxdecpar:
  | argvar DEFEQ nxlet BAR nxdecpar { UTLetPatternCons($1, $3, $5) }
  | argvar DEFEQ nxlet              { UTLetPatternCons($1, $3, UTEndOfLetPattern) }
;
nxlazydec:
  | VAR DEFEQ nxlet LETAND nxlazydec {
        let rng = make_range (Untyped $3) (Untyped $3) in
          make_mutual_let_cons None $1 UTEndOfArgumentVariable (rng, UTLazyContent($3)) $5
      }
  | VAR COLON txfunc DEFEQ nxlet LETAND nxlazydec {
        let rng = make_range (Untyped $5) (Untyped $5) in
          make_mutual_let_cons (Some $3) $1 UTEndOfArgumentVariable (rng, UTLazyContent($5)) $7
      }
  | VAR DEFEQ nxlet {
        let rng = make_range (Untyped $3) (Untyped $3) in
          make_mutual_let_cons None $1 UTEndOfArgumentVariable (rng, UTLazyContent($3)) UTEndOfMutualLet
      }
  | VAR COLON txfunc DEFEQ nxlet {
        let rng = make_range (Untyped $5) (Untyped $5) in
          make_mutual_let_cons (Some $3) $1 UTEndOfArgumentVariable (rng, UTLazyContent($5)) UTEndOfMutualLet
      }
  | CTRLSEQ DEFEQ nxlet LETAND nxlazydec {
        let rng = make_range (Untyped $3) (Untyped $3) in
          make_mutual_let_cons None $1 UTEndOfArgumentVariable (rng, UTLazyContent(class_and_id_region $3)) $5
      }
  | CTRLSEQ COLON txfunc DEFEQ nxlet LETAND nxlazydec {
        let rng = make_range (Untyped $5) (Untyped $5) in
          make_mutual_let_cons (Some $3) $1 UTEndOfArgumentVariable (rng, UTLazyContent(class_and_id_region $5)) $7
      }
  | CTRLSEQ DEFEQ nxlet {
        let rng = make_range (Untyped $3) (Untyped $3) in
          make_mutual_let_cons None $1 UTEndOfArgumentVariable (rng, UTLazyContent(class_and_id_region $3)) UTEndOfMutualLet
      }
  | CTRLSEQ COLON txfunc DEFEQ nxlet {
        let rng = make_range (Untyped $5) (Untyped $5) in
          make_mutual_let_cons (Some $3) $1 UTEndOfArgumentVariable (rng, UTLazyContent(class_and_id_region $5)) UTEndOfMutualLet
      }
/* -- for syntax error log -- */
  | VAR error                                 { report_error (TokArg $1) "" }
  | VAR COLON error                           { report_error (Tok $2) ":" }
  | VAR COLON txfunc DEFEQ error              { report_error (Tok $4) "=" }
  | VAR COLON txfunc DEFEQ nxlet LETAND error { report_error (Tok $6) "and" }
  | VAR DEFEQ error                           { report_error (Tok $2) "=" }
  | VAR DEFEQ nxlet LETAND error              { report_error (Tok $4) "and" }
/* -- -- */
;
nxpubdec: /* -> untyped_mutual_let_cons */
  | VAR COLON txfunc DEFEQ nxlet LETAND nxpubdec     { make_mutual_let_cons (Some $3) $1 UTEndOfArgumentVariable $5 $7 }
  | VAR COLON txfunc DEFEQ nxlet                     { make_mutual_let_cons (Some $3) $1 UTEndOfArgumentVariable $5 UTEndOfMutualLet }

  | VAR argvar DEFEQ nxlet LETAND nxpubdec           { make_mutual_let_cons None $1 $2 $4 $6 }
  | VAR COLON txfunc BAR
        argvar DEFEQ nxlet LETAND nxpubdec           { make_mutual_let_cons (Some $3) $1 $5 $7 $9 }

  | VAR argvar DEFEQ nxlet BAR nxdecpar LETAND nxdec { make_mutual_let_cons_par None $1 (UTLetPatternCons($2, $4, $6)) $8 }
  | VAR COLON txfunc BAR
        argvar DEFEQ nxlet BAR nxdecpar LETAND nxdec { make_mutual_let_cons_par (Some $3) $1 (UTLetPatternCons($5, $7, $9)) $11 }

  | VAR argvar DEFEQ nxlet                           { make_mutual_let_cons None $1 $2 $4 UTEndOfMutualLet }
  | VAR COLON txfunc BAR
        argvar DEFEQ nxlet                           { make_mutual_let_cons (Some $3) $1 $5 $7 UTEndOfMutualLet }

  | VAR argvar DEFEQ nxlet BAR nxdecpar              { make_mutual_let_cons_par None $1 (UTLetPatternCons($2, $4, $6)) UTEndOfMutualLet }
  | VAR COLON txfunc BAR
        argvar DEFEQ nxlet BAR nxdecpar              { make_mutual_let_cons_par (Some $3) $1 (UTLetPatternCons($5, $7, $9)) UTEndOfMutualLet }
/* -- for syntax error log -- */
  | VAR error                               { report_error (TokArg $1) "" }
  | VAR COLON error                         { report_error (Tok $2) ":" }
  | VAR COLON txfunc BAR error              { report_error (Tok $4) "|" }
  | VAR argvar DEFEQ error                  { report_error (Tok $3) "=" }
  | VAR argvar DEFEQ nxlet LETAND error     { report_error (Tok $5) "and" }
  | VAR argvar DEFEQ nxlet BAR error        { report_error (Tok $5) "|" }
/* -- -- */
;
nxpublazydec:
  | VAR DEFEQ nxlet LETAND nxpublazydec {
        let rng = make_range (Untyped $3) (Untyped $3) in
          make_mutual_let_cons None $1 UTEndOfArgumentVariable (rng, UTLazyContent($3)) $5
      }
  | VAR COLON txfunc DEFEQ nxlet LETAND nxpublazydec {
        let rng = make_range (Untyped $5) (Untyped $5) in
          make_mutual_let_cons (Some $3) $1 UTEndOfArgumentVariable (rng, UTLazyContent($5)) $7
      }
  | VAR DEFEQ nxlet {
        let rng = make_range (Untyped $3) (Untyped $3) in
          make_mutual_let_cons None $1 UTEndOfArgumentVariable (rng, UTLazyContent($3)) UTEndOfMutualLet
      }
  | VAR COLON txfunc DEFEQ nxlet {
        let rng = make_range (Untyped $5) (Untyped $5) in
          make_mutual_let_cons (Some $3) $1 UTEndOfArgumentVariable (rng, UTLazyContent($5)) UTEndOfMutualLet
      }
/* -- for syntax error log -- */
  | VAR error                                 { report_error (TokArg $1) "" }
  | VAR COLON error                           { report_error (Tok $2) ":" }
  | VAR COLON txfunc DEFEQ error              { report_error (Tok $4) "=" }
  | VAR COLON txfunc DEFEQ nxlet LETAND error { report_error (Tok $6) "and" }
  | VAR DEFEQ error                           { report_error (Tok $2) "=" }
  | VAR DEFEQ nxlet LETAND error              { report_error (Tok $4) "and" }
/* -- -- */
;
nxdirectdec: /* -> untyped_mutual_let_cons */
  | CTRLSEQ COLON txfunc DEFEQ nxlet LETAND nxdirectdec { make_mutual_let_cons (Some $3) $1 UTEndOfArgumentVariable (class_and_id_region $5) $7 }

  | CTRLSEQ COLON txfunc DEFEQ nxlet                    { make_mutual_let_cons (Some $3) $1 UTEndOfArgumentVariable (class_and_id_region $5) UTEndOfMutualLet }

  | CTRLSEQ argvar DEFEQ nxlet LETAND nxdirectdec       { make_mutual_let_cons None $1 $2 (class_and_id_region $4) $6 }
  | CTRLSEQ COLON txfunc BAR
            argvar DEFEQ nxlet LETAND nxdirectdec       { make_mutual_let_cons (Some $3) $1 $5 (class_and_id_region $7) $9 }

  | CTRLSEQ argvar DEFEQ nxlet                          { make_mutual_let_cons None $1 $2 (class_and_id_region $4) UTEndOfMutualLet }
  | CTRLSEQ COLON txfunc BAR
            argvar DEFEQ nxlet                          { make_mutual_let_cons (Some $3) $1 $5 (class_and_id_region $7) UTEndOfMutualLet }

  | CTRLSEQ argvar DEFEQ nxlet BAR nxdecpar LETAND nxdirectdec { make_mutual_let_cons_par None $1 (UTLetPatternCons($2, class_and_id_region $4, $6)) $8 }
  | CTRLSEQ COLON txfunc BAR
            argvar DEFEQ nxlet BAR nxdecpar LETAND nxdirectdec { make_mutual_let_cons_par (Some $3) $1 (UTLetPatternCons($5, class_and_id_region $7, $9)) $11 }

  | CTRLSEQ argvar DEFEQ nxlet BAR nxdecpar                    { make_mutual_let_cons_par None $1 (UTLetPatternCons($2, class_and_id_region $4, $6)) UTEndOfMutualLet }
  | CTRLSEQ COLON txfunc BAR
            argvar DEFEQ nxlet BAR nxdecpar                    { make_mutual_let_cons_par (Some $3) $1 (UTLetPatternCons($5, class_and_id_region $7, $9)) UTEndOfMutualLet }
/* -- for syntax error log -- */
  | CTRLSEQ error                                        { report_error (TokArg $1) "" }
  | CTRLSEQ COLON error                                  { report_error (Tok $2) ":" }
  | CTRLSEQ COLON txfunc DEFEQ error                     { report_error (Tok $4) "=" }
  | CTRLSEQ COLON txfunc DEFEQ nxlet LETAND error        { report_error (Tok $6) "and" }
  | CTRLSEQ COLON txfunc BAR
            error                                        { report_error (Tok $4) "|" }
  | CTRLSEQ COLON txfunc BAR
            argvar DEFEQ error                           { report_error (Tok $6) "=" }
  | CTRLSEQ COLON txfunc BAR
            argvar DEFEQ nxlet LETAND error              { report_error (Tok $8) "and" }
  | CTRLSEQ COLON txfunc BAR
            argvar DEFEQ nxlet BAR error                 { report_error (Tok $8) "|" }
  | CTRLSEQ COLON txfunc BAR
            argvar DEFEQ nxlet BAR nxdecpar LETAND error { report_error (Tok $10) "and" }
  | CTRLSEQ argvar DEFEQ error                           { report_error (Tok $3) "=" }
  | CTRLSEQ argvar DEFEQ nxlet LETAND error              { report_error (Tok $5) "and" }
  | CTRLSEQ argvar DEFEQ nxlet BAR error                 { report_error (Tok $5) "|" }
  | CTRLSEQ argvar DEFEQ nxlet BAR nxdecpar LETAND error { report_error (Tok $7) "and" }
/* -- -- */
;
nxdirectlazydec:
  | CTRLSEQ DEFEQ nxlet LETAND nxdirectlazydec {
        let rng = make_range (Untyped $3) (Untyped $3) in
          make_mutual_let_cons None $1 UTEndOfArgumentVariable (rng, UTLazyContent(class_and_id_region $3)) $5
      }
  | CTRLSEQ COLON txfunc DEFEQ nxlet LETAND nxdirectlazydec {
        let rng = make_range (Untyped $5) (Untyped $5) in
          make_mutual_let_cons (Some $3) $1 UTEndOfArgumentVariable (rng, UTLazyContent(class_and_id_region $5)) $7
      }
  | CTRLSEQ DEFEQ nxlet {
        let rng = make_range (Untyped $3) (Untyped $3) in
          make_mutual_let_cons None $1 UTEndOfArgumentVariable (rng, UTLazyContent(class_and_id_region $3)) UTEndOfMutualLet
      }
  | CTRLSEQ COLON txfunc DEFEQ nxlet {
        let rng = make_range (Untyped $5) (Untyped $5) in
          make_mutual_let_cons (Some $3) $1 UTEndOfArgumentVariable (rng, UTLazyContent(class_and_id_region $5)) UTEndOfMutualLet
      }
/* -- for syntax error log -- */
  | CTRLSEQ error                                 { report_error (TokArg $1) "" }
  | CTRLSEQ COLON error                           { report_error (Tok $2) ":" }
  | CTRLSEQ COLON txfunc DEFEQ error              { report_error (Tok $4) "=" }
  | CTRLSEQ COLON txfunc DEFEQ nxlet LETAND error { report_error (Tok $6) "and" }
  | CTRLSEQ DEFEQ error                           { report_error (Tok $2) "=" }
  | CTRLSEQ DEFEQ nxlet LETAND error              { report_error (Tok $4) "and" }
/* -- -- */
;
nxvariantdec: /* -> untyped_mutual_variant_cons */
  | xpltyvars VAR DEFEQ variants LETAND nxvariantdec     { make_mutual_variant_cons $1 $2 $4 $6 }
  | xpltyvars VAR DEFEQ variants                         { make_mutual_variant_cons $1 $2 $4 UTEndOfMutualVariant }
  | xpltyvars VAR DEFEQ BAR variants LETAND nxvariantdec { make_mutual_variant_cons $1 $2 $5 $7 }
  | xpltyvars VAR DEFEQ BAR variants                     { make_mutual_variant_cons $1 $2 $5 UTEndOfMutualVariant }
  | xpltyvars VAR DEFEQ txfunc LETAND nxvariantdec       { make_mutual_synonym_cons $1 $2 $4 $6 }
  | xpltyvars VAR DEFEQ txfunc                           { make_mutual_synonym_cons $1 $2 $4 UTEndOfMutualVariant }
/* -- for syntax error log -- */
  | xpltyvars VAR error                           { report_error (TokArg $2) "" }
  | xpltyvars VAR DEFEQ error                     { report_error (Tok $3) "=" }
  | xpltyvars VAR DEFEQ BAR error                 { report_error (Tok $4) "|" }
  | xpltyvars VAR DEFEQ BAR variants LETAND error { report_error (Tok $6) "and" }
/* -- -- */
;
xpltyvars: /* -> untyped_explicit_type_variable_cons */
  | TYPEVAR xpltyvars { let (rng, tyargnm) = $1 in UTTypeArgumentCons(rng, tyargnm, $2) }
  |                   { UTEndOfTypeArgument }
;
nxlet:
  | MATCH nxlet WITH pats      {
        let (lastrng, pmcons) = $4 in make_standard (Tok $1) (Rng lastrng) (UTPatternMatch($2, pmcons)) }
  | MATCH nxlet WITH BAR pats  {
        let (lastrng, pmcons) = $5 in make_standard (Tok $1) (Rng lastrng) (UTPatternMatch($2, pmcons)) }
  | nxletsub                   { $1 }
/* -- for syntax error log -- */
  | MATCH error                { report_error (Tok $1) "match" }
  | MATCH nxlet WITH error     { report_error (Tok $3) "with" }
  | MATCH nxlet WITH BAR error { report_error (Tok $4) "|" }
/* -- -- */
nxletsub:
  | LET nxdec IN nxlet                        { make_let_expression $1 $2 $4 }
  | LETMUTABLE VAR OVERWRITEEQ nxlet IN nxlet { make_let_mutable_expression $1 $2 $4 $6 }
  | nxwhl { $1 }
/* -- for syntax error log -- */
  | LET error                                 { report_error (Tok $1) "let" }
  | LETMUTABLE error                          { report_error (Tok $1) "let-mutable" }
  | LETMUTABLE VAR error                      { report_error (TokArg $2) "" }
  | LETMUTABLE VAR OVERWRITEEQ error          { report_error (Tok $3) "->" }
  | LETMUTABLE VAR OVERWRITEEQ nxlet IN error { report_error (Tok $5) "in" }
/* -- -- */
;
nxwhl:
  | WHILE nxlet DO nxwhl { make_standard (Tok $1) (Untyped $4) (UTWhileDo($2, $4)) }
  | nxif                 { $1 }
/* -- for syntax error log -- */
  | WHILE error          { report_error (Tok $1) "while" }
  | WHILE nxlet DO error { report_error (Tok $3) "do" }
/* -- -- */
nxif:
  | IF nxlet THEN nxlet ELSE nxlet       { make_standard (Tok $1) (Untyped $6) (UTIfThenElse($2, $4, $6)) }
  | nxbfr                                { $1 }
/* -- for syntax error log -- */
  | IF error                             { report_error (Tok $1) "if" }
  | IF nxlet THEN error                  { report_error (Tok $3) "then" }
  | IF nxlet THEN nxlet ELSE error       { report_error (Tok $5) "else" }
/* -- -- */
;
nxbfr:
  | nxlambda BEFORE nxbfr { make_standard (Untyped $1) (Untyped $3) (UTSequential($1, $3)) }
  | nxlambda              { $1 }
/* -- for syntax error log -- */
  | nxlambda BEFORE error { report_error (Tok $2) "before" }
/* -- -- */
;
nxlambda:
  | VAR OVERWRITEEQ nxlor {
        let (varrng, varnm) = $1 in
          make_standard (TokArg $1) (Untyped $3) (UTOverwrite(varrng, varnm, $3)) }
  | NEWGLOBALHASH nxlet OVERWRITEGLOBALHASH nxlor {
        make_standard (Tok $1) (Untyped $4) (UTDeclareGlobalHash($2, $4)) }
  | RENEWGLOBALHASH nxlet OVERWRITEGLOBALHASH nxlor {
        make_standard (Tok $1) (Untyped $4) (UTOverwriteGlobalHash($2, $4)) }
  | LAMBDA argvar ARROW nxlor {
        let rng = make_range (Tok $1) (Untyped $4) in curry_lambda_abstract rng $2 $4 }
  | nxlor { $1 }
/* -- for syntax error log -- */
  | VAR error                                       { report_error (TokArg $1) "" }
  | NEWGLOBALHASH error                             { report_error (Tok $1) "new-global-hash" }
  | NEWGLOBALHASH nxlet OVERWRITEGLOBALHASH error   { report_error (Tok $3) "<<-" }
  | RENEWGLOBALHASH error                           { report_error (Tok $1) "renew-global-hash" }
  | RENEWGLOBALHASH nxlet OVERWRITEGLOBALHASH error { report_error (Tok $3) "<<-" }
  | LAMBDA error                                    { report_error (Tok $1) "function" }
  | LAMBDA argvar ARROW error                       { report_error (Tok $3) "->" }
/* -- -- */
;
argvar: /* -> argument_variable_cons */
  | patbot argvar                           { UTArgumentVariableCons($1, $2) }
/*
  | patbot argvar                           { UTArgumentVariableCons($1, NoTypeAnnotationForArgument, $2) }
  | LPAREN patas RPAREN argvar              { UTArgumentVariableCons($2, NoTypeAnnotationForArgument, $4) }
  | LPAREN patas COLON txfunc RPAREN argvar { UTArgumentVariableCons($2, TypeAnnotationForArgument($4), $6) }
*/
  |                                         { UTEndOfArgumentVariable }
;
nxlor:
  | nxland LOR nxlor    { binary_operator "||" $1 $2 $3 }
  | nxland              { $1 }
/* -- for syntax error log -- */
  | nxland LOR error    { report_error (Tok $2) "||" }
/* -- -- */
;
nxland:
  | nxcomp LAND nxland  { binary_operator "&&" $1 $2 $3 }
  | nxcomp              { $1 }
/* -- for syntax error log -- */
  | nxcomp LAND error   { report_error (Tok $2) "&&" }
/* -- -- */
;
nxcomp:
  | nxconcat EQ nxcomp  { binary_operator "==" $1 $2 $3 }
  | nxconcat NEQ nxcomp { binary_operator "<>" $1 $2 $3 }
  | nxconcat GEQ nxcomp { binary_operator ">=" $1 $2 $3 }
  | nxconcat LEQ nxcomp { binary_operator "<=" $1 $2 $3 }
  | nxconcat GT nxcomp  { binary_operator ">" $1 $2 $3 }
  | nxconcat LT nxcomp  { binary_operator "<" $1 $2 $3 }
  | nxconcat            { $1 }
/* -- for syntax error log -- */
  | nxconcat EQ error   { report_error (Tok $2) "==" }
  | nxconcat NEQ error  { report_error (Tok $2) "<>" }
  | nxconcat GEQ error  { report_error (Tok $2) ">=" }
  | nxconcat LEQ error  { report_error (Tok $2) "<=" }
  | nxconcat GT error   { report_error (Tok $2) ">" }
  | nxconcat LT error   { report_error (Tok $2) "<" }
/* -- -- */
;
nxconcat:
  | nxlplus CONCAT nxconcat { binary_operator "^" $1 $2 $3 }
  | nxlplus CONS nxconcat   { binary_operator "::" $1 $2 $3 }
  | nxlplus                 { $1 }
/* -- for syntax error log -- */
  | nxlplus CONCAT error    { report_error (Tok $2) "^" }
/* -- -- */
;
nxlplus:
  | nxlminus PLUS nxrplus   { binary_operator "+" $1 $2 $3 }
  | nxlminus                { $1 }
/* -- for syntax error log -- */
  | nxlminus PLUS error     { report_error (Tok $2) "+" }
/* -- -- */
;
nxlminus:
  | nxlplus MINUS nxrtimes  { binary_operator "-" $1 $2 $3 }
  | nxltimes                { $1 }
/* -- for syntax error log -- */
  | nxlplus MINUS error     { report_error (Tok $2) "-" }
/* -- -- */
;
nxrplus:
  | nxrminus PLUS nxrplus   { binary_operator "+" $1 $2 $3 }
  | nxrminus                { $1 }
/* -- for syntax error log -- */
  | nxrminus PLUS error     { report_error (Tok $2) "+" }
/* -- -- */
;
nxrminus:
  | nxrplus MINUS nxrtimes  { binary_operator "-" $1 $2 $3 }
  | nxrtimes                { $1 }
/* -- for syntax error log -- */
  | nxrplus MINUS error     { report_error (Tok $2) "-" }
/* -- -- */
;
nxltimes:
  | nxun TIMES nxrtimes     { binary_operator "*" $1 $2 $3 }
  | nxltimes DIVIDES nxapp  { binary_operator "/" $1 $2 $3 }
  | nxltimes MOD nxapp      { binary_operator "mod" $1 $2 $3 }
  | nxun                    { $1 }
/* -- for syntax error log -- */
  | nxun TIMES error        { report_error (Tok $2) "*" }
  | nxltimes DIVIDES error  { report_error (Tok $2) "/" }
  | nxltimes MOD error      { report_error (Tok $2) "mod" }
/* -- -- */
;
nxrtimes:
  | nxapp TIMES nxrtimes   { binary_operator "*" $1 $2 $3 }
  | nxrtimes DIVIDES nxapp { binary_operator "/" $1 $2 $3 }
  | nxrtimes MOD nxapp     { binary_operator "mod" $1 $2 $3 }
  | nxapp                  { $1 }
/* -- for syntax error log -- */
  | nxapp TIMES error      { report_error (Tok $2) "*" }
  | nxrtimes DIVIDES error { report_error (Tok $2) "/" }
  | nxrtimes MOD error     { report_error (Tok $2) "mod" }
/* -- -- */
;
nxun:
  | MINUS nxapp       { binary_operator "-" (Range.dummy "zero-of-unary-minus", UTNumericConstant(0)) $1 $2 }
  | LNOT nxapp        { make_standard (Tok $1) (Untyped $2) (UTApply(($1, UTContentOf("not")), $2)) }
  | CONSTRUCTOR nxbot { make_standard (TokArg $1) (Untyped $2) (UTConstructor(extract_name $1, $2)) }
  | CONSTRUCTOR       { make_standard (TokArg $1) (TokArg $1)
                          (UTConstructor(extract_name $1, (Range.dummy "constructor-unitvalue", UTUnitConstant))) }
  | nxapp             { $1 }
/* -- for syntax error log -- */
  | MINUS error       { report_error (Tok $1) "-" }
  | LNOT error        { report_error (Tok $1) "not" }
/* -- -- */
;
nxapp:
  | nxapp nxbot    { make_standard (Untyped $1) (Untyped $2) (UTApply($1, $2)) }
  | REFNOW nxbot   { make_standard (Tok $1) (Untyped $2) (UTApply(($1, UTContentOf("!")), $2)) }
  | REFFINAL nxbot { make_standard (Tok $1) (Untyped $2) (UTReferenceFinal($2)) }
  | nxbot          { $1 }
/* -- for syntax error log -- */
  | REFNOW error   { report_error (Tok $1) "!" }
  | REFFINAL error { report_error (Tok $1) "!!" }
/* -- -- */
;
nxbot:
  | nxbot ACCESS VAR    { make_standard (Untyped $1) (TokArg $3) (UTAccessField($1, extract_name $3)) }
  | VAR                 { make_standard (TokArg $1) (TokArg $1) (UTContentOf(extract_name $1)) }
  | CONSTRUCTOR DOT VAR { make_standard (TokArg $1) (TokArg $3) (UTContentOf((extract_name $1) ^ "." ^ (extract_name $3))) }
  | NUMCONST            { make_standard (TokArg $1) (TokArg $1)  (UTNumericConstant(int_of_string (extract_name $1))) }
  | TRUE                            { make_standard (Tok $1) (Tok $1) (UTBooleanConstant(true)) }
  | FALSE                           { make_standard (Tok $1) (Tok $1) (UTBooleanConstant(false)) }
  | UNITVALUE                       { make_standard (Tok $1) (Tok $1) UTUnitConstant }
  | LPAREN nxlet RPAREN             { make_standard (Tok $1) (Tok $3) (extract_main $2) }
  | LPAREN nxlet COMMA tuple RPAREN { make_standard (Tok $1) (Tok $5) (UTTupleCons($2, $4)) }
  | OPENSTR sxsep CLOSESTR          { make_standard (Tok $1) (Tok $3) (extract_main $2) }
  | OPENQT sxblock CLOSEQT          { make_standard (Tok $1) (Tok $3) (omit_spaces $2) }
  | BLIST ELIST                     { make_standard (Tok $1) (Tok $2) UTEndOfList }
  | BLIST nxlet nxlist ELIST        { make_standard (Tok $1) (Tok $4) (UTListCons($2, $3)) }
  | LPAREN binop RPAREN             { make_standard (Tok $1) (Tok $3) (UTContentOf($2)) }
  | BRECORD ERECORD                 { make_standard (Tok $1) (Tok $2) (UTRecord([])) }
  | BRECORD nxrecord ERECORD        { make_standard (Tok $1) (Tok $3) (UTRecord($2)) }
/* -- for syntax error log -- */
  | BLIST error   { report_error (Tok $1) "[" }
  | OPENSTR error { report_error (Tok $1) "{ (beginning of text area)" }
  | LPAREN error  { report_error (Tok $1) "(" }
  | BRECORD error { report_error (Tok $1) "(|" }
/* -- -- */
;
nxrecord:
  | VAR DEFEQ nxlet                    { (extract_name $1, $3) :: [] }
  | VAR DEFEQ nxlet LISTPUNCT nxrecord { (extract_name $1, $3) :: $5 }
/* -- for syntax error log -- */
  | VAR DEFEQ error { report_error (TokArg $1) ((extract_name $1) ^ " =") }
/* -- -- */
;
nxlist:
  | LISTPUNCT nxlet nxlist { make_standard (Tok $1) (Untyped $3) (UTListCons($2, $3)) }
  |                        { (Range.dummy "end-of-list", UTEndOfList) }
/* -- for syntax error log -- */
  | LISTPUNCT error        { report_error (Tok $1) ";" }
/* -- -- */
;
variants: /* -> untyped_variant_cons */
  | CONSTRUCTOR OF txfunc BAR variants  { make_standard (TokArg $1) (VarntCons $5)
                                            (UTVariantCons(extract_name $1, $3, $5)) }
  | CONSTRUCTOR OF txfunc               { make_standard (TokArg $1) (TypeStr $3)
                                            (UTVariantCons(extract_name $1, $3, (Range.dummy "end-of-variant1", UTEndOfVariant))) }
  | CONSTRUCTOR BAR variants            { make_standard (TokArg $1) (VarntCons $3)
                                             (UTVariantCons(extract_name $1, (Range.dummy "dec-constructor-unit1", VariantType([], "unit")), $3)) }
  | CONSTRUCTOR { make_standard (TokArg $1) (TokArg $1)
                    (UTVariantCons(extract_name $1, (Range.dummy "dec-constructor-unit2", VariantType([], "unit")), (Range.dummy "end-of-variant2", UTEndOfVariant))) }
/* -- for syntax error log -- */
  | CONSTRUCTOR OF error            { report_error (Tok $2) "of" }
  | CONSTRUCTOR OF txfunc BAR error { report_error (Tok $4) "|" }
/* -- -- */
;
txfunc: /* -> type_struct */
  | txprod ARROW txfunc {
        let rng = make_range (TypeStr $1) (TypeStr $3) in (rng, FuncType($1, $3)) }
  | txprod { $1 }
/* -- for syntax error log -- */
  | txprod ARROW error { report_error (Tok $2) "->" }
/* -- -- */
;
txprod: /* -> type_struct */
  | txapppre TIMES txprod {
        let rng = make_range (TypeStr $1) (TypeStr $3) in
          match $3 with
          | (_, ProductType(tylist)) -> (rng, ProductType($1 :: tylist))
          | other                    -> (rng, ProductType([$1; $3]))
      }
  | txapppre { $1 }
/* -- for syntax error log -- */
  | txapppre TIMES error { report_error (Tok $2) "*" }
/* -- -- */
;
txapppre: /* ->type_struct */
  | txapp {
          match $1 with
          | (lst, (rng, VariantType([], tynm))) -> (rng, VariantType(lst, tynm))
          | ([], tystr)                         -> tystr
          | _                                   -> assert false
      }
  | LPAREN txfunc RPAREN { $2 }
  | TYPEVAR {
        let (rng, tyargnm) = $1 in (rng, TypeArgument(tyargnm))
      }
;
txapp: /* type_struct list * type_struct */
  | txbot txapp                { let (lst, tystr) = $2 in ($1 :: lst, tystr) }
  | LPAREN txfunc RPAREN txapp { let (lst, tystr) = $4 in ($2 :: lst, tystr) }
  | TYPEVAR txapp              {
        let (rng, tyargnm) = $1 in
        let (lst, tystr) = $2 in
          ((rng, TypeArgument(tyargnm)) :: lst, tystr)
      }
  | txbot                      { ([], $1) }
;
txbot: /* -> type_struct */
  | VAR {
        let (rng, tynm) = $1 in (rng, VariantType([], tynm))
      }
  | CONSTRUCTOR DOT VAR {
      let (rng1, mdlnm) = $1 in
      let (rng2, tynm)  = $3 in
      let rng = make_range (Rng rng1) (Rng rng2) in
        (rng, VariantType([], mdlnm ^ "." ^ tynm))
  }
;
tuple: /* -> untyped_tuple_cons */
  | nxlet             { make_standard (Untyped $1) (Untyped $1) (UTTupleCons($1, (Range.dummy "end-of-tuple'", UTEndOfTuple))) }
  | nxlet COMMA tuple { make_standard (Untyped $1) (Untyped $3) (UTTupleCons($1, $3)) }
/* -- for syntax error log -- */
  | nxlet COMMA error { report_error (Tok $2) "," }
/* -- -- */
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
/* -- for syntax error log -- */
  | patas ARROW error                            { report_error (Tok $2) "->" }
  | patas ARROW nxletsub BAR error               { report_error (Tok $4) "|" }
  | patas WHEN error                             { report_error (Tok $2) "when" }
  | patas WHEN nxletsub ARROW error              { report_error (Tok $4) "->" }
  | patas WHEN nxletsub ARROW nxletsub BAR error { report_error (Tok $6) "|" }
/* -- -- */
;
patas:
  | pattr AS VAR       { make_standard (Pat $1) (TokArg $3) (UTPAsVariable(extract_name $3, $1)) }
  | pattr              { $1 }
/* -- for syntax error log -- */
  | pattr AS error   { report_error (Tok $2) "as" }
/* -- -- */
;
pattr: /* -> Types.untyped_pattern_tree */
  | patbot CONS pattr  { make_standard (Pat $1) (Pat $3) (UTPListCons($1, $3)) }
  | CONSTRUCTOR patbot { make_standard (TokArg $1) (Pat $2) (UTPConstructor(extract_name $1, $2)) }
  | CONSTRUCTOR        { make_standard (TokArg $1) (TokArg $1) (UTPConstructor(extract_name $1, (Range.dummy "constructor-unit-value", UTPUnitConstant))) }
  | patbot             { $1 }
/* -- for syntax error log -- */
  | patbot CONS error { report_error (Tok $2) "::" }
  | CONSTRUCTOR error { report_error (TokArg $1) "" }
/* -- -- */
;
patbot: /* -> Types.untyped_pattern_tree */
  | NUMCONST           { make_standard (TokArg $1) (TokArg $1) (UTPNumericConstant(int_of_string (extract_name $1))) }
  | TRUE               { make_standard (Tok $1) (Tok $1) (UTPBooleanConstant(true)) }
  | FALSE              { make_standard (Tok $1) (Tok $1) (UTPBooleanConstant(false)) }
  | UNITVALUE          { make_standard (Tok $1) (Tok $1) UTPUnitConstant }
  | WILDCARD           { make_standard (Tok $1) (Tok $1) UTPWildCard }
  | VAR                { make_standard (TokArg $1) (TokArg $1) (UTPVariable(extract_name $1)) }
  | LPAREN patas RPAREN                { make_standard (Tok $1) (Tok $3) (extract_main $2) }
  | LPAREN patas COMMA pattuple RPAREN { make_standard (Tok $1) (Tok $5) (UTPTupleCons($2, $4)) }
  | BLIST ELIST                        { make_standard (Tok $1) (Tok $2) UTPEndOfList }
  | OPENQT sxblock CLOSEQT {
        let rng = make_range (Tok $1) (Tok $3) in (rng, UTPStringConstant(rng, omit_spaces $2)) }
/* -- for syntax error log -- */
  | LPAREN error             { report_error (Tok $1) "(" }
  | LPAREN patas COMMA error { report_error (Tok $3) "," }
  | BLIST error              { report_error (Tok $1) "[" }
  | OPENQT error             { report_error (Tok $1) "`" }
/* -- -- */
;
pattuple: /* -> untyped_pattern_tree */
  | patas                { make_standard (Pat $1) (Pat $1) (UTPTupleCons($1, (Range.dummy "end-of-tuple-pattern", UTPEndOfTuple))) }
  | patas COMMA pattuple { make_standard (Pat $1) (Pat $3) (UTPTupleCons($1, $3)) }
/* -- for syntax error log -- */
  | patas COMMA error    { report_error (Tok $2) "," }
/* -- -- */
;
binop:
  | PLUS    { "+" }      | MINUS   { "-" }      | MOD     { "mod" }
  | TIMES   { "*" }      | DIVIDES { "/" }      | CONCAT  { "^" }
  | EQ      { "==" }     | NEQ     { "<>" }     | GEQ     { ">=" }
  | LEQ     { "<=" }     | GT      { ">" }      | LT      { "<" }
  | LAND    { "&&" }     | LOR     { "||" }     | LNOT    { "not" }
  | BEFORE  { "before" }
;
sxsep:
  | SEP sxsepsub { $2 }
  | sxblock      { $1 }
  | sxitemize    { make_list_to_itemize $1 }
/* -- for syntax error log -- */
  | SEP error    { report_error (Tok $1) "|" }
/* -- -- */
;
sxitemize:
  | ITEM sxblock sxitemize {
      let (rng, depth) = $1 in
        (rng, depth, $2) :: $3
    }
  | ITEM sxblock {
      let (rng, depth) = $1 in
        (rng, depth, $2) :: []
}
;
sxsepsub:
  | sxblock SEP sxsepsub { make_standard (Untyped $1) (Untyped $3) (UTListCons($1, $3)) }
  |                      { (Range.dummy "end-of-string-list", UTEndOfList) }
/* -- for syntax error log -- */
  | sxblock SEP error    { report_error (Tok $2) "|" }
/* -- -- */
;
sxblock:
  | sxbot sxblock { make_standard (Untyped $1) (Untyped $2) (UTConcat($1, $2)) }
  |               { (Range.dummy "string-empty", UTStringEmpty) }
;
sxbot:
  | CHAR  { let (rng, ch) = $1 in (rng, UTStringConstant(ch)) }
  | SPACE { let rng = $1 in (rng, UTStringConstant(" ")) }
  | BREAK { let rng = $1 in (rng, UTBreakAndIndent) }
  | VARINSTR END { make_standard (TokArg $1) (Tok $2) (UTContentOf(extract_name $1)) }
  | CTRLSEQ sxclsnm sxidnm narg sarg {
        let (csrng, csnm) = $1 in
          convert_into_apply (csrng, UTContentOf(csnm)) $2 $3 (append_argument_list $4 $5)
      }
/* -- for syntax error log -- */
  | VARINSTR error { report_error (TokArg $1) "" }
  | CTRLSEQ error  { report_error (TokArg $1) "" }
/* -- -- */
sxclsnm:
  | CLASSNAME { make_standard (TokArg $1) (TokArg $1) (class_name_to_abstract_tree (extract_name $1)) }
  |           { (Range.dummy "no-class-name1", UTConstructor("Nothing", (Range.dummy "no-class-name2", UTUnitConstant))) }
sxidnm:
  | IDNAME    { make_standard (TokArg $1) (TokArg $1) (id_name_to_abstract_tree (extract_name $1)) }
  |           { (Range.dummy "no-id-name1", UTConstructor("Nothing", (Range.dummy "no-id-name2", UTUnitConstant))) }
;
narg: /* -> untyped_argument_cons */
  | OPENNUM nxlet CLOSENUM narg { let rng = make_range (Tok $1) (Tok $3) in UTArgumentCons((rng, extract_main $2), $4) }
  |                             { UTEndOfArgument }
/* -- for syntax error log -- */
  | OPENNUM error                { report_error (Tok $1) "(" }
  | OPENNUM nxlet CLOSENUM error { report_error (Tok $3) ")" }
/* -- -- */
;
sarg: /* -> Types.untyped_argument_cons */
  | BGRP sxsep EGRP sargsub        { let rng = make_range (Tok $1) (Tok $3) in UTArgumentCons((rng, extract_main $2), $4) }
  | OPENQT sxblock CLOSEQT sargsub { let rng = make_range (Tok $1) (Tok $3) in UTArgumentCons((rng, omit_spaces $2), $4) }
  | END                            { UTEndOfArgument }
/* -- for syntax error log --*/
  | BGRP error            { report_error (Tok $1) "{" }
  | BGRP sxsep EGRP error { report_error (Tok $3) "}" }
/* -- -- */
;
sargsub: /* -> Types.argument_cons */
  | BGRP sxsep EGRP sargsub        { let rng = make_range (Tok $1) (Tok $3) in UTArgumentCons((rng, extract_main $2), $4) }
  | OPENQT sxblock CLOSEQT sargsub { let rng = make_range (Tok $1) (Tok $3) in UTArgumentCons((rng, omit_spaces $2), $4) }
  |                                { UTEndOfArgument }
/* -- for syntax error log -- */
  | BGRP error                   { report_error (Tok $1) "{" }
  | BGRP sxsep EGRP error        { report_error (Tok $3) "}" }
  | OPENQT error                 { report_error (Tok $1) "`" }
  | OPENQT sxblock CLOSEQT error { report_error (Tok $3) "`" }
/* -- -- */
;
