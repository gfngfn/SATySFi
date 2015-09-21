%{
  open Types

  type literal_reading_state = Normal | ReadingSpace
  type range_kind =
    | Tok       of token_position
    | TokArg    of (token_position * string)
    | Untyped   of untyped_abstract_tree
    | Pat       of untyped_pattern_tree
    | PatCons   of untyped_pattern_match_cons
    | TypeStr   of type_struct
    | VarntCons of untyped_variant_cons

  (* range_kind -> range_kind -> code_range *)
  let make_range sttx endx =
    let (sttln, sttpos) =
      match sttx with
      | Tok(l, s, _)                 -> (l, s)
      | TokArg((l, s, _), _)         -> (l, s)
      | Untyped((sl, sp, _, _), _)   -> (sl, sp)
      | Pat((sl, sp, _, _), _)       -> (sl, sp)
      | PatCons((sl, sp, _, _), _)   -> (sl, sp)
      | VarntCons((sl, sp, _, _), _) -> (sl, sp)
      | TypeStr(tystr)               ->
          let (sl, sp, _, _) = Typeenv.get_range_from_type tystr in (sl, sp)
    in
    let (endln, endpos) =
      match endx with
      | Tok(l, _, e)                 -> (l, e)
      | TokArg((l, _, e), _)         -> (l, e)
      | Untyped((_, _, el, ep), _)   -> (el, ep)
      | Pat((_, _, el, ep), _)       -> (el, ep)
      | PatCons((_, _, el, ep), _)   -> (el, ep)
      | VarntCons((_, _, el, ep), _) -> (el, ep)
      | TypeStr(tystr)               ->
          let (_, _, el, ep) = Typeenv.get_range_from_type tystr in (el, ep)
    in
      (sttln, sttpos, endln, endpos)

  (* token_position -> code_range *)
  let extract_range (ln, sttpos, endpos) = (ln, sttpos, ln, endpos)

  (* token_position * string -> code_range * string *)
  let extract_range_and_name ((ln, sttpos, endpos), name) = ((ln, sttpos, ln, endpos), name)

  (* untyped_argument_cons -> untyped_argument_cons -> untyped_argument_cons *)
  let rec append_argument_list arglsta arglstb =
    match arglsta with
    | UTEndOfArgument              -> arglstb
    | UTArgumentCons(arg, arglstl) ->
        UTArgumentCons(arg, (append_argument_list arglstl arglstb))

  (* ctrlseq_name -> untyped_abstract_tree -> untyped_abstract_tree -> untyped_argument_cons -> untyped_abstract_tree *)
  let rec convert_into_apply csutast clsnmutast idnmutast argcons =
    convert_into_apply_sub argcons ((-12, 0, 0, 0), UTApplyClassAndID(clsnmutast, idnmutast, csutast))

  (* argument_cons -> untyped_abstract_tree -> untyped_abstract_tree *)
  and convert_into_apply_sub argcons utastconstr =
    match argcons with
    | UTEndOfArgument             -> utastconstr
    | UTArgumentCons(arg, actail) ->
        let (rng, _) = arg in
          convert_into_apply_sub actail (rng, UTApply(utastconstr, arg))

  let class_name_to_abstract_tree clsnm =
    UTStringConstant((String.sub clsnm 1 ((String.length clsnm) - 1)))

  let id_name_to_abstract_tree idnm =
    UTStringConstant((String.sub idnm 1 ((String.length idnm) - 1)))

  let rec curry_lambda_abstract rng argvarcons utastdef =
    match argvarcons with
    | UTEndOfArgumentVariable                        -> utastdef
    | UTArgumentVariableCons(varrng, argvar, avtail) ->
        (rng, UTLambdaAbstract(varrng, argvar, curry_lambda_abstract (-11, 0, 0, 0) avtail utastdef))

  let report_error rngknd tok =
    match rngknd with
    | Tok(tp) ->
        let rng = extract_range tp in
          raise (ParseErrorDetail(
            "syntax error:\n"
            ^ "    unexpected token after '" ^ tok ^ "'\n"
            ^ "    " ^ (Display.describe_position rng)))
    | TokArg(tp, nm) ->
        let rng = extract_range tp in
          raise (ParseErrorDetail(
            "syntax error:\n"
            ^ "    unexpected token after '" ^ nm ^ "'\n"
            ^ "    " ^ (Display.describe_position rng)))
    | _ -> raise (ParseErrorDetail("something is wrong"))

  let rec string_of_avc argvarcons =
    match argvarcons with
    | UTEndOfArgumentVariable                   -> ""
    | UTArgumentVariableCons(_, argvar, avtail) -> argvar ^ " " ^ (string_of_avc avtail)

  let rec stringify_literal ltrl =
    let (_, ltrlmain) = ltrl in
      match ltrlmain with
      | UTConcat(utastf, utastl) -> (stringify_literal utastf) ^ (stringify_literal utastl)
      | UTStringConstant(s)      -> s
      | UTStringEmpty            -> ""
      | _                        -> assert false

  let rec omit_pre_spaces str =
    if String.sub str 0 1 = " "  then
      omit_pre_spaces (String.sub str 1 ((String.length str) - 1)) 
(*    else if String.sub str 0 1 = "\n" then
      (String.sub str 1 ((String.length str) - 1)) *)
    else
      str

  let rec omit_post_spaces str =
    if String.sub str ((String.length str) - 1) 1 = " " then
      omit_post_spaces (String.sub str 0 ((String.length str) - 1))
    else if String.sub str ((String.length str) - 1) 1 = "\n" then
      (String.sub str 0 ((String.length str) - 1))
    else
      str

  (* untyped_abstract_tree -> untyped_abstract_tree_main *)
  let rec omit_spaces ltrl =
    let str_ltrl = omit_post_spaces (omit_pre_spaces (stringify_literal ltrl)) in
      let min_indent = min_indent_space str_ltrl in
        let str_shaved = shave_indent str_ltrl min_indent in
          if str_shaved.[(String.length str_shaved) - 1] = '\n' then
            let str_no_last_break = String.sub str_shaved 0 ((String.length str_shaved) - 1) in
              UTConcat(
                ((-13, 0, 0, 0), UTStringConstant(str_no_last_break)),
                ((-14, 0, 0, 0), UTBreakAndIndent)
              )
          else
            UTStringConstant(str_shaved)

  (* string -> int *)
  and min_indent_space str_ltrl =
    min_indent_space_sub str_ltrl 0 ReadingSpace 0 (String.length str_ltrl)

  (* string -> int -> literal_reading_state -> int -> int -> int *)
  and min_indent_space_sub str_ltrl index lrstate spnum minspnum =
    if index >= (String.length str_ltrl) then
      (* ( print_string ("min_indent: " ^ (string_of_int minspnum) ^ "\n") ; *)
        minspnum
      (* ) *)
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
          ( match str_ltrl.[index] with
            | '\n' -> shave_indent_sub str_ltrl minspnum (index + 1) (str_constr ^ "\n") ReadingSpace 0
            | ch   -> shave_indent_sub str_ltrl minspnum (index + 1) (str_constr ^ (String.make 1 ch)) Normal 0
          )
      | ReadingSpace ->
          ( match str_ltrl.[index] with
            | ' ' ->
                if spnum < minspnum then
                  shave_indent_sub str_ltrl minspnum (index + 1) str_constr ReadingSpace (spnum + 1)
                else
                  shave_indent_sub str_ltrl minspnum (index + 1) (str_constr ^ " ") ReadingSpace (spnum + 1)

            | '\n' -> shave_indent_sub str_ltrl minspnum (index + 1) (str_constr ^ "\n") ReadingSpace 0
            | ch   -> shave_indent_sub str_ltrl minspnum (index + 1) (str_constr ^ (String.make 1 ch)) Normal 0
          )

  let binary_operator opname lft op rgt =
    let oprng = extract_range op in
    let rng = make_range (Untyped lft) (Untyped rgt) in
      (rng, UTApply(((-15, 0, 0, 0), UTApply((oprng, UTContentOf(opname)), lft)), rgt))

  let make_let_expression lettk decs utastaft =
    let rng = make_range (Tok lettk) (Untyped utastaft) in
      (rng, UTLetIn(decs, utastaft))

  let make_let_mutable_expression letmuttk vartk utastdef utastaft =
    let (varrng, varnm) = extract_range_and_name vartk in
    let rng = make_range (Tok letmuttk) (Untyped utastaft) in
      (rng, UTLetMutableIn(varrng, varnm, utastdef, utastaft))

  let make_variant_declaration firsttk varntdecs utastaft =
    let rng = make_range (Tok firsttk) (Untyped utastaft) in
      (rng, UTDeclareVariantIn(varntdecs, utastaft))

  let make_mutual_let_cons firsttk vartk argcons utastdef tailcons =
    let (varrng, varnm) = extract_range_and_name vartk in
    let curried = curry_lambda_abstract varrng argcons utastdef in
      UTMutualLetCons(varnm, curried, tailcons)

  let make_mutual_variant_cons typetk constrdecs tailcons =
    let (_, typenm) = extract_range_and_name typetk in
      UTMutualVariantCons(typenm, constrdecs, tailcons)

  let make_standard sttknd endknd utastmain =
    let rng = make_range sttknd endknd in (rng, utastmain)

  let extract_main (_, utastmain) = utastmain

%}

%token <Types.token_position * Types.var_name> VAR
%token <Types.token_position * Types.var_name> VARINSTR
%token <Types.token_position * string> NUMCONST CHAR
%token <Types.token_position> SPACE BREAK
%token <Types.token_position * Types.ctrlseq_name> CTRLSEQ
%token <Types.token_position * Types.id_name>      IDNAME
%token <Types.token_position * Types.class_name>   CLASSNAME
%token <Types.token_position> LAMBDA ARROW
%token <Types.token_position> LET DEFEQ LETAND IN MUTUAL ENDMUTUAL
%token <Types.token_position> LETMUTABLE OVERWRITEEQ
%token <Types.token_position> REFNOW REFFINAL
%token <Types.token_position> IF THEN ELSE IFCLASSISVALID IFIDISVALID
%token <Types.token_position> LPAREN RPAREN
%token <Types.token_position> TIMES DIVIDES MOD PLUS MINUS
%token <Types.token_position> EQ NEQ GEQ LEQ GT LT
%token <Types.token_position> LNOT LAND LOR
%token <Types.token_position> CONCAT
%token <Types.token_position> OPENQT CLOSEQT
%token <Types.token_position> OPENSTR CLOSESTR
%token <Types.token_position> OPENNUM CLOSENUM
%token <Types.token_position> BGRP EGRP
%token <Types.token_position> TRUE FALSE
%token <Types.token_position> FINISH
%token <Types.token_position> SEP END COMMA
%token <Types.token_position> BLIST LISTPUNCT ELIST CONS
%token <Types.token_position> BEFORE UNITVALUE WHILE DO
%token <Types.token_position> NEWGLOBALHASH OVERWRITEGLOBALHASH RENEWGLOBALHASH
%token <Types.token_position> MATCH WITH BAR WILDCARD WHEN AS
%token <Types.token_position> VARIANT OF
%token <Types.token_position * Types.constructor_name> CONSTRUCTOR
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
%left PLUS
%right MINUS
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
%type <Types.untyped_pattern_match_cons> pats
%type <Types.untyped_pattern_tree> pattr
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
  | LET nxdec nxtoplevel                        { make_let_expression $1 $2 $3 }
  | LET nxdec EOI                               { make_let_expression $1 $2 untyped_finish }
  | LETMUTABLE VAR OVERWRITEEQ nxlet nxtoplevel { make_let_mutable_expression $1 $2 $4 $5 }
  | LETMUTABLE VAR OVERWRITEEQ nxlet EOI        { make_let_mutable_expression $1 $2 $4 untyped_finish }
  | MUTUAL nxmutual nxtoplevel                  { make_let_expression $1 $2 $3 }
  | MUTUAL nxmutual EOI                         { make_let_expression $1 $2 untyped_finish }
  | VARIANT nxvariantdec nxtoplevel             { make_variant_declaration $1 $2 $3 }
  | VARIANT nxvariantdec EOI                    { make_variant_declaration $1 $2 untyped_finish }
/* ---- transition to expression style ---- */
  | LET nxdec IN nxlet EOI                        { make_let_expression $1 $2 $4 }
  | LETMUTABLE VAR OVERWRITEEQ nxlet IN nxlet EOI { make_let_mutable_expression $1 $2 $4 $6 }
  | MUTUAL nxmutual IN nxlet EOI                  { make_let_expression $1 $2 $4 }
  | VARIANT nxvariantdec IN nxlet EOI             { make_variant_declaration $1 $2 $4 }
/* ---- for syntax error log ---- */
  | LET error                                 { report_error (Tok $1) "let" }
  | LET nxdec IN error                        { report_error (Tok $3) "in" }
  | LETMUTABLE error                          { report_error (Tok $1) "let-mutable"}
  | LETMUTABLE VAR error                      { report_error (TokArg $2) "" }
  | LETMUTABLE VAR OVERWRITEEQ error          { report_error (Tok $3) "<-" }
  | LETMUTABLE VAR OVERWRITEEQ nxlet IN error { report_error (Tok $5) "in" }
  | MUTUAL error                              { report_error (Tok $1) "mutual" }
  | VARIANT error                             { report_error (Tok $1) "variant" }
;
nxmutual: /* -> Types.untyped_mutual_let_cons */
  | LET VAR argvar DEFEQ nxlet nxmutual      { make_mutual_let_cons $1 $2 $3 $5 $6 }
  | LET VAR argvar DEFEQ nxlet ENDMUTUAL     { make_mutual_let_cons $1 $2 $3 $5 UTEndOfMutualLet }
  | LET CTRLSEQ argvar DEFEQ nxlet nxmutual  { make_mutual_let_cons $1 $2 $3 $5 $6 }
  | LET CTRLSEQ argvar DEFEQ nxlet ENDMUTUAL { make_mutual_let_cons $1 $2 $3 $5 UTEndOfMutualLet }
/* -- for syntax error log -- */
  | LET error                      { report_error (Tok $1) "and" }
  | LET VAR error                  { report_error (TokArg $2) "" }
  | LET VAR argvar DEFEQ error     { report_error (Tok $4) "=" }
  | LET CTRLSEQ error              { report_error (TokArg $2) "" }
  | LET CTRLSEQ argvar DEFEQ error { report_error (Tok $4) "=" }
;
nxdec: /* -> untyped_mutual_let_cons */
  | VAR argvar DEFEQ nxlet LETAND nxdec     { make_mutual_let_cons $1 $1 $2 $4 $6 }
  | VAR argvar DEFEQ nxlet                  { make_mutual_let_cons $1 $1 $2 $4 UTEndOfMutualLet }
  | CTRLSEQ argvar DEFEQ nxlet LETAND nxdec { make_mutual_let_cons $1 $1 $2 $4 $6 }
  | CTRLSEQ argvar DEFEQ nxlet              { make_mutual_let_cons $1 $1 $2 $4 UTEndOfMutualLet }
/* -- for syntax error log -- */
  | VAR error                               { report_error (TokArg $1) "" }
  | VAR argvar DEFEQ error                  { report_error (Tok $3) "=" }
  | VAR argvar DEFEQ nxlet LETAND error     { report_error (Tok $5) "and" }
  | CTRLSEQ error                           { report_error (TokArg $1) "" }
  | CTRLSEQ argvar DEFEQ error              { report_error (Tok $3) "=" }
  | CTRLSEQ argvar DEFEQ nxlet LETAND error { report_error (Tok $5) "and" }
;
nxvariantdec: /* -> untyped_mutual_variant_cons */
  | VAR DEFEQ variants LETAND nxvariantdec     { make_mutual_variant_cons $1 $3 $5 }
  | VAR DEFEQ variants                         { make_mutual_variant_cons $1 $3 UTEndOfMutualVariant }
  | VAR DEFEQ BAR variants LETAND nxvariantdec { make_mutual_variant_cons $1 $4 $6 }
  | VAR DEFEQ BAR variants                     { make_mutual_variant_cons $1 $4 UTEndOfMutualVariant }
/* -- for syntax error log -- */
  | VAR error                           { report_error (TokArg $1) "" }
  | VAR DEFEQ error                     { report_error (Tok $2) "=" }
  | VAR DEFEQ BAR error                 { report_error (Tok $3) "|" }
  | VAR DEFEQ BAR variants LETAND error { report_error (Tok $5) "and" }
;
nxlet:
  | MATCH nxlet WITH pats {
        let rng = make_range (Tok $1) (PatCons $4) in
          (rng, UTPatternMatch($2, $4))
      }
  | MATCH nxlet WITH BAR pats {
        let rng = make_range (Tok $1) (PatCons $5) in
          (rng, UTPatternMatch($2, $5))
      }
  | nxletsub { $1 }
/* -- for syntax error log -- */
  | MATCH error                { report_error (Tok $1) "match" }
  | MATCH nxlet WITH error     { report_error (Tok $3) "with" }
  | MATCH nxlet WITH BAR error { report_error (Tok $4) "|" }
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
;
nxwhl:
  | WHILE nxlet DO nxwhl { make_standard (Tok $1) (Untyped $4) (UTWhileDo($2, $4)) }
  | nxif                 { $1 }
/* -- for syntax error log --*/
  | WHILE error          { report_error (Tok $1) "while" }
  | WHILE nxlet DO error { report_error (Tok $3) "do" }
nxif:
  | IF nxlet THEN nxlet ELSE nxlet       { make_standard (Tok $1) (Untyped $6) (UTIfThenElse($2, $4, $6)) }
  | IFCLASSISVALID nxlet ELSE nxlet      { make_standard (Tok $1) (Untyped $4) (UTIfClassIsValid($2, $4)) }
  | IFCLASSISVALID THEN nxlet ELSE nxlet { make_standard (Tok $1) (Untyped $5) (UTIfClassIsValid($3, $5)) }
  | IFIDISVALID nxlet ELSE nxlet         { make_standard (Tok $1) (Untyped $4) (UTIfIDIsValid($2, $4)) }
  | IFIDISVALID THEN nxlet ELSE nxlet    { make_standard (Tok $1) (Untyped $5) (UTIfIDIsValid($3, $5)) }
  | nxbfr                                { $1 }
/* -- for syntax error log -- */
  | IF error                             { report_error (Tok $1) "if" }
  | IF nxlet THEN error                  { report_error (Tok $3) "then" }
  | IF nxlet THEN nxlet ELSE error       { report_error (Tok $5) "else" }
  | IFCLASSISVALID error                 { report_error (Tok $1) "if-class-is-valid" }
  | IFCLASSISVALID nxlet ELSE error      { report_error (Tok $3) "else" }
  | IFCLASSISVALID THEN error            { report_error (Tok $2) "then" }
  | IFCLASSISVALID THEN nxlet ELSE error { report_error (Tok $4) "else" }
  | IFIDISVALID error                    { report_error (Tok $1) "if-class-is-valid" }
  | IFIDISVALID nxlet ELSE error         { report_error (Tok $3) "else" }
  | IFIDISVALID THEN error               { report_error (Tok $2) "then" }
  | IFIDISVALID THEN nxlet ELSE error    { report_error (Tok $4) "else" }
;
nxbfr:
  | nxlambda BEFORE nxbfr { make_standard (Untyped $1) (Untyped $3) (UTSequential($1, $3)) }
  | nxlambda              { $1 }
/* -- for syntax error log -- */
  | nxlambda BEFORE error { report_error (Tok $2) "before" }
;
nxlambda:
  | VAR OVERWRITEEQ nxlor {
        let (varrng, varnm) = extract_range_and_name $1 in
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
;
argvar: /* -> Types.argument_variable_cons */
  | VAR argvar {
        let (varrng, varnm) = extract_range_and_name $1 in
          UTArgumentVariableCons(varrng, varnm, $2)
      }
  | { UTEndOfArgumentVariable }
/* -- for syntax error log -- */
  | VAR error { report_error (TokArg $1) "" }
;
nxlor:
  | nxland LOR nxlor    { binary_operator "||" $1 $2 $3 }
  | nxland              { $1 }
/* -- for syntax error log -- */
  | nxland LOR error    { report_error (Tok $2) "||" }
;
nxland:
  | nxcomp LAND nxland  { binary_operator "&&" $1 $2 $3 }
  | nxcomp              { $1 }
/* -- for syntax error log -- */
  | nxcomp LAND error   { report_error (Tok $2) "&&" }
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
;
nxconcat:
  | nxlplus CONCAT nxconcat { binary_operator "^" $1 $2 $3 }
  | nxlplus CONS nxconcat   { binary_operator "::" $1 $2 $3 }
  | nxlplus                 { $1 }
/* -- for syntax error log -- */
  | nxlplus CONCAT error    { report_error (Tok $2) "^" }
;
nxlplus:
  | nxlminus PLUS nxrplus   { binary_operator "+" $1 $2 $3 }
  | nxlminus                { $1 }
/* -- for syntax error log -- */
  | nxlminus PLUS error     { report_error (Tok $2) "+" }
;
nxlminus:
  | nxlplus MINUS nxrtimes  { binary_operator "-" $1 $2 $3 }
  | nxltimes                { $1 }
/* -- for syntax error log -- */
  | nxlplus MINUS error     { report_error (Tok $2) "-" }
;
nxrplus:
  | nxrminus PLUS nxrplus   { binary_operator "+" $1 $2 $3 }
  | nxrminus                { $1 }
/* -- for syntax error log -- */
  | nxrminus PLUS error     { report_error (Tok $2) "+" }
;
nxrminus:
  | nxrplus MINUS nxrtimes  { binary_operator "-" $1 $2 $3 }
  | nxrtimes                { $1 }
/* -- for syntax error log -- */
  | nxrplus MINUS error     { report_error (Tok $2) "-" }
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
;
nxrtimes:
  | nxapp TIMES nxrtimes    { binary_operator "*" $1 $2 $3 }
  | nxrtimes DIVIDES nxapp  { binary_operator "/" $1 $2 $3 }
  | nxrtimes MOD nxapp      { binary_operator "mod" $1 $2 $3 }
  | nxapp                   { $1 }
/* -- for syntax error log -- */
  | nxapp TIMES error       { report_error (Tok $2) "*" }
  | nxrtimes DIVIDES error  { report_error (Tok $2) "/" }
  | nxrtimes MOD error      { report_error (Tok $2) "mod" }
;
nxun:
  | MINUS nxapp { binary_operator "-" ((-16, 0, 0, 0), UTNumericConstant(0)) $1 $2 }
  | LNOT nxapp  {
        let lnotrng = extract_range $1 in
          make_standard (Tok $1) (Untyped $2) (UTApply((lnotrng, UTContentOf("not")), $2)) }
  | REFNOW nxapp {
        let refnowrng = extract_range $1 in
          make_standard (Tok $1) (Untyped $2) (UTApply((refnowrng, UTContentOf("!")), $2)) }
  | REFFINAL nxapp { make_standard (Tok $1) (Untyped $2) (UTReferenceFinal($2)) }
  | nxapp          { $1 }
/* -- for syntax error log -- */
  | MINUS error    { report_error (Tok $1) "-" }
  | LNOT error     { report_error (Tok $1) "not" }
  | REFNOW error   { report_error (Tok $1) "!" }
  | REFFINAL error { report_error (Tok $1) "!!" }
;
nxapp:
  | nxapp nxbot { make_standard (Untyped $1) (Untyped $2) (UTApply($1, $2)) }
  | nxbot       { $1 }
;
nxbot:
  | VAR {
        let (rng, varnm) = extract_range_and_name $1 in
          (rng, UTContentOf(varnm)) }
  | CONSTRUCTOR nxbot {
        let (_, constrnm) = extract_range_and_name $1 in
          make_standard (TokArg $1) (Untyped $2) (UTConstructor(constrnm, $2)) }
  | NUMCONST {
        let (rng, ncs) = extract_range_and_name $1 in
          (rng, UTNumericConstant(int_of_string ncs)) }
  | TRUE                            { make_standard (Tok $1) (Tok $1) (UTBooleanConstant(true)) }
  | FALSE                           { make_standard (Tok $1) (Tok $1) (UTBooleanConstant(false)) }
  | LPAREN nxlet RPAREN             { make_standard (Tok $1) (Tok $3) (extract_main $2) }
  | LPAREN nxlet COMMA tuple RPAREN { make_standard (Tok $1) (Tok $5) (UTTupleCons($2, $4)) }
  | OPENSTR sxsep CLOSESTR          { make_standard (Tok $1) (Tok $3) (extract_main $2) }
  | OPENQT sxblock CLOSEQT          { make_standard (Tok $1) (Tok $3) (omit_spaces $2) }
  | BLIST ELIST                     { make_standard (Tok $1) (Tok $2) UTEndOfList }
  | BLIST nxlet nxlist ELIST        { make_standard (Tok $1) (Tok $4) (UTListCons($2, $3)) }
  | UNITVALUE                       { make_standard (Tok $1) (Tok $1) UTUnitConstant }
  | FINISH                          { make_standard (Tok $1) (Tok $1) UTFinishHeaderFile }
  | LPAREN binop RPAREN             { make_standard (Tok $1) (Tok $3) (UTContentOf($2)) }
/* -- for syntax error log -- */
  | BLIST error   { report_error (Tok $1) "[" }
  | OPENSTR error { report_error (Tok $1) "{ (beginning of text area)" }
  | LPAREN error  { report_error (Tok $1) "(" }
;
nxlist:
  | LISTPUNCT nxlet nxlist { make_standard (Tok $1) (Untyped $3) (UTListCons($2, $3)) }
  |                        { ((-17, 0, 0, 0), UTEndOfList) }
/* -- for syntax error log -- */
  | LISTPUNCT error        { report_error (Tok $1) ";" }
;
variants: /* -> untyped_variant_cons */
  | CONSTRUCTOR OF txfunc BAR variants {
        let (_, constrnm) = extract_range_and_name $1 in
          make_standard (TokArg $1) (VarntCons $5) (UTVariantCons(constrnm, $3, $5)) }
  | CONSTRUCTOR OF txfunc {
        let (_, constrnm) = extract_range_and_name $1 in
          make_standard (TokArg $1) (TypeStr $3) (UTVariantCons(constrnm, $3, ((-400, 0, 0, 0), UTEndOfVariant))) }
/* -- for syntax error log -- */
  | CONSTRUCTOR error               { report_error (TokArg $1) "" }
  | CONSTRUCTOR OF error            { report_error (Tok $2) "of" }
  | CONSTRUCTOR OF txfunc BAR error { report_error (Tok $4) "|" }
;
txfunc: /* -> type_struct */
  | txprod ARROW txfunc {
        let rng = make_range (TypeStr $1) (TypeStr $3) in FuncType(rng, $1, $3) }
  | txprod { $1 }
/* -- for syntax error log -- */
  | txprod ARROW error { report_error (Tok $2) "->" }
;
txprod: /* -> type_struct */
  | txbot TIMES txprod {
        let rng = make_range (TypeStr $1) (TypeStr $3) in
          match $3 with
          | ProductType(_, tylist) -> ProductType(rng, $1 :: tylist)
          | other                  -> ProductType(rng, [$1; $3])
      }
  | txbot VAR {
        let (_, tyschnm) = extract_range_and_name $2 in
        let rng = make_range (TypeStr $1) (TokArg $2) in
          match tyschnm with
          | "list" -> ListType(rng, $1)
          | "ref"  -> RefType(rng, $1)
          | other  -> raise (ParseErrorDetail("undefined type scheme '" ^ other ^ "'"))
      }
  | txbot { $1 }
/* -- for syntax error log -- */
  | txbot TIMES error { report_error (Tok $2) "*" }
;
txbot: /* -> type_struct */
  | VAR {
        let (rng, tynm) = extract_range_and_name $1 in
          match tynm with
          | "int"    -> IntType(rng)
          | "bool"   -> BoolType(rng)
          | "string" -> StringType(rng)
          | "unit"   -> UnitType(rng)
          | other    -> VariantType(rng, other)
      }
  | LPAREN txfunc RPAREN { $2 }
/* -- for syntax error log -- */
  | LPAREN error         { report_error (Tok $1) "(" }
;
tuple: /* -> untyped_tuple_cons */
  | nxlet             { make_standard (Untyped $1) (Untyped $1) (UTTupleCons($1, ((-5000, 0, 0, 0), UTEndOfTuple))) }
  | nxlet COMMA tuple { make_standard (Untyped $1) (Untyped $3) (UTTupleCons($1, $3)) }
/* -- for syntax error log -- */
  | nxlet COMMA error { report_error (Tok $2) "," }
;
pats: /* -> untyped_patter_match_cons */
  | pattr ARROW nxletsub 
      { make_standard (Pat $1) (Untyped $3) (UTPatternMatchCons($1, $3, ((-5001, 0, 0, 0), UTEndOfPatternMatch))) }
  | pattr ARROW nxletsub BAR pats 
      { make_standard (Pat $1) (PatCons $5) (UTPatternMatchCons($1, $3, $5)) }
  | pattr WHEN nxletsub ARROW nxletsub
      { make_standard (Pat $1) (Untyped $5) (UTPatternMatchConsWhen($1, $3, $5, ((-5001, 0, 0, 0), UTEndOfPatternMatch))) }
  | pattr WHEN nxletsub ARROW nxletsub BAR pats
      { make_standard (Pat $1) (PatCons $7) (UTPatternMatchConsWhen($1, $3, $5, $7)) }
/* -- for syntax error log -- */
  | pattr ARROW error                            { report_error (Tok $2) "->" }
  | pattr ARROW nxletsub BAR error               { report_error (Tok $4) "|" }
  | pattr WHEN error                             { report_error (Tok $2) "when" }
  | pattr WHEN nxletsub ARROW error              { report_error (Tok $4) "->" }
  | pattr WHEN nxletsub ARROW nxletsub BAR error { report_error (Tok $6) "|" }
;
pattr: /* -> Types.untyped_pattern_tree */
  | patbot CONS pattr { make_standard (Pat $1) (Pat $3) (UTPListCons($1, $3)) }
  | pattr AS VAR {
        let (_, varnm) = extract_range_and_name $3 in
          make_standard (Pat $1) (TokArg $3) (UTPAsVariable(varnm, $1)) }
  | patbot { $1 }
/* -- for syntax error log -- */
  | patbot CONS error { report_error (Tok $2) "::" }
  | patbot AS error   { report_error (Tok $2) "as" }
;
patbot: /* -> Types.untyped_pattern_tree */
  | NUMCONST {
        let (rng, ncs) = extract_range_and_name $1 in
          (rng, UTPNumericConstant(int_of_string ncs)) }
  | TRUE      { make_standard (Tok $1) (Tok $1) (UTPBooleanConstant(true)) }
  | FALSE     { make_standard (Tok $1) (Tok $1) (UTPBooleanConstant(false)) }
  | UNITVALUE { make_standard (Tok $1) (Tok $1) UTPUnitConstant }
  | WILDCARD  { make_standard (Tok $1) (Tok $1) UTPWildCard }
  | VAR {
        let (rng, varnm) = extract_range_and_name $1 in
          (rng, UTPVariable(varnm)) }
  | CONSTRUCTOR patbot {
        let (_, constrnm) = extract_range_and_name $1 in
          make_standard (TokArg $1) (Pat $2) (UTPConstructor(constrnm, $2)) }
  | LPAREN pattr RPAREN                { make_standard (Tok $1) (Tok $3) (extract_main $2) }
  | LPAREN pattr COMMA pattuple RPAREN { make_standard (Tok $1) (Tok $5) (UTPTupleCons($2, $4)) }
  | BLIST ELIST                        { make_standard (Tok $1) (Tok $2) UTPEndOfList }
  | OPENQT sxblock CLOSEQT {
        let rng = make_range (Tok $1) (Tok $3) in
          (rng, UTPStringConstant(rng, omit_spaces $2)) }
/* -- for syntax error log -- */
  | CONSTRUCTOR error        { report_error (TokArg $1) "" }
  | LPAREN error             { report_error (Tok $1) "(" }
  | LPAREN pattr COMMA error { report_error (Tok $3) "," }
  | BLIST error              { report_error (Tok $1) "[" }
  | OPENQT error             { report_error (Tok $1) "`" }
;
pattuple: /* -> untyped_pattern_tree */
  | pattr                { make_standard (Pat $1) (Pat $1) (UTPTupleCons($1, ((-5002, 0, 0, 0), UTPEndOfTuple))) }
  | pattr COMMA pattuple { make_standard (Pat $1) (Pat $3) (UTPTupleCons($1, $3)) }
/* -- for syntax error log -- */
  | pattr COMMA error    { report_error (Tok $2) "," }
;
binop:
  | PLUS    { "+" }
  | MINUS   { "-" }
  | MOD     { "mod" }
  | TIMES   { "*" }
  | DIVIDES { "/" }
  | CONCAT  { "^" }
  | EQ      { "==" }
  | NEQ     { "<>" }
  | GEQ     { ">=" }
  | LEQ     { "<=" }
  | GT      { ">" }
  | LT      { "<" }
  | LAND    { "&&" }
  | LOR     { "||" }
  | LNOT    { "not" }
  | BEFORE  { "before" }
;
sxsep:
  | SEP sxsepsub { $2 }
  | sxblock      { $1 }
/* -- for syntax error log -- */
  | SEP error    { report_error (Tok $1) "|" }
;
sxsepsub:
  | sxblock SEP sxsepsub { make_standard (Untyped $1) (Untyped $3) (UTListCons($1, $3)) }
  |                      { ((-18, 0, 0, 0), UTEndOfList) }
/* -- for syntax error log -- */
  | sxblock SEP error    { report_error (Tok $2) "|" }
;
sxblock:
  | sxbot sxblock { make_standard (Untyped $1) (Untyped $2) (UTConcat($1, $2)) }
  |               { ((-19, 0, 0, 0), UTStringEmpty) }
;
sxbot:
  | CHAR  { let (rng, ch) = extract_range_and_name $1 in (rng, UTStringConstant(ch)) }
  | SPACE { let rng = extract_range $1 in (rng, UTStringConstant(" ")) }
  | BREAK { let rng = extract_range $1 in (rng, UTBreakAndIndent) }
  | VARINSTR END {
        let (_, varnm) = extract_range_and_name $1 in
        let rng = make_range (TokArg $1) (Tok $2) in
          (rng, UTContentOf(varnm))
      }
  | CTRLSEQ sxclsnm sxidnm narg sarg {
        let (csrng, csnm) = extract_range_and_name $1 in
          convert_into_apply (csrng, UTContentOf(csnm)) $2 $3 (append_argument_list $4 $5)
      }
/* -- for syntax error log -- */
  | CTRLSEQ error { report_error (TokArg $1) "" }
sxclsnm:
  | CLASSNAME {
        let (rng, clsnm) = extract_range_and_name $1 in
          (rng, class_name_to_abstract_tree clsnm)
      }
  | { ((-20, 0, 0, 0), UTNoContent) }
sxidnm:
  | IDNAME {
        let (rng, idnm) = extract_range_and_name $1 in
          (rng, id_name_to_abstract_tree idnm)
      }
  | { ((-21, 0, 0, 0), UTNoContent) }
;
narg: /* -> untyped_argument_cons */
  | OPENNUM nxlet CLOSENUM narg {
        let rng = make_range (Tok $1) (Tok $3) in
          UTArgumentCons((rng, extract_main $2), $4) }
  | { UTEndOfArgument }
/* -- for syntax error log -- */
  | OPENNUM error { report_error (Tok $1) "(" }
  | OPENNUM nxlet CLOSENUM error { report_error (Tok $3) ")" }
;
sarg: /* -> Types.untyped_argument_cons */
  | BGRP sxsep EGRP sargsub { UTArgumentCons($2, $4) }
  | OPENQT sxblock CLOSEQT sargsub {
        let rng = make_range (Tok $1) (Tok $3) in
          UTArgumentCons((rng, omit_spaces $2), $4)
      }
  | END { UTEndOfArgument }
/* -- for syntax error log */
  | BGRP error            { report_error (Tok $1) "{" }
  | BGRP sxsep EGRP error { report_error (Tok $3) "}" }
;
sargsub: /* -> Types.argument_cons */
  | BGRP sxsep EGRP sargsub {
        let rng = make_range (Tok $1) (Tok $3) in
          UTArgumentCons((rng, extract_main $2), $4) }
  | OPENQT sxblock CLOSEQT sargsub {
        let rng = make_range (Tok $1) (Tok $3) in
          UTArgumentCons((rng, omit_spaces $2), $4)
      }
  | { UTEndOfArgument }
/* -- for syntax error log */
  | BGRP error                   { report_error (Tok $1) "{" }
  | BGRP sxsep EGRP error        { report_error (Tok $3) "}" }
  | OPENQT error                 { report_error (Tok $1) "`" }
  | OPENQT sxblock CLOSEQT error { report_error (Tok $3) "`" }
;
