%{
  open Types

  exception MyParseError of string

  let rec append_argument_list arglsta arglstb =
    match arglsta with
    | EndOfArgument -> arglstb
    | ArgumentCons(arg, arglstl) ->
        ArgumentCons(arg, (append_argument_list arglstl arglstb))

  let rec append_avc avlsta avlstb =
    match avlsta with
    | EndOfArgumentVariable -> avlstb
    | ArgumentVariableCons(av, avlstl) ->
        ArgumentVariableCons(av, (append_avc avlstl avlstb))

  (* ctrlseq_name -> abstract_tree -> abstract_tree -> argument_cons -> abstract_tree *)
  let rec convert_into_numeric_apply csnm clsnmast idnmast argcons =
    convert_into_numeric_apply_sub argcons (ApplyClassAndID(clsnmast, idnmast, ContentOf(csnm)))
  
  (* argument_cons -> abstract_tree -> abstract_tree *)
  and convert_into_numeric_apply_sub argcons astconstr =
    match argcons with
    | EndOfArgument -> astconstr
    | ArgumentCons(arg, actail) ->
        convert_into_numeric_apply_sub actail (NumericApply(astconstr, arg))

  let class_name_to_abstract_tree clsnm =
    StringConstant((String.sub clsnm 1 ((String.length clsnm) - 1)))

  let id_name_to_abstract_tree idnm =
    StringConstant((String.sub idnm 1 ((String.length idnm) - 1)))

  let rec curry_lambda_abstract argvarcons astdef =
    match argvarcons with
    | EndOfArgumentVariable -> astdef
    | ArgumentVariableCons(argvar, avtail) -> 
        LambdaAbstract(argvar, curry_lambda_abstract avtail astdef)

  let error_reporting msg disp ln =
    "Syntax error: " ^ msg ^ ".\n\n    " ^ disp ^ "\n\n  (at line " ^ (string_of_int ln) ^ ")"

  let rec string_of_avc argvarcons =
    match argvarcons with
    | EndOfArgumentVariable -> ""
    | ArgumentVariableCons(argvar, avtail) -> argvar ^ " " ^ (string_of_avc avtail)
%}
%token <Types.var_name> NUMVAR
%token <Types.var_name> STRVAR
%token <string> NUMCONST
%token <string> CHAR
%token SPACE
%token BREAK
%token <Types.ctrlseq_name> CTRLSEQ
%token <Types.id_name> IDNAME
%token <Types.class_name> CLASSNAME
%token END
%token LAMBDA ARROW
%token <int> LET LETAND IN
%token DEFEQ
%token <int> IF THEN ELSE
%token EOI
%token LPAREN RPAREN
%token TIMES DIVIDES
%token MOD
%token PLUS MINUS
%token EQ NEQ GEQ LEQ GT LT
%token LNOT
%token LAND
%token LOR
%token CONCAT
%token OPENQT CLOSEQT
%token OPENSTR CLOSESTR
%token OPENNUM CLOSENUM
%token BGRP EGRP
%token TRUE FALSE
%token FINISH
%token SEP
%token BLIST ELIST LISTPUNCT
%token IGNORED

%nonassoc LET DEFEQ IN LETAND
%nonassoc IF THEN ELSE
%left LOR
%left LAND
%nonassoc LNOT
%left EQ NEQ
%left GEQ LEQ GT LT
%left PLUS
%right MINUS
%left TIMES
%right MOD DIVIDES
%nonassoc NUMVAR
%nonassoc LPAREN RPAREN

%start main
%type <Types.abstract_tree> main
%type <Types.abstract_tree> nxlet
%type <Types.mutual_let_cons> nxdec
%type <Types.abstract_tree> nxlor
%type <Types.abstract_tree> nxland
%type <Types.abstract_tree> nxcomp
%type <Types.abstract_tree> nxconcat
%type <Types.abstract_tree> nxlplus
%type <Types.abstract_tree> nxltimes
%type <Types.abstract_tree> nxrplus
%type <Types.abstract_tree> nxrtimes
%type <Types.abstract_tree> nxun
%type <Types.abstract_tree> nxapp
%type <Types.abstract_tree> nxbot
%type <Types.abstract_tree> nxlist
%type <Types.abstract_tree> sxsep
%type <Types.abstract_tree> sxsepsub
%type <Types.abstract_tree> sxblock
%type <Types.abstract_tree> sxbot
%type <Types.argument_cons> narg
%type <Types.argument_cons> sarg
%type <Types.argument_cons> sargsub
%type <Types.argument_variable_cons> nargvar
%type <Types.argument_variable_cons> sargvar

%%

main:
  | nxlet EOI { $1 }
  | sxblock EOI { $1 }
;
nxlet:
  | LET STRVAR DEFEQ nxlet IN nxlet {
        Types.LetIn(Types.MutualLetCons($2, $4, Types.EndOfMutualLet), $6)
      }
  | LET NUMVAR nargvar sargvar DEFEQ nxlet nxdec nxlet {
        let argvarcons = (append_avc $3 $4) in
        let curried = curry_lambda_abstract argvarcons $6 in
          Types.LetIn(Types.MutualLetCons($2, curried, $7), $8)
      }
  | LET CTRLSEQ nargvar sargvar DEFEQ nxlet nxdec nxlet {
        let argvarcons = (append_avc $3 $4) in
        let curried = curry_lambda_abstract argvarcons $6 in
          Types.LetIn(Types.MutualLetCons($2, curried, $7), $8)
      }
  | nxif { $1 }
/* -- for syntax error log -- */
  | LET error {
        raise (ParseErrorDetail(error_reporting "illegal token after 'let'" "let ..<!>.." $1))
      }
  | LET STRVAR error {
        raise (ParseErrorDetail(error_reporting "missing '='" ("let " ^ $2 ^ " ..<!>..") $1))
      }
  | LET STRVAR DEFEQ error {
        raise (ParseErrorDetail(error_reporting "illegal token after '='" ("let " ^ $2 ^ " = ..<!>..") $1))
      }
  | LET STRVAR DEFEQ nxlet IN error {
        raise (ParseErrorDetail(error_reporting "illegal token after 'in'" ("let " ^ $2 ^ " = ... in ..<!>..") $5))
      }
  | LET NUMVAR error {
        raise (ParseErrorDetail(error_reporting "missing '=' or illegal argument" ("let " ^ $2 ^ " ..<!>..") $1))
      }
  | LET NUMVAR nargvar sargvar DEFEQ error {
        raise (ParseErrorDetail(error_reporting "illegal token after '='"
          ("let " ^ $2 ^ " " ^ (string_of_avc (append_avc $3 $4)) ^ "= ..<!>..") $1))
      }
  | LET NUMVAR nargvar sargvar DEFEQ nxlet nxdec error {
        raise (ParseErrorDetail(error_reporting "illegal token after 'in'"
          ("let " ^ $2 ^ " " ^ (string_of_avc (append_avc $3 $4)) ^ "= ... in ..<!>..") $1))
      }
  | LET CTRLSEQ error {
        raise (ParseErrorDetail(error_reporting "missing '=' or illegal argument" ("let " ^ $2 ^ " ..<!>..") $1))
      }
  | LET CTRLSEQ nargvar sargvar DEFEQ error {
        raise (ParseErrorDetail(error_reporting "illegal token after '='"
          ("let " ^ $2 ^ " " ^ (string_of_avc (append_avc $3 $4)) ^ " = ..<!>..") $1))
      }
  | LET CTRLSEQ nargvar sargvar DEFEQ nxlet nxdec error {
        raise (ParseErrorDetail(error_reporting "illegal token after 'in'"
          ("let " ^ $2 ^ " " ^ (string_of_avc (append_avc $3 $4)) ^ "= ... in ..<!>..") $1))
      }
;
nxdec:
  | LETAND NUMVAR nargvar sargvar DEFEQ nxlet nxdec {
        let argvarcons = (append_avc $3 $4) in
        let curried = curry_lambda_abstract argvarcons $6 in
          Types.MutualLetCons($2, curried, $7)
      }
  | LETAND CTRLSEQ nargvar sargvar DEFEQ nxlet nxdec {
        let argvarcons = (append_avc $3 $4) in
        let curried = curry_lambda_abstract argvarcons $6 in
          Types.MutualLetCons($2, curried, $7)
      }
  | IN { Types.EndOfMutualLet }
/* -- for syntax error log -- */
  | LETAND NUMVAR error {
        raise (ParseErrorDetail(error_reporting "illegal token after 'and'"
          ("and " ^ $2 ^ " ..<!>..") $1))
      }
  | LETAND CTRLSEQ error {
        raise (ParseErrorDetail(error_reporting "illegal token after 'and'"
          ("and " ^ $2 ^ " ..<!>..") $1))
      }
;
nxif:
  | IF nxif THEN nxif ELSE nxif { Types.IfThenElse($2, $4, $6) }
  | nxlambda { $1 }
/* -- for syntax error log -- */
  | IF error {
        raise (ParseErrorDetail(error_reporting "illegal token after 'if'" "if ..<!>.." $1))
      }
  | IF nxif THEN error {
        raise (ParseErrorDetail(error_reporting "illegal token after 'then'" "then ..<!>.." $3))
      }
  | IF nxif THEN nxif ELSE error {
        raise (ParseErrorDetail(error_reporting "illegal token after 'else'" "else ..<!>.." $5))
      }
;
nxlambda:
  | LAMBDA nargvar sargvar ARROW nxlor {
        let argvarcons = append_avc $2 $3 in
          curry_lambda_abstract argvarcons $5
      }
  | nxlor { $1 }
;
nargvar:
  | NUMVAR nargvar { Types.ArgumentVariableCons($1, $2) }
  | { Types.EndOfArgumentVariable }
;
sargvar:
  | STRVAR sargvar { Types.ArgumentVariableCons($1, $2) }
  | { Types.EndOfArgumentVariable }
;
nxlor:
  | nxland LOR nxlor { Types.LogicalOr($1, $3) }
  | nxland { $1 }
;
nxland:
  | nxcomp LAND nxland { Types.LogicalAnd($1, $3) }
  | nxcomp { $1 }
;
nxcomp:
  | nxconcat EQ nxcomp { Types.EqualTo($1, $3) }
  | nxconcat NEQ nxcomp { Types.LogicalNot(Types.EqualTo($1, $3)) }
  | nxconcat GEQ nxcomp { Types.LogicalNot(Types.LessThan($1, $3)) }
  | nxconcat LEQ nxcomp { Types.LogicalNot(Types.GreaterThan($1, $3)) }
  | nxconcat GT nxcomp { Types.GreaterThan($1, $3) }
  | nxconcat LT nxcomp { Types.LessThan($1, $3) }
  | nxconcat { $1 }
;
nxconcat:
  | nxlplus CONCAT nxconcat { Types.ConcatOperation($1, $3) }
  | nxlplus { $1 }
;
nxlplus:
  | nxlminus PLUS nxrplus { Types.Plus($1, $3) }
  | nxlminus { $1 }
;
nxlminus:
  | nxlplus MINUS nxrtimes { Types.Minus($1, $3) }
  | nxltimes { $1 }
;
nxrplus:
  | nxrminus PLUS nxrplus { Types.Plus($1, $3) }
  | nxrminus { $1 }
;
nxrminus:
  | nxrplus MINUS nxrtimes { Types.Minus($1, $3) }
  | nxrtimes { $1 }
;
nxltimes:
  | nxun TIMES nxrtimes { Types.Times($1, $3) }
  | nxltimes DIVIDES nxapp { Types.Divides($1, $3) }
  | nxltimes MOD nxapp { Types.Mod($1, $3) }
  | nxun { $1 }
;
nxrtimes:
  | nxapp TIMES nxrtimes { Types.Times($1, $3) }
  | nxrtimes DIVIDES nxapp { Types.Divides($1, $3) }
  | nxrtimes MOD nxapp { Types.Mod($1, $3) }
  | nxapp { $1 }
;
nxun:
  | MINUS nxapp { Types.Minus(Types.NumericConstant(0), $2) }
  | LNOT nxapp { Types.LogicalNot($2) }
  | nxapp { $1 }
;
nxapp:
  | nxapp nxbot { Types.NumericApply($1, $2) }
  | nxbot { $1 }
;
nxbot:
  | NUMVAR { Types.ContentOf($1) }
  | NUMCONST { Types.NumericConstant(int_of_string $1) }
  | TRUE { Types.BooleanConstant(true) }
  | FALSE { Types.BooleanConstant(false) }
  | LPAREN nxlet RPAREN { $2 }
  | OPENSTR sxsep CLOSESTR { $2 }
  | OPENQT sxsep CLOSEQT { LiteralArea($2) }
  | FINISH { Types.FinishHeaderFile }
  | BLIST ELIST { Types.EndOfList }
  | BLIST nxlet nxlist ELIST { Types.ListCons($2, $3) }
;
nxlist:
  | LISTPUNCT nxlet nxlist { Types.ListCons($2, $3) }
  | { Types.EndOfList }
;
sxsep:
  | sxblock SEP sxsepsub { ListCons($1, $3) }
  | sxblock { $1 }
;
sxsepsub:
  | sxblock SEP sxsepsub { ListCons($1, $3) }
  | sxblock { ListCons($1, EndOfList) }
;
sxblock:
  | sxbot sxblock { Types.Concat($1, $2) }
  | { Types.StringEmpty }
;
sxbot:
  | CHAR { Types.StringConstant($1) }
  | SPACE { Types.StringConstant(" ") }
  | BREAK { Types.BreakAndIndent }
  | STRVAR END { Types.ContentOf($1) }
  | CTRLSEQ narg sarg {
        convert_into_numeric_apply $1 NoContent NoContent (append_argument_list $2 $3)
        (* Types.StringApply($1, Types.NoClassName, Types.NoIDName, (append_argument_list $2 $3)) *)
      }
  | CTRLSEQ CLASSNAME narg sarg {
        let clsnmast = class_name_to_abstract_tree $2 in
          convert_into_numeric_apply $1 clsnmast NoContent (append_argument_list $3 $4)
        (* Types.StringApply($1, Types.ClassName($2), Types.NoIDName, (append_argument_list $3 $4)) *)
      }
  | CTRLSEQ IDNAME narg sarg {
        let idnmast = id_name_to_abstract_tree $2 in
          convert_into_numeric_apply $1 NoContent idnmast (append_argument_list $3 $4)
        (* Types.StringApply($1, Types.NoClassName, Types.IDName($2), (append_argument_list $3 $4)) *)
      }
  | CTRLSEQ CLASSNAME IDNAME narg sarg {
        let clsnmast = class_name_to_abstract_tree $2 in
        let idnmast = id_name_to_abstract_tree $3 in
          convert_into_numeric_apply $1 clsnmast idnmast (append_argument_list $4 $5)
        (* Types.StringApply($1, Types.ClassName($2), Types.IDName($3), (append_argument_list $4 $5)) *)
      }
;
narg: /* -> Types.argument_cons */
  | OPENNUM nxlet CLOSENUM narg { Types.ArgumentCons($2, $4) }
  | { Types.EndOfArgument }
;
sarg: /* -> Types.argument_cons */
  | BGRP sxsep EGRP sargsub { Types.ArgumentCons($2, $4) }
  | OPENQT sxsep CLOSEQT sargsub { Types.ArgumentCons(LiteralArea($2), $4) }
  | END { Types.EndOfArgument }
;
sargsub: /* -> Types.argument_cons */
  | BGRP sxsep EGRP sargsub { Types.ArgumentCons($2, $4) }
  | OPENQT sxsep CLOSEQT sargsub { Types.ArgumentCons(LiteralArea($2), $4) }
  | { Types.EndOfArgument }
;
