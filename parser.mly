%{
  open Types

  let parse_error msg =
    print_string ("! [ERROR IN PARSER] " ^ msg ^ "\n")

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
%token LET IN DEFEQ
%token IF THEN ELSE
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

%token IGNORED

%nonassoc LET DEFEQ IN
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
%type <Types.abstract_tree> sxblock
%type <Types.abstract_tree> sxbot
%type <Types.abstract_tree> narg
%type <Types.abstract_tree> sarg
%type <Types.abstract_tree> sargsub

%%

main:
  | nxlet EOI { $1 }
;
nxlet:
  | LET NUMVAR DEFEQ nxlet IN nxlet { Types.LetNumIn($2, $4, $6) }
  | LET STRVAR DEFEQ nxlet IN nxlet { Types.LetStrIn($2, $4, $6) }
  | nxif { $1 }
;
nxif:
  | IF nxif THEN nxif ELSE nxif { Types.IfThenElse($2, $4, $6) }
  | nxlor { $1 }
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
  | nxlplus CONCAT nxconcat { Types.Concat($1, $3) }
  | nxlplus { $1 }
;
nxlplus:
  | nxltimes PLUS nxrplus { Types.Plus($1, $3) }
  | nxlplus MINUS nxrtimes { Types.Minus($1, $3) }
  | nxltimes { $1 }
;
nxrplus:
  | nxrtimes PLUS nxrplus { Types.Plus($1, $3) }
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
  | NUMVAR { Types.NumericContentOf($1) }
  | NUMCONST { Types.NumericConstant(int_of_string $1) }
  | LPAREN nxlet RPAREN { $2 }
  | OPENSTR sxblock CLOSESTR { $2 }
  | OPENQT sxblock CLOSEQT { $2 }
;
sxblock:
  | sxbot sxblock { Types.Concat($1, $2) }
  | { Types.StringEmpty }
;
sxbot:
  | CHAR { Types.StringConstant($1) }
  | SPACE { Types.StringConstant(" ") }
  | BREAK { Types.BreakAndIndent }
  | STRVAR END { Types.StringContentOf($1) }
  | CTRLSEQ narg sarg {
        Types.StringApply($1, Types.NoClassName, Types.NoIDName, $2, $3)
      }
  | CTRLSEQ CLASSNAME narg sarg {
        Types.StringApply($1, Types.ClassName($2), Types.NoIDName, $3, $4)
      }
  | CTRLSEQ IDNAME narg sarg {
        Types.StringApply($1, Types.NoClassName, Types.IDName($2), $3, $4)
      }
  | CTRLSEQ CLASSNAME IDNAME narg sarg {
        Types.StringApply($1, Types.ClassName($2), Types.IDName($3), $4, $5)
      }
;
narg:
  | OPENNUM nxlet CLOSENUM narg { Types.NumericArgument($2, $4) }
  | { Types.EndOfArgument }
;
sarg:
  | BGRP sxblock EGRP sargsub { Types.StringArgument($2, $4) }
  | OPENQT sxblock CLOSEQT sargsub { Types.StringArgument($2, $4) }
  | END { Types.EndOfArgument }
;
sargsub:
  | BGRP sxblock EGRP sargsub { Types.StringArgument($2, $4) }
  | OPENQT sxblock CLOSEQT sargsub { Types.StringArgument($2, $4) }
  | { Types.EndOfArgument }
;
