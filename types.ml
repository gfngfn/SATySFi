(* mainly for Mcdlexer *)

type macro_name = string
type var_name = string
type id_name = string
type class_name = string

type token =
(* numeric token *)
  | NUMCONST of string
  | NUMVAR of var_name
  | LET | IN
  | IF | THEN | ELSE
  | FUN | DEFEQ
  | LPAREN | RPAREN
  | TIMES | DIVIDES | MOD | PLUS | MINUS | UMINUS
  | EQ | NEQ | GEQ | LEQ | GT | LT
  | LAND | LOR | LNOT
  | CONCAT
  | SEQEXEC
  | OPENSTR | CLOSESTR
  | EOI
(* string token *)
  | CHAR of string
  | BREAK | SPACE
  | CTRLSEQ of macro_name
  | STRVAR of var_name
  | IDNAME of id_name
  | CLASSNAME of class_name
  | BGRP | EGRP
  | OPENQT | CLOSEQT
  | OPENNUM | CLOSENUM
  | END
  | SEP


(* mainly for Mcdparser *)

exception LineUnderflow
type nonterminal = Total | Sentence | Block | Group | Args | Params | ListBySep | CharOfLiteral
type tree = Terminal of token | NonTerminal of nonterminal * (tree list)


(* mainly for Mcdabs *)

type abstract_id_name = NoID | RealID of id_name
type abstract_tree =
  | EmptyAbsBlock
  | AbsBlock of abstract_tree * abstract_tree
  | StringContentOf of var_name
  | NumericContentOf of var_name
  | StringConstant of string
  | NumericConstant of int
  | Macro of macro_name * (var_name list) * abstract_tree * abstract_tree
  | Apply of macro_name * abstract_id_name * (abstract_tree list)
  | Invalid
  | UnderConstruction (* for 'compensate' *)
  | Separated of abstract_tree * abstract_tree
  | BreakAndIndent
  | DeeperIndent of abstract_tree


(* for Mcdsemantics *)

exception IncorrespondenceOfLength
exception ValueNotFound


(* for Mcdout *)

exception IllegalOut
