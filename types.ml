
type ctrlseq_name = string
type var_name = string
type id_name = string
type class_name = string
(*
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
    | IGNORED
  (* string token *)
    | CHAR of string
    | BREAK | SPACE
    | CTRLSEQ of ctrlseq_name
    | STRVAR of var_name
    | IDNAME of id_name
    | CLASSNAME of class_name
    | BGRP | EGRP
    | OPENQT | CLOSEQT
    | OPENNUM | CLOSENUM
    | END
    | SEP
*)
type id_name_arg = NoIDName | IDName of id_name
type class_name_arg = NoClassName | ClassName of class_name
type abstract_tree =
  | Concat of abstract_tree * abstract_tree
  | NumericEmpty
  | StringEmpty
  | NumericContentOf of var_name
  | StringContentOf of var_name
  | NumericConstant of int
  | StringConstant of string
  | NumericApply of abstract_tree * abstract_tree
  | StringApply of ctrlseq_name * class_name_arg * id_name_arg * abstract_tree * abstract_tree
  | NumericArgument of abstract_tree * abstract_tree
  | StringArgument of abstract_tree * abstract_tree
  | EndOfArgument
(*
  | Macro of ctrlseq_name * (var_name list) * abstract_tree * abstract_tree
  | Apply of ctrlseq_name * abstract_id_name * (abstract_tree list)
*)
  | Invalid
  | UnderConstruction (* for 'compensate' *)
  | Separated of abstract_tree * abstract_tree
  | BreakAndIndent
  | DeeperIndent of abstract_tree
  | Times of abstract_tree * abstract_tree
  | Divides of abstract_tree * abstract_tree
  | Mod of abstract_tree * abstract_tree
  | Plus of abstract_tree * abstract_tree
  | Minus of abstract_tree * abstract_tree
  | GreaterThan of abstract_tree * abstract_tree
  | LessThan of abstract_tree * abstract_tree
  | EqualTo of abstract_tree * abstract_tree
  | LogicalAnd of abstract_tree * abstract_tree
  | LogicalOr of abstract_tree * abstract_tree
  | LogicalNot of abstract_tree
  | LetNumIn of var_name * abstract_tree * abstract_tree
  | LetStrIn of var_name * abstract_tree * abstract_tree
  | IfThenElse of abstract_tree * abstract_tree * abstract_tree

(* for Mcdsemantics *)

exception IncorrespondenceOfLength
exception ValueNotFound


(* for Mcdout *)

exception IllegalOut
