(* mainly for Mcdlexer *)

exception SequenceUnderflow
type macro_name = string
type var_name = string
type id_name = string
type literal_name = string
type letter = string
type token = CTRLSEQ of macro_name | VAR of var_name | ID of id_name
           | END | BGRP | EGRP | SEP | CHAR of letter
           | BEGINNING_OF_INPUT | END_OF_INPUT
           | BREAK | FINALBREAK
           | MACRO | MACROWID | POP
           | SPACE
           | BLTRL of literal_name | ELTRL


(* mainly for Mcdparser *)

exception StackUnderflow
exception LineUnderflow
type nonterminal = Total | Sentence | Block | Group | Args | Params | ListBySep | CharOfLiteral
type tree = Terminal of token | NonTerminal of nonterminal * (tree list)


(* mainly for Mcdabs *)

type abstract_id_name = NoID | RealID of id_name
type abstract_tree = EmptyAbsBlock
                   | AbsBlock of abstract_tree * abstract_tree
                   | ContentOf of var_name
                   | Output of letter
                   | Pop of var_name * var_name * abstract_tree * abstract_tree
                   | Macro of macro_name * (var_name list) * abstract_tree * abstract_tree
                   | Apply of macro_name * abstract_id_name * (abstract_tree list)
                   | Invalid
                   | UnderConstruction
                       (* for 'compensate' *)
                   | Separated of abstract_tree * abstract_tree
                   | PrimitiveIfEmpty of abstract_tree * abstract_tree * abstract_tree
                   | PrimitiveIfSame of abstract_tree * abstract_tree * abstract_tree * abstract_tree
                   | PrimitiveReplace of abstract_tree * abstract_tree * abstract_tree
                   | PrimitivePrefix of abstract_tree * abstract_tree
                   | PrimitivePostfix of abstract_tree * abstract_tree
                   | PrimitiveInclude of abstract_tree
                   | LiteralBlock of literal_name * abstract_tree
                   | OutputOfLiteral of letter
                   | Indent
                   | DeeperIndent of abstract_tree
                   | ShallowerIndent of abstract_tree


(* for Mcdsemantics *)

exception IncorrespondenceOfLength
exception ValueNotFound


(* for Mcdout *)

exception IllegalOut
