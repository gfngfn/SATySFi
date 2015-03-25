(* mainly for McdLexer *)

type macro_name = string
type var_name = string
type id_name = string
type letter = string
type token = CTRLSEQ of macro_name | VAR of var_name | ID of id_name
           | END | BGRP | EGRP | SEP | CHAR of letter
           | BEGINNING_OF_INPUT | END_OF_INPUT
           | MACRO | POP


(* mainly for Mcdparser *)

exception StackUnderflow
exception LineUnderflow
type nonterminal = Total | Sentence | Block | Group | Args | Params | ListBySep
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
                   | Separated of abstract_tree * abstract_tree
                   | DeepenIndent
                   | ShallowIndent


(* for Mcdsemantics *)

exception IllegalLengthOfLists
exception ValueNotFound


(* for Mcdout *)

exception IllegalOut
