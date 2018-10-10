
open Types_

module CodeNameMap : Map.S with type key = string

type command = module_name list * var_name

type command_record = {
  paragraph          : command;
  hr                 : command;
  h1                 : command;
  h2                 : command;
  h3                 : command;
  h4                 : command;
  h5                 : command;
  h6                 : command;
  ul_inline          : command;
  ul_block           : command;
  ol_inline          : command;
  ol_block           : command;
  code_block_map     : command CodeNameMap.t;
  code_block_default : command;

  emph               : command;
  bold               : command;
  hard_break         : command;
  code_map           : command CodeNameMap.t;
  code_default       : command;
}

val decode : command_record -> string -> untyped_abstract_tree
