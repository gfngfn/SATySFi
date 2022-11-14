
open Types

module CodeNameMap : Map.S with type key = string

type command = Range.t * (module_name list * var_name)

type command_record = {
  document           : command;

  paragraph          : command;
  hr                 : command;
  h1                 : command;
  h2                 : command;
  h3                 : command;
  h4                 : command;
  h5                 : command;
  h6                 : command;
  ul_block           : command;
  ol_block           : command;
  code_block_map     : command CodeNameMap.t;
  code_block_default : command;
  blockquote         : command;

  emph               : command;
  bold               : command;
  hard_break         : command option;
  code_default       : command;
  url                : command;
  img                : command;
}

type t

val decode : string -> DocumentAttribute.t * module_name * t

val convert : command_record -> t -> untyped_abstract_tree
