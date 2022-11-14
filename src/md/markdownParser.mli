
open Types

type error =
  | InvalidHeaderComment
  | InvalidExtraExpression
  | FailedToMakeDocumentAttribute of DocumentAttribute.error

type command = Range.t * (module_name list * var_name)

type command_record = {
  document   : command;

  paragraph  : command;
  hr         : command;
  h1         : command;
  h2         : command;
  h3         : command;
  h4         : command;
  h5         : command;
  h6         : command;
  ul         : command;
  ol         : command;
  code_block : command;
  blockquote : command;

  emph       : command;
  strong     : command;
  hard_break : command option;
  code       : command;
  link       : command;
  img        : command;
}

type t

val decode : string -> (DocumentAttribute.t * module_name * t, error) result

val convert : command_record -> t -> untyped_abstract_tree
