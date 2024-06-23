(*
open MyUtil
*)

type envelope_name = string
[@@deriving show]

type relative_path = string
[@@deriving show]

type font_spec = {
  font_item_name    : string;
  used_as_math_font : bool;
}
[@@deriving show]

type font_file_contents =
  | OpentypeSingle     of font_spec
  | OpentypeCollection of font_spec list
[@@deriving show]

type font_file_description = {
  font_file_path     : relative_path;
  font_file_contents : font_file_contents;
}
[@@deriving show]

type package_conversion_spec = unit (* TODO *)
[@@deriving show]

type long_inline_command = LongInlineCommand of {
  modules             : string list;
  main_without_prefix : string;
}
[@@deriving show { with_path = false }]

type long_block_command = LongBlockCommand of {
  modules             : string list;
  main_without_prefix : string;
}
[@@deriving show { with_path = false }]

type long_identifier = LongIdentifier of {
  modules : string list;
  main    : string;
}
[@@deriving show { with_path = false }]

type markdown_conversion = MarkdownConversion of {
  document   : long_identifier;

  paragraph  : long_block_command;
  hr         : long_block_command;
  h1         : long_block_command;
  h2         : long_block_command;
  h3         : long_block_command;
  h4         : long_block_command;
  h5         : long_block_command;
  h6         : long_block_command;
  ul         : long_block_command;
  ol         : long_block_command;
  code_block : long_block_command;
  blockquote : long_block_command;

  emph       : long_inline_command;
  strong     : long_inline_command;
  hard_break : long_inline_command option;
  code       : long_inline_command;
  link       : long_inline_command;
  img        : long_inline_command;
}
[@@deriving show { with_path = false }]

type envelope_contents =
  | Library of {
      main_module_name    : string;
      source_directories  : relative_path list;
      test_directories    : relative_path list;
      markdown_conversion : markdown_conversion option;
    }
  | Font of {
      main_module_name       : string;
      font_file_descriptions : font_file_description list;
    }
[@@deriving show]

type envelope_config = {
  envelope_contents : envelope_contents;
}
[@@deriving show]

type envelope_dependency = {
  dependency_name    : envelope_name;
  dependency_used_as : string;
}
[@@deriving show]

type envelope_spec = {
  envelope_name          : envelope_name;
  envelope_path          : string;
  envelope_dependencies  : envelope_dependency list;
  test_only_envelope     : bool;
}
[@@deriving show]

type deps_config = {
  envelopes                  : envelope_spec list;
  explicit_dependencies      : envelope_dependency list;
  explicit_test_dependencies : envelope_dependency list;
}
[@@deriving show]
