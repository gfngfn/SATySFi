
open MyUtil
open ConfigError


type 'a ok = ('a, config_error) result

type relative_path = string

type font_file_contents =
  | OpentypeSingle     of string
  | OpentypeCollection of string list
[@@deriving show]

type font_file_description = {
  font_file_path     : relative_path;
  font_file_contents : font_file_contents;
  used_as_math_font  : bool;
}

type envelope_conversion_spec = unit (* TODO *)
(*
  | MarkdownConversion of MarkdownParser.command_record
*)

type envelope_contents =
  | Library of {
      main_module_name   : string;
      source_directories : relative_path list;
      test_directories   : relative_path list;
      conversion_specs   : envelope_conversion_spec list;
    }
  | Font of {
      main_module_name       : string;
      font_file_descriptions : font_file_description list;
    }

type t = {
  envelope_contents : envelope_contents;
}


let load (_abspath : abs_path) : t ok =
  failwith "TODO: EnvelopeConfig.load"
