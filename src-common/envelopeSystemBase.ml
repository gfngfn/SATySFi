
open MyUtil


type envelope_name = string

type envelope_info = {
  envelope_name         : envelope_name;
  envelope_directory    : abs_path;
  envelope_dependencies : envelope_name list;
}

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

type envelope_contents =
  | Library of {
      main_module_name   : string;
      source_directories : relative_path list;
      test_directories   : relative_path list;
      conversion_specs   : package_conversion_spec list;
    }
  | Font of {
      main_module_name       : string;
      font_file_descriptions : font_file_description list;
    }
[@@deriving show]

type envelope_config = {
  envelope_contents : envelope_contents;
}
