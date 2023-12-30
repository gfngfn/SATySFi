
open MyUtil


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

type package_conversion_spec = unit (* TODO *)

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

type t = {
  envelope_contents : envelope_contents;
}


let font_file_contents_encoder (contents : font_file_contents) : Yaml.value =
  match contents with
  | OpentypeSingle(name) ->
      `O([
        ("type", `String("opentype_single"));
        ("name", `String(name));
      ])

  | OpentypeCollection(names) ->
      `O([
        ("type", `String("opentype_collection"));
        ("names", `A(names |> List.map (fun name -> `String(name))));
      ])


let font_file_description_encoder (descr : font_file_description) : Yaml.value =
  `O([
    ("path", `String(descr.font_file_path));
    ("math", `Bool(descr.used_as_math_font));
    ("contents", font_file_contents_encoder descr.font_file_contents);
  ])


let envelope_config_encoder (envelope_config : t) : Yaml.value =
  match envelope_config.envelope_contents with
  | Library{ main_module_name; source_directories; test_directories; _ } ->
      (* TODO: encode conversion specs *)
      `O([
        ("main_module", `String(main_module_name));
        ("source_directories", `A(source_directories |> List.map (fun s -> `String(s))));
        ("test_directories", `A(test_directories |> List.map (fun s -> `String(s))));
      ])

  | Font{ main_module_name; font_file_descriptions } ->
      `O([
        ("main_module", `String(main_module_name));
        ("fonts", `A(font_file_descriptions |> List.map font_file_description_encoder));
       ])


let write (abspath_envelope_config : abs_path) (envelope_config : t) : unit =
  let yaml = envelope_config_encoder envelope_config in
  match Yaml.to_string ~encoding:`Utf8 ~layout_style:`Block ~scalar_style:`Plain yaml with
  | Ok(data) ->
      Core.Out_channel.write_all (get_abs_path_string abspath_envelope_config) ~data;
      Logging.end_envelope_output abspath_envelope_config

  | Error(_) ->
      assert false
