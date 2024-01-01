
open MyUtil
open ConfigError
open PackageSystemBase


type relative_path = string

type font_file_description = {
  font_file_path     : relative_path;
  font_file_contents : font_file_contents;
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


let font_spec_encoder (font_spec : font_spec) : Yaml.value =
  `O([
    ("name", `String(font_spec.font_item_name));
    ("math", `Bool(font_spec.used_as_math_font));
  ])


let font_file_contents_encoder (contents : font_file_contents) : Yaml.value =
  match contents with
  | OpentypeSingle(font_spec) ->
      `O([
        ("type", `String("opentype_single"));
        ("contents", font_spec_encoder font_spec)
      ])

  | OpentypeCollection(font_specs) ->
      `O([
        ("type", `String("opentype_collection"));
        ("contents", `A(font_specs |> List.map font_spec_encoder));
      ])


let font_file_description_encoder (descr : font_file_description) : Yaml.value =
  `O([
    ("path", `String(descr.font_file_path));
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


let write (abspath_envelope_config : abs_path) (envelope_config : t) : (unit, config_error) result =
  let yaml = envelope_config_encoder envelope_config in
  let data = encode_yaml yaml in
  write_file abspath_envelope_config data
    |> Result.map_error (fun message ->
      CannotWriteEnvelopeConfig{ message; path = abspath_envelope_config }
    )
