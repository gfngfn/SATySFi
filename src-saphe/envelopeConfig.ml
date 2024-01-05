
open MyUtil
open EnvelopeSystemBase


type t = envelope_config


let font_spec_encoder (font_spec : font_spec) : Yaml.value =
  let { font_item_name; used_as_math_font } = font_spec in
  `O([
    ("name", `String(font_item_name));
    ("math", `Bool(used_as_math_font));
  ])


let font_file_contents_encoder (contents : font_file_contents) : (string * Yaml.value) list =
  match contents with
  | OpentypeSingle(font_spec) ->
      [ ("opentype_single", font_spec_encoder font_spec) ]

  | OpentypeCollection(font_specs) ->
      [ ("opentype_collection", `A(font_specs |> List.map font_spec_encoder)) ]


let font_file_description_encoder (descr : font_file_description) : Yaml.value =
  let fields_common = [ ("path", `String(descr.font_file_path)) ] in
  let fields_contents = font_file_contents_encoder descr.font_file_contents in
  `O(List.append fields_common fields_contents)


let envelope_config_encoder (envelope_config : t) : Yaml.value =
  let { envelope_contents } = envelope_config in
  match envelope_contents with
  | Library{ main_module_name; source_directories; test_directories; _ } ->
      (* TODO: encode conversion specs *)
      `O([
        ("library", `O([
          ("main_module", `String(main_module_name));
          ("source_directories", `A(source_directories |> List.map (fun s -> `String(s))));
          ("test_directories", `A(test_directories |> List.map (fun s -> `String(s))));
        ]));
      ])

  | Font{ main_module_name; font_file_descriptions } ->
      `O([
        ("font", `O([
          ("main_module", `String(main_module_name));
          ("files", `A(font_file_descriptions |> List.map font_file_description_encoder));
        ]));
       ])


let write (abspath_envelope_config : abs_path) (envelope_config : t) : (unit, string) result =
  let yaml = envelope_config_encoder envelope_config in
  let data = encode_yaml yaml in
  write_file abspath_envelope_config data
