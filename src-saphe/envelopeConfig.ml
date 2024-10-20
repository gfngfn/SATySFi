
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


let inline_command_encoder (LongInlineCommand{ modules; main_without_prefix }) : Yaml.value =
  `String(Printf.sprintf "\\%s" (String.concat "." (List.append modules [ main_without_prefix ])))


let block_command_encoder (LongBlockCommand{ modules; main_without_prefix }) : Yaml.value =
  `String(Printf.sprintf "+%s" (String.concat "." (List.append modules [ main_without_prefix ])))


let identifier_encoder (LongIdentifier{ modules; main }) : Yaml.value =
  `String(String.concat "." (List.append modules [ main ]))


let option_encoder (e : 'a -> Yaml.value) (opt : 'a option) : Yaml.value =
  match opt with
  | None    -> `Null
  | Some(v) -> e v


let markdown_conversion_encoder (markdown_conversion : markdown_conversion) : Yaml.value =
  let
    MarkdownConversion{
      document;

      paragraph;
      hr;
      h1; h2; h3; h4; h5; h6;
      ul;
      ol;
      code_block;
      blockquote;

      emph;
      strong;
      hard_break;
      code;
      link;
      img;
    } = markdown_conversion
  in
  `O([
    ("document", identifier_encoder document);

    ("paragraph", block_command_encoder paragraph);
    ("hr", block_command_encoder hr);
    ("h1", block_command_encoder h1);
    ("h2", block_command_encoder h2);
    ("h3", block_command_encoder h3);
    ("h4", block_command_encoder h4);
    ("h5", block_command_encoder h5);
    ("h6", block_command_encoder h6);
    ("ul", block_command_encoder ul);
    ("ol", block_command_encoder ol);
    ("code_block", block_command_encoder code_block);
    ("blockquote", block_command_encoder blockquote);

    ("emph", inline_command_encoder emph);
    ("strong", inline_command_encoder strong);
    ("hard_break", option_encoder inline_command_encoder hard_break);
    ("code", inline_command_encoder code);
    ("link", inline_command_encoder link);
    ("img", inline_command_encoder img);
  ])


let envelope_config_encoder (envelope_config : t) : Yaml.value =
  let { envelope_contents } = envelope_config in
  match envelope_contents with
  | Library{ main_module_name; source_directories; test_directories; markdown_conversion } ->
      let fields_mandatory =
        [
          ("main_module", `String(main_module_name));
          ("source_directories", `A(source_directories |> List.map (fun s -> `String(s))));
          ("test_directories", `A(test_directories |> List.map (fun s -> `String(s))));
        ]
      in
      let fields_markdown =
        match markdown_conversion with
        | None    -> []
        | Some(m) -> [ ("markdown_conversion", markdown_conversion_encoder m) ]
      in
      `O([ ("library", `O(List.append fields_mandatory fields_markdown)); ])

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
  AbsPathIo.write_file abspath_envelope_config data
