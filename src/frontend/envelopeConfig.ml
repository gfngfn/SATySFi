
open MyUtil
open CommonUtil
open EnvelopeSystemBase
open ConfigError
open ConfigUtil


type t = envelope_config


let font_spec_decoder : font_spec ConfigDecoder.t =
  let open ConfigDecoder in
  get "name" lowercased_identifier_decoder >>= fun font_item_name ->
  get "math" bool >>= fun used_as_math_font ->
  succeed { font_item_name; used_as_math_font }


let font_file_contents_decoder : font_file_contents ConfigDecoder.t =
  let open ConfigDecoder in
  branch [
    "opentype_single" ==> begin
      font_spec_decoder >>= fun font_spec ->
      succeed @@ OpentypeSingle(font_spec)
    end;
    "opentype_collection" ==> begin
      list font_spec_decoder >>= fun font_specs ->
      succeed @@ OpentypeCollection(font_specs)
    end;
  ]


let font_file_description_decoder : font_file_description ConfigDecoder.t =
  let open ConfigDecoder in
  get "path" relative_path_decoder >>= fun font_file_path ->
  font_file_contents_decoder >>= fun font_file_contents ->
  succeed @@ {
    font_file_path;
    font_file_contents;
  }


let command_decoder ~(prefix : string) (k : string list -> string -> 'a) : 'a ConfigDecoder.t =
  let open ConfigDecoder in
  string >>= fun s ->
  match parse_long_command ~prefix s with
  | None ->
      failure (fun context -> NotACommand{ context; prefix; got = s })

  | Some((modnms, varnm)) ->
      succeed @@ k modnms varnm


let inline_command_decoder =
  command_decoder ~prefix:"\\" (fun modules main_without_prefix ->
    LongInlineCommand{ modules; main_without_prefix }
  )


let block_command_decoder =
  command_decoder ~prefix:"+" (fun modules main_without_prefix ->
    LongBlockCommand{ modules; main_without_prefix }
  )


let long_identifier_decoder : long_identifier ConfigDecoder.t =
  let open ConfigDecoder in
  string >>= fun s ->
  match parse_long_identifier s with
  | None ->
      failure (fun context -> NotAChainedIdentifier{ context; got = s })

  | Some((modnms, varnm)) ->
      succeed @@ LongIdentifier{ modules = modnms; main = varnm }


let markdown_conversion_decoder : markdown_conversion ConfigDecoder.t =
  let open ConfigDecoder in
  get "document" long_identifier_decoder >>= fun document ->
  get "paragraph" block_command_decoder >>= fun paragraph ->
  get "hr" block_command_decoder >>= fun hr ->
  get "h1" block_command_decoder >>= fun h1 ->
  get "h2" block_command_decoder >>= fun h2 ->
  get "h3" block_command_decoder >>= fun h3 ->
  get "h4" block_command_decoder >>= fun h4 ->
  get "h5" block_command_decoder >>= fun h5 ->
  get "h6" block_command_decoder >>= fun h6 ->
  get "ul" block_command_decoder >>= fun ul ->
  get "ol" block_command_decoder >>= fun ol ->
  get "code_block" block_command_decoder >>= fun code_block ->
  get "blockquote" block_command_decoder >>= fun blockquote ->
  get "emph" inline_command_decoder >>= fun emph ->
  get "strong" inline_command_decoder >>= fun strong ->
  get_opt "hard_break" inline_command_decoder >>= fun hard_break ->
  get "code" inline_command_decoder >>= fun code ->
  get "link" inline_command_decoder >>= fun link ->
  get "img" inline_command_decoder >>= fun img ->
  succeed @@ MarkdownConversion{
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
  }


let envelope_config_decoder : t ConfigDecoder.t =
  let open ConfigDecoder in
  branch [
    "library" ==> begin
      get "main_module" string >>= fun main_module_name ->
      get "source_directories" (list string) >>= fun source_directories ->
      get "test_directories" (list string) >>= fun test_directories ->
      get_opt "markdown_conversion" markdown_conversion_decoder >>= fun markdown_conversion ->
      succeed {
        envelope_contents =
          Library{ main_module_name; source_directories; test_directories; markdown_conversion };
      }
    end;
    "font" ==> begin
      get "main_module" string >>= fun main_module_name ->
      get "files" (list font_file_description_decoder) >>= fun font_file_descriptions ->
      succeed {
        envelope_contents =
          Font{ main_module_name; font_file_descriptions }
      }
    end;
  ]


let load (abspath_envelope_config : abs_path) : (t, config_error) result =
  let open ResultMonad in
  let* s =
    AbsPathIo.read_file abspath_envelope_config
      |> Result.map_error (fun _ -> EnvelopeConfigNotFound(abspath_envelope_config))
  in
  ConfigDecoder.run envelope_config_decoder s
    |> Result.map_error (fun e -> EnvelopeConfigError(abspath_envelope_config, e))
