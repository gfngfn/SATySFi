
open MyUtil
open EnvelopeSystemBase
open ConfigError
open ConfigUtil


type t = envelope_config


let font_spec_decoder : font_spec ConfigDecoder.t =
  let open ConfigDecoder in
  get "name" string >>= fun font_item_name ->
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


let font_spec_decoder : font_spec ConfigDecoder.t =
  let open ConfigDecoder in
  get "name" string >>= fun font_item_name ->
  get_or_else "math" bool false >>= fun used_as_math_font ->
  succeed { font_item_name; used_as_math_font }


let font_file_description_decoder : font_file_description ConfigDecoder.t =
  let open ConfigDecoder in
  get "path" string >>= fun font_file_path ->
  font_file_contents_decoder >>= fun font_file_contents ->
  succeed @@ {
    font_file_path;
    font_file_contents;
  }


let envelope_config_decoder : t ConfigDecoder.t =
  let open ConfigDecoder in
  branch [
    "library" ==> begin
      get "main_module" string >>= fun main_module_name ->
      get "source_directories" (list string) >>= fun source_directories ->
      get "test_directories" (list string) >>= fun test_directories ->
      let conversion_specs = [] in (* TODO *)
      succeed {
        envelope_contents =
          Library{ main_module_name; source_directories; test_directories; conversion_specs };
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
    read_file abspath_envelope_config
      |> Result.map_error (fun _ -> EnvelopeConfigNotFound(abspath_envelope_config))
  in
  ConfigDecoder.run envelope_config_decoder s
    |> Result.map_error (fun e -> EnvelopeConfigError(abspath_envelope_config, e))
