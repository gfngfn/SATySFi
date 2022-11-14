
open MyUtil
open Types
open ConfigError
open ConfigUtil
open PackageSystemBase


type 'a ok = ('a, config_error) result

type relative_path = string

type font_file_description = {
  font_file_path     : relative_path;
  font_file_contents : font_file_contents;
  used_as_math_font  : bool;
}

type package_conversion_spec =
  | MarkdownConversion of DecodeMD.command_record

type package_contents =
  | Library of {
      main_module_name   : module_name;
      source_directories : relative_path list;
      dependencies       : package_dependency list;
      conversion_specs   : package_conversion_spec list;
    }
  | Font of {
      main_module_name       : module_name;
      font_file_descriptions : font_file_description list;
    }

type t = {
  package_contents : package_contents;
}


let font_file_contents_decoder : font_file_contents ConfigDecoder.t =
  let open ConfigDecoder in
  branch "type" [
    "opentype_single" ==> begin
      get "name" string >>= fun name ->
      succeed @@ OpentypeSingle(name)
    end;
    "opentype_collection" ==> begin
      get "names" (list string) >>= fun names ->
      succeed @@ OpentypeCollection(names)
    end;
  ]
  ~other:(fun tag ->
    failure (fun context -> UnexpectedTag(context, tag))
  )


let font_file_description_decoder : font_file_description ConfigDecoder.t =
  let open ConfigDecoder in
  get "path" string >>= fun font_file_path ->
  get_or_else "math" bool false >>= fun used_as_math_font ->
  get "contents" font_file_contents_decoder >>= fun font_file_contents ->
  succeed @@ {
    font_file_path;
    font_file_contents;
    used_as_math_font;
  }


let cut_module_names (s : string) : string list * string =
  match List.rev (String.split_on_char '.' s) with
  | varnm :: modnms_rev -> (List.rev modnms_rev, varnm)
  | _                   -> assert false (* `String.split_on_char` always returns a non-empty list *)


let command_decoder ~(prefix : char) : DecodeMD.command ConfigDecoder.t =
  let open ConfigDecoder in
  string >>= fun s ->
  try
    if Char.equal prefix (String.get s 0) then
      let s_tail = (String.sub s 1 (String.length s - 1)) in
      let (modnms, varnm) = cut_module_names s_tail in
      succeed (Range.dummy "command_decoder", (modnms, varnm))
    else
      failure (fun context -> NotACommand{ context; prefix; string = s })
  with
  | Invalid_argument(_) ->
      failure (fun context -> NotACommand{ context; prefix; string = s })


let inline_command_decoder =
  command_decoder ~prefix:'\\'


let block_command_decoder =
  command_decoder ~prefix:'+'


let identifier_decoder : DecodeMD.command ConfigDecoder.t =
  let open ConfigDecoder in
  string >>= fun s ->
  let (modnms, varnm) = cut_module_names s in
  succeed (Range.dummy "identifier_decoder", (modnms, varnm))


let hard_break_decoder : (DecodeMD.command option) ConfigDecoder.t =
  let open ConfigDecoder in
  branch "type" [
    "none" ==> begin
      succeed None
    end;
    "some" ==> begin
      get "command" inline_command_decoder >>= fun command ->
      succeed @@ Some(command)
    end;
  ]
  ~other:(fun tag ->
    failure (fun context -> UnexpectedTag(context, tag))
  )


let code_block_entry_decoder : (string * DecodeMD.command) ConfigDecoder.t =
  let open ConfigDecoder in
  get "name" string >>= fun name ->
  get "command" block_command_decoder >>= fun command ->
  succeed (name, command)


let conversion_spec_decoder : package_conversion_spec ConfigDecoder.t =
  let open ConfigDecoder in
  branch "type" [
    "markdown" ==> begin
      get "document" identifier_decoder >>= fun document ->
      get "default_header" string >>= fun header_default ->
      get "paragraph" block_command_decoder >>= fun paragraph ->
      get "hr" block_command_decoder >>= fun hr ->
      get "h1" block_command_decoder >>= fun h1 ->
      get "h2" block_command_decoder >>= fun h2 ->
      get "h3" block_command_decoder >>= fun h3 ->
      get "h4" block_command_decoder >>= fun h4 ->
      get "h5" block_command_decoder >>= fun h5 ->
      get "h6" block_command_decoder >>= fun h6 ->
      get "ul_inline" block_command_decoder >>= fun ul_inline ->
      get "ul_block" block_command_decoder >>= fun ul_block ->
      get "ol_inline" block_command_decoder >>= fun ol_inline ->
      get "ol_block" block_command_decoder >>= fun ol_block ->
      get_or_else "code_block" (list code_block_entry_decoder) [] >>= fun code_block_entries ->
      get "default_code_block" block_command_decoder >>= fun code_block_default ->
      get "blockquote" block_command_decoder >>= fun blockquote ->
      get "error_block" block_command_decoder >>= fun err_block ->
      get "emph" inline_command_decoder >>= fun emph ->
      get "bold" inline_command_decoder >>= fun bold ->
      get "hard_break" hard_break_decoder >>= fun hard_break ->
      get "default_code" inline_command_decoder >>= fun code_default ->
      get "url" inline_command_decoder >>= fun url ->
      get "reference" inline_command_decoder >>= fun reference ->
      get "img" inline_command_decoder >>= fun img ->
      get "embed_block" inline_command_decoder >>= fun embed_block ->
      get "error_inline" inline_command_decoder >>= fun err_inline ->
      let code_block_map =
        code_block_entries |> List.fold_left (fun code_block_map (name, command) ->
          code_block_map |> DecodeMD.CodeNameMap.add name command
        ) DecodeMD.CodeNameMap.empty
      in
      succeed @@ MarkdownConversion(DecodeMD.{
        document;
        header_default;

        paragraph;
        hr;
        h1; h2; h3; h4; h5; h6;
        ul_inline; ul_block;
        ol_inline; ol_block;
        code_block_map; code_block_default;
        blockquote;
        err_block;

        emph;
        bold;
        hard_break;
        code_default;
        url;
        reference;
        img;
        embed_block;
        err_inline;
      })
    end;
  ]
  ~other:(fun tag ->
    failure (fun context -> UnexpectedTag(context, tag))
  )


let contents_decoder : package_contents ConfigDecoder.t =
  let open ConfigDecoder in
  branch "type" [
    "library" ==> begin
      get "main_module" string >>= fun main_module_name ->
      get "source_directories" (list string) >>= fun source_directories ->
      get_or_else "dependencies" (list dependency_decoder) [] >>= fun dependencies ->
      get_or_else "conversion" (list conversion_spec_decoder) [] >>= fun conversion_specs ->
      succeed @@ Library {
        main_module_name;
        source_directories;
        dependencies;
        conversion_specs;
      }
    end;
    "font" ==> begin
      get "main_module" string >>= fun main_module_name ->
      get "elements" (list font_file_description_decoder) >>= fun font_file_descriptions ->
      succeed @@ Font {
        main_module_name;
        font_file_descriptions;
      }
    end;
  ]
  ~other:(fun tag ->
    failure (fun context -> UnexpectedTag(context, tag))
  )


let version_0_1_config_decoder : t ConfigDecoder.t =
  let open ConfigDecoder in
  get "contents" contents_decoder >>= fun package_contents ->
  succeed @@ {
    package_contents;
  }


let config_decoder =
  let open ConfigDecoder in
  get "language" string >>= fun language ->
  match language with
  | "0.1.0" -> version_0_1_config_decoder
  | _       -> failure (fun _context -> UnexpectedLanguage(language))


let load (absdir_package : abs_path) : t ok =
  let open ResultMonad in
  let abspath_config =
    make_abs_path (Filename.concat (get_abs_path_string absdir_package) "satysfi.yaml")
  in
  let* s =
    read_file abspath_config
      |> Result.map_error (fun _ -> PackageConfigNotFound(abspath_config))
  in
  ConfigDecoder.run config_decoder s
    |> Result.map_error (fun e -> PackageConfigError(abspath_config, e))
