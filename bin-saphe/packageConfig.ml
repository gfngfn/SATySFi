
open MyUtil
open ConfigError
open ConfigUtil
open PackageSystemBase


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

type package_conversion_spec = unit (* TODO *)
(*
  | MarkdownConversion of MarkdownParser.command_record
*)

type package_contents =
  | Library of {
      main_module_name   : string;
      source_directories : relative_path list;
      test_directories   : relative_path list;
      dependencies       : package_dependency list;
      test_dependencies  : package_dependency list;
      conversion_specs   : package_conversion_spec list;
    }
  | Font of {
      main_module_name       : string;
      font_file_descriptions : font_file_description list;
    }

type t = {
  language_requirement : SemanticVersion.requirement;
  package_name         : package_name;
  package_authors      : string list;
  external_sources     : (string * external_source) list;
  package_contents     : package_contents;
  registry_specs       : registry_remote RegistryLocalNameMap.t;
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


(*
let cut_module_names (s : string) : string list * string =
  match List.rev (String.split_on_char '.' s) with
  | varnm :: modnms_rev -> (List.rev modnms_rev, varnm)
  | _                   -> assert false (* `String.split_on_char` always returns a non-empty list *)


let command_decoder ~(prefix : char) : MarkdownParser.command ConfigDecoder.t =
  let open ConfigDecoder in
  string >>= fun s ->
  try
    if Char.equal prefix (String.get s 0) then
      let s_tail = (String.sub s 1 (String.length s - 1)) in
      let (modnms, varnm) = cut_module_names s_tail in
      let csnm = Printf.sprintf "%c%s" prefix varnm in
      succeed (Range.dummy "command_decoder", (modnms, csnm))
    else
      failure (fun context -> NotACommand{ context; prefix; string = s })
  with
  | Invalid_argument(_) ->
      failure (fun context -> NotACommand{ context; prefix; string = s })


let inline_command_decoder =
  command_decoder ~prefix:'\\'


let block_command_decoder =
  command_decoder ~prefix:'+'


let identifier_decoder : MarkdownParser.command ConfigDecoder.t =
  let open ConfigDecoder in
  string >>= fun s ->
  let (modnms, varnm) = cut_module_names s in
  succeed (Range.dummy "identifier_decoder", (modnms, varnm))


let hard_break_decoder : (MarkdownParser.command option) ConfigDecoder.t =
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


let conversion_spec_decoder : package_conversion_spec ConfigDecoder.t =
  let open ConfigDecoder in
  branch "type" [
    "markdown" ==> begin
      get "document" identifier_decoder >>= fun document ->
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
      get "hard_break" hard_break_decoder >>= fun hard_break ->
      get "code" inline_command_decoder >>= fun code ->
      get "link" inline_command_decoder >>= fun link ->
      get "img" inline_command_decoder >>= fun img ->
      succeed @@ MarkdownConversion(MarkdownParser.{
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
      })
    end;
  ]
  ~other:(fun tag ->
    failure (fun context -> UnexpectedTag(context, tag))
  )
*)

(* TODO: fix this *)
let conversion_spec_decoder =
  let open ConfigDecoder in
  succeed ()


let contents_decoder : package_contents ConfigDecoder.t =
  let open ConfigDecoder in
  branch "type" [
    "library" ==> begin
      get "main_module" string >>= fun main_module_name ->
      get "source_directories" (list string) >>= fun source_directories ->
      get_or_else "test_directories" (list string) [] >>= fun test_directories ->
      get_or_else "dependencies" (list dependency_decoder) [] >>= fun dependencies ->
      get_or_else "test_dependencies" (list dependency_decoder) [] >>= fun test_dependencies ->
      get_or_else "conversion" (list conversion_spec_decoder) [] >>= fun conversion_specs ->
      succeed @@ Library {
        main_module_name;
        source_directories;
        test_directories;
        dependencies;
        test_dependencies;
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


let registry_spec_decoder =
  let open ConfigDecoder in
  get "name" string >>= fun registry_local_name ->
  get "remote" registry_remote_decoder >>= fun registry_remote ->
  succeed (registry_local_name, registry_remote)


let extraction_decoder : extraction ConfigDecoder.t =
  let open ConfigDecoder in
  get "from" string >>= fun extracted_from ->
  get "to" string >>= fun extracted_to ->
  succeed { extracted_from; extracted_to }


let external_source_decoder : (string * external_source) ConfigDecoder.t =
  let open ConfigDecoder in
  branch "type" [
    "zip" ==> begin
      get "name" name_decoder >>= fun name ->
      get "url" string >>= fun url ->
      get "checksum" string >>= fun checksum ->
      get "extractions" (list extraction_decoder) >>= fun extractions ->
      succeed (name, ExternalZip{ url; checksum; extractions })
    end;
  ]
  ~other:(fun tag ->
    failure (fun context -> UnexpectedTag(context, tag))
  )


let config_decoder : t ConfigDecoder.t =
  let open ConfigDecoder in
  get "ecosystem" (version_checker Constant.current_ecosystem_version) >>= fun () ->
  get "language" requirement_decoder >>= fun language_requirement ->
  get "name" package_name_decoder >>= fun package_name ->
  get "authors" (list string) >>= fun package_authors ->
  get_or_else "registries" (list registry_spec_decoder) [] >>= fun registry_specs ->
  get_or_else "external_sources" (list external_source_decoder) [] >>= fun external_sources ->
  get "contents" contents_decoder >>= fun package_contents ->
  registry_specs |> List.fold_left (fun res (registry_local_name, registry_remote) ->
    res >>= fun map ->
    if map |> RegistryLocalNameMap.mem registry_local_name then
      failure (fun context -> DuplicateRegistryLocalName{ context; registry_local_name })
    else
      succeed (map |> RegistryLocalNameMap.add registry_local_name registry_remote)
  ) (succeed RegistryLocalNameMap.empty) >>= fun registry_specs ->
  succeed @@ {
    language_requirement;
    package_name;
    package_authors;
    external_sources;
    package_contents;
    registry_specs;
  }


let load (abspath_config : abs_path) : t ok =
  print_endline @@ "!!!! LOAD: " ^ get_abs_path_string abspath_config;
  let open ResultMonad in
  let* s =
    read_file abspath_config
      |> Result.map_error (fun _ -> PackageConfigNotFound(abspath_config))
  in
  ConfigDecoder.run config_decoder s
    |> Result.map_error (fun e -> PackageConfigError(abspath_config, e))
