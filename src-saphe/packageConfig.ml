
open MyUtil
open CommonUtil
open ConfigError
open ConfigUtil
open EnvelopeSystemBase
open PackageSystemBase
open PackageConfigImpl


type 'a ok = ('a, config_error) result

type t = {
  language_requirement   : SemanticVersion.requirement;
  package_name           : package_name option;
  package_authors        : string list;
  package_contributors   : string list;
  external_resources     : (string * external_resource) list; (* Names must not contain '/'. *)
  intermediate_directory : relative_path option;
  package_contents       : package_contents;
  registry_remotes       : registry_remote list;
  source_dependencies    : package_dependency list;
  test_dependencies      : package_dependency list;
}


let font_spec_decoder : font_spec ConfigDecoder.t =
  let open ConfigDecoder in
  get "name" lowercased_identifier_decoder >>= fun font_item_name ->
  get_or_else "math" bool false >>= fun used_as_math_font ->
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


let contents_decoder : package_contents ConfigDecoder.t =
  let open ConfigDecoder in
  branch [
    "library" ==> begin
      get "main_module" uppercased_identifier_decoder >>= fun main_module_name ->
      get "source_directories" (list relative_path_decoder) >>= fun source_directories ->
      get_or_else "test_directories" (list relative_path_decoder) [] >>= fun test_directories ->
      get_opt "markdown_conversion" markdown_conversion_decoder >>= fun markdown_conversion ->
      succeed @@ Library {
        main_module_name;
        source_directories;
        test_directories;
        markdown_conversion;
      }
    end;
    "font" ==> begin
      get "main_module" uppercased_identifier_decoder >>= fun main_module_name ->
      get "files" (list font_file_description_decoder) >>= fun font_file_descriptions ->
      succeed @@ Font { main_module_name; font_file_descriptions }
    end;
    "document" ==> begin
      succeed @@ Document
    end;
  ]


let extraction_decoder : extraction ConfigDecoder.t =
  let open ConfigDecoder in
  get "from" relative_path_decoder >>= fun extracted_from ->
  get "to" relative_path_decoder >>= fun extracted_to ->
  succeed { extracted_from; extracted_to }


let external_resource_decoder : (string * external_resource) ConfigDecoder.t =
  let open ConfigDecoder in
  get "name" writable_name_decoder >>= fun name ->
  branch [
    "zip" ==> begin
      get "url" string >>= fun url ->
      get "checksum" string >>= fun checksum ->
      get "extractions" (list extraction_decoder) >>= fun extractions ->
      succeed (name, ExternalZip{ url; checksum; extractions })
    end;
  ]


let config_decoder : parsed_package_config ConfigDecoder.t =
  let open ConfigDecoder in
  get "saphe" (version_checker Constant.current_ecosystem_version) >>= fun () ->
  get "satysfi" requirement_decoder >>= fun language_requirement ->
  get_opt "name" package_name_decoder >>= fun package_name ->
  get "authors" (list string) >>= fun package_authors ->
  get_or_else "contributors" (list string) [] >>= fun package_contributors ->
  get_or_else "registries" (list registry_spec_decoder) [] >>= fun registry_specs ->
  get_or_else "external_resources" (list external_resource_decoder) [] >>= fun external_resources ->
  get_opt "intermediate_directory" relative_path_decoder >>= fun intermediate_directory ->
  get "contents" contents_decoder >>= fun package_contents ->
  get_or_else "dependencies" (list dependency_decoder) [] >>= fun source_dependencies ->
  get_or_else "test_dependencies" (list dependency_decoder) [] >>= fun test_dependencies ->
  succeed @@ ParsedPackageConfig{
    language_requirement;
    package_name;
    package_authors;
    package_contributors;
    registry_specs;
    external_resources;
    intermediate_directory;
    package_contents;
    source_dependencies;
    test_dependencies;
  }


let validate_dependency ~dir:(absdir_config : abs_path) (localmap : registry_remote RegistryLocalNameMap.t) (dep : parsed_package_dependency) : package_dependency ok =
  let open ResultMonad in
  let ParsedPackageDependency{ used_as; spec } = dep in
  let* spec =
    match spec with
    | ParsedRegisteredDependency{
        package_name;
        registry_local_name;
        version_requirement;
      } ->
        let* registry_hash_value = ConfigUtil.lookup_registry_hash_value registry_local_name localmap in
        return @@ RegisteredDependency{
          registered_package_id = RegisteredPackageId.{ package_name; registry_hash_value };
          version_requirement;
        }

    | ParsedLocalFixedDependency{ relative_path } ->
        return @@ LocalFixedDependency{
          absolute_path = AbsPath.append_to_directory absdir_config relative_path;
        }
  in
  return @@ PackageDependency{ used_as; spec }


let validate ~(dir : abs_path) (p_package_config : parsed_package_config) : t ok =
  let open ResultMonad in
  let
    ParsedPackageConfig{
      language_requirement;
      package_name;
      package_authors;
      package_contributors;
      external_resources;
      intermediate_directory;
      package_contents;
      registry_specs;
      source_dependencies;
      test_dependencies;
    } = p_package_config
  in
  let* (localmap, registry_remotes) = ConfigUtil.construct_registry_local_map registry_specs in
  let* source_dependencies = mapM (validate_dependency ~dir localmap) source_dependencies in
  let* test_dependencies = mapM (validate_dependency ~dir localmap) test_dependencies in
  return {
    language_requirement;
    package_name;
    package_authors;
    package_contributors;
    external_resources;
    intermediate_directory;
    package_contents;
    registry_remotes;
    source_dependencies;
    test_dependencies;
  }


let parse (s : string) : (parsed_package_config, yaml_error) result =
  ConfigDecoder.run config_decoder s


let load (abspath_config : abs_path) : t ok =
  let open ResultMonad in
  let* s =
    AbsPathIo.read_file abspath_config
      |> Result.map_error (fun _ -> PackageConfigNotFound(abspath_config))
  in
  let* internal =
    parse s
      |> Result.map_error (fun e -> PackageConfigError(abspath_config, e))
  in
  validate ~dir:(dirname abspath_config) internal
