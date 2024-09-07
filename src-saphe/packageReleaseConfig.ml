
open MyUtil
open ConfigError
open ConfigUtil
open PackageSystemBase
open PackageReleaseConfigImpl


type 'a ok = ('a, config_error) result

type t = {
  ecosystem_requirement : SemanticVersion.requirement;
  registry_remotes      : registry_remote list;
  package_name          : package_name;
  implementation        : implementation_record;
}
[@@deriving show { with_path = false }]


let source_decoder : implementation_source ConfigDecoder.t =
  let open ConfigDecoder in
  branch [
    "tar_gzip" ==> begin
      get "url" string >>= fun url ->
      get "checksum" string >>= fun checksum ->
      succeed @@ TarGzip{ url; checksum }
    end;
  ]


let dependency_in_registry_config_decoder : parsed_package_dependency_in_registry ConfigDecoder.t =
  let open ConfigDecoder in
  get "used_as" uppercased_identifier_decoder >>= fun used_as ->
  get_opt "registry" registry_local_name_decoder >>= fun registry_local_name_option ->
  get "name" package_name_decoder >>= fun package_name ->
  get "requirement" requirement_decoder >>= fun version_requirement ->
  succeed @@ ParsedPackageDependencyInRegistry{
    used_as;
    registry_local_name_option;
    package_name;
    version_requirement;
  }


let release_config_decoder : parsed_package_release_config ConfigDecoder.t =
  let open ConfigDecoder in
  get "saphe" requirement_decoder >>= fun ecosystem_requirement ->
  get "satysfi" requirement_decoder >>= fun language_requirement ->
  get "name" package_name_decoder >>= fun package_name ->
  get "version" version_decoder >>= fun package_version ->
  get_or_else "source" source_decoder NoSource >>= fun source ->
  get_or_else "registries" (list registry_spec_decoder) [] >>= fun registry_specs ->
  get "dependencies" (list dependency_in_registry_config_decoder) >>= fun dependencies ->
  succeed @@ ParsedPackageReleaseConfig{
    ecosystem_requirement;
    language_requirement;
    package_name;
    package_version;
    source;
    registry_specs;
    dependencies;
  }


let validate_dependency (localmap : registry_remote RegistryLocalNameMap.t) (p_dep : parsed_package_dependency_in_registry) : package_dependency_in_registry ok =
  let open ResultMonad in
  let
    ParsedPackageDependencyInRegistry{
      used_as;
      registry_local_name_option;
      package_name;
      version_requirement;
    } = p_dep
  in
  let* external_registry_hash_value =
    match registry_local_name_option with
    | None ->
        return None

    | Some(registry_local_name) ->
        let* r = ConfigUtil.lookup_registry_hash_value registry_local_name localmap in
        return @@ Some(r)
  in
  return @@ PackageDependencyInRegistry{
    used_as;
    external_registry_hash_value;
    package_name;
    version_requirement;
  }

let validate (p_package_release_config : parsed_package_release_config) : t ok =
  let open ResultMonad in
  let
    ParsedPackageReleaseConfig{
      ecosystem_requirement;
      language_requirement;
      package_name;
      package_version;
      source;
      registry_specs;
      dependencies;
    } = p_package_release_config
  in
  let* (localmap, registry_remotes) = ConfigUtil.construct_registry_local_map registry_specs in
  let* dependencies = mapM (validate_dependency localmap) dependencies in
  return @@ {
    ecosystem_requirement;
    package_name;
    registry_remotes;
    implementation = ImplRecord{
      language_requirement;
      package_version;
      source;
      dependencies;
    };
  }


let parse (s : string) : (parsed_package_release_config, yaml_error) result =
  ConfigDecoder.run release_config_decoder s


let load (abspath_release_config : abs_path) : t ok =
  let open ResultMonad in
  let* s =
    read_file abspath_release_config
      |> Result.map_error (fun _ -> ReleaseConfigNotFound(abspath_release_config))
  in
  let* internal =
    parse s
      |> Result.map_error (fun e -> ReleaseConfigError(abspath_release_config, e))
  in
  validate internal
