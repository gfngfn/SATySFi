
open MyUtil
open ConfigError
open ConfigUtil
open PackageSystemBase


type 'a ok = ('a, config_error) result

type t = implementation_record
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


let dependency_in_registry_config_decoder =
  let open ConfigDecoder in
  get "used_as" string >>= fun used_as ->
  get "name" package_name_decoder >>= fun package_name ->
  get "requirement" requirement_decoder >>= fun version_requirement ->
  succeed @@ PackageDependencyInRegistry{
    used_as;
    package_name;
    version_requirement;
  }


let release_config_decoder : implementation_record ConfigDecoder.t =
  let open ConfigDecoder in
  get "saphe" requirement_decoder >>= fun ecosystem_requirement ->
  get "satysfi" requirement_decoder >>= fun language_requirement ->
  get "name" string >>= fun package_name ->
  get "version" version_decoder >>= fun package_version ->
  get_or_else "source" source_decoder NoSource >>= fun source ->
  get "dependencies" (list dependency_in_registry_config_decoder) >>= fun dependencies ->
  succeed @@ ImplRecord{
    ecosystem_requirement;
    language_requirement;
    package_name;
    package_version;
    source;
    dependencies;
  }


let parse (s : string) : (t, yaml_error) result =
  ConfigDecoder.run release_config_decoder s


let load (abspath_release_config : abs_path) : t ok =
  let open ResultMonad in
  let* s =
    read_file abspath_release_config
      |> Result.map_error (fun _ -> ReleaseConfigNotFound(abspath_release_config))
  in
  parse s
    |> Result.map_error (fun e -> ReleaseConfigError(abspath_release_config, e))
