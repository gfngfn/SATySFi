
open MyUtil
open ConfigError
open ConfigUtil
open PackageSystemBase


type t = {
  registries : registry_remote RegistryHashValueMap.t;
}


let registry_remote_decoder : registry_remote ConfigDecoder.t =
  let open ConfigDecoder in
  branch [
    "git" ==> begin
      get "url" string >>= fun url ->
      get "branch" string >>= fun branch ->
      succeed @@ GitRegistry{ url; branch }
    end;
  ]


let registry_remote_encoder = function
  | GitRegistry{ url; branch } ->
      [
        ("url", `String(url));
        ("branch", `String(branch));
      ]


let registry_spec_decoder : (registry_hash_value * registry_remote) ConfigDecoder.t =
  let open ConfigDecoder in
  get "hash_value" string >>= fun registry_hash_value ->
  registry_remote_decoder >>= fun registry_remote ->
  succeed (registry_hash_value, registry_remote)


let registry_spec_encoder (registry_hash_value, registry_remote) =
  let fields_common = [ ("hash_value", `String(registry_hash_value)) ] in
  let fields_remote = registry_remote_encoder registry_remote in
  `O(List.append fields_common fields_remote)


let config_decoder : t ConfigDecoder.t =
  let open ConfigDecoder in
  get "ecosystem" (version_checker Constant.current_ecosystem_version) >>= fun () ->
  get "registries" (list registry_spec_decoder) >>= fun registries ->
  registries |> List.fold_left (fun res (registry_hash_value, registry_remote) ->
    res >>= fun map ->
    if map |> RegistryHashValueMap.mem registry_hash_value then
      failure (fun context -> DuplicateRegistryHashValue{ context; registry_hash_value })
    else
      succeed (map |> RegistryHashValueMap.add registry_hash_value registry_remote)
  ) (succeed RegistryHashValueMap.empty) >>= fun registries ->
  succeed { registries }


let config_encoder (store_root_config : t) : Yaml.value =
  let language = SemanticVersion.(requirement_to_string (CompatibleWith(Constant.current_ecosystem_version))) in
  let registry_specs =
    store_root_config.registries |> RegistryHashValueMap.bindings |> List.map registry_spec_encoder
  in
  `O[
    ("ecosystem", `String(language));
    ("registries", `A(registry_specs));
  ]


let write (abspath_config : abs_path) (store_root_config : t) : (unit, config_error) result =
  let yaml = config_encoder store_root_config in
  let data = encode_yaml yaml in
  write_file abspath_config data
    |> Result.map_error (fun message ->
      CannotWriteStoreRootConfig{ message; path = abspath_config }
    )


let load_or_initialize (abspath_config : abs_path) : (t * bool, config_error) result =
  let open ResultMonad in
  match read_file abspath_config with
  | Ok(s) ->
      let* store_root_config =
        ConfigDecoder.run config_decoder s
          |> Result.map_error (fun e -> StoreRootConfigError(abspath_config, e))
      in
      return (store_root_config, false)

  | Error(_) ->
      let store_root_config = { registries = RegistryHashValueMap.empty } in
      let* () = write abspath_config store_root_config in
      return (store_root_config, true)
