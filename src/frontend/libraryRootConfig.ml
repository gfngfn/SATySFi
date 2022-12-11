
open MyUtil
open ConfigError
open ConfigUtil
open PackageSystemBase


type t = {
  registries : registry_remote RegistryHashValueMap.t;
}


let registry_remote_decoder : registry_remote ConfigDecoder.t =
  let open ConfigDecoder in
  branch "type" [
    "git" ==> begin
      get "url" string >>= fun url ->
      get "branch" string >>= fun branch ->
      succeed @@ GitRegistry{ url; branch }
    end;
  ]
  ~other:(fun tag ->
    failure (fun context -> UnexpectedTag(context, tag))
  )


let registry_spec_decoder : (registry_hash_value * registry_remote) ConfigDecoder.t =
  let open ConfigDecoder in
  get "hash_value" string >>= fun registry_hash_value ->
  get "remote" registry_remote_decoder >>= fun registry_remote ->
  succeed (registry_hash_value, registry_remote)


let config_decoder : t ConfigDecoder.t =
  let open ConfigDecoder in
  get "language" language_version_checker >>= fun () ->
  get "registries" (list registry_spec_decoder) >>= fun registries ->
  registries |> List.fold_left (fun res (registry_hash_value, registry_remote) ->
    res >>= fun map ->
    if map |> RegistryHashValueMap.mem registry_hash_value then
      failure (fun context -> DuplicateRegistryHashValue{ context; registry_hash_value })
    else
      succeed (map |> RegistryHashValueMap.add registry_hash_value registry_remote)
  ) (succeed RegistryHashValueMap.empty) >>= fun registries ->
  succeed { registries }


let load (abspath_config : abs_path) : (t, config_error) result =
  let open ResultMonad in
  let* s =
    read_file abspath_config
      |> Result.map_error (fun _ -> LibraryRootConfigNotFound(abspath_config))
  in
  ConfigDecoder.run config_decoder s
    |> Result.map_error (fun e -> LibraryRootConfigError(abspath_config, e))
