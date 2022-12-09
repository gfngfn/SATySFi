
open MyUtil
open ConfigError
open ConfigUtil
open PackageSystemBase


type 'a ok = ('a, config_error) result


let source_decoder : implementation_source ConfigDecoder.t =
  let open ConfigDecoder in
  branch "type" [
    "tar_gzip" ==> begin
      get "url" string >>= fun url ->
      succeed @@ TarGzip{ url }
    end;
  ]
  ~other:(fun tag ->
    failure (fun context -> UnexpectedTag(context, tag))
  )


let implementation_decoder : implementation_record ConfigDecoder.t =
  let open ConfigDecoder in
  get "version" version_decoder >>= fun version ->
  get_or_else "source" source_decoder NoSource >>= fun source ->
  get "dependencies" (list dependency_decoder) >>= fun dependencies ->
  succeed @@ ImplRecord{ version; source; dependencies }


let package_decoder : (package_name * implementation_record list) ConfigDecoder.t =
  let open ConfigDecoder in
  get "name" package_name_decoder >>= fun package_name ->
  get "implementations" (list implementation_decoder) >>= fun impls ->
  succeed (package_name, impls)


let registry_config_decoder : package_context ConfigDecoder.t =
  let open ConfigDecoder in
  get "packages" (list package_decoder) >>= fun packages ->
  packages |> List.fold_left (fun res (package_name, impls) ->
    res >>= fun map ->
    if map |> PackageNameMap.mem package_name then
      failure (fun yctx -> MultiplePackageDefinition{ context = yctx; package_name })
    else
      succeed (map |> PackageNameMap.add package_name impls)
  ) (succeed PackageNameMap.empty) >>= fun registry_contents ->
  succeed { registry_contents }


let load (abspath_registry_config : abs_path) : package_context ok =
  let open ResultMonad in
  let* s =
    read_file abspath_registry_config
      |> Result.map_error (fun _ -> RegistryConfigNotFound(abspath_registry_config))
  in
  ConfigDecoder.run registry_config_decoder s
    |> Result.map_error (fun e -> RegistryConfigError(abspath_registry_config, e))
