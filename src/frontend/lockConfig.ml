
open MyUtil
open Types
open ConfigError


type 'a ok = ('a, config_error) result

type t = {
  locked_packages : lock_info list;
}


let lock_location_decoder : abs_path YamlDecoder.t =
  let open YamlDecoder in
  branch "type" [
    "global" ==> begin
      get "path" string >>= fun s_libpath ->
      match Config.resolve_lib_file (make_lib_path s_libpath) with
      | Ok(abspath) -> succeed abspath
      | Error(_e)   -> failure (Printf.sprintf "locked package not found at '%s'" s_libpath)
    end;
  ]
  ~on_error:(fun other ->
    Printf.sprintf "unknown type '%s' for lock locations" other
  )


let lock_decoder : lock_info YamlDecoder.t =
  let open YamlDecoder in
  get "name" string >>= fun lock_name ->
  get "location" lock_location_decoder >>= fun lock_directory ->
  get_or_else "dependencies" (list string) [] >>= fun lock_dependencies ->
  succeed {
    lock_name;
    lock_directory;
    lock_dependencies;
  }


let lock_config_decoder : t YamlDecoder.t =
  let open YamlDecoder in
  get_or_else "locks" (list lock_decoder) [] >>= fun locked_packages ->
  succeed {
    locked_packages;
  }


let load (abspath_lock_config : abs_path) : t ok =
  let open ResultMonad in
  let* inc =
    try
      return (open_in_abs abspath_lock_config)
    with
    | Sys_error(_) -> err (LockConfigNotFound(abspath_lock_config))
  in
  let s = Core.In_channel.input_all inc in
  close_in inc;
  YamlDecoder.run lock_config_decoder s |> Result.map_error (fun e -> LockConfigError(abspath_lock_config, e))
