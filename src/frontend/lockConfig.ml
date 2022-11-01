
open MyUtil
open Types
open ConfigError


type 'a ok = ('a, config_error) result

type t = {
  locked_packages : lock_info list;
}


module LockConfigDecoder = YamlDecoder.Make(YamlError)


let lock_location_decoder : abs_path LockConfigDecoder.t =
  let open LockConfigDecoder in
  branch "type" [
    "global" ==> begin
      get "path" string >>= fun s_libpath ->
      let libpath = make_lib_path s_libpath in
      match Config.resolve_lib_file libpath with
      | Ok(abspath)       -> succeed abspath
      | Error(candidates) -> failure (fun _context -> PackageNotFound(libpath, candidates))
    end;
  ]
  ~other:(fun tag ->
    failure (fun context -> UnexpectedTag(context, tag))
  )


let lock_decoder : lock_info LockConfigDecoder.t =
  let open LockConfigDecoder in
  get "name" string >>= fun lock_name ->
  get "location" lock_location_decoder >>= fun lock_directory ->
  get_or_else "dependencies" (list string) [] >>= fun lock_dependencies ->
  succeed {
    lock_name;
    lock_directory;
    lock_dependencies;
  }


let lock_config_decoder : t LockConfigDecoder.t =
  let open LockConfigDecoder in
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
  LockConfigDecoder.run lock_config_decoder s
    |> Result.map_error (fun e -> LockConfigError(abspath_lock_config, e))
