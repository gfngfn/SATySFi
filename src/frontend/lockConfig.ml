
open MyUtil
open Types
open ConfigError


type 'a ok = ('a, config_error) result

type t = {
  locked_packages : lock_info list;
}


module LockConfigDecoder = YamlDecoder.Make(YamlError)


let lock_location_decoder ~(lock_config_dir : abs_path) : abs_path LockConfigDecoder.t =
  let open LockConfigDecoder in
  branch "type" [
    "global" ==> begin
      get "path" string >>= fun s_libpath ->
      let libpath = make_lib_path s_libpath in
      match Config.resolve_lib_file libpath with
      | Ok(abspath)       -> succeed abspath
      | Error(candidates) -> failure (fun _context -> PackageNotFound(libpath, candidates))
    end;
    "local" ==> begin
      get "path" string >>= fun s_relpath ->
      let abspath =
        make_abs_path (Filename.concat (get_abs_path_string lock_config_dir) s_relpath)
      in
      succeed abspath
    end;
  ]
  ~other:(fun tag ->
    failure (fun context -> UnexpectedTag(context, tag))
  )


let lock_decoder ~(lock_config_dir : abs_path) : lock_info LockConfigDecoder.t =
  let open LockConfigDecoder in
  get "name" string >>= fun lock_name ->
  get "location" (lock_location_decoder ~lock_config_dir) >>= fun lock_directory ->
  get_or_else "dependencies" (list string) [] >>= fun lock_dependencies ->
  succeed {
    lock_name;
    lock_directory;
    lock_dependencies;
  }


let lock_config_decoder ~(lock_config_dir : abs_path) : t LockConfigDecoder.t =
  let open LockConfigDecoder in
  get_or_else "locks" (list (lock_decoder ~lock_config_dir)) [] >>= fun locked_packages ->
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
  let lock_config_dir = make_abs_path (Filename.dirname (get_abs_path_string abspath_lock_config)) in
  LockConfigDecoder.run (lock_config_decoder ~lock_config_dir) s
    |> Result.map_error (fun e -> LockConfigError(abspath_lock_config, e))


let write (_abspath_lock_config : abs_path) (_lock_config : t) : unit =
  failwith "TODO: LockConfig.write"
