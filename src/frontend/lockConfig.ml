
open MyUtil
open Types
open ConfigError


type 'a ok = ('a, config_error) result

type lock_location =
  | GlobalLocation of {
      path : string;
    }
  | LocalLocation of {
      path : string;
    }

type locked_package = {
  lock_name         : lock_name;
  lock_location     : lock_location;
  lock_dependencies : lock_name list;
}

type t = {
  locked_packages : locked_package list;
}


module LockConfigDecoder = YamlDecoder.Make(YamlError)


let lock_location_decoder : lock_location LockConfigDecoder.t =
  let open LockConfigDecoder in
  branch "type" [
    "global" ==> begin
      get "path" string >>= fun s_libpath ->
      succeed @@ GlobalLocation{ path = s_libpath }
    end;
    "local" ==> begin
      get "path" string >>= fun s_relpath ->
      succeed @@ LocalLocation{ path = s_relpath }
    end;
  ]
  ~other:(fun tag ->
    failure (fun context -> UnexpectedTag(context, tag))
  )


let lock_decoder : locked_package LockConfigDecoder.t =
  let open LockConfigDecoder in
  get "name" string >>= fun lock_name ->
  get "location" lock_location_decoder >>= fun lock_location ->
  get_or_else "dependencies" (list string) [] >>= fun lock_dependencies ->
  succeed {
    lock_name;
    lock_location;
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


let write (_abspath_lock_config : abs_path) (_lock_config : t) : unit =
  failwith "TODO: LockConfig.write"
