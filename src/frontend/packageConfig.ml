
open MyUtil
open Types
open ConfigError


type 'a ok = ('a, config_error) result

type relative_path = string

type t =
  | Version_0_1 of {
      main_module_name   : module_name;
      source_directories : relative_path list;
      dependencies       : module_name list;
    }


let config_version_0_1_decoder =
  let open YamlDecoder in
  get "main_module" string >>= fun main_module_name ->
  get "source_directories" (list string) >>= fun source_directories ->
  get_or_else "dependencies" (list string) [] >>= fun dependencies ->
  succeed @@ Version_0_1 {
    main_module_name;
    source_directories;
    dependencies;
  }


let config_decoder =
  let open YamlDecoder in
  get "language" string >>= fun language ->
  match language with
  | "v0.1.0" -> config_version_0_1_decoder
  | _        -> failure (Printf.sprintf "unknown language version '%s'" language)


let load (absdir_package : abs_path) : t ok =
  let open ResultMonad in
  let abspath_config =
    make_abs_path (Filename.concat (get_abs_path_string absdir_package) "satysfi.yaml")
  in
  let* inc =
    try
      return (open_in_abs abspath_config)
    with
    | Sys_error(_) -> err (PackageConfigNotFound(abspath_config))
  in
  let s = Core.In_channel.input_all inc in
  close_in inc;
  YamlDecoder.run config_decoder s |> Result.map_error (fun e -> PackageConfigError(e))
