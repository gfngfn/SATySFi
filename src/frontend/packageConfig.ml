
open MyUtil
open Types
open ConfigError
open ConfigUtil
open PackageSystemBase


type 'a ok = ('a, config_error) result

type relative_path = string

type package_contents =
  | Library of {
      main_module_name   : module_name;
      source_directories : relative_path list;
      dependencies       : package_dependency list;
    }

type t = {
  package_contents : package_contents;
}


let contents_decoder : package_contents ConfigDecoder.t =
  let open ConfigDecoder in
  branch "type" [
    "library" ==> begin
      get "main_module" string >>= fun main_module_name ->
      get "source_directories" (list string) >>= fun source_directories ->
      get_or_else "dependencies" (list dependency_decoder) [] >>= fun dependencies ->
      succeed @@ Library {
        main_module_name;
        source_directories;
        dependencies;
      }
    end;
  ]
  ~other:(fun tag ->
    failure (fun context -> UnexpectedTag(context, tag))
  )


let version_0_1_config_decoder : t ConfigDecoder.t =
  let open ConfigDecoder in
  get "contents" contents_decoder >>= fun package_contents ->
  succeed @@ {
    package_contents;
  }


let config_decoder =
  let open ConfigDecoder in
  get "language" string >>= fun language ->
  match language with
  | "0.1.0" -> version_0_1_config_decoder
  | _       -> failure (fun _context -> UnexpectedLanguage(language))


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
  ConfigDecoder.run config_decoder s
    |> Result.map_error (fun e -> PackageConfigError(abspath_config, e))
