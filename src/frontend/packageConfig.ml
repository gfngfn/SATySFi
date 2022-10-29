
open MyUtil
open Types
open ConfigError


type 'a ok = ('a, config_error) result

type relative_path = string

type dependency_spec = {
  depended_package_name : string;
  version_constraints   : unit; (* TODO: define this *)
}

type package_contents =
  | Library of {
      main_module_name   : module_name;
      source_directories : relative_path list;
      dependencies       : dependency_spec list;
    }
  | Document of {
      document_file : relative_path;
      dependencies  : dependency_spec list;
    }

type t = {
  package_name     : string;
  package_version  : string;
  package_contents : package_contents;
}


let dependency_decoder : dependency_spec YamlDecoder.t =
  let open YamlDecoder in
  get "package_name" string >>= fun depended_package_name ->
  succeed {
    depended_package_name;
    version_constraints = ();
  }


let contents_decoder : package_contents YamlDecoder.t =
  let open YamlDecoder in
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
    "document" ==> begin
      get "file" string >>= fun document_file ->
      get_or_else "dependencies" (list dependency_decoder) [] >>= fun dependencies ->
      succeed @@ Document {
        document_file;
        dependencies;
      }
    end;
  ]
  ~on_error:(fun other ->
    Printf.sprintf "unsupported type '%s' for specifying package contents" other
  )


let config_decoder : t YamlDecoder.t =
  let open YamlDecoder in
  get "package_name" string >>= fun package_name ->
  get "version" string >>= fun package_version ->
  get "contents" contents_decoder >>= fun package_contents ->
  succeed @@ {
    package_name;
    package_version;
    package_contents;
  }


let config_decoder =
  let open YamlDecoder in
  get "language" string >>= fun language ->
  match language with
  | "0.1.0" -> config_decoder
  | _       -> failure (Printf.sprintf "unknown language version '%s'" language)


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
  YamlDecoder.run config_decoder s |> Result.map_error (fun e -> PackageConfigError(abspath_config, e))
