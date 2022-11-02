
open MyUtil
open Types
open ConfigError


type 'a ok = ('a, config_error) result

type relative_path = string

type package_contents =
  | Library of {
      main_module_name   : module_name;
      source_directories : relative_path list;
    }
  | Document of {
      document_file : relative_path;
    }

type t = {
  package_contents : package_contents;
}


module PackageConfigDecoder = YamlDecoder.Make(YamlError)


let contents_decoder : package_contents PackageConfigDecoder.t =
  let open PackageConfigDecoder in
  branch "type" [
    "library" ==> begin
      get "main_module" string >>= fun main_module_name ->
      get "source_directories" (list string) >>= fun source_directories ->
      succeed @@ Library {
        main_module_name;
        source_directories;
      }
    end;
    "document" ==> begin
      get "file" string >>= fun document_file ->
      succeed @@ Document {
        document_file;
      }
    end;
  ]
  ~other:(fun tag ->
    failure (fun context -> UnexpectedTag(context, tag))
  )


let version_0_1_config_decoder : t PackageConfigDecoder.t =
  let open PackageConfigDecoder in
  get "contents" contents_decoder >>= fun package_contents ->
  succeed @@ {
    package_contents;
  }


let config_decoder =
  let open PackageConfigDecoder in
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
  PackageConfigDecoder.run config_decoder s
    |> Result.map_error (fun e -> PackageConfigError(abspath_config, e))
