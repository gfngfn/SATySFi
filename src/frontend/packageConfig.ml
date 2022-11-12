
open MyUtil
open Types
open ConfigError
open ConfigUtil
open PackageSystemBase


type 'a ok = ('a, config_error) result

type relative_path = string

type font_file_description = {
  font_file_path     : relative_path;
  font_file_contents : font_file_contents;
  used_as_math_font  : bool;
}

type package_contents =
  | Library of {
      main_module_name   : module_name;
      source_directories : relative_path list;
      dependencies       : package_dependency list;
    }
  | Font of {
      main_module_name       : module_name;
      font_file_descriptions : font_file_description list;
    }

type t = {
  package_contents : package_contents;
}


let font_file_contents_decoder : font_file_contents ConfigDecoder.t =
  let open ConfigDecoder in
  branch "type" [
    "opentype_single" ==> begin
      get "name" string >>= fun name ->
      succeed @@ OpentypeSingle(name)
    end;
    "opentype_collection" ==> begin
      get "names" (list string) >>= fun names ->
      succeed @@ OpentypeCollection(names)
    end;
  ]
  ~other:(fun tag ->
    failure (fun context -> UnexpectedTag(context, tag))
  )


let font_file_description_decoder : font_file_description ConfigDecoder.t =
  let open ConfigDecoder in
  get "path" string >>= fun font_file_path ->
  get_or_else "math" bool false >>= fun used_as_math_font ->
  get "contents" font_file_contents_decoder >>= fun font_file_contents ->
  succeed @@ {
    font_file_path;
    font_file_contents;
    used_as_math_font;
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
    "font" ==> begin
      get "main_module" string >>= fun main_module_name ->
      get "elements" (list font_file_description_decoder) >>= fun font_file_descriptions ->
      succeed @@ Font {
        main_module_name;
        font_file_descriptions;
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
  let* s =
    read_file abspath_config
      |> Result.map_error (fun _ -> PackageConfigNotFound(abspath_config))
  in
  ConfigDecoder.run config_decoder s
    |> Result.map_error (fun e -> PackageConfigError(abspath_config, e))
