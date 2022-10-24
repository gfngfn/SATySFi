
open MyUtil
open Types

type error =
  | PackageConfigNotFound of abs_path
  | PackageConfigError    of YamlDecoder.error
  | FailedToParse         of Range.t
  | NotALibraryFile       of abs_path

type 'a ok = ('a, error) result

type relative_path = string

type config =
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


let load_config (absdir_package : abs_path) : config ok =
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


let listup_sources_in_directory (extensions : string list) (absdir_src : abs_path) : abs_path list =
  let filenames = Sys.readdir (get_abs_path_string absdir_src) |> Array.to_list in
  filenames |> List.filter_map (fun filename ->
    if extensions |> List.exists (fun suffix -> Core.String.is_suffix filename ~suffix) then
      Some(make_abs_path (Filename.concat (get_abs_path_string absdir_src) filename))
    else
      None
  )


let main (absdir_package : abs_path) : package_info ok =
  let open ResultMonad in
  let* config = load_config absdir_package in
  let* package =
    match config with
    | Version_0_1 {
        main_module_name;
        source_directories;
        dependencies;
      } ->
        let absdirs_src =
          source_directories |> List.map (fun source_directory ->
            make_abs_path (Filename.concat (get_abs_path_string absdir_package) source_directory)
          )
        in
        let extensions = [ ".satyh"; ".satyg" ] in (* TODO: generalize this to the text mode *)
        let abspaths_src = absdirs_src |> List.map (listup_sources_in_directory extensions) |> List.concat in
        let* acc =
          abspaths_src |> foldM (fun acc abspath_src ->
            let* utsrc =
              ParserInterface.process_file abspath_src
                |> Result.map_error (fun rng -> FailedToParse(rng))
            in
            match utsrc with
            | UTLibraryFile(utlib) ->
                return @@ Alist.extend acc (abspath_src, utlib)

            | UTDocumentFile(_) ->
                err @@ NotALibraryFile(abspath_src)

          ) Alist.empty
        in
        let modules = Alist.to_list acc in
        return {
          main_module_name;
          modules;
          dependencies;
        }
  in
  return package
