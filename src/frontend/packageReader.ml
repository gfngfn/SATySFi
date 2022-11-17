
open MyUtil
open Types
open ConfigError


type 'a ok = ('a, config_error) result


let listup_sources_in_directory (extensions : string list) (absdir_src : abs_path) : abs_path list =
  let filenames = Sys.readdir (get_abs_path_string absdir_src) |> Array.to_list in
  filenames |> List.filter_map (fun filename ->
    if extensions |> List.exists (fun suffix -> Core.String.is_suffix filename ~suffix) then
      Some(make_abs_path (Filename.concat (get_abs_path_string absdir_src) filename))
    else
      None
  )


let make_path_list_absolute ~(origin : abs_path) (reldirs : string list) : abs_path list =
  reldirs |> List.map (fun reldir ->
    make_abs_path (Filename.concat (get_abs_path_string origin) reldir)
  )


let main ~(use_test_files : bool) ~(extensions : string list) (absdir_package : abs_path) : (PackageConfig.t * untyped_package) ok =
  let open ResultMonad in
  let* config = PackageConfig.load absdir_package in
  let* package =
    match config.package_contents with
    | PackageConfig.Library{ main_module_name; source_directories; test_directories; _ } ->
        let absdirs_src = source_directories |> make_path_list_absolute ~origin:absdir_package in
        let abspaths_src = absdirs_src |> List.map (listup_sources_in_directory extensions) |> List.concat in
        let abspaths =
          if use_test_files then
            let absdirs_test = test_directories |> make_path_list_absolute ~origin:absdir_package in
            let abspaths_test = absdirs_test |> List.map (listup_sources_in_directory extensions) |> List.concat in
            List.append abspaths_src abspaths_test
          else
            abspaths_src
        in
        let* acc =
          abspaths |> foldM (fun acc abspath_src ->
            let* utsrc =
              Logging.begin_to_parse_file abspath_src;
              ParserInterface.process_file abspath_src |> Result.map_error (fun e -> FailedToParse(e))
            in
            match utsrc with
            | UTLibraryFile(utlib) ->
                return @@ Alist.extend acc (abspath_src, utlib)

            | UTDocumentFile(_) ->
                err @@ NotALibraryFile(abspath_src)

          ) Alist.empty
        in
        let modules = Alist.to_list acc in
        return @@ UTLibraryPackage{
          main_module_name;
          modules;
        }

    | PackageConfig.Font{ main_module_name; font_file_descriptions } ->
        let font_files =
          font_file_descriptions |> List.map (fun font_file_description ->
            let PackageConfig.{ font_file_path; font_file_contents; used_as_math_font } = font_file_description in
            let abspath = make_abs_path (Filename.concat (get_abs_path_string absdir_package) font_file_path) in
            {
              r_font_file_path     = abspath;
              r_font_file_contents = font_file_contents;
              r_used_as_math_font  = used_as_math_font;
            }
          )
        in
        return @@ UTFontPackage{
          main_module_name;
          font_files;
        }
  in
  return (config, package)
