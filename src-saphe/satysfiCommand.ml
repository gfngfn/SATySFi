
open MyUtil
open LoggingUtil


type run_result = {
  exit_status : int;
  command     : string;
}


(* Escapes double quotes and backslashes. TODO: refine this *)
let escape_string =
  String.escaped


let escape_command_line (args : string list) : string =
  String.concat " " (args |> List.map (fun s -> Printf.sprintf "\"%s\"" (escape_string s)))


let make_mode_args (text_mode_formats_str_opt : string option) =
  match text_mode_formats_str_opt with
  | None                        -> []
  | Some(text_mode_formats_str) -> [ "--text-mode"; text_mode_formats_str ]


let make_verbosity_args (verbosity : verbosity) =
  match verbosity with
  | Verbose         -> [ "--verbose" ]
  | NormalVerbosity -> []
  | Quiet           -> [ "--quiet" ]


type package_build_option = PackageBuildOption of {
  show_full_path : bool;
  verbosity      : verbosity;
}


let make_package_build_option_args (PackageBuildOption(options) : package_build_option) =
  List.concat [
    if options.show_full_path then [ "--full-path" ] else [];
    make_verbosity_args options.verbosity;
  ]


let build_package
  ~envelope:(abspath_envelope_config : abs_path)
  ~deps:(abspath_deps_config : abs_path)
  ~mode:(text_mode_formats_str_opt : string option)
  ~(options : package_build_option) : run_result
=
  let args_mandatory =
    [
      "satysfi"; "build"; "package";
      AbsPath.to_string abspath_envelope_config;
      "--deps"; AbsPath.to_string abspath_deps_config;
    ]
  in
  let args_mode = make_mode_args text_mode_formats_str_opt in
  let args_option = make_package_build_option_args options in
  let args = List.concat [ args_mandatory; args_mode; args_option ] in
  let command = escape_command_line args in
  let exit_status = Sys.command command in
  { exit_status; command }


type document_build_option = DocumentBuildOption of {
  show_full_path         : bool;
  verbosity              : verbosity;
  page_number_limit      : int;
  debug_show_bbox        : bool;
  debug_show_space       : bool;
  debug_show_block_bbox  : bool;
  debug_show_block_space : bool;
  debug_show_overfull    : bool;
  type_check_only        : bool;
  bytecomp               : bool;
}


let make_document_build_option_args (DocumentBuildOption(options) : document_build_option) =
  let flag b s = if b then [ s ] else [] in
  List.concat [
    flag options.show_full_path         "--full-path";
    make_verbosity_args options.verbosity;
    flag options.debug_show_bbox        "--debug-show-bbox";
    flag options.debug_show_space       "--debug-show-space";
    flag options.debug_show_block_bbox  "--debug-show-block-bbox";
    flag options.debug_show_block_space "--debug-show-block-space";
    flag options.debug_show_overfull    "--debug-show-overfull";
    flag options.type_check_only        "--type-check-only";
    flag options.bytecomp               "--bytecomp"; (* TODO: consider moving this to config *)
  ]


let build_document
  ~doc:(abspath_doc : abs_path)
  ~out:(abspath_out : abs_path)
  ~dump:(abspath_dump : abs_path)
  ~deps:(abspath_deps_config : abs_path)
  ~mode:(text_mode_formats_str_opt : string option)
  ~(options : document_build_option) : run_result
=
  let args_mandatory =
    [
      "satysfi"; "build"; "document";
      AbsPath.to_string abspath_doc;
      "--output"; AbsPath.to_string abspath_out;
      "--dump"; AbsPath.to_string abspath_dump;
      "--deps"; AbsPath.to_string abspath_deps_config;
    ]
  in
  let args_mode = make_mode_args text_mode_formats_str_opt in
  let args_option = make_document_build_option_args options in
  let args = List.concat [ args_mandatory; args_mode; args_option ] in
  let command = escape_command_line args in
  let exit_status = Sys.command command in
  { exit_status; command }


let test_package
  ~envelope:(abspath_envelope_config : abs_path)
  ~deps:(abspath_deps_config : abs_path)
  ~mode:(text_mode_formats_str_opt : string option)
  ~(options : package_build_option) : run_result
=
  let args_mandatory =
    [
      "satysfi"; "test"; "package";
      AbsPath.to_string abspath_envelope_config;
      "--deps"; AbsPath.to_string abspath_deps_config;
      "--full-path"; (* TODO: refine this *)
    ]
  in
  let args_mode = make_mode_args text_mode_formats_str_opt in
  let args_option = make_package_build_option_args options in
  let args = List.concat [ args_mandatory; args_mode; args_option ] in
  let command = escape_command_line args in
  let exit_status = Sys.command command in
  { exit_status; command }
