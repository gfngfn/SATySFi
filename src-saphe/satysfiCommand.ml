
open MyUtil


type run_result = {
  exit_status : int;
  command     : string;
}


(* Escapes double quotes and backslashes. TODO: refine this *)
let escape_string =
  String.escaped


let escape_command_line (args : string list) : string =
  String.concat " " (args |> List.map (fun s -> Printf.sprintf "\"%s\"" (escape_string s)))


type build_option = {
    page_number_limit      : int;
    debug_show_bbox        : bool;
    debug_show_space       : bool;
    debug_show_block_bbox  : bool;
    debug_show_block_space : bool;
    debug_show_overfull    : bool;
    type_check_only        : bool;
    bytecomp               : bool;
}


let make_mode_args (text_mode_formats_str_opt : string option) =
  match text_mode_formats_str_opt with
  | None ->
      []

  | Some(text_mode_formats_str) ->
      [ "--text-mode"; text_mode_formats_str ]


let build_package
  ~envelope:(abspath_envelope_config : abs_path)
  ~deps:(abspath_deps_config : abs_path)
  ~mode:(text_mode_formats_str_opt : string option)
  ~options:(_ : build_option) : run_result
=
  let args_mandatory =
    [
      "satysfi"; "build"; "package";
      get_abs_path_string abspath_envelope_config;
      "--deps"; get_abs_path_string abspath_deps_config;
      "--full-path"; (* TODO: refine this *)
    ]
  in
  let args_mode = make_mode_args text_mode_formats_str_opt in
  let args = List.concat [ args_mandatory; args_mode ] in
  let command = escape_command_line args in
  let exit_status = Sys.command command in
  { exit_status; command }


let build_document
  ~doc:(abspath_doc : abs_path)
  ~out:(abspath_out : abs_path)
  ~dump:(abspath_dump : abs_path)
  ~deps:(abspath_deps_config : abs_path)
  ~mode:(text_mode_formats_str_opt : string option)
  ~options:(_ : build_option) : run_result
=
  let args_mandatory =
    [
      "satysfi"; "build"; "document";
      get_abs_path_string abspath_doc;
      "--output"; get_abs_path_string abspath_out;
      "--dump"; get_abs_path_string abspath_dump;
      "--deps"; get_abs_path_string abspath_deps_config;
      "--full-path"; (* TODO: refine this *)
    ]
  in
  let args_mode = make_mode_args text_mode_formats_str_opt in
  let args = List.concat [ args_mandatory; args_mode ] in
  let command = escape_command_line args in
  let exit_status = Sys.command command in
  { exit_status; command }


let test_package
  ~envelope:(abspath_envelope_config : abs_path)
  ~deps:(abspath_deps_config : abs_path)
  ~mode:(text_mode_formats_str_opt : string option) : run_result
=
  let args_mandatory =
    [
      "satysfi"; "test"; "package";
      get_abs_path_string abspath_envelope_config;
      "--deps"; get_abs_path_string abspath_deps_config;
      "--full-path"; (* TODO: refine this *)
    ]
  in
  let args_mode = make_mode_args text_mode_formats_str_opt in
  let args = List.concat [ args_mandatory; args_mode ] in
  let command = escape_command_line args in
  let exit_status = Sys.command command in
  { exit_status; command }
