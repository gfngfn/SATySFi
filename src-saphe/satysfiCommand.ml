
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
    text_mode              : string option;
    page_number_limit      : int;
    show_full_path         : bool;
    debug_show_bbox        : bool;
    debug_show_space       : bool;
    debug_show_block_bbox  : bool;
    debug_show_block_space : bool;
    debug_show_overfull    : bool;
    type_check_only        : bool;
    bytecomp               : bool;
}


let build_package
  ~envelope:(_abspath_envelope_config : abs_path)
  ~deps:(_abspath_deps_config : abs_path)
  ~options:(_ : build_option) : run_result
=
  failwith "TODO: SatysfiCommand.build_package"


let build_document
  ~doc:(abspath_doc : abs_path)
  ~out:(abspath_out : abs_path)
  ~dump:(abspath_dump : abs_path)
  ~deps:(abspath_deps_config : abs_path)
  ~options:(_ : build_option) : run_result
=
  let args_mandatory =
    [
      "satysfi"; "build"; "document";
      get_abs_path_string abspath_doc;
      "--output"; get_abs_path_string abspath_out;
      "--dump"; get_abs_path_string abspath_dump;
      "--deps"; get_abs_path_string abspath_deps_config;
    ]
  in
  let args = args_mandatory in
  let command = escape_command_line args in
  let exit_status = Sys.command command in
  { exit_status; command }
