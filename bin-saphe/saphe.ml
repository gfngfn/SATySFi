
let solve
  fpath_in
  show_full_path
  config_paths_str_opt
  no_default_config
=
  Main.solve
    ~fpath_in
    ~show_full_path
    ~config_paths_str_opt
    ~no_default_config


let arg_in : string Cmdliner.Term.t =
  let open Cmdliner in
  Arg.(required (pos 0 (some file) None (info [])))


let flag_output : (string option) Cmdliner.Term.t =
  let open Cmdliner in
  let doc = "Specify output path." in
  Arg.(value (opt (some string) None (info [ "o"; "output" ] ~docv:"OUTPUT" ~doc)))


let flag_config =
  let open Cmdliner in
  let doc = "Add colon-separated paths to configuration search path" in
  Arg.(value (opt (some string) None (info [ "C"; "config" ] ~docv:"PATHS" ~doc)))


let flag_text_mode =
  let open Cmdliner in
  let doc = "Set text mode" in
  Arg.(value (opt (some string) None (info [ "text-mode" ] ~docv:"FORMATS" ~doc)))


let flag_page_number_limit : int Cmdliner.Term.t =
  let open Cmdliner in
  let doc = "Set the page number limit (default: 10000)" in
  Arg.(value (opt int 10000 (info [ "page-number-limit" ] ~docv:"INT" ~doc)))


let make_boolean_flag_spec ~(flags : string list) ~(doc : string) : bool Cmdliner.Term.t =
  let open Cmdliner in
  Arg.(value (flag (info flags ~doc)))


let flag_full_path =
  make_boolean_flag_spec
    ~flags:[ "full-path" ]
    ~doc:"Displays paths in full-path style"


let flag_debug_show_bbox =
  make_boolean_flag_spec
    ~flags:[ "debug-show-bbox" ]
    ~doc:"Outputs bounding boxes for glyphs"


let flag_debug_show_space =
  make_boolean_flag_spec
    ~flags:[ "debug-show-space" ]
    ~doc:"Outputs boxes for spaces"


let flag_debug_show_block_bbox =
  make_boolean_flag_spec
    ~flags:[ "debug-show-block-bbox" ]
    ~doc:"Outputs bounding boxes for blocks"


let flag_debug_show_block_space =
  make_boolean_flag_spec
    ~flags:[ "debug-show-block-space" ]
    ~doc:"Outputs visualized block spaces"


let flag_debug_show_overfull =
  make_boolean_flag_spec
    ~flags:[ "debug-show-overfull" ]
    ~doc:"Outputs visualized overfull or underfull lines"


let flag_type_check_only =
  make_boolean_flag_spec
    ~flags:[ "t"; "type-check-only" ]
    ~doc:"Stops after type checking"


let flag_bytecomp =
  make_boolean_flag_spec
    ~flags:[ "b"; "bytecomp" ]
    ~doc:"Use bytecode compiler"


let flag_no_default_config =
  make_boolean_flag_spec
    ~flags:[ "no-default-config" ]
    ~doc:"Does not use default configuration search path"


let command_solve =
  let open Cmdliner in
  let term : unit Term.t =
    Term.(const solve
      $ arg_in
      $ flag_full_path
      $ flag_config
      $ flag_no_default_config
    )
  in
  let info : Cmd.info =
    Cmd.info "solve"
  in
  Cmd.v info term


let () =
  let open Cmdliner in
  let term : unit Term.t =
    Term.(ret (const (`Error(true, "No subcommand specified."))))
  in
  let info : Cmd.info =
    Cmd.info ~version:Main.version "saphe"
  in
  let subcommands =
    [
      command_solve;
    ]
  in
  Stdlib.exit (Cmd.eval (Cmd.group ~default:term info subcommands))
