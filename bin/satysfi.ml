
let version =
  "SATySFi version 0.1.0 alpha"


let build
  fpath_in_opt
  fpath_out_opt
  config_paths_str_opt
  text_mode_formats_str_opt
  markdown_style_str_opt
  page_number_limit
  show_full_path
  debug_show_bbox
  debug_show_space
  debug_show_block_bbox
  debug_show_block_space
  debug_show_overfull
  type_check_only
  bytecomp
  show_fonts
  no_default_config
=
  Main.build
    ~fpath_in_opt
    ~fpath_out_opt
    ~config_paths_str_opt
    ~text_mode_formats_str_opt
    ~markdown_style_str_opt
    ~page_number_limit
    ~show_full_path
    ~debug_show_bbox
    ~debug_show_space
    ~debug_show_block_bbox
    ~debug_show_block_space
    ~debug_show_overfull
    ~type_check_only
    ~bytecomp
    ~show_fonts
    ~no_default_config


let arg_in : (string option) Cmdliner.Term.t =
  let open Cmdliner in
  Arg.(value (pos 0 (some file) None (info [])))


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


let flag_markdown =
  let open Cmdliner in
  let doc = "Pass Markdown source as input" in
  Arg.(value (opt (some string) None (info [ "markdown" ] ~docv:"STYLES" ~doc)))


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


let flag_show_fonts =
  make_boolean_flag_spec
    ~flags:[ "show-fonts" ]
    ~doc:"Displays all the available fonts"


let flag_no_default_config =
  make_boolean_flag_spec
    ~flags:[ "no-default-config" ]
    ~doc:"Does not use default configuration search path"


let command_main : unit Cmdliner.Cmd.t =
  let open Cmdliner in
  let term : unit Term.t =
    Term.(const build
      $ arg_in
      $ flag_output
      $ flag_config
      $ flag_text_mode
      $ flag_markdown
      $ flag_page_number_limit
      $ flag_full_path
      $ flag_debug_show_bbox
      $ flag_debug_show_space
      $ flag_debug_show_block_bbox
      $ flag_debug_show_block_space
      $ flag_debug_show_overfull
      $ flag_type_check_only
      $ flag_bytecomp
      $ flag_show_fonts
      $ flag_no_default_config
    )
  in
  let info : Cmd.info =
    Cmd.info ~version:version "satysfi"
  in
  Cmd.v info term

let () =
  let open Cmdliner in
  exit (Cmd.eval command_main)
