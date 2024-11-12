
open LoggingUtil


let build_package
  fpath_in
  fpath_deps
  text_mode_formats_str_opt
  show_full_path
  verbosity
=
  Main.build_package
    ~fpath_in
    ~fpath_deps
    ~text_mode_formats_str_opt
    ~show_full_path
    ~verbosity


let build_document
  fpath_in
  fpath_out
  fpath_dump
  fpath_deps
  text_mode_formats_str_opt
  page_number_limit
  max_repeats
  show_full_path
  verbosity
  debug_show_bbox
  debug_show_space
  debug_show_block_bbox
  debug_show_block_space
  debug_show_overfull
  type_check_only
  bytecomp
=
  Main.build_document
    ~fpath_in
    ~fpath_out
    ~fpath_dump
    ~fpath_deps
    ~text_mode_formats_str_opt
    ~page_number_limit
    ~max_repeats
    ~show_full_path
    ~verbosity
    ~debug_show_bbox
    ~debug_show_space
    ~debug_show_block_bbox
    ~debug_show_block_space
    ~debug_show_overfull
    ~type_check_only
    ~bytecomp


let test_package
  fpath_in
  fpath_deps
  text_mode_formats_str_opt
  show_full_path
  verbosity
=
  Main.test_package
    ~fpath_in
    ~fpath_deps
    ~text_mode_formats_str_opt
    ~show_full_path
    ~verbosity


let arg_in : string Cmdliner.Term.t =
  let open Cmdliner in
  Arg.(required (pos 0 (some file) None (info [])))


let flag_output : string Cmdliner.Term.t =
  let open Cmdliner in
  let doc = "Specify an output path" in
  Arg.(required (opt (some string) None (info [ "o"; "output" ] ~docv:"OUTPUT" ~doc)))


let flag_dump : string Cmdliner.Term.t =
  let open Cmdliner in
  let doc = "Specify a dump file path" in
  Arg.(required (opt (some string) None (info [ "dump" ] ~docv:"DUMP" ~doc)))


let flag_deps : string Cmdliner.Term.t =
  let open Cmdliner in
  let doc = "Specify a deps config path" in
  Arg.(required (opt (some string) None (info [ "deps" ] ~docv:"DEPS" ~doc)))


let flag_text_mode =
  let open Cmdliner in
  let doc = "Set the text-generating mode" in
  Arg.(value (opt (some string) None (info [ "text-mode" ] ~docv:"FORMATS" ~doc)))


let flag_page_number_limit : int Cmdliner.Term.t =
  let open Cmdliner in
  let doc = "Set the page number limit (default: 10000)" in
  Arg.(value (opt int 10000 (info [ "page-number-limit" ] ~docv:"INT" ~doc)))


let flag_max_repeats : int Cmdliner.Term.t =
  let open Cmdliner in
  let doc = "Set the limit of iteration for resolving cross-references (default: 4)" in
  Arg.(value (opt int 4 (info [ "max-repeats" ] ~docv:"INT" ~doc)))


let make_boolean_flag_spec ~(flags : string list) ~(doc : string) : bool Cmdliner.Term.t =
  let open Cmdliner in
  Arg.(value (flag (info flags ~doc)))


let flag_full_path =
  make_boolean_flag_spec
    ~flags:[ "full-path" ]
    ~doc:"Displays paths in full-path style"


let flag_verbosity =
  let open Cmdliner in
  let verbose = (Verbose, Arg.info [ "verbose" ] ~doc:"Make logs verbose") in
  let quiet = (Quiet, Arg.info [ "quiet" ] ~doc:"Disable logs other than errors and warnings") in
  Arg.(value (vflag NormalVerbosity [ verbose; quiet ]))


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


let command_build_document =
  let open Cmdliner in
  Cmd.v (Cmd.info "document")
    Term.(const build_document
      $ arg_in
      $ flag_output
      $ flag_dump
      $ flag_deps
      $ flag_text_mode
      $ flag_page_number_limit
      $ flag_max_repeats
      $ flag_full_path
      $ flag_verbosity
      $ flag_debug_show_bbox
      $ flag_debug_show_space
      $ flag_debug_show_block_bbox
      $ flag_debug_show_block_space
      $ flag_debug_show_overfull
      $ flag_type_check_only
      $ flag_bytecomp
    )


let command_build_package =
  let open Cmdliner in
  Cmd.v (Cmd.info "package")
    Term.(const build_package
      $ arg_in
      $ flag_deps
      $ flag_text_mode
      $ flag_full_path
      $ flag_verbosity
    )


let command_build =
  let open Cmdliner in
  Cmd.group (Cmd.info "build") [
    command_build_package;
    command_build_document;
  ]


let command_test_package =
  let open Cmdliner in
  Cmd.v (Cmd.info "package")
    Term.(const test_package
      $ arg_in
      $ flag_deps
      $ flag_text_mode
      $ flag_full_path
      $ flag_verbosity
    )


let command_test =
  let open Cmdliner in
  Cmd.group (Cmd.info "test") [
    command_test_package;
  ]


let () =
  let open Cmdliner in
  let info : Cmd.info =
    Cmd.info ~version:Main.version "satysfi"
  in
  let subcommands =
    [
      command_build;
      command_test;
    ]
  in
  Stdlib.exit (Cmd.eval (Cmd.group info subcommands))
