
open LoggingUtil


let init_document fpath_in =
  SapheMain.init_document ~fpath_in


let init_library fpath_in =
  SapheMain.init_library ~fpath_in


let cache_list () =
  SapheMain.cache_list ()


let update fpath_in =
  SapheMain.update ~fpath_in


let solve
  fpath_in
  show_full_path
  verbosity
=
  SapheMain.solve
    ~fpath_in
    ~show_full_path
    ~verbosity


let build
  fpath_in
  fpath_out_opt
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
  SapheMain.build
    ~fpath_in
    ~fpath_out_opt
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


let test
  fpath_in
  text_mode_formats_str_opt
  show_full_path
  verbosity
=
  SapheMain.test
    ~fpath_in
    ~text_mode_formats_str_opt
    ~show_full_path
    ~verbosity


let arg_in : string Cmdliner.Term.t =
  let open Cmdliner in
  Arg.(required (pos 0 (some file) None (info [])))


let arg_in_to_create : string Cmdliner.Term.t =
  let open Cmdliner in
  Arg.(required (pos 0 (some string) None (info [])))


let flag_output : (string option) Cmdliner.Term.t =
  let open Cmdliner in
  let doc = "Specify output path." in
  Arg.(value (opt (some string) None (info [ "o"; "output" ] ~docv:"OUTPUT" ~doc)))


let flag_text_mode =
  let open Cmdliner in
  let doc = "Set text mode" in
  Arg.(value (opt (some string) None (info [ "text-mode" ] ~docv:"FORMATS" ~doc)))


let flag_page_number_limit : int Cmdliner.Term.t =
  let open Cmdliner in
  let doc = "Set the page number limit (default: 10000)" in
  Arg.(value (opt int 10000 (info [ "page-number-limit" ] ~docv:"INT" ~doc)))


let flag_max_repeats : int Cmdliner.Term.t =
  let open Cmdliner in
  let doc = "Set the maximum number of iterations (default: 4)" in
  Arg.(value (opt int 4 (info [ "max-repeats" ] ~docv:"INT" ~doc)))


let make_boolean_flag_spec ~(flags : string list) ~(doc : string) : bool Cmdliner.Term.t =
  let open Cmdliner in
  Arg.(value (flag (info flags ~doc)))


let flag_full_path =
  make_boolean_flag_spec
    ~flags:[ "full-path" ]
    ~doc:"Displays paths in full-path style"


let flag_verbosity : verbosity Cmdliner.Term.t =
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


let command_init_document : unit Cmdliner.Cmd.t =
  let open Cmdliner in
  Cmd.v (Cmd.info "document")
    Term.(const init_document $ arg_in_to_create)


let command_init_library : unit Cmdliner.Cmd.t =
  let open Cmdliner in
  Cmd.v (Cmd.info "library")
    Term.(const init_library $ arg_in_to_create)


let command_init : unit Cmdliner.Cmd.t =
  let open Cmdliner in
  Cmd.group (Cmd.info "init") [
    command_init_document;
    command_init_library;
  ]


let command_update : unit Cmdliner.Cmd.t =
  let open Cmdliner in
  Cmd.v (Cmd.info "update")
    Term.(const update $ arg_in)


let command_solve : unit Cmdliner.Cmd.t =
  let open Cmdliner in
  Cmd.v (Cmd.info "solve")
    Term.(const solve
      $ arg_in
      $ flag_full_path
      $ flag_verbosity
    )


let command_build : unit Cmdliner.Cmd.t =
  let open Cmdliner in
  Cmd.v (Cmd.info "build")
    Term.(const build
      $ arg_in
      $ flag_output
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


let command_test : unit Cmdliner.Cmd.t =
  let open Cmdliner in
  Cmd.v (Cmd.info "test")
    Term.(const test
      $ arg_in
      $ flag_text_mode
      $ flag_full_path
      $ flag_verbosity
    )


let command_cache_list : unit Cmdliner.Cmd.t =
  let open Cmdliner in
  Cmd.v (Cmd.info "list")
    Term.(const cache_list $ const ())


let command_cache : unit Cmdliner.Cmd.t =
  let open Cmdliner in
  Cmd.group (Cmd.info "cache") [
    command_cache_list;
  ]


let () =
  let open Cmdliner in
  let info : Cmd.info =
    Cmd.info ~version:SapheMain.version "saphe"
  in
  let subcommands =
    [
      command_init;
      command_update;
      command_solve;
      command_build;
      command_test;
      command_cache;
    ]
  in
  Stdlib.exit (Cmd.eval (Cmd.group info subcommands))
