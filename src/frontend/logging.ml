
open MyUtil


type path_display_setting =
  | FullPath
  | RelativeToCwd of abs_path

type config = {
  path_display_setting : path_display_setting;
  verbose              : bool;
}


let show_path (config : config) (abspath : abs_path) =
  match config.path_display_setting with
  | FullPath                   -> AbsPath.to_string abspath
  | RelativeToCwd(abspath_cwd) -> AbsPath.make_relative ~from:abspath_cwd abspath


let begin_to_typecheck_file (config : config) (abspath_in : abs_path) =
  if config.verbose then begin
    print_endline " ---- ---- ---- ----";
    Printf.printf "  type checking '%s' ...\n"
      (show_path config abspath_in)
  end


let begin_to_preprocess_file (config : config) (abspath_in : abs_path) =
  if config.verbose then begin
    Printf.printf "  preprocessing '%s' ...\n"
      (show_path config abspath_in)
  end


let begin_to_eval_file (config : config) (abspath_in : abs_path) =
  if config.verbose then begin
    Printf.printf "  evaluating '%s' ...\n"
      (show_path config abspath_in)
  end


let begin_to_parse_file (config : config) (abspath_in : abs_path) =
  if config.verbose then begin
    Printf.printf "  parsing '%s' ...\n"
      (show_path config abspath_in)
  end


let pass_type_check (config : config) (opt : string option) =
  if config.verbose then
    match opt with
    | None      -> print_endline "  type check passed."
    | Some(str) -> Printf.printf "  type check passed. (%s)\n" str


let ordinal i =
  let suffix =
    match i mod 10 with
    | 1 -> "st"
    | 2 -> "nd"
    | 3 -> "rd"
    | _ -> "th"
  in
  Printf.sprintf "%d%s" i suffix


let start_evaluation (_config : config) (i : int) =
  print_endline " ---- ---- ---- ----";
  if i <= 1 then
    print_endline "  evaluating texts ..."
  else
    Printf.printf "  evaluating texts (%s trial) ...\n"
      (ordinal i)


let end_evaluation (_config : config) =
  print_endline "  evaluation done."


let start_page_break (_config : config) =
  print_endline " ---- ---- ---- ----";
  print_endline "  breaking contents into pages ..."


let needs_another_trial (_config : config) =
  print_endline "  needs another trial for solving cross references..."


let achieve_count_max (_config : config) =
  print_endline "  could not reach a fixpoint when resolving cross references."


let achieve_fixpoint (_config : config) (unresolved_crossrefs : string list) =
  match unresolved_crossrefs with
  | [] ->
      print_endline "  all cross references were solved."

  | _ :: _ ->
      print_endline "  some cross references were not solved:";
      unresolved_crossrefs |> List.iter (fun crossref ->
        Printf.printf "  - %s\n" crossref
      )


let end_output (config : config) (file_name_out : abs_path) =
  print_endline " ---- ---- ---- ----";
  Printf.printf "  output written on '%s'.\n"
    (show_path config file_name_out)


let target_file (config : config) (file_name_out : abs_path) =
  print_endline " ---- ---- ---- ----";
  Printf.printf "  target file: '%s'\n"
    (show_path config file_name_out)


let dump_file (config : config) ~(already_exists : bool) (dump_file : abs_path) =
  if already_exists then
    Printf.printf "  dump file: '%s' (already exists)\n"
      (show_path config dump_file)
  else
    Printf.printf "  dump file: '%s' (will be created)\n"
      (show_path config dump_file)


let deps_config_file (config : config) (abspath_deps_config : abs_path) =
  Printf.printf "  deps file: '%s'\n"
    (show_path config abspath_deps_config)


let begin_to_embed_fonts () =
  print_endline " ---- ---- ---- ----";
  print_endline "  embedding fonts ..."


let begin_to_write_page () =
  print_endline " ---- ---- ---- ----";
  print_endline "  writing pages ..."


let warn_cmyk_image (file_name : abs_path) =
  let config = { path_display_setting = FullPath; verbose = false } in (* TODO: make this changeable *)
  Printf.printf "  [Warning] Jpeg images with CMYK color mode are not fully supported: '%s'\n"
    (show_path config file_name);
  print_endline "  Please convert the image to a jpeg image with YCbCr (RGB) color model."


let warn_noninjective_cmap (uch1 : Uchar.t) (uch2 : Uchar.t) (gidorg : Otfed.Value.glyph_id) =
  Printf.printf "  [Warning] Multiple Unicode code points (U+%04X and U+%04X) are mapped to the same GID %d.\n"
    (Uchar.to_int uch1)
    (Uchar.to_int uch2)
    gidorg


let warn_noninjective_ligature (gidorg_lig : Otfed.Value.glyph_id) =
  Printf.printf "  [Warning] GID %d is used as more than one kind of ligatures.\n"
    gidorg_lig


let warn_nonattachable_mark (go_mark : Otfed.Value.glyph_id) (go_base : Otfed.Value.glyph_id) =
  Printf.printf "  [Warning] The combining diacritical mark of GID %d cannot be attached to the base glyph of GID %d.\n"
    go_mark
    go_base


let warn_no_glyph (fontname : string) (uch : Uchar.t) =
  Printf.printf "  [Warning] No glyph is provided for U+%04X by font '%s'.\n"
    (Uchar.to_int uch)
    fontname


let warn_no_math_glyph (fontname : string) (uch : Uchar.t) =
  Printf.printf "  [Warning] No glyph is provided for U+%04X by math font '%s'.\n"
    (Uchar.to_int uch)
    fontname


let warn_number_sign_end rng =
  Format.printf "  [Warning] at %a: '#' has no effect here\n"
    Range.pp rng


let warn_overfull_line (pageno : int) =
  Printf.printf "  [Warning] an overfull line occurs on page %d\n"
    pageno


let warn_underfull_line (pageno : int) =
  Printf.printf "  [Warning] an underfull line occurs on page %d\n"
    pageno


let warn_unreachable (pageno : int) =
  Printf.printf "  [Warning] a line unable to be broken into a paragraph occurs on page %d\n"
    pageno


let report_passed_test ~(test_name : string) =
  Printf.printf "  OK: %s\n" test_name


let report_failed_test ~(test_name : string) ~(message : string) =
  Printf.printf "! FAILED: %s\n" test_name;
  Printf.printf "  %s\n" message


let all_tests_passed () =
  print_endline " ---- ---- ---- ----";
  print_endline "  all tests have passed."


let some_test_failed () =
  print_endline " ---- ---- ---- ----";
  print_endline "! some test has failed."


let warn_wide_column_cell_overrides_nonempty_cell () =
  print_endline "  [Warning] a non-empty cell was overridden by a cell that has more than one column span."


let warn_wide_row_cell_overrides_nonempty_cell () =
  print_endline "  [Warning] a non-empty cell was overridden by a cell that has more than one row span."
