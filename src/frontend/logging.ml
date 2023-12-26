
open MyUtil


type config = {
  show_full_path : bool;
}


let show_path (config : config) (abspath : abs_path) =
  let pathstr = get_abs_path_string abspath in
  if config.show_full_path then pathstr else Filename.basename pathstr


let begin_to_typecheck_file (config : config) (abspath_in : abs_path) =
  print_endline (" ---- ---- ---- ----");
  print_endline ("  type checking '" ^ (show_path config abspath_in) ^ "' ...")


let begin_to_preprocess_file (config : config) (abspath_in : abs_path) =
  print_endline ("  preprocessing '" ^ (show_path config abspath_in) ^ "' ...")


let begin_to_eval_file (config : config) (abspath_in : abs_path) =
  print_endline ("  evaluating '" ^ (show_path config abspath_in) ^ "' ...")


let begin_to_parse_file (config : config) (abspath_in : abs_path) =
  print_endline ("  parsing '" ^ (show_path config abspath_in) ^ "' ...")


let pass_type_check opt =
  match opt with
  | None ->
      print_endline ("  type check passed.")

  | Some(str) ->
      print_endline ("  type check passed. (" ^ str ^ ")")


let ordinal i =
  let suffix =
    match i mod 10 with
    | 1 -> "st"
    | 2 -> "nd"
    | 3 -> "rd"
    | _ -> "th"
  in
  (string_of_int i) ^ suffix


let start_evaluation i =
  print_endline (" ---- ---- ---- ----");
  begin
    if i <= 1 then
      print_endline ("  evaluating texts ...")
    else
      print_endline ("  evaluating texts (" ^ (ordinal i) ^ " trial) ...")
  end


let end_evaluation () =
  print_endline ("  evaluation done.")


let start_page_break () =
  print_endline (" ---- ---- ---- ----");
  print_endline ("  breaking contents into pages ...")


let needs_another_trial () =
  print_endline ("  needs another trial for solving cross references...")


let achieve_count_max () =
  print_endline ("  could not reach to fixpoint when resolving cross references.")


let achieve_fixpoint unresolved_crossrefs =
  if unresolved_crossrefs = [] then
    print_endline ("  all cross references were solved.")
  else
    print_endline ("  some cross references were not solved: " ^ String.concat " " unresolved_crossrefs ^ ".")


let end_output (config : config) (file_name_out : abs_path) =
  print_endline (" ---- ---- ---- ----");
  print_endline ("  output written on '" ^ (show_path config file_name_out) ^ "'.")


let target_file (config : config) (file_name_out : abs_path) =
  print_endline (" ---- ---- ---- ----");
  print_endline ("  target file: '" ^ (show_path config file_name_out) ^ "'")


let dump_file (config : config) ~(already_exists : bool) (dump_file : abs_path) =
  if already_exists then
    print_endline ("  dump file: '" ^ (show_path config dump_file) ^ "' (already exists)")
  else
    print_endline ("  dump file: '" ^ (show_path config dump_file) ^ "' (will be created)")


let lock_config_file (config : config) (abspath_lock_config : abs_path) =
  Printf.printf "  lock file: '%s'\n" (show_path config abspath_lock_config)


let begin_to_embed_fonts () =
  print_endline (" ---- ---- ---- ----");
  print_endline ("  embedding fonts ...")


let begin_to_write_page () =
  print_endline (" ---- ---- ---- ----");
  print_endline ("  writing pages ...")


let warn_cmyk_image (file_name : abs_path) =
  let config = { show_full_path = true } in (* TODO: make this changeable *)
  print_endline ("  [Warning] (" ^ (show_path config file_name) ^ ") Jpeg images with CMYK color mode are not fully supported.");
  print_endline ("  Please convert the image to a jpeg image with YCbCr (RGB) color model.")


let warn_noninjective_cmap (uch1 : Uchar.t) (uch2 : Uchar.t) (gidorg : Otfed.Value.glyph_id) =
  Format.printf "  [Warning] Multiple Unicode code points (U+%04X and U+%04X) are mapped to the same GID %d.\n" (Uchar.to_int uch1) (Uchar.to_int uch2) gidorg


let warn_noninjective_ligature (gidorg_lig : Otfed.Value.glyph_id) =
  Format.printf "  [Warning] GID %d is used as more than one kind of ligatures.\n" gidorg_lig


let warn_nonattachable_mark (gomark : Otfed.Value.glyph_id) (gobase : Otfed.Value.glyph_id) =
  Format.printf "  [Warning] The combining diacritical mark of GID %d cannot be attached to the base glyph of GID %d.\n" gomark gobase


let warn_no_glyph (fontname : string) (uch : Uchar.t) =
  Format.printf "  [Warning] No glyph is provided for U+%04X by font `%s`.\n" (Uchar.to_int uch) fontname


let warn_no_math_glyph (fontname : string) (uch : Uchar.t) =
  Format.printf "  [Warning] No glyph is provided for U+%04X by math font `%s`.\n" (Uchar.to_int uch) fontname


let warn_number_sign_end rng =
  Format.printf "  [Warning] at %a: '#' has no effect here\n"
    Range.pp rng


let warn_overfull_line (pageno : int) =
  Format.printf "  [Warning] an overfull line occurs on page %d\n"
    pageno


let warn_underfull_line (pageno : int) =
  Format.printf "  [Warning] an underfull line occurs on page %d\n"
    pageno


let warn_unreachable (pageno : int) =
  Format.printf "  [Warning] a line unable to be broken into a paragraph occurs on page %d\n"
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
  Format.printf "  [Warning] a non-empty cell was overridden by a cell that has more than one column span.\n"


let warn_wide_row_cell_overrides_nonempty_cell () =
  Format.printf "  [Warning] a non-empty cell was overridden by a cell that has more than one row span.\n"
