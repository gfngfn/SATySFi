
open MyUtil


let show_path abspath =
  let pathstr = get_abs_path_string abspath in
  if OptionState.show_full_path () then pathstr else Filename.basename pathstr


let begin_to_read_file abspath_in =
  print_endline (" ---- ---- ---- ----");
  print_endline ("  reading '" ^ (show_path abspath_in) ^ "' ...")


let begin_to_parse_file abspath_in =
  print_endline ("  parsing '" ^ (show_path abspath_in) ^ "' ...")


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


let end_output file_name_out =
  print_endline (" ---- ---- ---- ----");
  print_endline ("  output written on '" ^ (show_path file_name_out) ^ "'.")


let no_output () =
  print_endline " ---- ---- ---- ----";
  print_endline "  no output."


let target_file file_name_out =
  print_endline (" ---- ---- ---- ----");
  print_endline ("  target file: '" ^ (show_path file_name_out) ^ "'")


let  dump_file dump_file_exists dump_file =
  if dump_file_exists then
    print_endline ("  dump file: '" ^ (show_path dump_file) ^ "' (already exists)")
  else
    print_endline ("  dump file: '" ^ (show_path dump_file) ^ "' (will be created)")


let begin_to_embed_fonts () =
  print_endline (" ---- ---- ---- ----");
  print_endline ("  embedding fonts ...")


let begin_to_write_page () =
  print_endline (" ---- ---- ---- ----");
  print_endline ("  writing pages ...")


let warn_deprecated msg =
  print_endline ("  [Warning] " ^ msg ^ "\n")


let warn_cmyk_image file_name =
  print_endline ("  [Warning] (" ^ (show_path file_name) ^ ") Jpeg images with CMYK color mode are not fully supported.");
  print_endline ("  Please convert the image to a jpeg image with YCbCr (RGB) color model.")


let warn_noninjective_cmap uchpre uch gidorg =
  Format.printf "  [Warning] Multiple Unicode code points (U+%04X and U+%04X) are mapped to the same GID %d.\n" (Uchar.to_int uchpre) (Uchar.to_int uch) gidorg


let warn_noninjective_ligature gidorglig =
  Format.printf "  [Warning] GID %d is used as more than one kind of ligatures.\n" gidorglig


let warn_nonattachable_mark gomark gobase =
  Format.printf "  [Warning] The combining diacritical mark of GID %d cannot be attached to the base glyph of GID %d.\n" gomark gobase


let warn_no_glyph abbrev uch =
  Format.printf "  [Warning] No glyph is provided for U+%04X by font `%s`.\n" (Uchar.to_int uch) abbrev


let warn_no_math_glyph mfabbrev uch =
  Format.printf "  [Warning] No glyph is provided for U+%04X by math font `%s`.\n" (Uchar.to_int uch) mfabbrev
