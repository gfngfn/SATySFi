(*
let file_name_in = "test_of_mcd_in.txt"

let file_name_out = "test_of_mcd_out.txt"
*)

let str_f_in = ref ""

let string_of_file filename =
  let f_in = open_in filename in
  let cat_sub () =
    while true do
      str_f_in := !str_f_in ^ (String.make 1 (input_char f_in))
    done
  in
    try cat_sub () with End_of_file -> close_in f_in

let main filename =
	string_of_file filename ;
(*
	!str_f_in
*)

  let content_in = !str_f_in in
  let lexed = McdLexer.mcdlex content_in in
  let parsed = McdParser.mcdparser lexed in
  let absed = McdAbs.concrete_to_abstract parsed in
  let content_out = McdOut.mcdout absed in
    content_out
