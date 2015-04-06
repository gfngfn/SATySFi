open Types

(* string -> string *)
let string_of_file_in file_name_in =
  let str_in = ref "" in
  let chnl_in = open_in file_name_in in
  let cat_sub () =
    while true do
      str_in := !str_in ^ (String.make 1 (input_char chnl_in))
    done
  in
    try
      ( cat_sub () ; "" )
    with
      End_of_file -> ( close_in chnl_in ; !str_in )

let rec string_of_file_in_list file_name_in_list =
	match file_name_in_list with
	  [] -> ""
	| head :: tail -> (string_of_file_in head) ^ (string_of_file_in_list tail)

(* string -> string -> unit *)
let file_out_of_string file_name_out content_out =
  let chnl_out = open_out file_name_out in
    output_string chnl_out content_out ;
    close_out chnl_out

let main file_name_in_list file_name_out =

  let content_in = (string_of_file_in_list file_name_in_list) in
  let lexed = Mcdlexer.mcdlex content_in in
  let parsed = Mcdparser.mcdparser lexed in
  let absed = Mcdabs.concrete_to_abstract parsed in
  let semed = Mcdsemantics.semantics absed in
  let content_out =
    try
      Mcdout.mcdout semed
    with
      IllegalOut -> ""
  in
    match content_out with
      "" -> ( print_string "No output." ; print_newline () )
    | _ -> (
          try
            file_out_of_string file_name_out content_out
          with
            Sys_error(s) -> ( print_string ("System error:" ^ s) ; print_newline () )
        )

let rec concat_list lsta lstb =
  match lsta with
    [] -> lstb
  | head :: tail -> head :: (concat_list tail lstb)

let rec see_argv num file_name_in_list file_name_out =
    if num == Array.length Sys.argv then
      main file_name_in_list file_name_out
    else (
      if (compare Sys.argv.(num) "-o") == 0 then (
        print_string ("[output] " ^ Sys.argv.(num + 1)) ; print_newline () ;
        see_argv (num + 2) file_name_in_list (Sys.argv.(num + 1))
      ) else (
        print_string ("[input] " ^ Sys.argv.(num)) ; print_newline () ;
        see_argv (num + 1) (concat_list file_name_in_list [Sys.argv.(num)]) file_name_out
      )
    )

let _ = see_argv 1 [] "mcrd.out"
