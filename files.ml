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
  | head :: tail ->
      let str_in = string_of_file_in head in
        str_in ^ (string_of_file_in_list tail)

(* string -> string -> unit *)
let file_out_of_string file_name_out content_out =
  let chnl_out = open_out file_name_out in
    output_string chnl_out content_out ;
    close_out chnl_out
