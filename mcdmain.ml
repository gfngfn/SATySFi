open Types

let report_error errmsg =
  print_string ("! [ERROR IN MAIN] " ^ errmsg) ; print_newline ()

let report_detail dtlmsg =
  print_string ("  " ^ dtlmsg) ; print_newline ()

let main file_name_in_list file_name_out =

  let content_in = (
      try Files.string_of_file_in_list file_name_in_list with
        Sys_error(s) -> (
            report_error ("System error - " ^ s) ; "" )
  ) in
  let lexed = Mcdlexer.mcdlex content_in in
  let parsed = Mcdparser.mcdparser lexed in
  let absed = Mcdabs.concrete_to_abstract parsed in
  let semed = Mcdsemantics.semantics absed in
  let content_out =
    try Mcdout.mcdout semed with
      IllegalOut -> ""
  in
    match content_out with
      "" -> report_detail ("No output for '" ^ file_name_out ^ "'.")
    | _ -> (
          try
            Files.file_out_of_string file_name_out content_out
          with
            Sys_error(s) -> report_error ("System error - " ^ s)
        )

let rec concat_list lsta lstb =
  match lsta with
    [] -> lstb
  | head :: tail -> head :: (concat_list tail lstb)

let rec see_argv num file_name_in_list file_name_out =
    if num == Array.length Sys.argv then (
      report_detail ("[output] " ^ file_name_out) ;
      main file_name_in_list file_name_out
    ) else (
      if (compare Sys.argv.(num) "-o") == 0 then
          try (
            see_argv (num + 2) file_name_in_list (Sys.argv.(num + 1))
          ) with
            Invalid_argument(s) -> report_error "missing file name after '-o' option"
      else (
        report_detail ("[input] " ^ Sys.argv.(num)) ;
        see_argv (num + 1) (concat_list file_name_in_list [Sys.argv.(num)]) file_name_out
      )
    )

let _ = see_argv 1 [] "mcrd.out"
