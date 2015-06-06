open Types

let report_error errmsg =
  print_string ("! [ERROR IN MAIN] " ^ errmsg) ; print_newline ()

let report_detail dtlmsg =
  print_string ("  " ^ dtlmsg) ; print_newline ()

let main file_name_in_list file_name_out =
  try
    match file_name_in_list with
    | [] -> ()
    | file_name_in :: tail ->
      let file_in = open_in file_name_in in
      let parsed = Parser.main Lexer.cut_token (Lexing.from_channel file_in) in
      let content_out = Out.main (Eval.main parsed) in
        Files.file_out_of_string file_name_out content_out
  with
  | Lexer.LexError(s) -> print_string ("! [ERROR IN LEXER] " ^ s ^ ".")
  | Parsing.Parse_error -> print_string ("! [ERROR IN PARSER]")
  | Typecheck.TypeCheckError(s) -> print_string ("! [ERROR IN TYPECHECK] " ^ s ^ ".")
(*  | Eval.EvalError(s) -> print_string ("! [ERROR IN EVAL]" ^ s ^ ".") *)
  | Out.IllegalOut(s) -> print_string ("! [ERROR IN OUT] " ^ s ^ ".")
  | Sys_error(s) -> report_error ("! System error - " ^ s)

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
