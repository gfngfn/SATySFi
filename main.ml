open Types

let report_detail dtlmsg =
  print_string ("  " ^ dtlmsg) ; print_newline ()

let main file_name_in_list file_name_out =
  try
    match file_name_in_list with
    | [] -> ()
    | file_name_in :: tail ->
      let file_in = open_in file_name_in in
      let parsed = Parser.main Lexer.cut_token (Lexing.from_channel file_in) in
      let typed = Typechecker.main parsed in
      ( print_string ("  [type check] " ^ typed ^ "\n") ;
        let content_out = Out.main (Evaluator.main parsed) in
          Files.file_out_of_string file_name_out content_out
      )
  with
  | Lexer.LexError(s) -> print_string ("! [ERROR IN LEXER] " ^ s ^ ".\n")
  | Parsing.Parse_error -> print_string ("! [ERROR IN PARSER]")
  | Typechecker.TypeCheckError(s) -> print_string ("! [ERROR IN TYPECHECK] " ^ s ^ ".\n")
  | Evaluator.EvalError(s) -> print_string ("! [ERROR IN EVAL]" ^ s ^ ".\n")
  | Out.IllegalOut(s) -> print_string ("! [ERROR IN OUT] " ^ s ^ ".\n")
  | Sys_error(s) -> print_string ("! [ERROR IN MAIN] System error - " ^ s ^ "\n")

let rec concat_list lsta lstb =
  match lsta with
    [] -> lstb
  | head :: tail -> head :: (concat_list tail lstb)

let rec see_argv num file_name_in_list file_name_out =
    if num == Array.length Sys.argv then (
      print_string ("  [output] " ^ file_name_out ^ "\n\n") ;
      main file_name_in_list file_name_out
    ) else (
      if (compare Sys.argv.(num) "-o") == 0 then
          try (
            see_argv (num + 2) file_name_in_list (Sys.argv.(num + 1))
          ) with
            Invalid_argument(s) -> print_string "! missing file name after '-o' option\n"
      else (
        print_string ("  [input] " ^ Sys.argv.(num) ^ "\n") ;
        see_argv (num + 1) (concat_list file_name_in_list [Sys.argv.(num)]) file_name_out
      )
    )

let _ = see_argv 1 [] "mcrd.out"
