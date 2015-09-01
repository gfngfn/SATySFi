open Types
open Typeenv
open Display

exception MainError of string

let show_control_sequence_type : bool ref = ref false
let show_function_type         : bool ref = ref false

(* string -> bool *)
let is_document_file str =
  if String.length str < 5 then false else
    (compare ".mcrd" (String.sub str ((String.length str) - 5) 5)) == 0

(* string -> bool *)
let is_header_file str =
  if String.length str < 6 then false else
    (compare ".mcrdh" (String.sub str ((String.length str) - 6) 6)) == 0

(* string -> bool *)
let is_standalone_file str =
  if String.length str < 6 then false else
    (compare ".mcrds" (String.sub str ((String.length str) - 6) 6)) == 0


(* type_environment -> environment -> string -> (type_environment * environment) *)
let make_environment_from_header_file tyenv env file_name_in =
  ( print_string (" ---- ---- ---- ----\n") ;
    print_string ("  reading '" ^ file_name_in ^ "' ...\n") ;
    let file_in = open_in file_name_in in
    ( Lexer.reset_to_numexpr () ;
      let utast = Parser.main Lexer.cut_token (Lexing.from_channel file_in) in
      let (ty, newtyenv, ast) = Typechecker.main tyenv utast in
      ( print_string ("  type check: " ^ (string_of_type_struct ty) ^ "\n") ;
        let evaled = Evaluator.interpret env ast in
          match evaled with
          | EvaluatedEnvironment(newenv) ->
              ( ( if !show_control_sequence_type then
                    if !show_function_type then
                      print_string (string_of_type_environment newtyenv "Environment")
                    else
                      print_string (string_of_control_sequence_type newtyenv)
                  else () ) ;
                (newtyenv, newenv)
              )
          | _ -> raise (MainError("'" ^ file_name_in ^ "' is not a header file"))
      )
    )
  )


(* type_environment -> environment -> string -> string -> unit *)
let read_standalone_file tyenv env file_name_in file_name_out =
  ( print_string (" ---- ---- ---- ----\n") ;
    print_string ("  reading '" ^ file_name_in ^ "' ...\n") ;
    let file_in = open_in file_name_in in
    ( Lexer.reset_to_numexpr () ;
      let utast = Parser.main Lexer.cut_token (Lexing.from_channel file_in) in
      let (ty, _, ast) = Typechecker.main tyenv utast in
      ( print_string ("  type check: " ^ (string_of_type_struct ty) ^ "\n") ;
        match ty with
        | StringType(_) ->
            let evaled = Evaluator.interpret env ast in
            let content_out = Out.main evaled in
            ( Files.file_out_of_string file_name_out content_out ;
              print_string (" ---- ---- ---- ----\n") ;
              print_string ("  output written on '" ^ file_name_out ^ "'.\n")
            )
        | _  -> raise (TypeCheckError("the output of '" ^ file_name_in ^ "' is not string"))
      )
    )
  )


(* type_environment -> environment -> string -> string -> unit *)
let read_document_file tyenv env file_name_in file_name_out =
  ( print_string (" ---- ---- ---- ----\n") ;
    print_string ("  reading '" ^ file_name_in ^ "' ...\n") ;
    let file_in = open_in file_name_in in
    ( Lexer.reset_to_strexpr () ;
      let utast = Parser.main Lexer.cut_token (Lexing.from_channel file_in) in
      let (ty, _, ast) = Typechecker.main tyenv utast in
      ( print_string ("  type check: " ^ (string_of_type_struct ty) ^ "\n") ;
        match ty with
        | StringType(_) ->
            let evaled = Evaluator.interpret env ast in
            let content_out = Out.main evaled in
            if (String.length content_out) == 0 then
            ( print_string " ---- ---- ---- ----\n" ;
              print_string "  no output.\n" )
            else
            ( Files.file_out_of_string file_name_out content_out ;
              print_string " ---- ---- ---- ----\n" ;
              print_string ("  output written on '" ^ file_name_out ^ "'.\n")
            )
        | _ -> raise (TypeCheckError("the output of '" ^ file_name_in ^ "' is not string"))
      )
    )
  )

(* type_environment -> environment -> (string list) -> string -> unit *)
let rec main tyenv env file_name_in_list file_name_out =
  try
    match file_name_in_list with
    | [] ->
        ( print_string " ---- ---- ---- ----\n" ;
        	print_string "  no output.\n" )
    | file_name_in :: tail ->
        if is_document_file file_name_in then
          read_document_file tyenv env file_name_in file_name_out
        else if is_header_file file_name_in then
          let (newtyenv, newenv) = make_environment_from_header_file tyenv env file_name_in in
            main newtyenv newenv tail file_name_out
        else if is_standalone_file file_name_in then
          read_standalone_file tyenv env file_name_in file_name_out
        else
          raise (MainError("'" ^ file_name_in ^ "' has illegal filename extension"))
  with
  | Lexer.LexError(s)             -> print_string ("! [ERROR IN LEXER] " ^ s ^ ".\n")
  | Parsing.Parse_error           -> print_string ("! [ERROR IN PARSER]")
  | ParseErrorDetail(s)           -> print_string ("! [ERROR IN PARSER] " ^ s ^ "\n")
  | Typeenv.TypeCheckError(s)     -> print_string ("! [ERROR IN TYPECHECK] " ^ s ^ ".\n")
  | Evaluator.EvalError(s)        -> print_string ("! [ERROR IN EVAL] " ^ s ^ ".\n")
  | Out.IllegalOut(s)             -> print_string ("! [ERROR IN OUT] " ^ s ^ ".\n")
  | MainError(s)                  -> print_string ("! [ERROR IN MAIN] " ^ s ^ ".\n")
  | Sys_error(s)                  -> print_string ("! [ERROR IN MAIN] System error - " ^ s ^ "\n")

(* int -> (string list) -> string -> unit *)
let rec see_argv num file_name_in_list file_name_out =
    if num == Array.length Sys.argv then
    ( print_string ("  [output] " ^ file_name_out ^ "\n\n") ;
      Typechecker.initialize () ;
      let tyenv = Primitives.make_type_environment () in
      let env = Primitives.make_environment () in
        main tyenv env file_name_in_list file_name_out )
    else
      match Sys.argv.(num) with
      | "-v" ->
          print_string "  Macrodown version 1.00b\n"
      | "-o" ->
          ( try
              see_argv (num + 2) file_name_in_list (Sys.argv.(num + 1))
            with
            | Invalid_argument(s) -> print_string "! missing file name after '-o' option\n"
          )
      | "-t" ->
          ( show_control_sequence_type := true ;
            see_argv (num + 1) file_name_in_list file_name_out
          )
      | "-f" ->
          ( show_control_sequence_type := true ;
            show_function_type         := true ;
            see_argv (num + 1) file_name_in_list file_name_out
          )
      | _    ->
          ( print_string ("  [input] " ^ Sys.argv.(num) ^ "\n") ;
            see_argv (num + 1) (file_name_in_list @ [Sys.argv.(num)]) file_name_out
          )

let _ = see_argv 1 [] "mcrd.out"
