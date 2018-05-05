type line =
  | NormalLine  of string
  | DisplayLine of string

type error_category =
  | Lexer
  | Parser
  | Typechecker
  | Evaluator
  | Interface
  | System

let show_error_category = function
  | Lexer       -> "Syntax Error at Lexer"
  | Parser      -> "Syntax Error at Parser"
  | Typechecker -> "Type Error"
  | Evaluator   -> "Error during Evaluation"
  | Interface   -> "Error"
  | System      -> "Error"


let report_error_noabort (cat : error_category) (lines : line list) =
  let rec aux lst =
    match lst with
    | []                     -> ()
    | NormalLine(s) :: tail  -> begin print_endline ("    " ^ s) ; aux tail end
    | DisplayLine(s) :: tail -> begin print_endline ("      " ^ s); aux tail end
  in
  let first lst =
    match lst with
    | []                     -> ()
    | NormalLine(s) :: tail  -> begin print_endline s; aux tail end
    | DisplayLine(s) :: tail -> begin print_endline ("\n      " ^ s); aux tail end
  in
  begin
    print_string ("! [" ^ (show_error_category cat) ^ "] ");
    first lines;
  end

let report_error cat lines =
  report_error_noabort cat lines;
  exit 1

exception ErrorReported

type error_store = {
  mutable rev_errors: (error_category * line list) list
}

let create_error_store () = {
  rev_errors = []
}

let report_recorded_errors do_raise store =
  let errors = List.rev store.rev_errors in
  List.iter (fun (cat, lines) ->
    report_error_noabort cat lines
  ) errors;
  if errors <> [] && do_raise then
    raise ErrorReported
  else
    ()

let with_error_store callback =
  let store = create_error_store () in
  let result =
    try
      callback store
    with e ->
      report_recorded_errors false store;
      raise e
  in
  report_recorded_errors true store;
  result

let record_error store (cat : error_category) (lines : line list) =
  store.rev_errors <- (cat, lines) :: store.rev_errors
