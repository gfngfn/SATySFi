#load "assoc.cmo";;
#load "stacklist.cmo";;
#load "tyvarid.cmo";;
#load "range.cmo";;
#load "types.cmo";;
#load "typeenv.cmo";;
#load "variantenv.cmo";;
#load "display.cmo";;
#load "parser.cmo";;
#load "lexer.cmo";;
#load "typechecker.cmo";;
#load "out.cmo";;
#load "evaluator.cmo";;
#load "primitives.cmo";;
#load "files.cmo";;
#load "main.cmo";;
let varntenv = Primitives.make_variant_environment ();;
let tyenv    = Primitives.make_type_environment ();;
let env      = Primitives.make_environment ();;

let init () = Lexer.reset_to_numexpr ();;

let parse s = init () ; Parser.main Lexer.cut_token (Lexing.from_string s);;

let parsestr s = Display.string_of_utast (parse s);;

let tcraw s =
  init () ;
  let (tyres, _, _, _) = (Typechecker.main varntenv tyenv (parse s)) in tyres
;;

let tc s v =
  Main.error_log_environment (fun () ->
    let (tyres, _, tyenvres, _) = (Typechecker.main varntenv tyenv (parse s)) in
    try
      let pty = Typeenv.find tyenvres v in
      print_endline ("TYPE = " ^ (Display.string_of_poly_type varntenv pty))
    with
    | Not_found -> print_endline ("! [Error at TOPLEVEL] '" ^ v ^ "' not found.")
  )
;;

let tcb s v =
  Main.error_log_environment (fun () ->
    init () ;
    let (tyres, _, tyenvres, _) = (Typechecker.main varntenv tyenv (parse s)) in
      print_endline (Display.string_of_poly_type_basic (Typeenv.find tyenvres v))
  )
;;

let eval s =
  init () ;
  let (_, _, _, ast) = (Typechecker.main varntenv tyenv (parse s)) in
    Evaluator.interpret env ast
;;

let evalstr s = Display.string_of_ast (eval s);;

let out s = init () ; Out.main (eval s);;
