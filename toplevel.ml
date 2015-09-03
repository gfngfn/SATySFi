#load "stacklist.cmo";;
#load "types.cmo";;
#load "typeenv.cmo";;
#load "display.cmo";;
#load "subst.cmo";;
#load "variantenv.cmo";;
#load "parser.cmo";;
#load "lexer.cmo";;
#load "typechecker.cmo";;
#load "out.cmo";;
#load "evaluator.cmo";;
#load "primitives.cmo";;
let varntenv = Primitives.make_variant_environment ();;
let tyenv    = Primitives.make_type_environment ();;
let env      = Primitives.make_environment ();;

let init () = Lexer.reset_to_numexpr ();;

let parse s = init () ; Parser.main Lexer.cut_token (Lexing.from_string s);;

let parsestr s = Display.string_of_utast (parse s);;

let tcraw s =
  init () ;
  let (typed, _, ast) = (Typechecker.main varntenv tyenv (parse s)) in typed
;;

let tc s = Display.string_of_type_struct (tcraw s);;

let eval s =
  init () ;
  let (typed, _, ast) = (Typechecker.main varntenv tyenv (parse s)) in
    Evaluator.interpret env ast
;;
let evalstr s = Display.string_of_ast (eval s);;

let out s = init () ; Out.main (eval s);;
