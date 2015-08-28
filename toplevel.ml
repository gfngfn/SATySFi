#load "stacklist.cmo";;
#load "types.cmo";;
#load "typeenv.cmo";;
#load "subst.cmo";;
#load "lexer.cmo";;
#load "parser.cmo";;
#load "typechecker.cmo";;
#load "out.cmo";;
#load "evaluator.cmo";;
#load "primitives.cmo";;
let env   = Primitives.make_environment ();;
let tyenv = Primitives.make_type_environment ();;

let init () = Lexer.reset_to_numexpr ();;

let parse s = init () ; Parser.main Lexer.cut_token (Lexing.from_string s);;

let parsestr s = Typeenv.string_of_utast (parse s);;

let tcraw s =
  init () ;
  let (typed, _, ast) = (Typechecker.main tyenv (parse s)) in typed
;;

let tc s = Typeenv.string_of_type_struct (tcraw s);;

let eval s =
  init () ;
  let (typed, _, ast) = (Typechecker.main tyenv (parse s)) in
    Evaluator.interpret env ast
;;
let evalstr s = Typeenv.string_of_ast (eval s);;

let out s = init () ; Out.main (eval s);;
