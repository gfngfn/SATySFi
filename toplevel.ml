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

let tc s    = init () ; let (typed, _, ast) = (Typechecker.main tyenv (parse s)) in typed;;

let eval s  =
  init () ;
  let (typed, _, ast) = (Typechecker.main tyenv (parse s)) in
    Evaluator.interpret env ast;;

let out s   = init () ; Out.main (eval s);;
