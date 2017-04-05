#load "assoc.cmo";;
#load "stacklist.cmo";;
#load "tyvarid.cmo";;
#load "range.cmo";;
#load "types.cmo";;
#load "kindenv.cmo";;
#load "typeenv.cmo";;
#load "variantenv.cmo";;
#load "display.cmo";;
#load "subst.cmo";;
#load "parser.cmo";;
#load "lexer.cmo";;
#load "typechecker.cmo";;
#load "out.cmo";;
#load "evaluator.cmo";;
#load "primitives.cmo";;
let varntenv = Primitives.make_variant_environment ();;
let kdenv    = Kindenv.empty;;
let tyenv    = Primitives.make_type_environment ();;
let env      = Primitives.make_environment ();;

let init () = Lexer.reset_to_numexpr ();;

let parse s = init () ; Parser.main Lexer.cut_token (Lexing.from_string s);;

let parsestr s = Display.string_of_utast (parse s);;

let tcraw s =
  init () ;
  let (tyres, _, _, _, _) = (Typechecker.main varntenv kdenv tyenv (parse s)) in tyres
;;

let tc s v =
  let (tyres, _, kdenvres, tyenvres, _) = (Typechecker.main varntenv kdenv tyenv (parse s)) in
    Display.string_of_poly_type varntenv kdenvres (Typeenv.find tyenvres v)
;;

let tcb s v =
  let (tyres, _, _, tyenvres, _) = (Typechecker.main varntenv kdenv tyenv (parse s)) in
    Display.string_of_poly_type_basic (Typeenv.find tyenvres v)
;;

let eval s =
  init () ;
  let (_, _, _, _, ast) = (Typechecker.main varntenv kdenv tyenv (parse s)) in
    Evaluator.interpret env ast
;;

let evalstr s = Display.string_of_ast (eval s);;

let out s = init () ; Out.main (eval s);;
