#load "assoc.cmo";;
#load "stacklist.cmo";;
#load "tyvarid.cmo";;
#load "range.cmo";;
#load "types.cmo";;
#load "hashTree.cmo";;
#load "typeenv.cmo";;
#load "directedGraph.cmo";;
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
let init () = Lexer.reset_to_numexpr ();;
Tyvarid.initialize () ;;
let varntenv = Primitives.make_variant_environment ();;
let tyenv    = Primitives.make_type_environment ();;
let env      = Primitives.make_environment ();;
init ();;


let parse s =
  let p = ref (Range.dummy "init", Types.UTNumericConstant(0)) in
  begin
    Main.error_log_environment (fun () ->
      begin
        init () ;
        p := Parser.main Lexer.cut_token (Lexing.from_string s)
      end
    ) ;
    !p
  end
;;

let parsestr s = Display.string_of_utast (parse s);;

let tcraw s =
  init () ;
  let (tyres, _, _, _) = (Typechecker.main varntenv tyenv (parse s)) in tyres
;;

let tc s mdlnmlst varnm =
  Main.error_log_environment (fun () ->
    begin
      init () ;
      let (tyres, varntenvres, tyenvres, _) = (Typechecker.main varntenv tyenv (parse s)) in
      try
        let pty = Typeenv.find tyenvres mdlnmlst varnm in
          print_endline ("TYPE = " ^ (Display.string_of_poly_type varntenvres pty))
      with
      | Not_found -> print_endline ("! [Error at TOPLEVEL] '" ^ varnm ^ "' not found.")
    end
  );;

let tcb s varnm =
  Main.error_log_environment (fun () ->
    begin
      init () ;
      let (tyres, varntenvres, tyenvres, _) = (Typechecker.main varntenv tyenv (parse s)) in
      try
        let pty = Typeenv.find tyenvres [] varnm in
          print_endline ("TYPE = " ^ (Types.string_of_poly_type_basic pty))
      with
      | Not_found -> print_endline ("! [Error at TOPLEVEL] '" ^ varnm ^ "' not found.")
    end
  );;

let eval s =
  init () ;
  let (_, _, _, ast) = (Typechecker.main varntenv tyenv (parse s)) in
    Evaluator.interpret env ast
;;

let evalstr s = Display.string_of_ast (eval s);;

let out s = init () ; Out.main (eval s);;
