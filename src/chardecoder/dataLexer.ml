open DataParser

let int_of_hex s = int_of_string ("0x" ^ s)

let lexeme = Sedlexing.Utf8.lexeme


let digit = [%sedlex.regexp? '0'..'9']
let hex = [%sedlex.regexp? digit | 'A'..'F']
let cp = [%sedlex.regexp? hex, hex, hex, hex, (Star hex)]
let break = [%sedlex.regexp? "\r\n" | '\r' | '\n']
let nonbreak = [%sedlex.regexp? Compl '\r' | Compl '\n']
let upper = [%sedlex.regexp? 'A'..'Z']
let lower = [%sedlex.regexp? 'a'..'z']
let alph = [%sedlex.regexp? upper | lower]
let space = [%sedlex.regexp? ' ' | '\t']


let rec expr_raw lexbuf =
  match%sedlex lexbuf with
  | Plus (space | break)            -> expr_raw lexbuf
  | "#"                             -> (
    let () = lex_comment lexbuf in
    expr_raw lexbuf
  )
  | cp -> (
    let cpstr1 = lexeme lexbuf in
    let cpstr2opt = lex_cp lexbuf in
    match cpstr2opt with
    | Some(cpstr2) -> CODEPOINTRANGE(int_of_hex cpstr1, int_of_hex cpstr2)
    | None         -> CODEPOINT(int_of_hex cpstr1)
  )
  | alph, Star (alph | digit | "_") -> DATA(lexeme lexbuf)
  | eof                             -> EOI
  | _                               -> failwith ("DataLexer: illegal token " ^ (lexeme lexbuf))


and lex_comment lexbuf =
  match%sedlex lexbuf with
  | break -> ()
  | any -> lex_comment lexbuf
  | _ -> failwith ("DataLexer: illegal token " ^ (lexeme lexbuf))


and lex_cp lexbuf =
  match%sedlex lexbuf with
  | (Star space), ';' -> None
  | ".."              -> Some(lex_cp2 lexbuf)
  | _                 -> failwith ("DataLexer: illegal token " ^ (lexeme lexbuf))


and lex_cp2 lexbuf =
  match%sedlex lexbuf with
  | cp -> (
    let cpstr2 = lexeme lexbuf in
    if lex_cp3 lexbuf then
      cpstr2
    else
      failwith ("DataLexer: illegal token " ^ (lexeme lexbuf))
  )
  | _ -> failwith ("DataLexer: illegal token " ^ (lexeme lexbuf))

and lex_cp3 lexbuf =
  match%sedlex lexbuf with
  | (Star space), ';' -> true
  | _                 -> false


let expr lexbuf = expr_raw lexbuf


let parse lexbuf =
  let lexer () =
    let (ante_position, post_position) =
      Sedlexing.lexing_positions lexbuf
    in
    let token = expr lexbuf in
    (token, ante_position, post_position)
  in
  let parser =
    MenhirLib.Convert.Simplified.traditional2revised DataParser.main
  in
  parser lexer
