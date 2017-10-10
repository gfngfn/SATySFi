{
  open DataParser

  let int_of_hex s = int_of_string ("0x" ^ s)
}

let hex = ( ['0'-'9'] | ['A'-'F'] )
let cp = (hex hex hex hex hex*)
let nonbreak = [^ '\n' '\r']
let break = ['\n' '\r']
let upper = ['A'-'Z']
let lower = ['a'-'z']
let space = [' ' '\t']

rule expr_raw = parse
  | (space | break)+                                { expr_raw lexbuf }
  | ("#" nonbreak* break)                           { expr_raw lexbuf }
  | ((cp as cpstr) space* ";")                      { CODEPOINT(int_of_hex cpstr) }
  | ((cp as cpstr1) ".." (cp as cpstr2) space* ";") { CODEPOINTRANGE(int_of_hex cpstr1, int_of_hex cpstr2) }
  | (upper | lower | "_")+                          { DATA(Lexing.lexeme lexbuf) }
  | eof                                             { EOI }
  | _                                               { failwith ("ScriptLexer: illegal token " ^ (Lexing.lexeme lexbuf)) }

{
  let expr lexbuf =
    expr_raw lexbuf
(*
    let tok = expr_raw lexbuf in
      print_endline begin
        match tok with
        | DATA(s)                  -> "DATA(" ^ s ^ ")"
        | CODEPOINT(cp)            -> Printf.sprintf "CP(U+%04X)" cp
        | CODEPOINTRANGE(cp1, cp2) -> Printf.sprintf "CPR(U+%04X..U+%04X)" cp1 cp2
        | EOI                      -> "EOI"
      end;
      tok
*)
}
