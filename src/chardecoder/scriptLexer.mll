{
  open ScriptParser

  let int_of_hex_string s = int_of_string ("0x" ^ s)
}

let hex = ( ['0'-'9'] | ['A'-'F'] )
let nonbreak = [^ '\n' '\r']
let break = ['\n' '\r']
let upper = ['A'-'Z']
let lower = ['a'-'z']
let space = [' ' '\t']

rule expr_raw = parse
  | (space | break)+       { expr_raw lexbuf }
  | ("#" nonbreak* break)  { expr_raw lexbuf }
  | ";"                    { SEMICOLON }
  | ".."                   { DOTS }
  | (hex+)                 { CODEPOINT(int_of_hex_string (Lexing.lexeme lexbuf)) }
  | (upper | lower | "_")+ { IDENTIFIER(Lexing.lexeme lexbuf) }
      (* -- the definition for CODEPOINT should be prior to that of IDENTIFIER -- *)
  | eof                    { EOI }
  | _                      { failwith ("ScriptLexer: illegal token " ^ (Lexing.lexeme lexbuf)) }

{
  let expr lexbuf =
    let tok = expr_raw lexbuf in
      print_endline begin
        match tok with
        | SEMICOLON     -> ";"
        | DOTS          -> ".."
        | IDENTIFIER(s) -> "ID(" ^ s ^ ")"
        | CODEPOINT(cp) -> Printf.sprintf "CP(U+%X)" cp
        | EOI           -> "EOI"
      end;
      tok
}
