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

rule expr = parse
  | space*                 { expr lexbuf }
  | ("#" nonbreak* break)  { expr lexbuf }
  | ";"                    { SEMICOLON }
  | ".."                   { DOTS }
  | (upper | lower | "_")+ { IDENTIFIER(Lexing.lexeme lexbuf) }
  | (hex+)                 { CODEPOINT(int_of_hex_string (Lexing.lexeme lexbuf)) }
  | eof                    { EOI }
