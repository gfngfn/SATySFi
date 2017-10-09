{
}

let hex = ( ['0'-'9'] | ['A'-'F'] )
let nonbreak = [^ '\n' '\r']
let break = ['\n' '\r']
let upper = ['A'-'Z']
let lower = ['a'-'z']

rule expr = parse
  | (hex+)                 { CODEPOINT(Lexing.lexeme lexbuf) }
  | ";"                    { SEMICOLON }
  | ".."                   { DOTS }
  | ("#" nonbreak* break)  { COMMENT }
  | (upper | lower | "_")+ { IDENTIFIER(Lexing.lexeme lexbuf) }
  | eof                    { EOI }
