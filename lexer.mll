{
  open Types
  open Parser

  exception LexError of string
  let line_no = ref 1
  let end_of_previousline = ref 0
(*  let get_pos lexbuf = (!line_no, (Lexing.lexeme_start lexbuf) - (!end_of_previousline)) *)

  type lexer_state = STATE_NUMEXPR | STATE_STREXPR | STATE_ACTIVE | STATE_COMMENT | STATE_LITERAL
  let next_state : lexer_state ref = ref STATE_NUMEXPR
  let after_literal_state : lexer_state ref = ref STATE_STREXPR
  let after_comment_state : lexer_state ref = ref STATE_STREXPR

  let ignore_space : bool ref = ref true
  let openqtdepth : int ref = ref 0
  let numdepth : int ref = ref 0
  let strdepth : int ref = ref 0
  let numdepth_stack : (int Stacklist.t) ref = ref Stacklist.empty
  let strdepth_stack : (int Stacklist.t) ref = ref Stacklist.empty

  let increment rfn = ( rfn := !rfn + 1 )

  let decrement rfn = ( rfn := !rfn - 1 )

  let error_reporting lexbuf errmsg =
    let column_from = (Lexing.lexeme_start lexbuf) - (!end_of_previousline) in
    let column_to = (Lexing.lexeme_end lexbuf) - (!end_of_previousline) in
      (errmsg ^ " (line " ^ (string_of_int !line_no) ^ ", column "
        ^ (string_of_int column_from) ^ "-" ^ (string_of_int column_to) ^ ")")

  let increment_line lexbuf =
    end_of_previousline := (Lexing.lexeme_end lexbuf) ;
    line_no := !line_no + 1

  let rec increment_line_for_each_break lexbuf str num =
    if num >= String.length str then () else (
      ( match str.[num] with
        | '\n' -> ( increment_line lexbuf )
        | _ -> () ) ;
      increment_line_for_each_break lexbuf str (num + 1)
    )

}

let space = [' ' '\t']
let break = ['\n']
let digit = ['0'-'9']
let latin = (['a'-'z'] | ['A'-'Z'])
let identifier = (latin (digit | latin | "-")*)
rule numexpr = parse
  | "%" {
      after_comment_state := STATE_NUMEXPR ;
      next_state := STATE_COMMENT ;
      IGNORED
    }
  | space { numexpr lexbuf }
  | break {
      end_of_previousline := (Lexing.lexeme_end lexbuf) ;
      line_no := !line_no + 1 ;
      numexpr lexbuf
    }
  | "(" { increment numdepth ; LPAREN }
  | ")" {
      decrement numdepth ;
      if Stacklist.is_empty numdepth_stack then
        RPAREN
      else
        if !numdepth == Stacklist.top numdepth_stack then
        ( Stacklist.delete_top numdepth_stack ;
          next_state := STATE_ACTIVE ;
          CLOSENUM )
        else
          RPAREN
    }
  | "{" (break | space)* {
      Stacklist.push strdepth_stack !strdepth ;
      increment strdepth ;
      next_state := STATE_STREXPR ;
      OPENSTR
    }
  | "`"+ {
      openqtdepth := String.length (Lexing.lexeme lexbuf) ;
      after_literal_state := STATE_NUMEXPR ;
      next_state := STATE_LITERAL ;
      OPENQT
    }
  | ("@" identifier) { (* STRVAR(_) in numeric expression *)
  	    let tok = Lexing.lexeme lexbuf in STRVAR(tok)
      }
  | ("\\" identifier) { (* CTRLSEQ(_) in numeric expression *)
  	    let tok = Lexing.lexeme lexbuf in CTRLSEQ(tok)
  	  }
  | "+" { PLUS }
  | "-" { MINUS }
  | "*" { TIMES }
  | "/" { DIVIDES }
  | "==" { EQ }
  | "=" { DEFEQ }
  | "<>" { NEQ }
  | "<=" { LEQ }
  | "<" { LT }
  | ">=" { GEQ }
  | ">" { GT }
  | "&&" { LAND }
  | "||" { LOR }
  | "^" { CONCAT }
  | "->" { ARROW }
  | "mod" { MOD }
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | "let" { LET }
  | "in" { IN }
  | "function" { LAMBDA }

  | (latin (digit | latin |"-")*) as tok { NUMVAR(tok) }
  | (digit digit*) as tok { NUMCONST(tok) }
  | eof { EOI }
  | _ {
        let tok = Lexing.lexeme lexbuf in
          raise (LexError(error_reporting lexbuf ("unexpected token '" ^ tok ^ "' in numeric expression")))
      }

and strexpr = parse
  | "%" {
      after_comment_state := STATE_STREXPR ;
      next_state := STATE_COMMENT ;
      IGNORED
    }
  | "{" { increment strdepth ; BGRP }
  | ((break | space)* "}") {
      decrement strdepth ;
      increment_line_for_each_break lexbuf (Lexing.lexeme lexbuf) 0 ;
      if !strdepth == Stacklist.top strdepth_stack then (
        Stacklist.delete_top strdepth_stack ;
        next_state := STATE_NUMEXPR ;
        CLOSESTR
      ) else (
        EGRP
      )
    }
  | break {
      increment_line lexbuf ;
      if !ignore_space then strexpr lexbuf else ( ignore_space := true ; BREAK )
    }
  | space {
      if !ignore_space then strexpr lexbuf else ( ignore_space := true ; SPACE )

    }
  | ("\\" identifier) {
      let tok = Lexing.lexeme lexbuf in
        next_state := STATE_ACTIVE ;
        CTRLSEQ(tok)
    }
  | ("@" identifier) {
      let tok = Lexing.lexeme lexbuf in
        next_state := STATE_ACTIVE ;
        STRVAR(tok)
    }
  | "`"+ {
      openqtdepth := String.length (Lexing.lexeme lexbuf) ;
      after_literal_state := STATE_STREXPR ;
      next_state := STATE_LITERAL ;
      OPENQT
    }
  | eof { raise (LexError(error_reporting lexbuf "input ended while reading string expression")) }
  | _ {
      ignore_space := false ;
      let tok = Lexing.lexeme lexbuf in CHAR(tok)
    }

and active = parse
  | "%" {
      after_comment_state := STATE_ACTIVE ;
      next_state := STATE_COMMENT ;
      IGNORED
    }
  | space { active lexbuf }
  | break { increment_line lexbuf ; active lexbuf }
  | ("#" identifier) { let tok = Lexing.lexeme lexbuf in IDNAME(tok) }
  | ("." identifier) { let tok = Lexing.lexeme lexbuf in CLASSNAME(tok) }
  | "(" {
      Stacklist.push numdepth_stack !numdepth ;
      increment numdepth ;
      next_state := STATE_NUMEXPR ;
      OPENNUM
    }
  | "{" {
      increment strdepth ;
      next_state := STATE_STREXPR ;
      BGRP
    }
  | "`"+ {
      openqtdepth := String.length (Lexing.lexeme lexbuf) ;
      after_literal_state := STATE_STREXPR ;
      next_state := STATE_LITERAL ;
      OPENQT
    }
  | ";" {
      next_state := STATE_STREXPR ;
      END
    }
  | eof {
      raise (LexError(error_reporting lexbuf "input ended while reading literal area"))
    }
  | _ {
      let tok = Lexing.lexeme lexbuf in
        raise (LexError(error_reporting lexbuf ("unexpected token '" ^ tok ^ "' in active area")))
    }

and literal = parse
  | "`"+ {
      let tok = Lexing.lexeme lexbuf in
      let len = String.length tok in
        if len < !openqtdepth then
          CHAR(tok)
        else if len > !openqtdepth then
          raise (LexError(error_reporting lexbuf "literal area was closed with too many '`'s"))
        else
        ( next_state := !after_literal_state ; CLOSEQT )
    }
  | _ { let tok = Lexing.lexeme lexbuf in CHAR(tok) }

and comment = parse
  | break {
      increment_line lexbuf ;
      next_state := !after_comment_state ;
      IGNORED
    }
  | eof { EOI }
  | _ { comment lexbuf }

{
  let rec cut_token lexbuf =
    let output =
      match !next_state with
      | STATE_NUMEXPR -> numexpr lexbuf
      | STATE_STREXPR -> strexpr lexbuf
      | STATE_ACTIVE -> active lexbuf
      | STATE_COMMENT -> comment lexbuf
      | STATE_LITERAL -> literal lexbuf
    in
      match output with
      | IGNORED -> cut_token lexbuf
      | _ -> output

  (* for test *)
  let rec make_token_list lexbuf =
    let output = cut_token lexbuf in
      match output with
      | EOI -> [EOI]
      | _ -> output :: (make_token_list lexbuf)

  (* for test *)
  let token_list_of_string instr =
    let lexbuf = Lexing.from_string instr in
      line_no := 1 ;
      end_of_previousline := 0 ;

      numdepth := 0 ;
      strdepth := 0 ;
      numdepth_stack := Stacklist.empty ;
      strdepth_stack := Stacklist.empty ;
      openqtdepth := 0 ;

      next_state := STATE_NUMEXPR ;

      make_token_list lexbuf
}
