{
  open Types
  open Parser

  exception LexError of string

  let line_no : int ref = ref 1
  let end_of_previousline : int ref = ref 0

  type lexer_state = STATE_NUMEXPR | STATE_STREXPR | STATE_ACTIVE | STATE_COMMENT | STATE_LITERAL
  let next_state : lexer_state ref = ref STATE_NUMEXPR
  let first_state : lexer_state ref = ref STATE_NUMEXPR
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

  let get_start_pos lexbuf = (Lexing.lexeme_start lexbuf) - !end_of_previousline
  let get_end_pos lexbuf = (Lexing.lexeme_end lexbuf) - !end_of_previousline

  let error_reporting lexbuf errmsg =
    let column_from = get_start_pos lexbuf in
    let column_to = get_end_pos lexbuf in
      "at line " ^ (string_of_int !line_no) ^ ", column "
        ^ (string_of_int column_from) ^ "-" ^ (string_of_int column_to) ^ ":\n    " ^ errmsg

  let get_pos lexbuf =
    let column_from = get_start_pos lexbuf in
    let column_to = get_end_pos lexbuf in
      (!line_no, column_from, column_to)

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

  let reset_to_numexpr () =
    ( first_state := STATE_NUMEXPR ;
      next_state := !first_state ;
      ignore_space := true ;
      line_no := 1 ;
      openqtdepth := 0 ;
      numdepth := 0 ;
      strdepth := 0 ;
      numdepth_stack := Stacklist.empty ;
      strdepth_stack := Stacklist.empty
    )
  let reset_to_strexpr () =
    ( first_state := STATE_STREXPR ;
      next_state := !first_state ;
      ignore_space := true ;
      line_no := 1 ;
      openqtdepth := 0 ;
      numdepth := 0 ;
      strdepth := 0 ;
      numdepth_stack := Stacklist.empty ;
      strdepth_stack := Stacklist.empty
    )

}

let space = [' ' '\t']
let break = ['\n']
let digit = ['0'-'9']
let latin = ( ['a'-'z'] | ['A'-'Z'] )
let identifier = (latin (digit | latin | "-")*)
let symbol = ( [' '-'@'] | ['['-'`'] | ['{'-'~'] )
let str = [^ ' ' '\t' '\n' '@' '`' '\\' '{' '}' '%' '|']
rule numexpr = parse
  | "%" {
      after_comment_state := STATE_NUMEXPR ;
      next_state := STATE_COMMENT ;
      IGNORED
    }
  | space { numexpr lexbuf }
  | break {
      increment_line lexbuf ;
      numexpr lexbuf
    }
  | ("(" (space | break)* ")") { UNITVALUE(get_pos lexbuf) }
  | "(" { increment numdepth ; LPAREN(get_pos lexbuf) }
  | ")" {
      decrement numdepth ;
      if Stacklist.is_empty numdepth_stack then
        RPAREN(get_pos lexbuf)
      else
        if !numdepth == Stacklist.top numdepth_stack then
        ( Stacklist.delete_top numdepth_stack ;
          next_state := STATE_ACTIVE ;
          CLOSENUM(get_pos lexbuf) )
        else
          RPAREN(get_pos lexbuf)
    }
  | "[" { BLIST(get_pos lexbuf) }
  | "]" { ELIST(get_pos lexbuf) }
  | ";" { LISTPUNCT(get_pos lexbuf) }
  | "{" {
      Stacklist.push strdepth_stack !strdepth ;
      increment strdepth ;
      next_state := STATE_STREXPR ;
      ignore_space := true ;
      OPENSTR(get_pos lexbuf)
    }
  | "`"+ {
      openqtdepth := String.length (Lexing.lexeme lexbuf) ;
      after_literal_state := STATE_NUMEXPR ;
      next_state := STATE_LITERAL ;
      OPENQT(get_pos lexbuf)
    }
  | ("\\" identifier) {
  	    let tok = Lexing.lexeme lexbuf in CTRLSEQ(get_pos lexbuf, tok)
  	  }
  | "+"   { PLUS(get_pos lexbuf) }
  | "-"   { MINUS(get_pos lexbuf) }
  | "*"   { TIMES(get_pos lexbuf) }
  | "/"   { DIVIDES(get_pos lexbuf) }
  | "=="  { EQ(get_pos lexbuf) }
  | "="   { DEFEQ(get_pos lexbuf) }
  | "<>"  { NEQ(get_pos lexbuf) }
  | "<="  { LEQ(get_pos lexbuf) }
  | "<"   { LT(get_pos lexbuf) }
  | ">="  { GEQ(get_pos lexbuf) }
  | ">"   { GT(get_pos lexbuf) }
  | "&&"  { LAND(get_pos lexbuf) }
  | "||"  { LOR(get_pos lexbuf) }
  | "^"   { CONCAT(get_pos lexbuf) }
  | "->"  { ARROW(get_pos lexbuf) }
  | "<-"  { OVERWRITEEQ(get_pos lexbuf) }
  | "<<-" { OVERWRITEGLOBALHASH(get_pos lexbuf) }
  | "!"   { REFNOW(get_pos lexbuf) }
  | "!!"  { REFFINAL(get_pos lexbuf) }
  | "::"  { CONS(get_pos lexbuf) }
  | ","   { COMMA(get_pos lexbuf) }
  | "|"   { BAR(get_pos lexbuf) }
  | "_"   { WILDCARD(get_pos lexbuf) }

  | (latin (digit | latin |"-")*) {
        let tok = Lexing.lexeme lexbuf in
        let pos = get_pos lexbuf in
        ( match tok with
          | "not"      -> LNOT(pos)
          | "mod"      -> MOD(pos)
          | "if"       -> IF(pos)
          | "then"     -> THEN(pos)
          | "else"     -> ELSE(pos)
          | "let"      -> LET(pos)
          | "and"      -> LETAND(pos)
          | "in"       -> IN(pos)
          | "function" -> LAMBDA(pos)
          | "true"     -> TRUE(pos)
          | "false"    -> FALSE(pos)
          | "before"   -> BEFORE(pos)
          | "while"    -> WHILE(pos)
          | "do"       -> DO(pos)
          | "finish"   -> FINISH(pos)
          | "mutual"      -> MUTUAL(pos)
          | "end-mutual"  -> ENDMUTUAL(pos)
          | "if-class-is-valid"   -> IFCLASSISVALID(pos)
          | "if-id-is-valid"      -> IFIDISVALID(pos)
          | "let-mutable"         -> LETMUTABLE(pos)
          | "new-global-hash"     -> NEWGLOBALHASH(pos)
          | "renew-global-hash"   -> RENEWGLOBALHASH(pos)
          | "match"    -> MATCH(pos)
          | "with"     -> WITH(pos)
          | "when"     -> WHEN(pos)
          | "as"       -> AS(pos)
          | _          -> VAR(pos, tok)
        )
      }
  | (digit digit*) {
        let tok = Lexing.lexeme lexbuf in NUMCONST(get_pos lexbuf, tok)
      }
  | eof {
        if !first_state == STATE_NUMEXPR then
          EOI
        else
          raise (LexError(error_reporting lexbuf ("input ended while reading numeric expression")))
      }
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
  | ((break | space)* "{") {
      increment_line_for_each_break lexbuf (Lexing.lexeme lexbuf) 0 ;
      increment strdepth ;
      ignore_space := true ;
      BGRP(get_pos lexbuf)
    }
  | ((break | space)* "}") {
      decrement strdepth ;
      increment_line_for_each_break lexbuf (Lexing.lexeme lexbuf) 0 ;
      if Stacklist.is_empty strdepth_stack then
      ( ignore_space := false ;
        EGRP(get_pos lexbuf) )
      else
        if !strdepth == Stacklist.top strdepth_stack then
        ( Stacklist.delete_top strdepth_stack ;
          next_state := STATE_NUMEXPR ;
          CLOSESTR(get_pos lexbuf) )
        else
        ( ignore_space := false ;
          EGRP(get_pos lexbuf) )
    }
  | ((break | space)* "|") {
      increment_line_for_each_break lexbuf (Lexing.lexeme lexbuf) 0 ;
      ignore_space := true ;
      SEP(get_pos lexbuf)
    }
  | break {
      increment_line lexbuf ;
      if !ignore_space then strexpr lexbuf else ( ignore_space := true ; BREAK(get_pos lexbuf) )
    }
  | space {
      if !ignore_space then strexpr lexbuf else ( ignore_space := true ; SPACE(get_pos lexbuf) )

    }
  | ("\\" identifier) {
      let tok = Lexing.lexeme lexbuf in
        next_state := STATE_ACTIVE ;
        CTRLSEQ(get_pos lexbuf, tok)
    }
  | ("\\" symbol) {
      let tok = String.sub (Lexing.lexeme lexbuf) 1 1 in CHAR(get_pos lexbuf, tok)
    }
  | ("@" identifier) {
        let tok = Lexing.lexeme lexbuf in
        let vnm = String.sub tok 1 ((String.length tok) - 1) in
        ( next_state := STATE_ACTIVE ;
          VARINSTR(get_pos lexbuf, vnm)
        )
    }
  | ((break | space)* ("`"+ as openqtstr)) {
      increment_line_for_each_break lexbuf (Lexing.lexeme lexbuf) 0 ;
      openqtdepth := String.length openqtstr ;
      after_literal_state := STATE_STREXPR ;
      next_state := STATE_LITERAL ;
      OPENQT(get_pos lexbuf)
    }
  | eof {
      if !first_state == STATE_STREXPR then
        EOI
      else
        raise (LexError(error_reporting lexbuf "input ended while reading string expression"))
    }
  | str+ {
      ignore_space := false ;
      let tok = Lexing.lexeme lexbuf in CHAR(get_pos lexbuf, tok)
    }

and active = parse
  | "%" {
      after_comment_state := STATE_ACTIVE ;
      next_state := STATE_COMMENT ;
      IGNORED
    }
  | space { active lexbuf }
  | break { increment_line lexbuf ; active lexbuf }
  | ("#" identifier) { let tok = Lexing.lexeme lexbuf in IDNAME(get_pos lexbuf, tok) }
  | ("." identifier) { let tok = Lexing.lexeme lexbuf in CLASSNAME(get_pos lexbuf, tok) }
  | "(" {
      Stacklist.push numdepth_stack !numdepth ;
      increment numdepth ;
      next_state := STATE_NUMEXPR ;
      OPENNUM(get_pos lexbuf)
    }
  | "{" {
      increment strdepth ;
      next_state := STATE_STREXPR ;
      ignore_space := true ;
      BGRP(get_pos lexbuf)
    }
  | "`"+ {
      openqtdepth := String.length (Lexing.lexeme lexbuf) ;
      ignore_space := false ;
      after_literal_state := STATE_STREXPR ;
      next_state := STATE_LITERAL ;
      OPENQT(get_pos lexbuf)
    }
  | ";" {
      next_state := STATE_STREXPR ;
      ignore_space := false ;
      END(get_pos lexbuf)
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
          CHAR(get_pos lexbuf, tok)
        else if len > !openqtdepth then
          raise (LexError(error_reporting lexbuf "literal area was closed with too many '`'s"))
        else
        ( next_state := !after_literal_state ; CLOSEQT(get_pos lexbuf) )
    }
  | "\n" { increment_line lexbuf ; CHAR(get_pos lexbuf, "\n") }
  | _ { let tok = Lexing.lexeme lexbuf in CHAR(get_pos lexbuf, tok) }

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
      | STATE_ACTIVE  -> active lexbuf
      | STATE_COMMENT -> comment lexbuf
      | STATE_LITERAL -> literal lexbuf
    in
      match output with
      | IGNORED -> cut_token lexbuf
      | _       -> output

  (* for test *)
  let rec make_token_list lexbuf =
    let output = cut_token lexbuf in
      match output with
      | EOI -> [EOI]
      | _   -> output :: (make_token_list lexbuf)

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
