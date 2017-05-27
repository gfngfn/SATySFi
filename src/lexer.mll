{
  open Types
  open Parser

  exception LexError of string

  type lexer_state = STATE_NUMEXPR | STATE_STREXPR | STATE_ACTIVE | STATE_COMMENT | STATE_LITERAL


  let line_no             : int ref = ref 1
  let end_of_previousline : int ref = ref 0

  let next_state  : lexer_state ref = ref STATE_NUMEXPR
  let first_state : lexer_state ref = ref STATE_NUMEXPR
  let after_literal_state : lexer_state ref = ref STATE_STREXPR
  let after_comment_state : lexer_state ref = ref STATE_STREXPR

  let ignore_space : bool ref = ref true
  let openqtdepth : int ref = ref 0
  let numdepth : int ref = ref 0
  let strdepth : int ref = ref 0
  let numdepth_stack : (int Stacklist.t) ref = ref Stacklist.empty
  let strdepth_stack : (int Stacklist.t) ref = ref Stacklist.empty

(*
  let increment rfn = ( rfn := !rfn + 1 )

  let decrement rfn = ( rfn := !rfn - 1 )
*)

  let get_start_pos lexbuf = (Lexing.lexeme_start lexbuf) - !end_of_previousline

  let get_end_pos lexbuf   = (Lexing.lexeme_end lexbuf) - !end_of_previousline

  let get_pos lexbuf =
    let pos_from = get_start_pos lexbuf in
    let pos_to = get_end_pos lexbuf in
      Range.make (!line_no) pos_from pos_to


  let error_reporting lexbuf errmsg =
    let column_from = get_start_pos lexbuf in
    let column_to = get_end_pos lexbuf in
      "at line " ^ (string_of_int !line_no) ^ ", column "
        ^ (string_of_int column_from) ^ "-" ^ (string_of_int column_to) ^ ":\n    " ^ errmsg


  let increment_line lexbuf =
    begin
      end_of_previousline := (Lexing.lexeme_end lexbuf) ;
      line_no := !line_no + 1
    end


  let rec increment_line_for_each_break lexbuf str num =
    if num >= String.length str then () else
      begin
        begin
          match str.[num] with
          | ( '\n' | '\r' ) -> ( increment_line lexbuf )
          | _               -> ()
        end ;
        increment_line_for_each_break lexbuf str (num + 1)
      end


  let reset_to_numexpr () =
    begin
      first_state := STATE_NUMEXPR ;
      next_state := !first_state ;
      ignore_space := true ;
      line_no := 1 ;
      end_of_previousline := 0;
      openqtdepth := 0 ;
      numdepth := 0 ;
      strdepth := 0 ;
      numdepth_stack := Stacklist.empty ;
      strdepth_stack := Stacklist.empty
    end


  let reset_to_strexpr () =
    begin
      first_state := STATE_STREXPR ;
      next_state := !first_state ;
      ignore_space := true ;
      line_no := 1 ;
      end_of_previousline := 0;
      openqtdepth := 0 ;
      numdepth := 0 ;
      strdepth := 0 ;
      numdepth_stack := Stacklist.empty ;
      strdepth_stack := Stacklist.empty
    end


  let split_module_list tokstr =
    let rec aux imax i acclst accstr =
      if i >= imax then (List.rev acclst, accstr) else
        match tokstr.[i] with
        | '.' -> aux imax (i + 1) (accstr :: acclst) ""
        | c   -> aux imax (i + 1) acclst (accstr ^ (String.make 1 c))
    in
      aux (String.length tokstr) 0 [] ""
}

let space = [' ' '\t']
let break = ['\n' '\r']
let digit = ['0'-'9']
let capital = ['A'-'Z']
let small = ['a'-'z']
let latin = ( small | capital )
let item  = "*"+
let identifier = (small (digit | latin | "-")*)
let constructor = (capital (digit | latin | "-")*)
let symbol = ( [' '-'@'] | ['['-'`'] | ['{'-'~'] )
let str = [^ ' ' '\t' '\n' '\r' '@' '`' '\\' '{' '}' '%' '|' '*']
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
  | "(" { incr numdepth ; LPAREN(get_pos lexbuf) }
  | ")" {
      decr numdepth ;
      if Stacklist.is_empty numdepth_stack then
        RPAREN(get_pos lexbuf)
      else
        if !numdepth = Stacklist.top numdepth_stack then
        begin
          Stacklist.delete_top numdepth_stack ;
          next_state := STATE_ACTIVE ;
          CLOSENUM(get_pos lexbuf)
        end
        else
          RPAREN(get_pos lexbuf)
    }
  | "(|" { incr numdepth ; BRECORD(get_pos lexbuf) }
  | "|)" {
        decr numdepth ;
        if Stacklist.is_empty numdepth_stack then
          ERECORD(get_pos lexbuf)
        else
          if !numdepth = Stacklist.top numdepth_stack then
          begin
            Stacklist.delete_top numdepth_stack ;
            next_state := STATE_ACTIVE ;
            CLOSENUM_AND_ERECORD(get_pos lexbuf)
          end
          else
            ERECORD(get_pos lexbuf)
      }
  | "[" { incr numdepth ; BLIST(get_pos lexbuf) }
  | "]" {
        decr numdepth ;
        if Stacklist.is_empty numdepth_stack then
          ELIST(get_pos lexbuf)
        else
          if !numdepth = Stacklist.top numdepth_stack then
          begin
            Stacklist.delete_top numdepth_stack ;
            next_state := STATE_ACTIVE ;
            CLOSENUM_AND_ELIST(get_pos lexbuf)
          end
          else
            ELIST(get_pos lexbuf)
         }
  | ";" { LISTPUNCT(get_pos lexbuf) }
  | "{" {
      Stacklist.push strdepth_stack !strdepth ;
      incr strdepth ;
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
  | ("\\" (identifier | constructor)) {
        let tok = Lexing.lexeme lexbuf in CTRLSEQ(get_pos lexbuf, tok)
      }
  | "#"   { ACCESS(get_pos lexbuf) }
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
  | "."   { DOT(get_pos lexbuf) }
  | ":"   { COLON(get_pos lexbuf) }

  | ("'" (identifier as xpltyvarnm)) { TYPEVAR(get_pos lexbuf, xpltyvarnm) }

  | ((constructor ".")+ identifier) {
        let tokstr = Lexing.lexeme lexbuf in
        let pos = get_pos lexbuf in
        let (mdlnmlst, varnm) = split_module_list tokstr in
          VARWITHMOD(pos, mdlnmlst, varnm)
      }

  | identifier {
        let tokstr = Lexing.lexeme lexbuf in
        let pos = get_pos lexbuf in
          match tokstr with
          | "not"               -> LNOT(pos)
          | "mod"               -> MOD(pos)
          | "if"                -> IF(pos)
          | "then"              -> THEN(pos)
          | "else"              -> ELSE(pos)
          | "let"               -> LET(pos)
          | "and"               -> LETAND(pos)
          | "in"                -> IN(pos)
          | "function"          -> LAMBDA(pos)
          | "true"              -> TRUE(pos)
          | "false"             -> FALSE(pos)
          | "before"            -> BEFORE(pos)
          | "while"             -> WHILE(pos)
          | "do"                -> DO(pos)
          | "let-mutable"       -> LETMUTABLE(pos)
          | "let-lazy"          -> LETLAZY(pos)
          | "new-global-hash"   -> NEWGLOBALHASH(pos)
          | "renew-global-hash" -> RENEWGLOBALHASH(pos)
          | "match"             -> MATCH(pos)
          | "with"              -> WITH(pos)
          | "when"              -> WHEN(pos)
          | "as"                -> AS(pos)
          | "type"              -> TYPE(pos)
          | "of"                -> OF(pos)
          | "module"            -> MODULE(pos)
          | "struct"            -> STRUCT(pos)
          | "sig"               -> SIG(pos)
          | "val"               -> VAL(pos)
          | "end"               -> END(pos)
          | "direct"            -> DIRECT(pos)
          | _                   -> VAR(pos, tokstr)
      }
  | constructor { CONSTRUCTOR(get_pos lexbuf, Lexing.lexeme lexbuf) }
  | (digit digit*) { NUMCONST(get_pos lexbuf, Lexing.lexeme lexbuf) }
  | eof {
        if !first_state = STATE_NUMEXPR then EOI else
          raise (LexError(error_reporting lexbuf ("text input ended while reading a program area")))
      }
  | _ as c { raise (LexError(error_reporting lexbuf ("illegal token '" ^ (String.make 1 c) ^ "' in a program area"))) }

and strexpr = parse
  | "%" {
      after_comment_state := STATE_STREXPR ;
      ignore_space := true ;
      next_state := STATE_COMMENT ;
      IGNORED
    }
  | ((break | space)* "{") {
      increment_line_for_each_break lexbuf (Lexing.lexeme lexbuf) 0 ;
      incr strdepth ;
      ignore_space := true ;
      BGRP(get_pos lexbuf)
    }
  | ((break | space)* "}") {
      decr strdepth ;
      increment_line_for_each_break lexbuf (Lexing.lexeme lexbuf) 0 ;
      if Stacklist.is_empty strdepth_stack then
        begin
          ignore_space := false ;
          EGRP(get_pos lexbuf)
        end
      else
        if !strdepth = Stacklist.top strdepth_stack then
          begin
            Stacklist.delete_top strdepth_stack ;
            next_state := STATE_NUMEXPR ;
            CLOSESTR(get_pos lexbuf)
          end
        else
          begin
            ignore_space := false ;
            EGRP(get_pos lexbuf)
          end
    }
  | ((break | space)* "|") {
      increment_line_for_each_break lexbuf (Lexing.lexeme lexbuf) 0 ;
      ignore_space := true ;
      SEP(get_pos lexbuf)
    }
  | break {
      increment_line lexbuf ;
      if !ignore_space then strexpr lexbuf else begin ignore_space := true ; BREAK(get_pos lexbuf) end
    }
  | space {
      if !ignore_space then strexpr lexbuf else begin ignore_space := true ; SPACE(get_pos lexbuf) end
    }
  | ((break | space)* (item as itemstr)) {
      increment_line_for_each_break lexbuf (Lexing.lexeme lexbuf) 0 ;
      ignore_space := true ;
      ITEM(get_pos lexbuf, String.length itemstr)
    }
  | ("\\" (identifier | constructor)) {
      let tok = Lexing.lexeme lexbuf in
      let rng = get_pos lexbuf in
        next_state := STATE_ACTIVE ;
        CTRLSEQ(rng, tok)
    }
  | ("\\" (constructor ".")* (identifier | constructor)) {
      let tokstrpure = Lexing.lexeme lexbuf in
      let tokstr = String.sub tokstrpure 1 ((String.length tokstrpure) - 1) in
      let (mdlnmlst, csnm) = split_module_list tokstr in
      let rng = get_pos lexbuf in
        next_state := STATE_ACTIVE ;
        CTRLSEQWITHMOD(rng, mdlnmlst, "\\" ^ csnm)
    }
  | ("\\" symbol) {
      let tok = String.sub (Lexing.lexeme lexbuf) 1 1 in
        begin
          ignore_space := false ;
          CHAR(get_pos lexbuf, tok)
        end
    }
  | ("@" identifier) {
        let tok = Lexing.lexeme lexbuf in
        let vnm = String.sub tok 1 ((String.length tok) - 1) in
          begin
            next_state := STATE_ACTIVE ;
            VARINSTR(get_pos lexbuf, vnm)
          end
    }
  | ((break | space)* ("`"+ as openqtstr)) {
      increment_line_for_each_break lexbuf (Lexing.lexeme lexbuf) 0 ;
      openqtdepth := String.length openqtstr ;
      after_literal_state := STATE_STREXPR ;
      next_state := STATE_LITERAL ;
      OPENQT(get_pos lexbuf)
    }
  | eof {
      if !first_state = STATE_STREXPR then EOI else
        raise (LexError(error_reporting lexbuf "program input ended while reading a text area"))
    }
  | str+ {
      ignore_space := false ;
      let tok = Lexing.lexeme lexbuf in CHAR(get_pos lexbuf, tok)
    }

  | _ as c { raise (LexError(error_reporting lexbuf "illegal token '" ^ (String.make 1 c) ^ "' in a text area"))}

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
      incr numdepth ;
      next_state := STATE_NUMEXPR ;
      OPENNUM(get_pos lexbuf)
    }
  | "(|" {
      Stacklist.push numdepth_stack !numdepth ;
      incr numdepth ;
      next_state := STATE_NUMEXPR ;
      OPENNUM_AND_BRECORD(get_pos lexbuf)
    }
  | "[" {
      Stacklist.push numdepth_stack !numdepth ;
      incr numdepth ;
      next_state := STATE_NUMEXPR ;
      OPENNUM_AND_BLIST(get_pos lexbuf)
    }
  | "{" {
      incr strdepth ;
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
      ENDACTIVE(get_pos lexbuf)
    }
  | eof {
      raise (LexError(error_reporting lexbuf "input ended while reading active area"))
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
          begin next_state := !after_literal_state ; CLOSEQT(get_pos lexbuf) end
    }
  | break { increment_line lexbuf ; CHAR(get_pos lexbuf, "\n") }
  | eof {
      raise (LexError(error_reporting lexbuf "input ended while reading literal area"))
    }
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

}
