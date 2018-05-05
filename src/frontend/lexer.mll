{
  open Types
  open Parser

  (*
   * The SATySFi lexer is stateful; the transitions are:
   * | to \ from |program|block |inline|active |  math  |
   * |-----------|-------|------|------|-------|--------|
   * |  program  | (   ) |      |      | (   ) | !(   ) |
   * |           | (| |) |      |      | (| |) | !(| |) |
   * |           | [   ] |      |      | [   ] | ![   ] |
   * |  block    | '<  > | <  > | <  > | <     | !<   > |
   * |  inline   | {   } | {  } | {  } | {     | !{   } |
   * |  active   |       | +x ; | \x ; |       |        |
   * |           |       | #x ; | #x ; |       |        |
   * |  math     | ${  } |      | ${ } |       | {    } |
   *
   * Note that the active-block and active-inline transitions are one-way.
   *)
  type lexer_state =
    | ProgramState (* program mode *)
    | VerticalState (* block mode *)
    | HorizontalState (* inline mode *)
    | ActiveState (* active mode *)
    | MathState (* math mode *)

  let get_pos lexbuf =
    let posS = Lexing.lexeme_start_p lexbuf in
    let posE = Lexing.lexeme_end_p lexbuf in
    let lnum = posS.Lexing.pos_lnum in
    let cnumS = posS.Lexing.pos_cnum - posS.Lexing.pos_bol in
    let cnumE = posE.Lexing.pos_cnum - posE.Lexing.pos_bol in
      Range.make lnum cnumS cnumE

  let record_error error_store lexbuf errmsg =
    let open ErrorReporting in
    let rng = get_pos lexbuf in
    record_error error_store Lexer [
      NormalLine("at " ^ (Range.to_string rng) ^ ":");
      NormalLine(errmsg);
    ]

  let pop error_store lexbuf errmsg stack =
    if Stack.length stack > 1 then
      Stack.pop stack |> ignore
    else
      record_error error_store lexbuf errmsg

  let increment_line lexbuf =
    begin
      Lexing.new_line lexbuf;
    end


  let rec increment_line_for_each_break lexbuf str =
    let len = String.length str in
    let rec aux num =
      if num >= len then () else
        begin
          begin
            match String.get str num with
            | ( '\n' | '\r' ) -> increment_line lexbuf
            | _               -> ()
          end;
          aux (num + 1)
        end
    in
      aux 0


  let initialize state =
    let stack = Stack.create () in
    Stack.push state stack;
    stack


  let reset_to_progexpr () =
    initialize ProgramState


  let reset_to_vertexpr () =
    initialize VerticalState


  let split_module_list tokstr =
    let rec aux imax i acclst accstr =
      if i >= imax then (List.rev acclst, accstr) else
        match String.get tokstr i with
        | '.' -> aux imax (i + 1) (accstr :: acclst) ""
        | c   -> aux imax (i + 1) acclst (accstr ^ (String.make 1 c))
    in
      aux (String.length tokstr) 0 [] ""
}

let space = [' ' '\t']
let break = ['\n' '\r']
let nonbreak = [^ '\n' '\r']
let nzdigit = ['1'-'9']
let digit = (nzdigit | "0")
let hex   = (digit | ['A'-'F'])
let capital = ['A'-'Z']
let small = ['a'-'z']
let latin = (small | capital)
let item  = "*"+
let identifier = (small (digit | latin | "-")*)
let constructor = (capital (digit | latin | "-")*)
let symbol = ( [' '-'@'] | ['['-'`'] | ['{'-'~'] )
let opsymbol = ( '+' | '-' | '*' | '/' | '^' | '&' | '|' | '!' | ':' | '=' | '<' | '>' | '~' | '\'' | '.' | '?' )
let str = [^ ' ' '\t' '\n' '\r' '@' '`' '\\' '{' '}' '<' '>' '%' '|' '*' '$' '#' ';']
let mathsymbol = ( '+' | '-' | '*' | '/' | ':' | '=' | '<' | '>' | '~' | '\'' | '.' | ',' | '?' | '`' )

rule progexpr error_store stack = parse
  | "%" {
      comment lexbuf;
      progexpr error_store stack lexbuf
    }
  | ("@" (identifier as headertype) ":" (" "*) (nonbreak* as content) (break | eof)) {
      let pos = get_pos lexbuf in
      increment_line lexbuf;
      match headertype with
      | "require" -> HEADER_REQUIRE(pos, content)
      | "import"  -> HEADER_IMPORT(pos, content)
      | _         ->
          record_error error_store lexbuf ("undefined header type '" ^ headertype ^ "'");
          progexpr error_store stack lexbuf
    }
  | space { progexpr error_store stack lexbuf }
  | break {
      increment_line lexbuf;
      progexpr error_store stack lexbuf
    }
  | "(" { Stack.push ProgramState stack; LPAREN(get_pos lexbuf) }
  | ")" {
      let pos = get_pos lexbuf in
      pop error_store lexbuf "too many closing" stack;
      RPAREN(pos)
    }
  | "(|" { Stack.push ProgramState stack; BRECORD(get_pos lexbuf) }
  | "|)" {
      let pos = get_pos lexbuf in
      pop error_store lexbuf "too many closing" stack;
      ERECORD(pos)
    }
  | "[" { Stack.push ProgramState stack; BLIST(get_pos lexbuf) }
  | "]" {
      let pos = get_pos lexbuf in
      pop error_store lexbuf "too many closing" stack;
      ELIST(pos)
     }
  | ";" { LISTPUNCT(get_pos lexbuf) }
  | "{" {
      Stack.push HorizontalState stack;
      skip_spaces lexbuf;
      BHORZGRP(get_pos lexbuf)
    }
  | "'<" {
      Stack.push VerticalState stack;
      BVERTGRP(get_pos lexbuf)
    }
  | "${" {
      Stack.push MathState stack;
      BMATHGRP(get_pos lexbuf)
    }
  | "<[" { BPATH(get_pos lexbuf) }
  | "]>" { EPATH(get_pos lexbuf) }
  | ".." { PATHCURVE(get_pos lexbuf) }
  | "--" { PATHLINE(get_pos lexbuf) }  (* -- prior to BINOP_MINUS -- *)
  | "`"+ {
      let quote_range = get_pos lexbuf in
      let quote_length = String.length (Lexing.lexeme lexbuf) in
      let buffer = Buffer.create 256 in
      literal error_store quote_range quote_length buffer lexbuf
    }
  | ("\\" (constructor ".")* (identifier | constructor)) {
      let tokstrpure = Lexing.lexeme lexbuf in
      let tokstr = String.sub tokstrpure 1 ((String.length tokstrpure) - 1) in
      let (mdlnmlst, csnm) = split_module_list tokstr in
      HORZCMD(get_pos lexbuf, mdlnmlst, "\\" ^ csnm)
    }
  | ("+" (constructor ".")* (identifier | constructor)) {
      let tokstrpure = Lexing.lexeme lexbuf in
      let tokstr = String.sub tokstrpure 1 ((String.length tokstrpure) - 1) in
      let (mdlnmlst, csnm) = split_module_list tokstr in
      VERTCMD(get_pos lexbuf, mdlnmlst, "+" ^ csnm)
    }
  | "#"   { ACCESS(get_pos lexbuf) }
  | "->"  { ARROW(get_pos lexbuf) }
  | "<-"  { OVERWRITEEQ(get_pos lexbuf) }
  | "|"   { BAR(get_pos lexbuf) }
  | "_"   { WILDCARD(get_pos lexbuf) }
  | "."   { DOT(get_pos lexbuf) }
  | ":"   { COLON(get_pos lexbuf) }
  | ","   { COMMA(get_pos lexbuf) }
  | "::"  { CONS(get_pos lexbuf) }
  | "-"   { EXACT_MINUS(get_pos lexbuf) }
  | "="   { DEFEQ(get_pos lexbuf) }
  | "*"   { EXACT_TIMES(get_pos lexbuf) }

(* -- binary operators; should be extended -- *)
  | ("+" opsymbol*) { BINOP_PLUS(get_pos lexbuf, Lexing.lexeme lexbuf) }
  | ("-" opsymbol+) { BINOP_MINUS(get_pos lexbuf, Lexing.lexeme lexbuf) }
  | ("*" opsymbol+) { BINOP_TIMES(get_pos lexbuf, Lexing.lexeme lexbuf) }
  | ("/" opsymbol*) { BINOP_DIVIDES(get_pos lexbuf, Lexing.lexeme lexbuf) }
  | ("=" opsymbol+) { BINOP_EQ(get_pos lexbuf, Lexing.lexeme lexbuf) }
  | ("<" opsymbol*) { BINOP_LT(get_pos lexbuf, Lexing.lexeme lexbuf) }
  | (">" opsymbol*) { BINOP_GT(get_pos lexbuf, Lexing.lexeme lexbuf) }
  | ("&" opsymbol+) { BINOP_AMP(get_pos lexbuf, Lexing.lexeme lexbuf) }
  | ("|" opsymbol+) { BINOP_BAR(get_pos lexbuf, Lexing.lexeme lexbuf) }
  | ("^" opsymbol*) { BINOP_HAT(get_pos lexbuf, Lexing.lexeme lexbuf) }
  | "?"  { OPTIONALTYPE(get_pos lexbuf) }
  | "?:" { OPTIONAL(get_pos lexbuf) }
  | "?*" { OMISSION(get_pos lexbuf) }
  | "!" { DEREF(get_pos lexbuf) }
  | ("'" (identifier as xpltyvarnm)) { TYPEVAR(get_pos lexbuf, xpltyvarnm) }

  | ((constructor ".")+ identifier) {
        let tokstr = Lexing.lexeme lexbuf in
        let pos = get_pos lexbuf in
        let (mdlnmlst, varnm) = split_module_list tokstr in
          VAR(pos, mdlnmlst, varnm)
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
          | "let"               -> LETNONREC(pos)
          | "let-rec"           -> LETREC(pos)
          | "and"               -> LETAND(pos)
          | "in"                -> IN(pos)
          | "fun"               -> LAMBDA(pos)
          | "true"              -> TRUE(pos)
          | "false"             -> FALSE(pos)
          | "before"            -> BEFORE(pos)
          | "while"             -> WHILE(pos)
          | "do"                -> DO(pos)
          | "let-mutable"       -> LETMUTABLE(pos)
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
          | "constraint"        -> CONSTRAINT(pos)
          | "let-inline"        -> LETHORZ(pos)
          | "let-block"         -> LETVERT(pos)
          | "let-math"          -> LETMATH(pos)
          | "controls"          -> CONTROLS(pos)
          | "cycle"             -> CYCLE(pos)
          | "inline-cmd"        -> HORZCMDTYPE(pos)
          | "block-cmd"         -> VERTCMDTYPE(pos)
          | "math-cmd"          -> MATHCMDTYPE(pos)
          | "command"           -> COMMAND(pos)
          | _                   -> VAR(pos, [], tokstr)
      }
  | constructor { CONSTRUCTOR(get_pos lexbuf, Lexing.lexeme lexbuf) }
  | (digit | (nzdigit digit+))                            { INTCONST(get_pos lexbuf, int_of_string (Lexing.lexeme lexbuf)) }
  | (("0x" | "0X") hex+)                                  { INTCONST(get_pos lexbuf, int_of_string (Lexing.lexeme lexbuf)) }
  | ((digit+ "." digit*) | ("." digit+))                  { FLOATCONST(get_pos lexbuf, float_of_string (Lexing.lexeme lexbuf)) }
  | (((digit | (nzdigit digit+)) as i) (identifier as unitnm))  { LENGTHCONST(get_pos lexbuf, float_of_int (int_of_string i), unitnm) }
  | (((digit+ "." digit*) as flt) (identifier as unitnm))       { LENGTHCONST(get_pos lexbuf, float_of_string flt, unitnm) }
  | ((("." digit+) as flt) (identifier as unitnm))              { LENGTHCONST(get_pos lexbuf, float_of_string flt, unitnm) }
  | eof {
      if Stack.length stack <> 1 then
        record_error error_store lexbuf "text input ended while reading a program area";
      EOI
    }
  | _ as c {
      record_error error_store lexbuf ("illegal token '" ^ (String.make 1 c) ^ "' in a program area");
      progexpr error_store stack lexbuf
    }


and vertexpr error_store stack = parse
  | "%" {
      comment lexbuf;
      vertexpr error_store stack lexbuf
    }
  | (break | space)* {
      increment_line_for_each_break lexbuf (Lexing.lexeme lexbuf);
      vertexpr error_store stack lexbuf
    }
  | ("#" (identifier as varnm)) {
      Stack.push ActiveState stack;
      VARINVERT(get_pos lexbuf, [], varnm)
    }
  | ("#" (constructor ".")* (identifier | constructor)) {
      let csnmpure = Lexing.lexeme lexbuf in
      let csstr = String.sub csnmpure 1 ((String.length csnmpure) - 1) in
      let (mdlnmlst, csnm) = split_module_list csstr in
        Stack.push ActiveState stack;
        VARINVERT(get_pos lexbuf, mdlnmlst, csnm)
    }
  | ("+" (constructor ".")* (identifier | constructor)) {
      let tokstrpure = Lexing.lexeme lexbuf in
      let tokstr = String.sub tokstrpure 1 ((String.length tokstrpure) - 1) in
      let (mdlnmlst, csnm) = split_module_list tokstr in
      Stack.push ActiveState stack;
      VERTCMD(get_pos lexbuf, mdlnmlst, "+" ^ csnm)
    }
  | "<" { Stack.push VerticalState stack; BVERTGRP(get_pos lexbuf) }
  | ">" {
      let pos = get_pos lexbuf in
      pop error_store lexbuf "too many closing" stack;
      EVERTGRP(pos)
    }
  | "{" {
      Stack.push HorizontalState stack;
      skip_spaces lexbuf;
      BHORZGRP(get_pos lexbuf)
    }
  | eof {
      if Stack.length stack <> 1 then
        record_error error_store lexbuf "unexpected end of input while reading a vertical area";
      EOI
    }
  | _ as c {
      record_error error_store lexbuf ("unexpected character '" ^ (String.make 1 c) ^ "' in a vertical area");
      vertexpr error_store stack lexbuf
    }

and horzexpr error_store stack = parse
  | "%" {
      comment lexbuf;
      skip_spaces lexbuf;
      horzexpr error_store stack lexbuf
    }
  | ((break | space)* "{") {
      increment_line_for_each_break lexbuf (Lexing.lexeme lexbuf);
      Stack.push HorizontalState stack;
      skip_spaces lexbuf;
      BHORZGRP(get_pos lexbuf)
    }
  | ((break | space)* "}") {
      increment_line_for_each_break lexbuf (Lexing.lexeme lexbuf);
      let pos = get_pos lexbuf in
      pop error_store lexbuf "too many closing" stack;
      EHORZGRP(pos)
    }
  | ((break | space)* "<") {
      increment_line_for_each_break lexbuf (Lexing.lexeme lexbuf);
      Stack.push VerticalState stack;
      BVERTGRP(get_pos lexbuf)
    }
  | ((break | space)* "|") {
      increment_line_for_each_break lexbuf (Lexing.lexeme lexbuf);
      skip_spaces lexbuf;
      SEP(get_pos lexbuf)
    }
  | break {
      increment_line lexbuf;
      skip_spaces lexbuf;
      BREAK(get_pos lexbuf)
    }
  | space {
      skip_spaces lexbuf;
      SPACE(get_pos lexbuf)
    }
  | ((break | space)* (item as itemstr)) {
      increment_line_for_each_break lexbuf (Lexing.lexeme lexbuf);
      skip_spaces lexbuf;
      ITEM(get_pos lexbuf, String.length itemstr)
    }
  | ("#" (identifier as varnm)) {
      Stack.push ActiveState stack;
      VARINHORZ(get_pos lexbuf, [], varnm)
    }
  | ("#" (constructor ".")* (identifier | constructor)) {
      let csnmpure = Lexing.lexeme lexbuf in
      let csstr = String.sub csnmpure 1 ((String.length csnmpure) - 1) in
      let (mdlnmlst, csnm) = split_module_list csstr in
        Stack.push ActiveState stack;
        VARINHORZ(get_pos lexbuf, mdlnmlst, csnm)
    }
  | ("\\" (constructor ".")* (identifier | constructor)) {
      let tokstrpure = Lexing.lexeme lexbuf in
      let tokstr = String.sub tokstrpure 1 ((String.length tokstrpure) - 1) in
      let (mdlnmlst, csnm) = split_module_list tokstr in
      Stack.push ActiveState stack;
      HORZCMD(get_pos lexbuf, mdlnmlst, "\\" ^ csnm)
    }
  | ("\\" symbol) {
      let tok = String.sub (Lexing.lexeme lexbuf) 1 1 in
        begin
          CHAR(get_pos lexbuf, tok)
        end
    }
  | ((break | space)* ("`"+ as openqtstr)) {
      increment_line_for_each_break lexbuf (Lexing.lexeme lexbuf);
      let quote_range = get_pos lexbuf in
      let quote_length = String.length openqtstr in
      let buffer = Buffer.create 256 in
      literal error_store quote_range quote_length buffer lexbuf
    }
  | "${" {
      Stack.push MathState stack;
      BMATHGRP(get_pos lexbuf)
    }
  | eof {
      if Stack.length stack <> 1 then
        record_error error_store lexbuf "unexpected end of input while reading an inline text area";
      EOI
    }
  | str+ {
      let tok = Lexing.lexeme lexbuf in CHAR(get_pos lexbuf, tok)
    }

  | _ as c {
      record_error error_store lexbuf ("illegal token '" ^ (String.make 1 c) ^ "' in an inline text area");
      horzexpr error_store stack lexbuf
    }

and mathexpr error_store stack = parse
  | space { mathexpr error_store stack lexbuf }
  | break { increment_line lexbuf; mathexpr error_store stack lexbuf }
  | "%" {
      comment lexbuf;
      mathexpr error_store stack lexbuf
    }
  | "!{" {
      Stack.push HorizontalState stack;
      skip_spaces lexbuf;
      BHORZGRP(get_pos lexbuf);
    }
  | "!<" {
      Stack.push VerticalState stack;
      BVERTGRP(get_pos lexbuf)
    }
  | "!(" {
      Stack.push ProgramState stack;
      LPAREN(get_pos lexbuf)
    }
  | "![" {
      Stack.push ProgramState stack;
      BLIST(get_pos lexbuf)
    }
  | "!(|" {
      Stack.push ProgramState stack;
      BRECORD(get_pos lexbuf)
    }
  | "{" {
      Stack.push MathState stack;
      BMATHGRP(get_pos lexbuf)
    }
  | "}" {
      let pos = get_pos lexbuf in
      pop error_store lexbuf "too many closing" stack;
      EMATHGRP(pos)
    }
  | "^" { SUPERSCRIPT(get_pos lexbuf) }
  | "_" { SUBSCRIPT(get_pos lexbuf) }
  | mathsymbol+     { MATHCHAR(get_pos lexbuf, Lexing.lexeme lexbuf) }
  | (latin | digit) { MATHCHAR(get_pos lexbuf, Lexing.lexeme lexbuf) }
  | ("#" (constructor ".")* (identifier | constructor)) {
      let csnmpure = Lexing.lexeme lexbuf in
      let csstr = String.sub csnmpure 1 ((String.length csnmpure) - 1) in
      let (mdlnmlst, csnm) = split_module_list csstr in
        VARINMATH(get_pos lexbuf, mdlnmlst, csnm)
    }
  | ("\\" (constructor ".")* (identifier | constructor)) {
      let csnmpure = Lexing.lexeme lexbuf in
      let csstr = String.sub csnmpure 1 ((String.length csnmpure) - 1) in
      let (mdlnmlst, csnm) = split_module_list csstr in
      MATHCMD(get_pos lexbuf, mdlnmlst, "\\" ^ csnm)
    }
  | ("\\" symbol) {
      let tok = String.sub (Lexing.lexeme lexbuf) 1 1 in
        MATHCHAR(get_pos lexbuf, tok)
    }
  | _ as c {
      record_error error_store lexbuf ("illegal token '" ^ (String.make 1 c) ^ "' in a math area");
      mathexpr error_store stack lexbuf
    }

  | eof {
      record_error error_store lexbuf "unexpected end of file in a math area";
      EOI
    }

and active error_store stack = parse
  | "%" {
      comment lexbuf;
      active error_store stack lexbuf
    }
  | space { active error_store stack lexbuf }
  | break { increment_line lexbuf; active error_store stack lexbuf }
  | "?:" { OPTIONAL(get_pos lexbuf) }
  | "?*" { OMISSION(get_pos lexbuf) }
  | "(" {
      Stack.push ProgramState stack;
      LPAREN(get_pos lexbuf)
    }
  | "(|" {
      Stack.push ProgramState stack;
      BRECORD(get_pos lexbuf)
    }
  | "[" {
      Stack.push ProgramState stack;
      BLIST(get_pos lexbuf)
    }
  | "{" {
      let pos = get_pos lexbuf in
      pop error_store lexbuf "BUG; this cannot happen" stack;
      Stack.push HorizontalState stack;
      skip_spaces lexbuf;
      BHORZGRP(pos)
    }
  | "<" {
      let pos = get_pos lexbuf in
      pop error_store lexbuf "BUG; this cannot happen" stack;
      Stack.push VerticalState stack;
      BVERTGRP(pos)
    }
  | ";" {
      let pos = get_pos lexbuf in
      pop error_store lexbuf "BUG; this cannot happen" stack;
      ENDACTIVE(pos)
    }
  | eof {
      record_error error_store lexbuf "unexpected end of input while reading an active area";
      EOI
    }
  | _ {
      let tok = Lexing.lexeme lexbuf in
      record_error error_store lexbuf ("unexpected token '" ^ tok ^ "' in an active area");
      active error_store stack lexbuf
    }

and literal error_store quote_range quote_length buffer = parse
  | "`"+ {
      let tok = Lexing.lexeme lexbuf in
      let len = String.length tok in
        if len < quote_length then begin
          Buffer.add_string buffer tok;
          literal error_store quote_range quote_length buffer lexbuf
        end else begin
          if len > quote_length then
            record_error error_store lexbuf "literal area was closed with too many '`'s";
          let pos = Range.unite quote_range (get_pos lexbuf) in
          LITERAL(pos, Buffer.contents buffer)
        end
    }
  | break {
      let tok = Lexing.lexeme lexbuf in
      increment_line lexbuf;
      Buffer.add_string buffer tok;
      literal error_store quote_range quote_length buffer lexbuf
    }
  | eof {
      record_error error_store lexbuf "unexpected end of input while reading literal area";
      let pos = Range.unite quote_range (get_pos lexbuf) in
      LITERAL(pos, Buffer.contents buffer)
    }
  | _ {
      let tok = Lexing.lexeme lexbuf in
      Buffer.add_string buffer tok;
      literal error_store quote_range quote_length buffer lexbuf
    }

and comment = parse
  | break {
      increment_line lexbuf;
    }
  | eof { () }
  | _ { comment lexbuf }

and skip_spaces = parse
  | break {
      increment_line lexbuf;
      skip_spaces lexbuf
    }
  | space {
      skip_spaces lexbuf
    }
  | "%" {
      comment lexbuf;
      skip_spaces lexbuf
    }
  | "" { () }

{
  let cut_token error_store stack lexbuf =
    match Stack.top stack with
    | ProgramState    -> progexpr error_store stack lexbuf
    | VerticalState   -> vertexpr error_store stack lexbuf
    | HorizontalState -> horzexpr error_store stack lexbuf
    | ActiveState     -> active error_store stack lexbuf
    | MathState       -> mathexpr error_store stack lexbuf

}
