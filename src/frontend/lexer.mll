{
  open Types
  open Parser

  exception LexError of Range.t * string

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
    | ProgramState    (* program mode *)
    | VerticalState   (* block mode *)
    | HorizontalState (* inline mode *)
    | ActiveState     (* active mode *)
    | MathState       (* math mode *)


  let get_pos lexbuf =
    let posS = Lexing.lexeme_start_p lexbuf in
    let posE = Lexing.lexeme_end_p lexbuf in
    let fname = posS.Lexing.pos_fname in
    let lnum = posS.Lexing.pos_lnum in
    let cnumS = posS.Lexing.pos_cnum - posS.Lexing.pos_bol in
    let cnumE = posE.Lexing.pos_cnum - posE.Lexing.pos_bol in
      Range.make fname lnum cnumS cnumE


  let report_error lexbuf errmsg =
    let rng = get_pos lexbuf in
      raise (LexError(rng, errmsg))


  let pop lexbuf errmsg stack =
    if Stack.length stack > 1 then
      Stack.pop stack |> ignore
    else
      report_error lexbuf errmsg


  let increment_line lexbuf =
    begin
      Lexing.new_line lexbuf;
    end


  let adjust_bol lexbuf amt =
    let open Lexing in
    let lcp = lexbuf.lex_curr_p in
      lexbuf.lex_curr_p <- { lcp with pos_bol = lcp.pos_cnum + amt; }


  let rec increment_line_for_each_break lexbuf str =
    let len = String.length str in
    let has_break = ref false in
    let rec aux num tail_spaces prev =
      if num >= len then tail_spaces else
        begin
          match (prev, String.get str num) with
          | (Some('\r'), '\n') ->
              aux (num + 1) (tail_spaces + 1) (Some('\n'))

          | (_, (('\n' | '\r') as c)) ->
              has_break := true;
              increment_line lexbuf;
              aux (num + 1) 0 (Some(c))

          | _ ->
              aux (num + 1) (tail_spaces + 1) None
        end;
    in
    let amt = aux 0 0 None in
      if !has_break then
        adjust_bol lexbuf (-amt)
      else
        ()


  let initialize state =
    let stack = Stack.create () in
    Stack.push state stack;
    stack


  let reset_to_progexpr () =
    initialize ProgramState


  let reset_to_vertexpr () =
    initialize VerticalState


  let split_module_list tokstr =
    let lst = String.split_on_char '.' tokstr in
      match List.rev lst with
      | ident :: mdllstrev -> (List.rev mdllstrev, ident)
      | []                 -> assert false

}

let space = [' ' '\t']
let break = ('\r' '\n' | '\n' | '\r')
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
let mathsymboltop = ('+' | '-' | '*' | '/' | ':' | '=' | '<' | '>' | '~' | '.' | ',' | '`')
let mathsymbol = (mathsymboltop | '?')
let mathascii = (small | capital | digit)
let mathstr = [^ '+' '-' '*' '/' ':' '=' '<' '>' '~' '.' ',' '`' '?' ' ' '\t' '\n' '\r' '\\' '{' '}' '%' '|' '$' '#' ';' '\'' '^' '_' '!' 'a'-'z' 'A'-'Z' '0'-'9']

rule progexpr stack = parse
  | "%" {
      comment lexbuf;
      progexpr stack lexbuf
    }
  | ("@" (identifier as headertype) ":" (" "*) (nonbreak* as content) (break | eof)) {
      let pos = get_pos lexbuf in
      increment_line lexbuf;
      match headertype with
      | "require" -> HEADER_REQUIRE(pos, content)
      | "import"  -> HEADER_IMPORT(pos, content)

      | "stage" ->
          begin
            match content with
            | "persistent" -> HEADER_PERSISTENT0(pos)
            | "0"          -> HEADER_STAGE0(pos)
            | "1"          -> HEADER_STAGE1(pos)
            | _            -> raise (LexError(pos, "undefined stage type '" ^ content ^ "'; should be 'persistent', '0', or '1'."))
          end

      | _ ->
          raise (LexError(pos, "undefined header type '" ^ headertype ^ "'"))
    }
  | space { progexpr stack lexbuf }
  | break {
      increment_line lexbuf;
      progexpr stack lexbuf
    }
  | "(" { Stack.push ProgramState stack; LPAREN(get_pos lexbuf) }
  | ")" {
      let pos = get_pos lexbuf in
      pop lexbuf "too many closing" stack;
      RPAREN(pos)
    }
  | "(|" { Stack.push ProgramState stack; BRECORD(get_pos lexbuf) }
  | "|)" {
      let pos = get_pos lexbuf in
      pop lexbuf "too many closing" stack;
      ERECORD(pos)
    }
  | "[" { Stack.push ProgramState stack; BLIST(get_pos lexbuf) }
  | "]" {
      let pos = get_pos lexbuf in
      pop lexbuf "too many closing" stack;
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
      let pos_start = get_pos lexbuf in
      let quote_length = String.length (Lexing.lexeme lexbuf) in
      let buffer = Buffer.create 256 in
      let (pos_last, s, omit_post) = literal quote_length buffer lexbuf in
      let pos = Range.unite pos_start pos_last in
      LITERAL(pos, s, true, omit_post)
    }
  | ("#" ("`"+ as tok)) {
      let pos_start = get_pos lexbuf in
      let quote_length = String.length tok in
      let buffer = Buffer.create 256 in
      let (pos_last, s, omit_post) = literal quote_length buffer lexbuf in
      let pos = Range.unite pos_start pos_last in
      LITERAL(pos, s, false, omit_post)
    }
  | ("@" ("`"+ as tok)) {
      let pos_start = get_pos lexbuf in
      let quote_length = String.length tok in
      let buffer = Buffer.create 256 in
      let (pos_last, s, omit_post) = literal quote_length buffer lexbuf in
      let pos = Range.unite pos_start pos_last in
      if not omit_post then Logging.warn_number_sign_end pos_last;
      match Range.get_last pos_start with
      | None ->
          assert false

      | Some(last) ->
          let (fname, ln, col) = last in
          let ipos =
            {
              input_file_name = fname;
              input_line      = ln;
              input_column    = col;
            }
          in
          POSITIONED_LITERAL(pos, ipos, s)
    }
  | ("\\" (identifier | constructor) "@") {
      let tok = Lexing.lexeme lexbuf in HORZMACRO(get_pos lexbuf, tok)
    }
  | ("\\" (identifier | constructor)) {
      let tok = Lexing.lexeme lexbuf in HORZCMD(get_pos lexbuf, tok)
    }
  | ("+" (identifier | constructor) "@") {
      let tok = Lexing.lexeme lexbuf in VERTMACRO(get_pos lexbuf, tok)
    }
  | ("+" (identifier | constructor)) {
      let tok = Lexing.lexeme lexbuf in VERTCMD(get_pos lexbuf, tok)
    }
  | "#"   { ACCESS(get_pos lexbuf) }
  | "->"  { ARROW(get_pos lexbuf) }
  | "<-"  { OVERWRITEEQ(get_pos lexbuf) }
  | "|"   { BAR(get_pos lexbuf) }
  | "_"   { WILDCARD(get_pos lexbuf) }
  | ":"   { COLON(get_pos lexbuf) }
  | ","   { COMMA(get_pos lexbuf) }
  | "::"  { CONS(get_pos lexbuf) }
  | "-"   { EXACT_MINUS(get_pos lexbuf) }
  | "="   { DEFEQ(get_pos lexbuf) }
  | "*"   { EXACT_TIMES(get_pos lexbuf) }
  | "&"   { EXACT_AMP(get_pos lexbuf) }
  | "~"   { EXACT_TILDE(get_pos lexbuf) }

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
  | "?->" { OPTIONALARROW(get_pos lexbuf) }
  | "?:" { OPTIONAL(get_pos lexbuf) }
  | "?*" { OMISSION(get_pos lexbuf) }
  | ("!" opsymbol*) { UNOP_EXCLAM(get_pos lexbuf, Lexing.lexeme lexbuf) }
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
          | "open"              -> OPEN(pos)
          | "include"           -> INCLUDE(pos)
          | _                   -> VAR(pos, tokstr)
      }
  | constructor { CONSTRUCTOR(get_pos lexbuf, Lexing.lexeme lexbuf) }
  | ((constructor as mdlnm) ".(") { Stack.push ProgramState stack; OPENMODULE(get_pos lexbuf, mdlnm) }
  | (digit | (nzdigit digit+))                            { INTCONST(get_pos lexbuf, int_of_string (Lexing.lexeme lexbuf)) }
  | (("0x" | "0X") hex+)                                  { INTCONST(get_pos lexbuf, int_of_string (Lexing.lexeme lexbuf)) }
  | ((digit+ "." digit*) | ("." digit+))                  { FLOATCONST(get_pos lexbuf, float_of_string (Lexing.lexeme lexbuf)) }
  | (((("-"? digit) | ("-"? nzdigit digit+)) as i) (identifier as unitnm))  { LENGTHCONST(get_pos lexbuf, float_of_int (int_of_string i), unitnm) }
  | ((("-"? digit+ "." digit*) as flt) (identifier as unitnm))       { LENGTHCONST(get_pos lexbuf, float_of_string flt, unitnm) }
  | ((("-"? "." digit+) as flt) (identifier as unitnm))              { LENGTHCONST(get_pos lexbuf, float_of_string flt, unitnm) }
  | eof {
      let pos = get_pos lexbuf in
      if Stack.length stack = 1 then EOI(pos) else
        report_error lexbuf "text input ended while reading a program area"
    }
  | _ as c { report_error lexbuf ("illegal token '" ^ (String.make 1 c) ^ "' in a program area") }


and vertexpr stack = parse
  | "%" {
      comment lexbuf;
      vertexpr stack lexbuf
    }
  | (break | space)* {
      increment_line_for_each_break lexbuf (Lexing.lexeme lexbuf);
      vertexpr stack lexbuf
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
  | ("+" (identifier | constructor) "@") {
      Stack.push ActiveState stack;
      VERTMACRO(get_pos lexbuf, Lexing.lexeme lexbuf)
    }
  | ("+" (identifier | constructor)) {
      Stack.push ActiveState stack;
      VERTCMD(get_pos lexbuf, Lexing.lexeme lexbuf)
    }
  | ("+" (constructor ".")* (identifier | constructor)) {
      let tokstrpure = Lexing.lexeme lexbuf in
      let tokstr = String.sub tokstrpure 1 ((String.length tokstrpure) - 1) in
      let (mdlnmlst, csnm) = split_module_list tokstr in
      Stack.push ActiveState stack;
      VERTCMDWITHMOD(get_pos lexbuf, mdlnmlst, "+" ^ csnm)
    }
  | "<" { Stack.push VerticalState stack; BVERTGRP(get_pos lexbuf) }
  | ">" {
      let pos = get_pos lexbuf in
      pop lexbuf "too many closing" stack;
      EVERTGRP(pos)
    }
  | "{" {
      Stack.push HorizontalState stack;
      skip_spaces lexbuf;
      BHORZGRP(get_pos lexbuf)
    }
  | eof {
      let pos = get_pos lexbuf in
      if Stack.length stack = 1 then EOI(pos) else
        report_error lexbuf "unexpected end of input while reading a vertical area"
    }
  | _ as c {
      report_error lexbuf ("unexpected character '" ^ (String.make 1 c) ^ "' in a vertical area")
    }

and horzexpr stack = parse
  | "%" {
      comment lexbuf;
      skip_spaces lexbuf;
      horzexpr stack lexbuf
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
        pop lexbuf "too many closing" stack;
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
  | ("\\" (identifier | constructor)) {
      let tok = Lexing.lexeme lexbuf in
      let rng = get_pos lexbuf in
      Stack.push ActiveState stack;
      HORZCMD(rng, tok)
    }
  | ("\\" (identifier | constructor) "@") {
      let tok = Lexing.lexeme lexbuf in
      let rng = get_pos lexbuf in
      Stack.push ActiveState stack;
      HORZMACRO(rng, tok)
    }
  | ("\\" (constructor ".")* (identifier | constructor)) {
      let tokstrpure = Lexing.lexeme lexbuf in
      let tokstr = String.sub tokstrpure 1 ((String.length tokstrpure) - 1) in
      let (mdlnmlst, csnm) = split_module_list tokstr in
      let rng = get_pos lexbuf in
        Stack.push ActiveState stack;
        HORZCMDWITHMOD(rng, mdlnmlst, "\\" ^ csnm)
    }
  | ("\\" symbol) {
      let tok = String.sub (Lexing.lexeme lexbuf) 1 1 in
        begin
          CHAR(get_pos lexbuf, tok)
        end
    }
  | "${" {
      Stack.push MathState stack;
      BMATHGRP(get_pos lexbuf)
    }
  | "`"+ {
      let pos_start = get_pos lexbuf in
      let quote_length = String.length (Lexing.lexeme lexbuf) in
      let buffer = Buffer.create 256 in
      let (pos_last, s, omit_post) = literal quote_length buffer lexbuf in
      let pos = Range.unite pos_start pos_last in
      LITERAL(pos, s, true, omit_post)
    }
  | ("#" ("`"+ as tok)) {
      let pos_start = get_pos lexbuf in
      let quote_length = String.length tok in
      let buffer = Buffer.create 256 in
      let (pos_last, s, omit_post) = literal quote_length buffer lexbuf in
      let pos = Range.unite pos_start pos_last in
      LITERAL(pos, s, false, omit_post)
    }
  | eof {
      let pos = get_pos lexbuf in
      if Stack.length stack = 1 then EOI(pos) else
        report_error lexbuf "unexpected end of input while reading an inline text area"
    }
  | str+ {
      let tok = Lexing.lexeme lexbuf in CHAR(get_pos lexbuf, tok)
    }

  | _ as c { report_error lexbuf ("illegal token '" ^ (String.make 1 c) ^ "' in an inline text area") }


and mathexpr stack = parse
  | space { mathexpr stack lexbuf }
  | break { increment_line lexbuf; mathexpr stack lexbuf }
  | "%" {
      comment lexbuf;
      mathexpr stack lexbuf
    }
  | "?:" {
      OPTIONAL(get_pos lexbuf)
    }
  | "?*" {
      OMISSION(get_pos lexbuf)
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
      pop lexbuf "too many closing" stack;
      EMATHGRP(pos)
    }
  | "|" { SEP(get_pos lexbuf) }
  | "^" { SUPERSCRIPT(get_pos lexbuf) }
  | "_" { SUBSCRIPT(get_pos lexbuf) }
  | "'"+ { let n = String.length (Lexing.lexeme lexbuf) in PRIMES(get_pos lexbuf, n) }
  | (mathsymboltop (mathsymbol*)) { MATHCHARS(get_pos lexbuf, Lexing.lexeme lexbuf) }
  | mathascii { MATHCHARS(get_pos lexbuf, Lexing.lexeme lexbuf) }
  | mathstr+ { MATHCHARS(get_pos lexbuf, Lexing.lexeme lexbuf) }
  | ("#" (identifier as varnm)) {
      VARINMATH(get_pos lexbuf, [], varnm)
    }
  | ("#" (constructor ".")* (identifier | constructor)) {
      let csnmpure = Lexing.lexeme lexbuf in
      let csstr = String.sub csnmpure 1 ((String.length csnmpure) - 1) in
      let (mdlnmlst, csnm) = split_module_list csstr in
        VARINMATH(get_pos lexbuf, mdlnmlst, csnm)
    }
  | ("\\" (identifier | constructor)) {
      let csnm = Lexing.lexeme lexbuf in
        MATHCMD(get_pos lexbuf, csnm)
    }
  | ("\\" (constructor ".")* (identifier | constructor)) {
      let csnmpure = Lexing.lexeme lexbuf in
      let csstr = String.sub csnmpure 1 ((String.length csnmpure) - 1) in
      let (mdlnmlst, csnm) = split_module_list csstr in
        MATHCMDWITHMOD(get_pos lexbuf, mdlnmlst, "\\" ^ csnm)
    }
  | ("\\" symbol) {
      let tok = String.sub (Lexing.lexeme lexbuf) 1 1 in
        MATHCHARS(get_pos lexbuf, tok)
    }
  | _ as c { report_error lexbuf ("illegal token '" ^ (String.make 1 c) ^ "' in a math area") }

  | eof { report_error lexbuf "unexpected end of file in a math area" }


and active stack = parse
  | "%" {
      comment lexbuf;
      active stack lexbuf
    }
  | space { active stack lexbuf }
  | break { increment_line lexbuf; active stack lexbuf }
  | "?:" { OPTIONAL(get_pos lexbuf) }
  | "?*" { OMISSION(get_pos lexbuf) }
  | "~" { EXACT_TILDE(get_pos lexbuf) }
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
      pop lexbuf "BUG; this cannot happen" stack;
      Stack.push HorizontalState stack;
      skip_spaces lexbuf;
      BHORZGRP(pos)
    }
  | "<" {
      let pos = get_pos lexbuf in
      pop lexbuf "BUG; this cannot happen" stack;
      Stack.push VerticalState stack;
      BVERTGRP(pos)
    }
  | ";" {
      let pos = get_pos lexbuf in
      pop lexbuf "BUG; this cannot happen" stack;
      ENDACTIVE(pos)
    }
  | eof {
      report_error lexbuf "unexpected end of input while reading an active area"
    }
  | _ {
      let tok = Lexing.lexeme lexbuf in
      report_error lexbuf ("unexpected token '" ^ tok ^ "' in an active area")
    }


and literal quote_length buffer = parse
  | "`"+ {
      let tok = Lexing.lexeme lexbuf in
      let len = String.length tok in
        if len < quote_length then begin
          Buffer.add_string buffer tok;
          literal quote_length buffer lexbuf
        end else if len > quote_length then
          report_error lexbuf "literal area was closed with too many '`'s"
        else
          let pos_last = get_pos lexbuf in
          (pos_last, Buffer.contents buffer, true)
    }
  | (("`"+ as tok) "#") {
      let len = String.length tok in
        if len < quote_length then begin
          Buffer.add_string buffer (tok ^ "#");
          literal quote_length buffer lexbuf
        end else if len > quote_length then
          report_error lexbuf "literal area was closed with too many '`'s"
        else
          let pos_last = get_pos lexbuf in
          (pos_last, Buffer.contents buffer, false)
    }
  | break {
      let tok = Lexing.lexeme lexbuf in
      increment_line lexbuf;
      Buffer.add_string buffer tok;
      literal quote_length buffer lexbuf
    }
  | eof {
      report_error lexbuf "unexpected end of input while reading literal area"
    }
  | _ {
      let tok = Lexing.lexeme lexbuf in
      Buffer.add_string buffer tok;
      literal quote_length buffer lexbuf
    }


and comment = parse
  | break { increment_line lexbuf; }
  | eof   { () }
  | _     { comment lexbuf }


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
  let cut_token stack lexbuf =
    match Stack.top stack with
    | ProgramState    -> progexpr stack lexbuf
    | VerticalState   -> vertexpr stack lexbuf
    | HorizontalState -> horzexpr stack lexbuf
    | ActiveState     -> active stack lexbuf
    | MathState       -> mathexpr stack lexbuf

}
