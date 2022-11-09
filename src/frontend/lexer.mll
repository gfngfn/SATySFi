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
    | ProgramState (* program mode *)
    | BlockState   (* block mode *)
    | InlineState  (* inline mode *)
    | ActiveState  (* active mode *)
    | MathState    (* math mode *)


  let get_pos (lexbuf : Lexing.lexbuf) : Range.t =
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


  let increment_line (lexbuf : Lexing.lexbuf) : unit =
    Lexing.new_line lexbuf


  let adjust_bol (lexbuf : Lexing.lexbuf) (amt : int) : unit =
    let open Lexing in
    let lcp = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <- { lcp with pos_bol = lcp.pos_cnum + amt; }


  let increment_line_for_each_break (lexbuf : Lexing.lexbuf) (str : string) : unit =
    let len = String.length str in
    let rec aux num has_break tail_spaces prev =
      if num >= len then
        (has_break, tail_spaces)
      else
        match (prev, String.get str num) with
        | (Some('\r'), '\n') ->
            aux (num + 1) has_break (tail_spaces + 1) (Some('\n'))

        | (_, (('\n' | '\r') as c)) ->
            increment_line lexbuf;
            aux (num + 1) true 0 (Some(c))

        | _ ->
            aux (num + 1) has_break (tail_spaces + 1) None
    in
    let (has_break, amt) = aux 0 false 0 None in
    if has_break then
      adjust_bol lexbuf (-amt)
    else
      ()


  let initialize state =
    let stack = Stack.create () in
    Stack.push state stack;
    stack


  let reset_to_program () =
    initialize ProgramState


  let split_module_list (rng : Range.t) (s : string) : (module_name ranged) list * var_name ranged =
    let (fname, ln1, pos1) =
      match Range.get_first rng with
      | None         -> assert false
      | Some(triple) -> triple
    in
    let idents =
      let ss = String.split_on_char '.' s in
      let (_, identacc) =
        List.fold_left (fun (pos, identacc) s ->
          let n = String.length s in
          let rng = Range.make fname ln1 pos (pos + n) in
          let ident = (rng, s) in
          (pos + n + 1, Alist.extend identacc ident)
        ) (pos1, Alist.empty) ss
      in
      Alist.to_list identacc
    in
    match List.rev idents with
    | ident :: modidents_rev -> (List.rev modidents_rev, ident)
    | []                     -> assert false

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
let lower = (small (digit | latin | "-")*)
let upper = (capital (digit | latin | "-")*)
let symbol = ( [' '-'@'] | ['['-'`'] | ['{'-'~'] )
let opsymbol = ( '+' | '-' | '*' | '/' | '^' | '&' | '|' | '!' | ':' | '=' | '<' | '>' | '~' | '\'' | '.' | '?' )
let str = [^ ' ' '\t' '\n' '\r' '@' '`' '\\' '{' '}' '<' '>' '%' '|' '*' '$' '#' ';']
let mathsymboltop = ('+' | '-' | '*' | '/' | ':' | '=' | '<' | '>' | '~' | '.' | ',' | '`')
let mathsymbol = (mathsymboltop | '?')
let mathascii = (small | capital | digit)
let mathstr = [^ '+' '-' '*' '/' ':' '=' '<' '>' '~' '.' ',' '`' '?' ' ' '\t' '\n' '\r' '\\' '{' '}' '%' '|' '$' '#' ';' '\'' '^' '_' '!' 'a'-'z' 'A'-'Z' '0'-'9']

rule lex_program stack = parse
  | "%"
      {
        comment lexbuf;
        lex_program stack lexbuf
      }
  | space
      { lex_program stack lexbuf }
  | break
      {
        increment_line lexbuf;
        lex_program stack lexbuf
      }
  | "("
      { Stack.push ProgramState stack; L_PAREN(get_pos lexbuf) }
  | ")"
      {
        let pos = get_pos lexbuf in
        pop lexbuf "too many closing" stack;
        R_PAREN(pos)
      }
  | "(|"
      { Stack.push ProgramState stack; L_RECORD(get_pos lexbuf) }
  | "|)"
      {
        let pos = get_pos lexbuf in
        pop lexbuf "too many closing" stack;
        R_RECORD(pos)
      }
  | "["
      { Stack.push ProgramState stack; L_SQUARE(get_pos lexbuf) }
  | "]"
      {
        let pos = get_pos lexbuf in
        pop lexbuf "too many closing" stack;
        R_SQUARE(pos)
      }
  | "{"
      {
        Stack.push InlineState stack;
        skip_spaces lexbuf;
        L_INLINE_TEXT(get_pos lexbuf)
      }
  | "'<"
      {
        Stack.push BlockState stack;
        L_BLOCK_TEXT(get_pos lexbuf)
      }
  | "${"
      {
        Stack.push MathState stack;
        L_MATH_TEXT(get_pos lexbuf)
      }
  | ("#[" (lower as s))
      { Stack.push ProgramState stack; ATTRIBUTE_L_SQUARE(get_pos lexbuf, s) }
  | "`"+
      {
        let pos_start = get_pos lexbuf in
        let quote_length = String.length (Lexing.lexeme lexbuf) in
        let buffer = Buffer.create 256 in
        let (pos_last, s, omit_post) = literal quote_length buffer lexbuf in
        let pos = Range.unite pos_start pos_last in
        STRING(pos, s, true, omit_post)
      }
  | ("#" ("`"+ as backticks))
      {
        let pos_start = get_pos lexbuf in
        let quote_length = String.length backticks in
        let buffer = Buffer.create 256 in
        let (pos_last, s, omit_post) = literal quote_length buffer lexbuf in
        let pos = Range.unite pos_start pos_last in
        STRING(pos, s, false, omit_post)
      }
  | ("@" ("`"+ as tok))
      {
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
            POSITIONED_STRING(pos, ipos, s)
      }
  | ("\\" (lower | upper))
      { BACKSLASH_CMD(get_pos lexbuf, Lexing.lexeme lexbuf) }
  | ("\\" (lower | upper) "@")
      { BACKSLASH_MACRO(get_pos lexbuf, Lexing.lexeme lexbuf) }
  | ("\\" (((upper ".")+ (lower | upper)) as s))
      {
        let pos = get_pos lexbuf in
        let (modidents, (rng, csnm)) = split_module_list pos s in
        LONG_BACKSLASH_CMD(pos, modidents, (rng, "\\" ^ csnm))
      }
  | ("\\" (((upper ".")+ (lower | upper)) "@" as s))
      {
        let pos = get_pos lexbuf in
        let (modidents, (rng, csnm)) = split_module_list pos s in
        LONG_BACKSLASH_MACRO(pos, modidents, (rng, "\\" ^ csnm))
      }
  | ("+" (lower | upper))
      { PLUS_CMD(get_pos lexbuf, Lexing.lexeme lexbuf) }
  | ("+" (lower | upper) "@")
      { PLUS_MACRO(get_pos lexbuf, Lexing.lexeme lexbuf) }
  | ("+" (((upper ".")+ (lower | upper)) as s))
      {
        let pos = get_pos lexbuf in
        let (modidents, (rng, csnm)) = split_module_list pos s in
        LONG_PLUS_CMD(pos, modidents, (rng, "+" ^ csnm))
      }
  | ("+" (((upper ".")+ (lower | upper)) "@" as s))
      {
        let pos = get_pos lexbuf in
        let (modidents, (rng, csnm)) = split_module_list pos s in
        LONG_PLUS_MACRO(pos, modidents, (rng, "+" ^ csnm))
      }

  | "?"  { QUESTION(get_pos lexbuf) }
  | ":>" { COERCE(get_pos lexbuf) }
  | "#"  { ACCESS(get_pos lexbuf) }
  | "->" { ARROW(get_pos lexbuf) }
  | "<-" { REVERSED_ARROW(get_pos lexbuf) }
  | "|"  { BAR(get_pos lexbuf) }
  | "_"  { WILDCARD(get_pos lexbuf) }
  | ":"  { COLON(get_pos lexbuf) }
  | ","  { COMMA(get_pos lexbuf) }
  | "::" { CONS(get_pos lexbuf) }
  | "-"  { EXACT_MINUS(get_pos lexbuf) }
  | "="  { EXACT_EQ(get_pos lexbuf) }
  | "*"  { EXACT_TIMES(get_pos lexbuf) }
  | "&"  { EXACT_AMP(get_pos lexbuf) }
  | "~"  { EXACT_TILDE(get_pos lexbuf) }

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
  | ("!" opsymbol*) { UNOP_EXCLAM(get_pos lexbuf, Lexing.lexeme lexbuf) }

  | ("'" (lower as tyvarnm))
      { TYPEVAR(get_pos lexbuf, tyvarnm) }

  | ("?'" (lower as tyvarnm))
      { ROWVAR(get_pos lexbuf, tyvarnm) }

  | ((upper ".")+ lower)
      {
        let pos = get_pos lexbuf in
        let s = Lexing.lexeme lexbuf in
        let (modidents, lower_ident) = split_module_list pos s in
        LONG_LOWER(pos, modidents, lower_ident)
      }
  | lower
      {
        let tokstr = Lexing.lexeme lexbuf in
        let pos = get_pos lexbuf in
        match tokstr with
        | "and"       -> AND(pos)
        | "as"        -> AS(pos)
        | "block"     -> BLOCK(pos)
        | "command"   -> COMMAND(pos)
        | "else"      -> ELSE(pos)
        | "end"       -> END(pos)
        | "false"     -> FALSE(pos)
        | "fun"       -> FUN(pos)
        | "if"        -> IF(pos)
        | "in"        -> IN(pos)
        | "include"   -> INCLUDE(pos)
        | "inline"    -> INLINE(pos)
        | "let"       -> LET(pos)
        | "mod"       -> MOD(pos)
        | "match"     -> MATCH(pos)
        | "math"      -> MATH(pos)
        | "module"    -> MODULE(pos)
        | "mutable"   -> MUTABLE(pos)
        | "of"        -> OF(pos)
        | "open"      -> OPEN(pos)
        | "package"   -> PACKAGE(pos)
        | "persistent"-> PERSISTENT(pos)
        | "rec"       -> REC(pos)
        | "sig"       -> SIG(pos)
        | "signature" -> SIGNATURE(pos)
        | "struct"    -> STRUCT(pos)
        | "then"      -> THEN(pos)
        | "true"      -> TRUE(pos)
        | "type"      -> TYPE(pos)
        | "use"       -> USE(pos)
        | "val"       -> VAL(pos)
        | "with"      -> WITH(pos)
        | _           -> LOWER(pos, tokstr)
      }
  | ((upper ".")+ upper)
      {
        let pos = get_pos lexbuf in
        let s = Lexing.lexeme lexbuf in
        let (modidents, upper_ident) = split_module_list pos s in
        LONG_UPPER(pos, modidents, upper_ident)
      }
  | (upper as s) "."
      {
        let pos = get_pos lexbuf in
        UPPER_DOT(pos, s)
      }
  | upper
      { UPPER(get_pos lexbuf, Lexing.lexeme lexbuf) }
  | (digit | (nzdigit digit+))
      { INT(get_pos lexbuf, int_of_string (Lexing.lexeme lexbuf)) }
  | (("0x" | "0X") hex+)
      { INT(get_pos lexbuf, int_of_string (Lexing.lexeme lexbuf)) }
  | ((digit+ "." digit*) | ("." digit+))
      { FLOAT(get_pos lexbuf, float_of_string (Lexing.lexeme lexbuf)) }
  | (((("-"? digit) | ("-"? nzdigit digit+)) as i) (lower as unitnm))
      { LENGTH(get_pos lexbuf, float_of_int (int_of_string i), unitnm) }
  | ((("-"? digit+ "." digit*) as flt) (lower as unitnm))
      { LENGTH(get_pos lexbuf, float_of_string flt, unitnm) }
  | ((("-"? "." digit+) as flt) (lower as unitnm))
      { LENGTH(get_pos lexbuf, float_of_string flt, unitnm) }
  | eof
      {
        let pos = get_pos lexbuf in
        if Stack.length stack = 1 then
          EOI(pos)
        else
          report_error lexbuf "text input ended while reading a program area"
      }
  | _ as c
      { report_error lexbuf (Printf.sprintf "illegal token '%s' in a program area" (String.make 1 c)) }


and lex_block stack = parse
  | "%"
      {
        comment lexbuf;
        lex_block stack lexbuf
      }
  | (break | space)+
      {
        increment_line_for_each_break lexbuf (Lexing.lexeme lexbuf);
        lex_block stack lexbuf
      }
  | ("#" (((upper ".")* lower) as s))
      {
        let pos = get_pos lexbuf in
        let (modidents, cs) = split_module_list pos s in
        Stack.push ActiveState stack;
        VAR_IN_TEXT(pos, modidents, cs)
      }
  | ("+" (lower | upper))
      {
        Stack.push ActiveState stack;
        PLUS_CMD(get_pos lexbuf, Lexing.lexeme lexbuf)
      }
  | ("+" (lower | upper) "@")
      {
        Stack.push ActiveState stack;
        PLUS_MACRO(get_pos lexbuf, Lexing.lexeme lexbuf)
      }
  | ("+" (((upper ".")+ (lower | upper)) as s))
      {
        let pos = get_pos lexbuf in
        let (modidents, (rng, csnm)) = split_module_list pos s in
        Stack.push ActiveState stack;
        LONG_PLUS_CMD(pos, modidents, (rng, "+" ^ csnm))
      }
  | ("+" (((upper ".")+ (lower | upper)) "@" as s))
      {
        let pos = get_pos lexbuf in
        let (modidents, (rng, csnm)) = split_module_list pos s in
        Stack.push ActiveState stack;
        LONG_PLUS_MACRO(pos, modidents, (rng, "+" ^ csnm))
      }
  | "<"
      { Stack.push BlockState stack; L_BLOCK_TEXT(get_pos lexbuf) }
  | ">"
      {
        let pos = get_pos lexbuf in
        pop lexbuf "too many closing" stack;
        R_BLOCK_TEXT(pos)
      }
  | "{"
      {
        Stack.push InlineState stack;
        skip_spaces lexbuf;
        L_INLINE_TEXT(get_pos lexbuf)
      }
  | eof
      {
        let pos = get_pos lexbuf in
        if Stack.length stack = 1 then
          EOI(pos)
        else
          report_error lexbuf "unexpected end of input while reading a block text area"
      }
  | _ as c
      { report_error lexbuf (Printf.sprintf "unexpected character '%s' in a block text area" (String.make 1 c)) }

and lex_inline stack = parse
  | "%"
      {
        comment lexbuf;
        skip_spaces lexbuf;
        lex_inline stack lexbuf
      }
  | ((break | space)* "{")
      {
        increment_line_for_each_break lexbuf (Lexing.lexeme lexbuf);
        Stack.push InlineState stack;
        skip_spaces lexbuf;
        L_INLINE_TEXT(get_pos lexbuf)
      }
  | ((break | space)* "}")
      {
        increment_line_for_each_break lexbuf (Lexing.lexeme lexbuf);
        let pos = get_pos lexbuf in
        pop lexbuf "too many closing" stack;
        R_INLINE_TEXT(pos)
      }
  | ((break | space)* "<")
      {
        increment_line_for_each_break lexbuf (Lexing.lexeme lexbuf);
        Stack.push BlockState stack;
        L_BLOCK_TEXT(get_pos lexbuf)
      }
  | ((break | space)* "|")
      {
        increment_line_for_each_break lexbuf (Lexing.lexeme lexbuf);
        skip_spaces lexbuf;
        BAR(get_pos lexbuf)
      }
  | break
      {
        increment_line lexbuf;
        skip_spaces lexbuf;
        BREAK(get_pos lexbuf)
      }
  | space
      {
        skip_spaces lexbuf;
        SPACE(get_pos lexbuf)
      }
  | ((break | space)* (item as itemstr))
      {
        increment_line_for_each_break lexbuf (Lexing.lexeme lexbuf);
        skip_spaces lexbuf;
        ITEM(get_pos lexbuf, String.length itemstr)
      }
  | ("#" (((upper ".")* lower) as s))
      {
        let pos = get_pos lexbuf in
        let (modidents, ident) = split_module_list pos s in
        Stack.push ActiveState stack;
        VAR_IN_TEXT(pos, modidents, ident)
      }
  | ("\\" (lower | upper))
      {
        let tok = Lexing.lexeme lexbuf in
        let rng = get_pos lexbuf in
        Stack.push ActiveState stack;
        BACKSLASH_CMD(rng, tok)
      }
  | ("\\" (lower | upper) "@")
      {
        let tok = Lexing.lexeme lexbuf in
        let rng = get_pos lexbuf in
        Stack.push ActiveState stack;
        BACKSLASH_MACRO(rng, tok)
      }
  | ("\\" (((upper ".")+ (lower | upper)) as s))
      {
        let pos = get_pos lexbuf in
        let (modidents, (rng, csnm)) = split_module_list pos s in
        Stack.push ActiveState stack;
        LONG_BACKSLASH_CMD(pos, modidents, (rng, "\\" ^ csnm))
      }
  | ("\\" (((upper ".")+ (lower | upper)) "@" as s))
      {
        let pos = get_pos lexbuf in
        let (modidents, (rng, csnm)) = split_module_list pos s in
        Stack.push ActiveState stack;
        LONG_BACKSLASH_MACRO(pos, modidents, (rng, "\\" ^ csnm))
      }
  | ("\\" symbol)
      {
        let tok = String.sub (Lexing.lexeme lexbuf) 1 1 in
        CHAR(get_pos lexbuf, tok)
      }
  | "${"
      {
        Stack.push MathState stack;
        L_MATH_TEXT(get_pos lexbuf)
      }
  | "`"+
      {
        let pos_start = get_pos lexbuf in
        let quote_length = String.length (Lexing.lexeme lexbuf) in
        let buffer = Buffer.create 256 in
        let (pos_last, s, omit_post) = literal quote_length buffer lexbuf in
        let pos = Range.unite pos_start pos_last in
        STRING(pos, s, true, omit_post)
      }
  | ("#" ("`"+ as backticks))
      {
        let pos_start = get_pos lexbuf in
        let quote_length = String.length backticks in
        let buffer = Buffer.create 256 in
        let (pos_last, s, omit_post) = literal quote_length buffer lexbuf in
        let pos = Range.unite pos_start pos_last in
        STRING(pos, s, false, omit_post)
      }
  | eof
      {
        let pos = get_pos lexbuf in
        if Stack.length stack = 1 then
          EOI(pos)
        else
          report_error lexbuf "unexpected end of input while reading an inline text area"
      }
  | str+
      { let tok = Lexing.lexeme lexbuf in CHAR(get_pos lexbuf, tok) }

  | _ as c
      { report_error lexbuf (Printf.sprintf "illegal token '%s' in an inline text area" (String.make 1 c)) }


and lex_math stack = parse
  | space
      { lex_math stack lexbuf }
  | break
      { increment_line lexbuf; lex_math stack lexbuf }
  | "%"
      {
        comment lexbuf;
        lex_math stack lexbuf
      }
  | "?"
      { QUESTION(get_pos lexbuf) }
  | "!{"
      {
        Stack.push InlineState stack;
        skip_spaces lexbuf;
        L_INLINE_TEXT(get_pos lexbuf);
      }
  | "!<"
      {
        Stack.push BlockState stack;
        L_BLOCK_TEXT(get_pos lexbuf)
      }
  | "!("
      {
        Stack.push ProgramState stack;
        L_PAREN(get_pos lexbuf)
      }
  | "!["
      {
        Stack.push ProgramState stack;
        L_SQUARE(get_pos lexbuf)
      }
  | "!(|"
      {
        Stack.push ProgramState stack;
        L_RECORD(get_pos lexbuf)
      }
  | "{"
      {
        Stack.push MathState stack;
        L_MATH_TEXT(get_pos lexbuf)
      }
  | "}"
      {
        let pos = get_pos lexbuf in
        pop lexbuf "too many closing" stack;
        R_MATH_TEXT(pos)
      }
  | "|"
      { BAR(get_pos lexbuf) }
  | "^"
      { SUPERSCRIPT(get_pos lexbuf) }
  | "_"
      { SUBSCRIPT(get_pos lexbuf) }
  | "'"+
      { let n = String.length (Lexing.lexeme lexbuf) in PRIMES(get_pos lexbuf, n) }
  | (mathsymboltop (mathsymbol*))
      { MATHCHARS(get_pos lexbuf, Lexing.lexeme lexbuf) }
  | mathascii
      { MATHCHARS(get_pos lexbuf, Lexing.lexeme lexbuf) }
  | mathstr+
      { MATHCHARS(get_pos lexbuf, Lexing.lexeme lexbuf) }
  | ("#" (((upper ".")* (lower | upper)) as s))
      {
        let pos = get_pos lexbuf in
        let (modidents, ident) = split_module_list pos s in
        VAR_IN_TEXT(pos, modidents, ident)
      }
  | ("\\" (lower | upper))
      {
        let csnm = Lexing.lexeme lexbuf in
        BACKSLASH_CMD(get_pos lexbuf, csnm)
      }
  | ("\\" (((upper ".")* (lower | upper)) as s))
      {
        let pos = get_pos lexbuf in
        let (modidents, (rng, csnm)) = split_module_list pos s in
        LONG_BACKSLASH_CMD(pos, modidents, (rng, "\\" ^ csnm))
      }
  | ("\\" symbol)
      {
        let tok = String.sub (Lexing.lexeme lexbuf) 1 1 in
        MATHCHARS(get_pos lexbuf, tok)
      }
  | _ as c
      { report_error lexbuf (Printf.sprintf "illegal token '%s' in a math area" (String.make 1 c)) }
  | eof
      { report_error lexbuf "unexpected end of file in a math area" }


and lex_active stack = parse
  | "%"
      {
        comment lexbuf;
        lex_active stack lexbuf
      }
  | space
      { lex_active stack lexbuf }
  | break
      { increment_line lexbuf; lex_active stack lexbuf }
  | "~"
      { EXACT_TILDE(get_pos lexbuf) }
  | "?"
      { QUESTION(get_pos lexbuf) }
  | "("
      {
        Stack.push ProgramState stack;
        L_PAREN(get_pos lexbuf)
      }
  | "(|"
      {
        Stack.push ProgramState stack;
        L_RECORD(get_pos lexbuf)
      }
  | "["
      {
        Stack.push ProgramState stack;
        L_SQUARE(get_pos lexbuf)
      }
  | "{"
      {
        let pos = get_pos lexbuf in
        pop lexbuf "BUG; this cannot happen" stack;
        Stack.push InlineState stack;
        skip_spaces lexbuf;
        L_INLINE_TEXT(pos)
      }
  | "<"
      {
        let pos = get_pos lexbuf in
        pop lexbuf "BUG; this cannot happen" stack;
        Stack.push BlockState stack;
        L_BLOCK_TEXT(pos)
      }
  | ";"
      {
        let pos = get_pos lexbuf in
        pop lexbuf "BUG; this cannot happen" stack;
        SEMICOLON(pos)
      }
  | eof
      { report_error lexbuf "unexpected end of input while reading an active area" }
  | _
      {
        let s = Lexing.lexeme lexbuf in
        report_error lexbuf (Printf.sprintf "unexpected token '%s' in an active area" s)
      }


and literal quote_length buffer = parse
  | "`"+
      {
        let backticks = Lexing.lexeme lexbuf in
        let len = String.length backticks in
        if len < quote_length then begin
          Buffer.add_string buffer backticks;
          literal quote_length buffer lexbuf
        end else if len > quote_length then
          report_error lexbuf "literal area was closed with too many '`'s"
        else
          let s = Buffer.contents buffer in
          let pos_last = get_pos lexbuf in
          (pos_last, s, true)
    }
  | (("`"+ as backticks) "#")
      {
        let len = String.length backticks in
        if len < quote_length then begin
          Buffer.add_string buffer backticks;
          Buffer.add_string buffer "#";
          literal quote_length buffer lexbuf
        end else if len > quote_length then
          report_error lexbuf "literal area was closed with too many '`'s"
        else
          let s = Buffer.contents buffer in
          let pos_last = get_pos lexbuf in
          (pos_last, s, false)
    }
  | break
      {
        let tok = Lexing.lexeme lexbuf in
        increment_line lexbuf;
        Buffer.add_string buffer tok;
        literal quote_length buffer lexbuf
      }
  | eof
      {
        report_error lexbuf "unexpected end of input while reading literal area"
      }
  | _
      {
        let s = Lexing.lexeme lexbuf in
        Buffer.add_string buffer s;
        literal quote_length buffer lexbuf
      }


and comment = parse
  | break { increment_line lexbuf; }
  | eof   { () }
  | _     { comment lexbuf }


and skip_spaces = parse
  | break
      {
        increment_line lexbuf;
        skip_spaces lexbuf
      }
  | space
      {
        skip_spaces lexbuf
      }
  | "%"
      {
        comment lexbuf;
        skip_spaces lexbuf
      }
  | ""
      { () }


{
  let cut_token stack lexbuf =
    match Stack.top stack with
    | ProgramState -> lex_program stack lexbuf
    | BlockState   -> lex_block stack lexbuf
    | InlineState  -> lex_inline stack lexbuf
    | ActiveState  -> lex_active stack lexbuf
    | MathState    -> lex_math stack lexbuf

}
