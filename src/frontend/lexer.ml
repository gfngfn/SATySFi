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


let get_pos (lexbuf : Sedlexing.lexbuf) : Range.t =
  let (posS, posE) = Sedlexing.lexing_positions lexbuf in
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


let initialize state =
  let stack = Stack.create () in
  Stack.push state stack;
  stack


let reset_to_program () =
  initialize ProgramState


let lexeme lexbuf = Sedlexing.Utf8.lexeme lexbuf


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


let split_length_unitnm tokstr =
  let re = Str.regexp "[-0-9\\.]+" in
  if Str.string_match re tokstr 0 then
    let matched = Str.matched_string tokstr in
    let start = String.length matched in
    let rest = String.sub tokstr start (String.length tokstr - start) in
    (matched, rest)
  else
    assert false

let remove_before_spaces tokstr = Base.String.lstrip tokstr

let get_head_uchar str =
  str
  |> InternalText.of_utf8
  |> InternalText.to_uchar_list
  |> List.hd



let space = [%sedlex.regexp? ' ' | '\t']
let break = [%sedlex.regexp? "\r\n" | '\r' | '\n']
let nzdigit = [%sedlex.regexp? '1'..'9']
let digit = [%sedlex.regexp? nzdigit | '0']
let hex = [%sedlex.regexp? digit | 'A'..'F']
let capital = [%sedlex.regexp? 'A'..'Z']
let small = [%sedlex.regexp? 'a'..'z']
let latin = [%sedlex.regexp? small | capital]
let item = [%sedlex.regexp? Plus '*']
let lower = [%sedlex.regexp? small, Star (digit | latin | '-')]
let upper = [%sedlex.regexp? capital, Star (digit | latin | '-')]
let symbol = [%sedlex.regexp? ' '..'@' | '['..'`' | '{'..'~']
let opsymbol = [%sedlex.regexp? '+' | '-' | '*' | '/' | '^' | '&' | '|' | '!' | ':' | '=' | '<' | '>' | '~' | '\'' | '.' | '?']
let nonstr = [%sedlex.regexp? ' ' | '\t' | '\n' | '\r' | '@' | '`' | '\\' | '{' | '}' | '<' | '>' | '%' | '|' | '*' | '$' | '#' | ';']
let mathsymboltop = [%sedlex.regexp? '+' | '-' | '*' | '/' | ':' | '=' | '<' | '>' | '~' | '.' | ',' | '`']
let mathsymbol = [%sedlex.regexp? mathsymboltop | '?']
let mathascii = [%sedlex.regexp? small | capital | digit]
let nonmathstr = [%sedlex.regexp? '+' | '-' | '*' | '/' | ':' | '=' | '<' | '>' | '~' | '.' | ',' | '`' | '?' | ' ' | '\t' | '\n' | '\r' | '\\' | '{' | '}' | '%' | '|' | '$' | '#' | ';' | '\'' | '^' | '_' | '!' | mathascii]



let rec lex_program stack lexbuf =
  match%sedlex lexbuf with
  | '%' -> (
    let () = comment lexbuf in
    lex_program stack lexbuf
  )
  | '@', Plus '`' -> (
    let pos_start = get_pos lexbuf in
    let quote_length = (String.length (lexeme lexbuf)) - 1 in
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
  )
  | '@' -> (
    let (headertype, content) = lex_header lexbuf in
    let pos = get_pos lexbuf in
    match headertype with
    | "require" -> HEADER_REQUIRE(pos, content)
    | "import"  -> HEADER_IMPORT(pos, content)
    | _         -> raise (LexError(pos, "undefined header type '" ^ headertype ^ "'"))
  )
  | space -> lex_program stack lexbuf
  | break -> lex_program stack lexbuf
  | "(|" -> ( Stack.push ProgramState stack; L_RECORD(get_pos lexbuf))
  | "|)" -> (
    let pos = get_pos lexbuf in
    pop lexbuf "too many closing" stack;
    R_RECORD(pos)
  )
  | "(" -> ( Stack.push ProgramState stack; L_PAREN(get_pos lexbuf))
  | ")" -> (
    let pos = get_pos lexbuf in
    pop lexbuf "too many closing" stack;
    R_PAREN(pos)
  )
  | "[" -> ( Stack.push ProgramState stack; L_SQUARE(get_pos lexbuf))
  | "]" -> (
    let pos = get_pos lexbuf in
    pop lexbuf "too many closing" stack;
    R_SQUARE(pos)
  )
  | "{" -> (
    Stack.push InlineState stack;
    skip_spaces lexbuf;
    L_INLINE_TEXT(get_pos lexbuf)
  )
  | "'<" -> (
    Stack.push BlockState stack;
    L_BLOCK_TEXT(get_pos lexbuf)
  )
  | "${" -> (
    Stack.push MathState stack;
    L_MATH_TEXT(get_pos lexbuf)
  )
  | Plus "`" -> (
    let pos_start = get_pos lexbuf in
    let quote_length = String.length (lexeme lexbuf) in
    let buffer = Buffer.create 256 in
    let (pos_last, s, omit_post) = literal quote_length buffer lexbuf in
    let pos = Range.unite pos_start pos_last in
    STRING(pos, s, true, omit_post)
  )
  | '#', Plus '`' -> (
    let pos_start = get_pos lexbuf in
    let quote_length = (String.length (lexeme lexbuf)) - 1 in
    let buffer = Buffer.create 256 in
    let (pos_last, s, omit_post) = literal quote_length buffer lexbuf in
    let pos = Range.unite pos_start pos_last in
    STRING(pos, s, false, omit_post)
  )
  | '\\', Plus (upper, "."), (lower| upper), '@' -> (
    let pos = get_pos lexbuf in
    let s = lexeme lexbuf in
    let len = String.length s in
    let s = String.sub s 1 (len - 2) in
    let (modidents, (rng, csnm)) = split_module_list pos s in
    LONG_BACKSLASH_MACRO(pos, modidents, (rng, "\\" ^ csnm))
  )
  | '\\', Plus (upper, "."), (lower| upper) -> (
    let pos = get_pos lexbuf in
    let s = lexeme lexbuf in
    let len = String.length s in
    let s = String.sub s 1 (len - 1) in
    let (modidents, (rng, csnm)) = split_module_list pos s in
    LONG_BACKSLASH_CMD(pos, modidents, (rng, "\\" ^ csnm))
  )
  | '\\', (lower| upper), '@' -> BACKSLASH_MACRO(get_pos lexbuf, lexeme lexbuf)
  | '\\', (lower| upper) -> BACKSLASH_CMD(get_pos lexbuf, lexeme lexbuf)
  | '+', Plus (upper, "."), (lower| upper), '@' -> (
    let pos = get_pos lexbuf in
    let s = lexeme lexbuf in
    let len = String.length s in
    let s = String.sub s 1 (len - 2) in
    let (modidents, (rng, csnm)) = split_module_list pos s in
    LONG_PLUS_MACRO(pos, modidents, (rng, "+" ^ csnm))
  )
  | '+', Plus (upper, "."), (lower| upper) -> (
    let pos = get_pos lexbuf in
    let s = lexeme lexbuf in
    let len = String.length s in
    let s = String.sub s 1 (len - 1) in
    let (modidents, (rng, csnm)) = split_module_list pos s in
    LONG_PLUS_CMD(pos, modidents, (rng, "+" ^ csnm))
  )
  | '+', (lower| upper), '@' -> PLUS_MACRO(get_pos lexbuf, lexeme lexbuf)
  | '+', (lower| upper) -> PLUS_CMD(get_pos lexbuf, lexeme lexbuf)
  | "?'", lower -> (
    let tok = lexeme lexbuf in
    let tok_len = String.length tok in
    let tyvarnm = String.sub tok 2 (tok_len - 2) in
    let pos = get_pos lexbuf in
    ROWVAR(pos, tyvarnm)
  )
  | "'", lower -> (
    let tok = lexeme lexbuf in
    let tok_len = String.length tok in
    let tyvarnm = String.sub tok 1 (tok_len - 1) in
    let pos = get_pos lexbuf in
    TYPEVAR(pos, tyvarnm)
  )
  | ":" -> COLON(get_pos lexbuf)
  | "-" -> EXACT_MINUS(get_pos lexbuf)
  | "?"  -> QUESTION(get_pos lexbuf)
  | ":>" -> COERCE(get_pos lexbuf)
  | "#" -> ACCESS(get_pos lexbuf)
  | "->"-> ARROW(get_pos lexbuf)
  | "<-"-> REVERSED_ARROW(get_pos lexbuf)
  | "|" -> BAR(get_pos lexbuf)
  | "_" -> WILDCARD(get_pos lexbuf)
  | "," -> COMMA(get_pos lexbuf)
  | "::"-> CONS(get_pos lexbuf)
  | "=" -> EXACT_EQ(get_pos lexbuf)
  | "*" -> EXACT_TIMES(get_pos lexbuf)
  | "&" -> EXACT_AMP(get_pos lexbuf)
  | "~" -> EXACT_TILDE(get_pos lexbuf)

  | "+", Star opsymbol -> BINOP_PLUS(get_pos lexbuf, lexeme lexbuf)
  | "-", Plus opsymbol -> BINOP_MINUS(get_pos lexbuf, lexeme lexbuf)
  | "*", Plus opsymbol -> BINOP_TIMES(get_pos lexbuf, lexeme lexbuf)
  | "/", Star opsymbol -> BINOP_DIVIDES(get_pos lexbuf, lexeme lexbuf)
  | "=", Plus opsymbol -> BINOP_EQ(get_pos lexbuf, lexeme lexbuf)
  | "<", Star opsymbol -> BINOP_LT(get_pos lexbuf, lexeme lexbuf)
  | ">", Star opsymbol -> BINOP_GT(get_pos lexbuf, lexeme lexbuf)
  | "&", Plus opsymbol -> BINOP_AMP(get_pos lexbuf, lexeme lexbuf)
  | "|", Plus opsymbol -> BINOP_BAR(get_pos lexbuf, lexeme lexbuf)
  | "^", Star opsymbol -> BINOP_HAT(get_pos lexbuf, lexeme lexbuf)
  | "!", Star opsymbol -> UNOP_EXCLAM(get_pos lexbuf, lexeme lexbuf)

  | Plus (upper, "."), lower -> (
    let pos = get_pos lexbuf in
    let s = lexeme lexbuf in
    let (modidents, lower_ident) = split_module_list pos s in
    LONG_LOWER(pos, modidents, lower_ident)
  )
  | lower -> (
    let tokstr = lexeme lexbuf in
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
    | "persistent"-> PERSISTENT(pos)
    | "rec"       -> REC(pos)
    | "sig"       -> SIG(pos)
    | "signature" -> SIGNATURE(pos)
    | "struct"    -> STRUCT(pos)
    | "then"      -> THEN(pos)
    | "true"      -> TRUE(pos)
    | "type"      -> TYPE(pos)
    | "val"       -> VAL(pos)
    | "with"      -> WITH(pos)
    | _           -> LOWER(pos, tokstr)
  )
  | Plus (upper, "."), upper -> (
    let pos = get_pos lexbuf in
    let s = lexeme lexbuf in
    let (modidents, lower_ident) = split_module_list pos s in
    LONG_UPPER(pos, modidents, lower_ident)
  )
  | upper -> UPPER(get_pos lexbuf, lexeme lexbuf)
  | eof -> (
    let pos = get_pos lexbuf in
    if Stack.length stack = 1 then
      EOI(pos)
    else
      report_error lexbuf "text input ended while reading a program area"
  )

  | ("0x" | "0X"), Plus hex -> INT(get_pos lexbuf, int_of_string (lexeme lexbuf))
  | ((Opt '-', digit) | (Opt '-', nzdigit, Plus digit)), lower -> (
    let tokstr = lexeme lexbuf in
    let (size_str, unitnm) = split_length_unitnm tokstr in
    let size = size_str |> int_of_string |> float_of_int in
    let pos = get_pos lexbuf in
    LENGTH(pos, size, unitnm)
  )
  | Opt '-', Plus digit, '.', Star digit, lower -> (
    let tokstr = lexeme lexbuf in
    let (size_str, unitnm) = split_length_unitnm tokstr in
    let size = size_str |> float_of_string in
    let pos = get_pos lexbuf in
    LENGTH(pos, size, unitnm)
  )
  | Opt '-', '.', Plus digit, lower -> (
    let tokstr = lexeme lexbuf in
    let (size_str, unitnm) = split_length_unitnm tokstr in
    let size = size_str |> float_of_string in
    let pos = get_pos lexbuf in
    LENGTH(pos, size, unitnm)
  )
  | digit | (nzdigit, Plus digit) -> INT(get_pos lexbuf, int_of_string (lexeme lexbuf))
  | Plus digit, '.', Star digit | '.', Plus digit -> FLOAT(get_pos lexbuf, float_of_string (lexeme lexbuf))
  | any -> (
    let s = lexeme lexbuf in
    report_error lexbuf (Printf.sprintf "illegal token '%s' in a program area" s)
  )
  | _ -> report_error lexbuf "illegal token in a program area"


and lex_block stack lexbuf =
  match%sedlex lexbuf with
  | "%" -> (
    comment lexbuf;
    lex_block stack lexbuf
  )
  | Plus (break | space) -> (
    lex_block stack lexbuf
  )
  | "#", Star (upper, "."), lower -> (
    let pos = get_pos lexbuf in
    let s = lexeme lexbuf in
    let len = String.length s in
    let s = String.sub s 1 (len - 1) in
    let (modidents, cs) = split_module_list pos s in
    Stack.push ActiveState stack;
    VAR_IN_TEXT(pos, modidents, cs)
  )
  | '+', Plus (upper, "."), (lower| upper), '@' -> (
    let pos = get_pos lexbuf in
    let s = lexeme lexbuf in
    let len = String.length s in
    let s = String.sub s 1 (len - 2) in
    let (modidents, (rng, csnm)) = split_module_list pos s in
    Stack.push ActiveState stack;
    LONG_PLUS_MACRO(pos, modidents, (rng, "+" ^ csnm))
  )
  | '+', Plus (upper, "."), (lower| upper) -> (
    let pos = get_pos lexbuf in
    let s = lexeme lexbuf in
    let len = String.length s in
    let s = String.sub s 1 (len - 1) in
    let (modidents, (rng, csnm)) = split_module_list pos s in
    Stack.push ActiveState stack;
    LONG_PLUS_CMD(pos, modidents, (rng, "+" ^ csnm))
  )
  | '+', (lower| upper), '@' -> (
    Stack.push ActiveState stack;
    PLUS_MACRO(get_pos lexbuf, lexeme lexbuf)
  )
  | '+', (lower| upper) -> (
    Stack.push ActiveState stack;
    PLUS_CMD(get_pos lexbuf, lexeme lexbuf)
  )
  | "<" -> Stack.push BlockState stack; L_BLOCK_TEXT(get_pos lexbuf)
  | ">" -> (
    let pos = get_pos lexbuf in
    pop lexbuf "too many closing" stack;
    R_BLOCK_TEXT(pos)
  )
  | "{" -> (
    Stack.push InlineState stack;
    skip_spaces lexbuf;
    L_INLINE_TEXT(get_pos lexbuf)
  )
  | eof -> (
    if Stack.length stack = 1 then
      let pos = get_pos lexbuf in
      EOI(pos)
    else
      report_error lexbuf "unexpected end of input while reading a block text area"
  )
  | any -> (
    let s = lexeme lexbuf in
    report_error lexbuf (Printf.sprintf "unexpected character '%s' in a block text area" s)
  )
  | _ -> report_error lexbuf "unexpected character in a block text area"


and lex_inline stack lexbuf =
  match%sedlex lexbuf with
  | "%" -> (
    comment lexbuf;
    skip_spaces lexbuf;
    lex_inline stack lexbuf
  )
  | Star (break | space), "{" -> (
    Stack.push InlineState stack;
    skip_spaces lexbuf;
    L_INLINE_TEXT(get_pos lexbuf)
  )
  | Star (break | space), "}" -> (
    pop lexbuf "too many closing" stack;
    R_INLINE_TEXT(get_pos lexbuf)
  )
  | Star (break | space), "<" -> (
    Stack.push BlockState stack;
    L_BLOCK_TEXT(get_pos lexbuf)
  )
  | Star (break | space), "|" -> (
    skip_spaces lexbuf;
    BAR(get_pos lexbuf)
  )
  | Star (break | space), item -> (
    let pos = get_pos lexbuf in
    let s = lexeme lexbuf in
    let items = remove_before_spaces s in
    skip_spaces lexbuf;
    ITEM(pos, String.length items)
  )
  | break -> (
    skip_spaces lexbuf;
    BREAK(get_pos lexbuf)
  )
  | space -> (
    skip_spaces lexbuf;
    SPACE(get_pos lexbuf)
  )
  | "#", Star (upper, "."), lower -> (
    let pos = get_pos lexbuf in
    let s = lexeme lexbuf in
    let len = String.length s in
    let s = String.sub s 1 (len - 1) in
    let (modidents, cs) = split_module_list pos s in
    Stack.push ActiveState stack;
    VAR_IN_TEXT(pos, modidents, cs)
  )
  | '\\', Plus (upper, "."), (lower| upper), '@' -> (
    let pos = get_pos lexbuf in
    let s = lexeme lexbuf in
    let len = String.length s in
    let s = String.sub s 1 (len - 2) in
    let (modidents, (rng, csnm)) = split_module_list pos s in
    Stack.push ActiveState stack;
    LONG_BACKSLASH_MACRO(pos, modidents, (rng, "\\" ^ csnm))
  )
  | '\\', Plus (upper, "."), (lower| upper) -> (
    let pos = get_pos lexbuf in
    let s = lexeme lexbuf in
    let len = String.length s in
    let s = String.sub s 1 (len - 1) in
    let (modidents, (rng, csnm)) = split_module_list pos s in
    Stack.push ActiveState stack;
    LONG_BACKSLASH_CMD(pos, modidents, (rng, "\\" ^ csnm))
  )
  | '\\', (lower| upper), '@' -> (
    Stack.push ActiveState stack;
    BACKSLASH_MACRO(get_pos lexbuf, lexeme lexbuf)
  )
  | '\\', (lower| upper) -> (
    Stack.push ActiveState stack;
    BACKSLASH_CMD(get_pos lexbuf, lexeme lexbuf)
  )
  | '\\', symbol -> (
    let tok = String.sub (lexeme lexbuf) 1 1 in
    CHAR(get_pos lexbuf, tok)
  )
  | "${" -> (
    Stack.push MathState stack;
    L_MATH_TEXT(get_pos lexbuf)
  )
  | Plus "`" -> (
    let pos_start = get_pos lexbuf in
    let quote_length = String.length (lexeme lexbuf) in
    let buffer = Buffer.create 256 in
    let (pos_last, s, omit_post) = literal quote_length buffer lexbuf in
    let pos = Range.unite pos_start pos_last in
    STRING(pos, s, true, omit_post)
  )
  | "#", Plus "`" -> (
    let pos_start = get_pos lexbuf in
    let quote_length = String.length (lexeme lexbuf) - 1 in
    let buffer = Buffer.create 256 in
    let (pos_last, s, omit_post) = literal quote_length buffer lexbuf in
    let pos = Range.unite pos_start pos_last in
    STRING(pos, s, false, omit_post)
  )
  | eof -> (
    if Stack.length stack = 1 then
      let pos = get_pos lexbuf in
      EOI(pos)
    else
      report_error lexbuf "unexpected end of input while reading a inline text area"
  )
  | nonstr -> (
    let s = lexeme lexbuf in
    report_error lexbuf (Printf.sprintf "unexpected character '%s' in a inline text area" s)
  )
  | any -> (
    let pos_start = get_pos lexbuf in
    let buffer = Buffer.create 256 in
    Buffer.add_string buffer (lexeme lexbuf);
    let s = lex_inline_char buffer lexbuf in
    let pos_last = get_pos lexbuf in
    let pos = Range.unite pos_start pos_last in
    CHAR(pos, s)
  )
  | _ -> report_error lexbuf "unexpected character in a inline text area"



and lex_inline_char buffer lexbuf =
  match%sedlex lexbuf with
  | nonstr -> (
    let _ = Sedlexing.backtrack lexbuf in
    Buffer.contents buffer
  )
  | any -> (
    Buffer.add_string buffer (lexeme lexbuf);
    lex_inline_char buffer lexbuf
  )
  | _ -> (
    let _ = Sedlexing.backtrack lexbuf in
    Buffer.contents buffer
  )



and lex_math stack lexbuf =
  match%sedlex lexbuf with
  | space -> lex_math stack lexbuf
  | break -> lex_math stack lexbuf
  | "%" -> (
    comment lexbuf;
    lex_math stack lexbuf
  )
  | "?" -> QUESTION(get_pos lexbuf)
  | "!{" -> (
    let pos = get_pos lexbuf in
    Stack.push InlineState stack;
    skip_spaces lexbuf;
    L_INLINE_TEXT(pos);
  )
  | "!<" -> (
    let pos = get_pos lexbuf in
    Stack.push BlockState stack;
    L_BLOCK_TEXT(pos);
  )
  | "!(|" -> (
    let pos = get_pos lexbuf in
    Stack.push ProgramState stack;
    L_RECORD(pos);
  )
  | "!(" -> (
    let pos = get_pos lexbuf in
    Stack.push ProgramState stack;
    L_PAREN(pos);
  )
  | "![" -> (
    let pos = get_pos lexbuf in
    Stack.push ProgramState stack;
    L_SQUARE(pos);
  )
  | "{" -> (
    Stack.push MathState stack;
    L_MATH_TEXT(get_pos lexbuf);
  )
  | "}" -> (
    let pos = get_pos lexbuf in
    pop lexbuf "too many closing" stack;
    R_MATH_TEXT(pos)
  )
  | "|" -> BAR(get_pos lexbuf)
  | "^" -> SUPERSCRIPT(get_pos lexbuf)
  | "_" -> SUBSCRIPT(get_pos lexbuf)
  | Plus "'" -> (
    let n = String.length (lexeme lexbuf) in
    PRIMES(get_pos lexbuf, n)
  )
  | mathsymboltop, Star mathsymbol -> MATHCHAR(get_pos lexbuf, lexbuf |> lexeme |> get_head_uchar)
  | mathascii -> MATHCHAR(get_pos lexbuf, lexbuf |> lexeme |> get_head_uchar)
  | "#", Star (upper, "."), (lower | upper) -> (
    let pos = get_pos lexbuf in
    let s = lexeme lexbuf in
    let len = String.length s in
    let s = String.sub s 1 (len - 1) in
    let (modidents, ident) = split_module_list pos s in
    VAR_IN_TEXT(pos, modidents, ident)
  )
  | '\\', Plus (upper, "."), (lower| upper) -> (
    let pos = get_pos lexbuf in
    let s = lexeme lexbuf in
    let len = String.length s in
    let s = String.sub s 1 (len - 1) in
    let (modidents, (rng, csnm)) = split_module_list pos s in
    LONG_BACKSLASH_CMD(pos, modidents, (rng, "\\" ^ csnm))
  )
  | "\\", (lower | upper) -> BACKSLASH_CMD(get_pos lexbuf, lexeme lexbuf)
  | "\\", symbol -> MATHCHAR(get_pos lexbuf, lexbuf |> lexeme |> get_head_uchar)
  | eof -> report_error lexbuf "unexpected end of file in a math area"
  | nonmathstr -> (
    let s = lexeme lexbuf in
    report_error lexbuf (Printf.sprintf "unexpected character '%s' in a math area" s)
  )
  | any -> MATHCHAR(get_pos lexbuf, lexbuf |> lexeme |> get_head_uchar)
  | _ -> report_error lexbuf "illegal token in a math area"


and lex_active stack lexbuf =
  match%sedlex lexbuf with
  | "%" -> (
    comment lexbuf;
    lex_active stack lexbuf
  )
  | space -> lex_active stack lexbuf
  | break -> lex_active stack lexbuf
  | "~" -> EXACT_TILDE(get_pos lexbuf)
  | "?" -> QUESTION(get_pos lexbuf)
  | "(|" -> (
    Stack.push ProgramState stack;
    L_RECORD(get_pos lexbuf)
  )
  | "(" -> (
    Stack.push ProgramState stack;
    L_PAREN(get_pos lexbuf)
  )
  | "[" -> (
    Stack.push ProgramState stack;
    L_SQUARE(get_pos lexbuf)
  )
  | "{" -> (
    let pos = get_pos lexbuf in
    pop lexbuf "BUG; this cannot happen" stack;
    Stack.push InlineState stack;
    skip_spaces lexbuf;
    L_INLINE_TEXT(pos)
  )
  | "<" -> (
    let pos = get_pos lexbuf in
    pop lexbuf "BUG; this cannot happen" stack;
    Stack.push BlockState stack;
    L_BLOCK_TEXT(pos)
  )
  | ";" -> (
    let pos = get_pos lexbuf in
    pop lexbuf "BUG; this cannot happen" stack;
    SEMICOLON(pos)
  )
  | eof -> report_error lexbuf "unexpected end of input while reading an active area"
  | any -> (
    let s = lexeme lexbuf in
    report_error lexbuf (Printf.sprintf "unexpected token '%s' in an active area" s)
  )
  | _ -> report_error lexbuf "unexpected token in an active area"



and lex_header lexbuf =
  match%sedlex lexbuf with
  | lower -> (
    let headertype = lexeme lexbuf in
    let () = lex_header_sub lexbuf in
    let buffer = Buffer.create 256 in
    let content = lex_header_content buffer lexbuf in
    (headertype, content)
  )
  | any -> (
    let s = lexeme lexbuf in
    report_error lexbuf ("illegal token '" ^ s ^ "' in a program area")
  )
  | _ -> report_error lexbuf "illegal token in a program area"

and lex_header_sub lexbuf =
  match%sedlex lexbuf with
  | ':', Star ' ' -> ()
  | any -> (
    let s = lexeme lexbuf in
    report_error lexbuf ("illegal token '" ^ s ^ "' in a program area")
  )
  | _ -> report_error lexbuf "illegal token in a program area"

and lex_header_content buffer lexbuf =
  match%sedlex lexbuf with
  | break -> Buffer.contents buffer
  | eof -> Buffer.contents buffer
  | any -> (
    let s = lexeme lexbuf in
    Buffer.add_string buffer s;
    lex_header_content buffer lexbuf
  )
  | _ -> report_error lexbuf "illegal token in a program area"


and literal quote_length buffer lexbuf =
  match%sedlex lexbuf with
  | Plus '`' -> (
    let backticks = lexeme lexbuf in
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
  )
  | Plus '`', '#' -> (
    let backticks = lexeme lexbuf in
    let len = String.length backticks - 1 in
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
  )
  | break -> (
    let tok = lexeme lexbuf in
    Buffer.add_string buffer tok;
    literal quote_length buffer lexbuf
  )
  | eof -> report_error lexbuf "unexpected end of input while reading literal area"
  | any -> (
    let s = lexeme lexbuf in
    Buffer.add_string buffer s;
    literal quote_length buffer lexbuf
  )
  | _ -> report_error lexbuf "illegal token in a literal area"


and comment lexbuf =
  match%sedlex lexbuf with
  | break -> ()
  | eof   -> ()
  | any   -> comment lexbuf
  | _     -> failwith "unexpected character in a comment area"


and skip_spaces lexbuf =
  match%sedlex lexbuf with
  | break -> skip_spaces lexbuf
  | space -> skip_spaces lexbuf
  | "%" -> (
    comment lexbuf;
    skip_spaces lexbuf
  )
  | "" -> ()
  | any -> (
    let _ = Sedlexing.backtrack lexbuf in
    ()
  )
  | _ -> (
    let _ = Sedlexing.backtrack lexbuf in
    ()
  )


let cut_token stack lexbuf =
  match Stack.top stack with
  | ProgramState -> lex_program stack lexbuf
  | BlockState   -> lex_block stack lexbuf
  | InlineState  -> lex_inline stack lexbuf
  | ActiveState  -> lex_active stack lexbuf
  | MathState    -> lex_math stack lexbuf

