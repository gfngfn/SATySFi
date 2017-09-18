{
  open Types
  open Parser

  exception LexError of string

  type lexer_state = ProgramState | VerticalState | HorizontalState | ActiveState | CommentState | LiteralState

  type transition = HtoV | HtoA | VtoH | VtoA | PtoH | PtoV | AtoPParen | AtoPRecord | AtoPList | Paren | Record | List | Brace | Angle


  let line_no             : int ref = ref 1
  let end_of_previousline : int ref = ref 0

  let next_state  : lexer_state ref = ref ProgramState
  let first_state : lexer_state ref = ref ProgramState
  let after_literal_state : lexer_state ref = ref HorizontalState
  let after_comment_state : lexer_state ref = ref HorizontalState

  let ignore_space : bool ref = ref true
  let openqtdepth : int ref = ref 0
  let stack : transition Stack.t = Stack.create ()


  let get_start_pos lexbuf = (Lexing.lexeme_start lexbuf) - !end_of_previousline

  let get_end_pos lexbuf   = (Lexing.lexeme_end lexbuf) - !end_of_previousline

  let report_error lexbuf errmsg =
    let column_from = get_start_pos lexbuf in
    let column_to = get_end_pos lexbuf in
      raise (LexError("at line " ^ (string_of_int !line_no) ^ ", column "
        ^ (string_of_int column_from) ^ "-" ^ (string_of_int column_to) ^ ":\n    " ^ errmsg))


  let pop lexbuf errmsg =
    try Stack.pop stack with
    | Stack.Empty -> report_error lexbuf errmsg

  let push trs =
    Stack.push trs stack

  let get_pos lexbuf =
    let pos_from = get_start_pos lexbuf in
    let pos_to = get_end_pos lexbuf in
      Range.make (!line_no) pos_from pos_to


  let increment_line lexbuf =
    begin
      end_of_previousline := (Lexing.lexeme_end lexbuf);
      incr line_no;
    end


  let rec increment_line_for_each_break lexbuf str num =
    if num >= String.length str then () else
      begin
        begin
          match str.[num] with
          | ( '\n' | '\r' ) -> increment_line lexbuf
          | _               -> ()
        end;
        increment_line_for_each_break lexbuf str (num + 1)
      end


  let initialize state =
    begin
      first_state := state;
      next_state := !first_state;
      ignore_space := true;
      line_no := 1;
      end_of_previousline := 0;
      openqtdepth := 0;
      stack |> Stack.clear;
    end


  let reset_to_progexpr () =
    initialize ProgramState


  let reset_to_vertexpr () =
    initialize VerticalState


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

rule progexpr = parse
  | "%" {
      after_comment_state := ProgramState;
      next_state := CommentState;
      IGNORED
    }
  | space { progexpr lexbuf }
  | break {
      increment_line lexbuf;
      progexpr lexbuf
    }
  | "()" { UNITVALUE(get_pos lexbuf) }
  | "(" { push Paren; LPAREN(get_pos lexbuf) }
  | ")" {
      let pos = get_pos lexbuf in
      let trs = pop lexbuf "too many closing" in
        match trs with
        | Paren     -> RPAREN(pos)
        | AtoPParen -> begin next_state := ActiveState; CLOSEPROG(pos) end
        | _         -> report_error lexbuf "unbalanced ')'"
    }
  | "(|" { push Record; BRECORD(get_pos lexbuf) }
  | "|)" {
      let pos = get_pos lexbuf in
      let trs = pop lexbuf "too many closing" in
        match trs with
        | Record     -> ERECORD(pos)
        | AtoPRecord -> begin next_state := ActiveState; CLOSEPROG_AND_ERECORD(pos) end
        | _          -> report_error lexbuf "unbalanced '|)'"
    }
  | "[" { push List; BLIST(get_pos lexbuf) }
  | "]" {
      let pos = get_pos lexbuf in
      let trs = pop lexbuf "too many closing" in
        match trs with
        | List     -> ELIST(pos)
        | AtoPList -> begin next_state := ActiveState; CLOSEPROG_AND_ELIST(pos) end
        | _        -> report_error lexbuf "unbalanced ']'"
     }
  | ";" { LISTPUNCT(get_pos lexbuf) }
  | "{" {
      push PtoH;
      next_state := HorizontalState;
      ignore_space := true;
      OPENHORZ(get_pos lexbuf)
    }
  | "'<" {
      push PtoV;
      next_state := VerticalState;
      OPENVERT(get_pos lexbuf)
    }
  | "`"+ {
      openqtdepth := String.length (Lexing.lexeme lexbuf);
      after_literal_state := ProgramState;
      next_state := LiteralState;
      OPENQT(get_pos lexbuf)
    }
  | ("\\" (identifier | constructor)) {
      let tok = Lexing.lexeme lexbuf in HORZCMD(get_pos lexbuf, tok)
    }
  | ("+" (identifier | constructor)) {
      let tok = Lexing.lexeme lexbuf in VERTCMD(get_pos lexbuf, tok)
    }
  | "#"   { ACCESS(get_pos lexbuf) }
  | "->"  { ARROW(get_pos lexbuf) }
  | "<-"  { OVERWRITEEQ(get_pos lexbuf) }
  | "<<-" { OVERWRITEGLOBALHASH(get_pos lexbuf) }
  | "|"   { BAR(get_pos lexbuf) }
  | "_"   { WILDCARD(get_pos lexbuf) }
  | "."   { DOT(get_pos lexbuf) }
  | ":"   { COLON(get_pos lexbuf) }
  | ","   { COMMA(get_pos lexbuf) }
  | "::"  { CONS(get_pos lexbuf) }
  | "-"   { MINUS(get_pos lexbuf) }

(* binary operators; should be extended *)
  | "+"   { PLUS(get_pos lexbuf) }
  | "++"  { HORZCONCAT(get_pos lexbuf) }
  | "+++" { VERTCONCAT(get_pos lexbuf) }
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
  | "!"   { REFNOW(get_pos lexbuf) }
  | "!!"  { REFFINAL(get_pos lexbuf) }

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
          | "constraint"        -> CONSTRAINT(pos)
          | "let-row"           -> LETHORZ(pos)
          | "let-col"           -> LETVERT(pos)
          | _                   -> VAR(pos, tokstr)
      }
  | constructor { CONSTRUCTOR(get_pos lexbuf, Lexing.lexeme lexbuf) }
  | (digit digit*) { NUMCONST(get_pos lexbuf, Lexing.lexeme lexbuf) }
  | eof {
      if !first_state = ProgramState then EOI else
        report_error lexbuf "text input ended while reading a program area"
    }
  | _ as c { report_error lexbuf ("illegal token '" ^ (String.make 1 c) ^ "' in a program area") }


and vertexpr = parse
  | "%" {
      after_comment_state := VerticalState;
      next_state := CommentState;
      IGNORED
    }
  | (break | space)* {
      increment_line_for_each_break lexbuf (Lexing.lexeme lexbuf) 0;
      vertexpr lexbuf
    }
  | ("+" (identifier | constructor)) {
      push VtoA;
      next_state := ActiveState;
      VERTCMD(get_pos lexbuf, Lexing.lexeme lexbuf)
    }
  | ("+" (constructor ".")* (identifier | constructor)) {
      let tokstrpure = Lexing.lexeme lexbuf in
      let tokstr = String.sub tokstrpure 1 ((String.length tokstrpure) - 1) in
      let (mdlnmlst, csnm) = split_module_list tokstr in
      push VtoA;
      next_state := ActiveState;
      VERTCMDWITHMOD(get_pos lexbuf, mdlnmlst, "+" ^ csnm)
    }
  | "<" { push Angle; BVERTGRP(get_pos lexbuf) }
  | ">" {
      let pos = get_pos lexbuf in
      let trs = pop lexbuf "too many closing" in
        match trs with
        | Angle -> EVERTGRP(pos)
        | PtoV  -> begin next_state := ProgramState; CLOSEVERT(pos) end
        | HtoV  -> begin next_state := HorizontalState; ignore_space := false; EVERTGRP(pos) end
        | _     -> report_error lexbuf "unbalanced '>'"
    }
  | "{" {
      push VtoH;
      next_state := HorizontalState;
      ignore_space := true;
      BHORZGRP(get_pos lexbuf)
    }
  | eof {
      if !first_state = VerticalState then EOI else
        report_error lexbuf "unexpected end of input while reading a vertical area"
    }

and horzexpr = parse
  | "%" {
      after_comment_state := HorizontalState;
      ignore_space := true;
      next_state := CommentState;
      IGNORED
    }
  | ((break | space)* "{") {
      increment_line_for_each_break lexbuf (Lexing.lexeme lexbuf) 0;
      push Brace;
      ignore_space := true;
      BHORZGRP(get_pos lexbuf)
    }
  | ((break | space)* "}") {
      let pos = get_pos lexbuf in
      let trs = pop lexbuf "too many closing" in
        match trs with
        | Brace -> begin ignore_space := false; EHORZGRP(pos) end
        | VtoH  -> begin next_state := VerticalState; EHORZGRP(pos) end
        | PtoH  -> begin next_state := ProgramState; CLOSEHORZ(pos) end
        | _     -> report_error lexbuf "unbalanced '}'"
    }
  | ((break | space)* "<") {
      increment_line_for_each_break lexbuf (Lexing.lexeme lexbuf) 0;
      push HtoV;
      next_state := VerticalState;
      BVERTGRP(get_pos lexbuf)
    }
  | ((break | space)* "|") {
      increment_line_for_each_break lexbuf (Lexing.lexeme lexbuf) 0;
      ignore_space := true;
      SEP(get_pos lexbuf)
    }
  | break {
      increment_line lexbuf;
      if !ignore_space then horzexpr lexbuf else
        begin ignore_space := true; BREAK(get_pos lexbuf) end
    }
  | space {
      if !ignore_space then horzexpr lexbuf else
        begin ignore_space := true; SPACE(get_pos lexbuf) end
    }
  | ((break | space)* (item as itemstr)) {
      increment_line_for_each_break lexbuf (Lexing.lexeme lexbuf) 0;
      ignore_space := true;
      ITEM(get_pos lexbuf, String.length itemstr)
    }
  | ("\\" (identifier | constructor)) {
      let tok = Lexing.lexeme lexbuf in
      let rng = get_pos lexbuf in
      push HtoA;
      next_state := ActiveState;
      HORZCMD(rng, tok)
    }
  | ("\\" (constructor ".")* (identifier | constructor)) {
      let tokstrpure = Lexing.lexeme lexbuf in
      let tokstr = String.sub tokstrpure 1 ((String.length tokstrpure) - 1) in
      let (mdlnmlst, csnm) = split_module_list tokstr in
      let rng = get_pos lexbuf in
        next_state := ActiveState;
        HORZCMDWITHMOD(rng, mdlnmlst, "\\" ^ csnm)
    }
  | ("\\" symbol) {
      let tok = String.sub (Lexing.lexeme lexbuf) 1 1 in
        begin
          ignore_space := false;
          CHAR(get_pos lexbuf, tok)
        end
    }
  | ("@" identifier) {
        let tok = Lexing.lexeme lexbuf in
        let vnm = String.sub tok 1 ((String.length tok) - 1) in
          begin
            next_state := ActiveState;
            VARINSTR(get_pos lexbuf, vnm)
          end
    }
  | ((break | space)* ("`"+ as openqtstr)) {
      increment_line_for_each_break lexbuf (Lexing.lexeme lexbuf) 0;
      openqtdepth := String.length openqtstr;
      after_literal_state := HorizontalState;
      next_state := LiteralState;
      OPENQT(get_pos lexbuf)
    }
  | eof {
      if !first_state = HorizontalState then EOI else
        report_error lexbuf "unexpected end of input while reading a horizontal area"
    }
  | str+ {
      ignore_space := false;
      let tok = Lexing.lexeme lexbuf in CHAR(get_pos lexbuf, tok)
    }

  | _ as c { report_error lexbuf ("illegal token '" ^ (String.make 1 c) ^ "' in a text area") }

and active = parse
  | "%" {
      after_comment_state := ActiveState;
      next_state := CommentState;
      IGNORED
    }
  | space { active lexbuf }
  | break { increment_line lexbuf; active lexbuf }
(*
  | ("#" identifier) { let tok = Lexing.lexeme lexbuf in IDNAME(get_pos lexbuf, tok) }
  | ("." identifier) { let tok = Lexing.lexeme lexbuf in CLASSNAME(get_pos lexbuf, tok) }
*)
  | "(" {
      push AtoPParen;
      next_state := ProgramState;
      OPENPROG(get_pos lexbuf)
    }
  | "(|" {
      push AtoPRecord;
      next_state := ProgramState;
      OPENPROG_AND_BRECORD(get_pos lexbuf)
    }
  | "[" {
      push AtoPList;
      next_state := ProgramState;
      OPENPROG_AND_BLIST(get_pos lexbuf)
    }
  | "{" {
      let pos = get_pos lexbuf in
      let trs = pop lexbuf "BUG; this cannot happen" in
        match trs with
        | HtoA ->
            begin
              push Brace;
              next_state := HorizontalState;
              ignore_space := true;
              BHORZGRP(pos)
            end
        | VtoA ->
            begin
              push VtoH;
              next_state := HorizontalState;
              ignore_space := true;
              BHORZGRP(pos)
            end
        | _    -> report_error lexbuf "BUG; this cannot happen"
    }
  | "<" {
      let pos = get_pos lexbuf in
      let trs = pop lexbuf "BUG; this cannot happen" in
        match trs with
        | HtoA ->
            begin
              push HtoV;
              next_state := VerticalState;
              BVERTGRP(pos)
            end
        | VtoA ->
            begin
              push Angle;
              next_state := VerticalState;
              BVERTGRP(pos)
            end
        | _    -> report_error lexbuf "BUG; this cannot happen"
    }
  | "`"+ {
      openqtdepth := String.length (Lexing.lexeme lexbuf);
      ignore_space := false;
      after_literal_state := HorizontalState;
      next_state := LiteralState;
      OPENQT(get_pos lexbuf)
    }
  | ";" {
      let pos = get_pos lexbuf in
      let trs = pop lexbuf "BUG; this cannot happen" in
        match trs with
        | HtoA -> begin next_state := HorizontalState; ignore_space := false; ENDACTIVE(pos) end
        | VtoA -> begin next_state := VerticalState; ENDACTIVE(pos) end
        | _    -> report_error lexbuf "BUG; this cannot happen"
    }
  | eof {
      report_error lexbuf "unexpected end of input while reading an active area"
    }
  | _ {
      let tok = Lexing.lexeme lexbuf in
      report_error lexbuf ("unexpected token '" ^ tok ^ "' in active area")
    }

and literal = parse
  | "`"+ {
      let tok = Lexing.lexeme lexbuf in
      let len = String.length tok in
        if len < !openqtdepth then
          CHAR(get_pos lexbuf, tok)
        else if len > !openqtdepth then
          report_error lexbuf "literal area was closed with too many '`'s"
        else
          begin next_state := !after_literal_state; CLOSEQT(get_pos lexbuf) end
    }
  | break { increment_line lexbuf; CHAR(get_pos lexbuf, "\n") }
  | eof {
      report_error lexbuf "unexpected end of input while reading literal area"
    }
  | _ { let tok = Lexing.lexeme lexbuf in CHAR(get_pos lexbuf, tok) }

and comment = parse
  | break {
      increment_line lexbuf;
      next_state := !after_comment_state;
      IGNORED
    }
  | eof { EOI }
  | _ { comment lexbuf }

{
  let rec cut_token lexbuf =
    let output =
      match !next_state with
      | ProgramState    -> progexpr lexbuf
      | VerticalState   -> vertexpr lexbuf
      | HorizontalState -> horzexpr lexbuf
      | ActiveState     -> active lexbuf
      | CommentState    -> comment lexbuf
      | LiteralState    -> literal lexbuf
    in

    let () = print_endline (  (* for debug *)
      match output with
      | VERTCMD(_, cs) -> "VCMD(" ^ cs ^ ")"
      | HORZCMD(_, cs) -> "HCMD(" ^ cs ^ ")"
      | BHORZGRP(_)    -> "{"
      | EHORZGRP(_)    -> "}"
      | BVERTGRP(_)    -> "<"
      | EVERTGRP(_)    -> ">"
      | CHAR(_, s)     -> "\"" ^ s ^ "\""
      | SPACE(_)       -> "SPACE"
      | BREAK(_)       -> "BREAK"
      | EOI            -> "EOI"
      | LETHORZ(_)     -> "LET-ROW"
      | LETVERT(_)     -> "LET-COL"
      | VAR(_, v)      -> "VAR(" ^ v ^ ")"
      | DEFEQ(_)       -> "="
      | _              -> "_"
    ) in

      match output with
      | IGNORED -> cut_token lexbuf
      | _       -> output

}
