{
  open Types
  open Parser

  exception LexError of string

  type lexer_state = ProgramState | VerticalState | HorizontalState | ActiveState | CommentState | LiteralState

  type horz_origin = ProgramToHorz | VertToHorz

  type vert_origin = ProgramToVert | HorzToVert

  let line_no             : int ref = ref 1
  let end_of_previousline : int ref = ref 0

  let next_state  : lexer_state ref = ref ProgramState
  let first_state : lexer_state ref = ref ProgramState
  let after_literal_state : lexer_state ref = ref HorizontalState
  let after_comment_state : lexer_state ref = ref HorizontalState

  let ignore_space : bool ref = ref true
  let openqtdepth : int ref = ref 0
  let progdepth : int ref = ref 0
  let horzdepth : int ref = ref 0
  let vertdepth : int ref = ref 0
  let progdepth_stack : int Stack.t = Stack.create ()
  let horzdepth_stack : (int * horz_origin) Stack.t = Stack.create ()
  let vertdepth_stack : (int * vert_origin) Stack.t = Stack.create ()


  let back_from_horz origin =
    match origin with
    | ProgramToHorz -> ProgramState
    | VertToHorz    -> VerticalState


  let back_from_vert origin =
    match origin with
    | ProgramToVert -> ProgramState
    | HorzToVert    -> HorizontalState


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
      progdepth := 0;
      horzdepth := 0;
      vertdepth := 0;
      progdepth_stack |> Stack.clear;
      horzdepth_stack |> Stack.clear;
      vertdepth_stack |> Stack.clear;
    end


  let reset_to_progexpr () =
    initialize ProgramState


  let reset_to_horzexpr () =
    initialize HorizontalState


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
  | "(" { incr progdepth; LPAREN(get_pos lexbuf) }
  | ")" {
      let pos = get_pos lexbuf in
      decr progdepth;
      if Stack.is_empty progdepth_stack then
        RPAREN(pos)
      else
        if !progdepth = Stack.top progdepth_stack then
        begin
          Stack.pop progdepth_stack |> ignore;
          next_state := ActiveState;
          CLOSEPROG(pos)
        end
        else
          RPAREN(pos)
    }
  | "(|" { incr progdepth; BRECORD(get_pos lexbuf) }
  | "|)" {
        let pos = get_pos lexbuf in
        decr progdepth;
        if Stack.is_empty progdepth_stack then
          ERECORD(pos)
        else
          if !progdepth = Stack.top progdepth_stack then
          begin
            Stack.pop progdepth_stack |> ignore;
            next_state := ActiveState;
            CLOSEPROG_AND_ERECORD(pos)
          end
          else
            ERECORD(pos)
      }
  | "[" { incr progdepth; BLIST(get_pos lexbuf) }
  | "]" {
        let pos = get_pos lexbuf in
        decr progdepth;
        if Stack.is_empty progdepth_stack then
          ELIST(pos)
        else
          if !progdepth = Stack.top progdepth_stack then
          begin
            Stack.pop progdepth_stack |> ignore;
            next_state := ActiveState;
            CLOSEPROG_AND_ELIST(pos)
          end
          else
            ELIST(pos)
         }
  | ";" { LISTPUNCT(get_pos lexbuf) }
  | "{" {
      horzdepth_stack |> Stack.push (!horzdepth, ProgramToHorz);
      incr horzdepth;
      next_state := HorizontalState;
      ignore_space := true;
      OPENHORZ(get_pos lexbuf)
    }
  | "'<" {
    vertdepth_stack |> Stack.push (!vertdepth, ProgramToVert);
    incr vertdepth;
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
          | _                   -> VAR(pos, tokstr)
      }
  | constructor { CONSTRUCTOR(get_pos lexbuf, Lexing.lexeme lexbuf) }
  | (digit digit*) { NUMCONST(get_pos lexbuf, Lexing.lexeme lexbuf) }
  | eof {
        if !first_state = ProgramState then EOI else
          raise (LexError(error_reporting lexbuf ("text input ended while reading a program area")))
      }
  | _ as c { raise (LexError(error_reporting lexbuf ("illegal token '" ^ (String.make 1 c) ^ "' in a program area"))) }


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
      VERTCMD(get_pos lexbuf, Lexing.lexeme lexbuf)
    }
  | ("+" (constructor ".")* (identifier | constructor)) {
      let tokstrpure = Lexing.lexeme lexbuf in
      let tokstr = String.sub tokstrpure 1 ((String.length tokstrpure) - 1) in
      let (mdlnmlst, csnm) = split_module_list tokstr in
      let rng = get_pos lexbuf in
        next_state := ActiveState;
        VERTCMDWITHMOD(rng, mdlnmlst, "+" ^ csnm)
    }
  | "<" {
      incr vertdepth;
      BVERTGRP(get_pos lexbuf)
    }
  | ">" {
      let pos = get_pos lexbuf in
      decr vertdepth;
      if Stack.is_empty vertdepth_stack then
        EVERTGRP(pos)
      else
        let (entering, origin) = Stack.top vertdepth_stack in
        if !vertdepth = entering then
          begin
            Stack.pop vertdepth_stack |> ignore;
            next_state := back_from_vert origin;
            CLOSEVERT(pos)
          end
        else
          EVERTGRP(pos)
    }
  | "{" {
      horzdepth_stack |> Stack.push (!horzdepth, VertToHorz);
      incr horzdepth;
      next_state := HorizontalState;
      ignore_space := true;
      BHORZGRP(get_pos lexbuf)
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
      incr horzdepth;
      ignore_space := true;
      BHORZGRP(get_pos lexbuf)
    }
  | ((break | space)* "}") {
      decr horzdepth;
      increment_line_for_each_break lexbuf (Lexing.lexeme lexbuf) 0;
      if Stack.is_empty horzdepth_stack then
        begin
          ignore_space := false;
          EHORZGRP(get_pos lexbuf)
        end
      else
        let (entering, origin) = Stack.top horzdepth_stack in
        if !horzdepth = entering then
          begin
            Stack.pop horzdepth_stack |> ignore;
            next_state := back_from_horz origin;
            CLOSEHORZ(get_pos lexbuf)
          end
        else
          begin
            ignore_space := false;
            EHORZGRP(get_pos lexbuf)
          end
    }
  | ((break | space)* "<") {
      increment_line_for_each_break lexbuf (Lexing.lexeme lexbuf) 0;
      incr vertdepth;
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
      if !ignore_space then horzexpr lexbuf else begin ignore_space := true; BREAK(get_pos lexbuf) end
    }
  | space {
      if !ignore_space then horzexpr lexbuf else begin ignore_space := true; SPACE(get_pos lexbuf) end
    }
  | ((break | space)* (item as itemstr)) {
      increment_line_for_each_break lexbuf (Lexing.lexeme lexbuf) 0;
      ignore_space := true;
      ITEM(get_pos lexbuf, String.length itemstr)
    }
  | ("\\" (identifier | constructor)) {
      let tok = Lexing.lexeme lexbuf in
      let rng = get_pos lexbuf in
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
        raise (LexError(error_reporting lexbuf "program input ended while reading a text area"))
    }
  | str+ {
      ignore_space := false;
      let tok = Lexing.lexeme lexbuf in CHAR(get_pos lexbuf, tok)
    }

  | _ as c { raise (LexError(error_reporting lexbuf "illegal token '" ^ (String.make 1 c) ^ "' in a text area"))}

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
      progdepth_stack |> Stack.push !progdepth;
      incr progdepth;
      next_state := ProgramState;
      OPENPROG(get_pos lexbuf)
    }
  | "(|" {
      progdepth_stack |> Stack.push !progdepth;
      incr progdepth;
      next_state := ProgramState;
      OPENPROG_AND_BRECORD(get_pos lexbuf)
    }
  | "[" {
      progdepth_stack |> Stack.push !progdepth;
      incr progdepth;
      next_state := ProgramState;
      OPENPROG_AND_BLIST(get_pos lexbuf)
    }
  | "{" {
      incr horzdepth;
      next_state := HorizontalState;
      ignore_space := true;
      BHORZGRP(get_pos lexbuf)
    }
  | "`"+ {
      openqtdepth := String.length (Lexing.lexeme lexbuf);
      ignore_space := false;
      after_literal_state := HorizontalState;
      next_state := LiteralState;
      OPENQT(get_pos lexbuf)
    }
  | ";" {
      next_state := HorizontalState;
      ignore_space := false;
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
          begin next_state := !after_literal_state; CLOSEQT(get_pos lexbuf) end
    }
  | break { increment_line lexbuf; CHAR(get_pos lexbuf, "\n") }
  | eof {
      raise (LexError(error_reporting lexbuf "input ended while reading literal area"))
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
      match output with
      | IGNORED -> cut_token lexbuf
      | _       -> output

}
