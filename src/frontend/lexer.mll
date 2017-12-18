{
  open Types
  open Parser

  exception LexError of Range.t * string

  type lexer_state =
    | ProgramState
    | VerticalState
    | HorizontalState
    | ActiveState
    | CommentState
    | LiteralState
    | MathState

  type transition =
    | HtoV        (* -- inline  -> block   (with "<"   ) -- *)
    | HtoA        (* -- inline  -> active  (with "\cmd") -- *)
    | VtoH        (* -- block   -> inline  (with "{"   ) -- *)
    | VtoA        (* -- block   -> active  (with "+cmd") -- *)
    | PtoH        (* -- program -> inline  (with "{"   ) -- *)
    | PtoV        (* -- program -> block   (with "'<"  ) -- *)
    | HtoM        (* -- inline  -> math    (with "${"  ) -- *)
    | PtoM        (* -- program -> math    (with "${"  ) -- *)
    | MtoH        (* -- math    -> inline  (with "!{"  ) -- *)
    | MtoV        (* -- math    -> block   (with "!<"  ) -- *)
    | MtoPParen   (* -- math    -> program (with "!("  ) -- *)
    | MtoPRecord  (* -- math    -> program (with "!(|" ) -- *)
    | MtoPList    (* -- math    -> program (with "!["  ) -- *)
    | AtoPParen   (* -- active  -> program (with "("   ) -- *)
    | AtoPRecord  (* -- active  -> program (with "(|"  ) -- *)
    | AtoPList    (* -- active  -> program (with "["   ) -- *)
    | Paren       (* -- "("  -- *)
    | Record      (* -- "(|" -- *)
    | List        (* -- "["  -- *)
    | Brace       (* -- "{"  -- *)
    | Angle       (* -- "<"  -- *)

  let next_state  : lexer_state ref = ref ProgramState
  let first_state : lexer_state ref = ref ProgramState
  let after_literal_state : lexer_state ref = ref HorizontalState
  let after_comment_state : lexer_state ref = ref HorizontalState

  let ignore_space : bool ref = ref true
  let openqtdepth : int ref = ref 0
  let stack : transition Stack.t = Stack.create ()

  let get_pos lexbuf =
    let posS = Lexing.lexeme_start_p lexbuf in
    let posE = Lexing.lexeme_end_p lexbuf in
    let lnum = posS.Lexing.pos_lnum in
    let cnumS = posS.Lexing.pos_cnum - posS.Lexing.pos_bol in
    let cnumE = posE.Lexing.pos_cnum - posE.Lexing.pos_bol in
      Range.make lnum cnumS cnumE

  let report_error lexbuf errmsg =
    let rng = get_pos lexbuf in
      raise (LexError(rng, errmsg))

  let pop lexbuf errmsg =
    try Stack.pop stack with
    | Stack.Empty -> report_error lexbuf errmsg

  let push trs =
    Stack.push trs stack

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
    begin
      first_state := state;
      next_state := !first_state;
      ignore_space := true;
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
        match String.get tokstr i with
        | '.' -> aux imax (i + 1) (accstr :: acclst) ""
        | c   -> aux imax (i + 1) acclst (accstr ^ (String.make 1 c))
    in
      aux (String.length tokstr) 0 [] ""
}

let space = [' ' '\t']
let break = ['\n' '\r']
let digit = ['0'-'9']
let hex   = (['0'-'9'] | ['A'-'F'])
let capital = ['A'-'Z']
let small = ['a'-'z']
let latin = (small | capital)
let item  = "*"+
let identifier = (small (digit | latin | "-")*)
let constructor = (capital (digit | latin | "-")*)
let symbol = ( [' '-'@'] | ['['-'`'] | ['{'-'~'] )
let opsymbol = ( '+' | '-' | '*' | '/' | '^' | '&' | '|' | '!' | ':' | '=' | '<' | '>' | '~' | '\'' | '.' | '?' )
let str = [^ ' ' '\t' '\n' '\r' '@' '`' '\\' '{' '}' '%' '|' '*']
let mathsymbol = ( '+' | '-' | '*' | '/' | ':' | '=' | '<' | '>' | '~' | '\'' | '.' | ',' | '?' | '`' )

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
        | MtoPParen -> begin next_state := MathState; CLOSEPROG(pos) end
        | _         -> report_error lexbuf "unbalanced ')'"
    }
  | "(|" { push Record; BRECORD(get_pos lexbuf) }
  | "|)" {
      let pos = get_pos lexbuf in
      let trs = pop lexbuf "too many closing" in
        match trs with
        | Record     -> ERECORD(pos)
        | AtoPRecord -> begin next_state := ActiveState; CLOSEPROG_AND_ERECORD(pos) end
        | MtoPRecord -> begin next_state := MathState; CLOSEPROG_AND_ERECORD(pos) end
        | _          -> report_error lexbuf "unbalanced '|)'"
    }
  | "[" { push List; BLIST(get_pos lexbuf) }
  | "]" {
      let pos = get_pos lexbuf in
      let trs = pop lexbuf "too many closing" in
        match trs with
        | List     -> ELIST(pos)
        | AtoPList -> begin next_state := ActiveState; CLOSEPROG_AND_ELIST(pos) end
        | MtoPList -> begin next_state := MathState; CLOSEPROG_AND_ELIST(pos) end
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
  | "${" {
      push PtoM;
      next_state := MathState;
      OPENMATH(get_pos lexbuf)
    }
  | "<[" { BPATH(get_pos lexbuf) }
  | "]>" { EPATH(get_pos lexbuf) }
  | ".." { PATHCURVE(get_pos lexbuf) }
  | "--" { PATHLINE(get_pos lexbuf) }  (* -- prior to BINOP_MINUS -- *)
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
(*
  | "<<-" { OVERWRITEGLOBALHASH(get_pos lexbuf) }
*)
  | "|"   { BAR(get_pos lexbuf) }
  | "_"   { WILDCARD(get_pos lexbuf) }
  | "."   { DOT(get_pos lexbuf) }
  | ":"   { COLON(get_pos lexbuf) }
  | ","   { COMMA(get_pos lexbuf) }
  | "::"  { CONS(get_pos lexbuf) }
  | "-"   { EXACT_MINUS(get_pos lexbuf) }
  | "="   { DEFEQ(get_pos lexbuf) }
  | "*"   { EXACT_TIMES(get_pos lexbuf) }

(* binary operators; should be extended *)
  | ("+" opsymbol*) { BINOP_PLUS(get_pos lexbuf, Lexing.lexeme lexbuf) }
  | ("-" opsymbol+) { BINOP_MINUS(get_pos lexbuf, Lexing.lexeme lexbuf) }
  | ("*" opsymbol+) { BINOP_TIMES(get_pos lexbuf, Lexing.lexeme lexbuf) }
  | ("/" opsymbol*) { BINOP_DIVIDES(get_pos lexbuf, Lexing.lexeme lexbuf) }
  | ("=" opsymbol*) { BINOP_EQ(get_pos lexbuf, Lexing.lexeme lexbuf) }
  | ("<" opsymbol*) { BINOP_LT(get_pos lexbuf, Lexing.lexeme lexbuf) }
  | (">" opsymbol*) { BINOP_GT(get_pos lexbuf, Lexing.lexeme lexbuf) }
  | ("&" opsymbol+) { BINOP_AMP(get_pos lexbuf, Lexing.lexeme lexbuf) }
  | ("|" opsymbol+) { BINOP_BAR(get_pos lexbuf, Lexing.lexeme lexbuf) }
  | ("^" opsymbol*) { BINOP_HAT(get_pos lexbuf, Lexing.lexeme lexbuf) }
  | "!" { REFNOW(get_pos lexbuf) }
(*
  | "!!"  { REFFINAL(get_pos lexbuf) }
*)
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
          | "fun"               -> LAMBDA(pos)
          | "true"              -> TRUE(pos)
          | "false"             -> FALSE(pos)
          | "before"            -> BEFORE(pos)
          | "while"             -> WHILE(pos)
          | "do"                -> DO(pos)
          | "let-mutable"       -> LETMUTABLE(pos)
(*
          | "new-global-hash"   -> NEWGLOBALHASH(pos)
          | "renew-global-hash" -> RENEWGLOBALHASH(pos)
*)
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
          | "let-block-detailed" -> LETVERTDETAILED(pos)
          | "controls"          -> CONTROLS(pos)
          | "cycle"             -> CYCLE(pos)
          | _                   -> VAR(pos, tokstr)
      }
  | constructor { CONSTRUCTOR(get_pos lexbuf, Lexing.lexeme lexbuf) }
  | (digit digit*)                                        { INTCONST(get_pos lexbuf, int_of_string (Lexing.lexeme lexbuf)) }
  | (("0x" | "0X") hex+)                                  { INTCONST(get_pos lexbuf, int_of_string (Lexing.lexeme lexbuf)) }
  | (digit+ "." digit*)                                   { FLOATCONST(get_pos lexbuf, float_of_string (Lexing.lexeme lexbuf)) }
  | ("." digit+)                                          { FLOATCONST(get_pos lexbuf, float_of_string (Lexing.lexeme lexbuf)) }
  | (((digit digit*) as i) (identifier as unitnm))        { LENGTHCONST(get_pos lexbuf, float_of_int (int_of_string i), unitnm) }
  | (((digit+ "." digit*) as flt) (identifier as unitnm)) { LENGTHCONST(get_pos lexbuf, float_of_string flt, unitnm) }
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
      increment_line_for_each_break lexbuf (Lexing.lexeme lexbuf);
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
        | MtoV  -> begin next_state := MathState; EVERTGRP(pos) end
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
  | _ as c {
      report_error lexbuf ("unexpected character '" ^ (String.make 1 c) ^ "' in a vertical area")
    }

and horzexpr = parse
  | "%" {
      after_comment_state := HorizontalState;
      ignore_space := true;
      next_state := CommentState;
      IGNORED
    }
  | ((break | space)* "{") {
      increment_line_for_each_break lexbuf (Lexing.lexeme lexbuf);
      push Brace;
      ignore_space := true;
      BHORZGRP(get_pos lexbuf)
    }
  | ((break | space)* "}") {
      increment_line_for_each_break lexbuf (Lexing.lexeme lexbuf);
      let pos = get_pos lexbuf in
      let trs = pop lexbuf "too many closing" in
        match trs with
        | Brace -> begin ignore_space := false; EHORZGRP(pos) end
        | VtoH  -> begin next_state := VerticalState; EHORZGRP(pos) end
        | MtoH  -> begin next_state := MathState; EHORZGRP(pos) end
        | PtoH  -> begin next_state := ProgramState; CLOSEHORZ(pos) end
        | _     -> report_error lexbuf "unbalanced '}'"
    }
  | ((break | space)* "<") {
      increment_line_for_each_break lexbuf (Lexing.lexeme lexbuf);
      push HtoV;
      next_state := VerticalState;
      BVERTGRP(get_pos lexbuf)
    }
  | ((break | space)* "|") {
      increment_line_for_each_break lexbuf (Lexing.lexeme lexbuf);
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
      increment_line_for_each_break lexbuf (Lexing.lexeme lexbuf);
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
(*
  | ("@" identifier) {
        let tok = Lexing.lexeme lexbuf in
        let vnm = String.sub tok 1 ((String.length tok) - 1) in
          begin
            next_state := ActiveState;
            VARINSTR(get_pos lexbuf, vnm)
          end
    }
*)
  | ((break | space)* ("`"+ as openqtstr)) {
      increment_line_for_each_break lexbuf (Lexing.lexeme lexbuf);
      openqtdepth := String.length openqtstr;
      after_literal_state := HorizontalState;
      next_state := LiteralState;
      OPENQT(get_pos lexbuf)
    }
  | "${" {
      push HtoM;
      next_state := MathState;
      OPENMATH(get_pos lexbuf)
    }
  | eof {
      if !first_state = HorizontalState then EOI else
        report_error lexbuf "unexpected end of input while reading an inline text area"
    }
  | str+ {
      ignore_space := false;
      let tok = Lexing.lexeme lexbuf in CHAR(get_pos lexbuf, tok)
    }

  | _ as c { report_error lexbuf ("illegal token '" ^ (String.make 1 c) ^ "' in an inline text area") }

and mathexpr = parse
  | space { mathexpr lexbuf }
  | break { increment_line lexbuf; mathexpr lexbuf }
  | "!{" {
      push MtoH;
      next_state := HorizontalState;
      ignore_space := true;
      BHORZGRP(get_pos lexbuf);
    }
  | "!<" {
      push MtoV;
      next_state := VerticalState;
      BVERTGRP(get_pos lexbuf)
    }
  | "!(" {
      push MtoPParen;
      next_state := ProgramState;
      OPENPROG(get_pos lexbuf)
    }
  | "![" {
      push MtoPList;
      next_state := ProgramState;
      OPENPROG_AND_BLIST(get_pos lexbuf)
    }
  | "!(|" {
      push MtoPRecord;
      next_state := ProgramState;
      OPENPROG_AND_BRECORD(get_pos lexbuf)
    }
  | "{" {
      push Brace;
      BMATHGRP(get_pos lexbuf)
    }
  | "}" {
      let pos = get_pos lexbuf in
      let trs = pop lexbuf "too many closing" in
        match trs with
        | Brace -> EMATHGRP(pos)
        | HtoM  -> begin next_state := HorizontalState; ignore_space := false; CLOSEMATH(pos) end
        | PtoM  -> begin next_state := ProgramState; CLOSEMATH(pos) end
        | _     -> report_error lexbuf "unbalanced '}'"
    }
  | "^" { SUPERSCRIPT(get_pos lexbuf) }
  | "_" { SUBSCRIPT(get_pos lexbuf) }
  | mathsymbol+     { MATHCHAR(get_pos lexbuf, Lexing.lexeme lexbuf) }
  | (latin | digit) { MATHCHAR(get_pos lexbuf, Lexing.lexeme lexbuf) }
  | ("\\" (identifier | constructor)) {
      let tok = Lexing.lexeme lexbuf in
        MATHCMD(get_pos lexbuf, tok)
    }
  | ("\\" (constructor ".")* (identifier | constructor)) {
      let tokstrpure = Lexing.lexeme lexbuf in
      let tokstr = String.sub tokstrpure 1 ((String.length tokstrpure) - 1) in
      let (mdlnmlst, csnm) = split_module_list tokstr in
        MATHCMDWITHMOD(get_pos lexbuf, mdlnmlst, "\\" ^ csnm)
    }
  | ("\\" symbol) {
      let tok = String.sub (Lexing.lexeme lexbuf) 1 1 in
        MATHCHAR(get_pos lexbuf, tok)
    }
  | _ as c { report_error lexbuf ("illegal token '" ^ (String.make 1 c) ^ "' in a math area") }

  | eof { report_error lexbuf "unexpected end of file in a math area" }

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
      report_error lexbuf ("unexpected token '" ^ tok ^ "' in an active area")
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
      | MathState       -> mathexpr lexbuf
    in

    (* begin: for debug *)
    let () = PrintForDebug.lexerE (
      match output with
      | VERTCMD(_, cs) -> "VCMD(" ^ cs ^ ")"
      | HORZCMD(_, cs) -> "HCMD(" ^ cs ^ ")"
      | BHORZGRP(_)    -> "{ (BHORZGRP)"
      | EHORZGRP(_)    -> "} (EHORZGRP)"
      | OPENHORZ(_)    -> "{ (OPENHORZ)"
      | CLOSEHORZ(_)   -> "} (CLOSEHORZ)"
      | BVERTGRP(_)    -> "< (BVERTGRP)"
      | EVERTGRP(_)    -> "> (EVERTGRP)"
      | OPENVERT(_)    -> "'< (OPENVERT)"
      | CLOSEVERT(_)   -> "> (CLOSEVERT)"
      | OPENPROG(_)    -> "( (OPENPROG)"
      | CLOSEPROG(_)   -> ") (CLOSEPROG)"
      | END(_)         -> "; (END)"
      | ENDACTIVE(_)   -> "; (ENDACTIVE)"
      | CHAR(_, s)     -> "\"" ^ s ^ "\""
      | SPACE(_)       -> "SPACE"
      | BREAK(_)       -> "BREAK"
      | EOI            -> "EOI"
      | LETHORZ(_)     -> "let-inline"
      | LETVERT(_)     -> "let-block"
      | VAR(_, v)      -> "VAR(" ^ v ^ ")"
      | DEFEQ(_)       -> "="
      | BAR(_)         -> "|"
      | LPAREN(_)      -> "("
      | RPAREN(_)      -> ")"
      | COMMA(_)       -> ","
      | LET(_)         -> "let"
      | CONTROLS(_)    -> "controls"
      | LETAND(_)      -> "and"
      | PATHLINE(_)    -> "--"
      | PATHCURVE(_)   -> ".."
      | CYCLE(_)       -> "cycle"
      | BPATH(_)       -> "<["
      | EPATH(_)       -> "]>"
      | BMATHGRP(_)    -> "BMATHGRP"
      | EMATHGRP(_)    -> "EMATHGRP"
      | OPENMATH(_)    -> "${ (OPENMATH)"
      | CLOSEMATH(_)   -> "} (CLOSEMATH)"
      | MATHCHAR(_, s) -> "MATHCHAR(" ^ s ^ ")"
      | SUBSCRIPT(_)   -> "_ (SUBSCRIPT)"
      | SUPERSCRIPT(_) -> "^ (SUPERSCRIPT)"

      | BINOP_PLUS(_, v)
      | BINOP_MINUS(_, v)
        -> "BIN(" ^ v ^ ")"

      | _              -> "_"
    ) in
    (* end: for debug *)

      match output with
      | IGNORED -> cut_token lexbuf
      | _       -> output

}
