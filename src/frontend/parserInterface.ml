
module I = Parser.MenhirInterpreter

open MyUtil
open Types


let k_success (utsrc : untyped_source_file) =
  utsrc


let k_fail chkpt =
  match chkpt with
  | I.HandlingError(penv) ->
      let (lposS, lposE) = I.positions penv in
      let cnumS = lposS.Lexing.pos_cnum - lposS.Lexing.pos_bol in
      let cnumE = lposE.Lexing.pos_cnum - lposE.Lexing.pos_bol in
      let rng = Range.make lposS.Lexing.pos_fname lposS.Lexing.pos_lnum cnumS cnumE in
      raise (ParseError(CannotProgressParsing(rng)))

  | _ ->
      assert false


let process_common (abspath : abs_path) (lexbuf : Lexing.lexbuf) =
  let open ResultMonad in
  let abspathstr = get_abs_path_string abspath in
  let stack = Lexer.reset_to_program () in
  lexbuf.Lexing.lex_curr_p <- { lexbuf.Lexing.lex_curr_p with pos_fname = abspathstr };
  let supplier = I.lexer_lexbuf_to_supplier (Lexer.cut_token stack) lexbuf in
  try
    return @@ I.loop_handle k_success k_fail supplier (Parser.Incremental.main lexbuf.Lexing.lex_curr_p)
  with
  | ParseError(e) ->
      err e


let process_file (abspath : abs_path) =
  let inc = AbsPathIo.open_in abspath in
  let lexbuf = Lexing.from_channel inc in
  let res = process_common abspath lexbuf in
  close_in inc;
  res


let process_text (abspath : abs_path) (s : string) =
  let lexbuf = Lexing.from_string s in
  process_common abspath lexbuf
