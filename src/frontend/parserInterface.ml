
exception Error of Range.t

module I = Parser.MenhirInterpreter

open Lexing
open Types


let k_success (utmain : header_element list * untyped_source_file) =
  utmain


let k_fail chkpt =
  match chkpt with
  | I.HandlingError(penv) ->
      let (lposS, lposE) = I.positions penv in
      let cnumS = lposS.Lexing.pos_cnum - lposS.Lexing.pos_bol in
      let cnumE = lposE.Lexing.pos_cnum - lposE.Lexing.pos_bol in
      let rng = Range.make lposS.Lexing.pos_fname lposS.Lexing.pos_lnum cnumS cnumE in
      raise (Error(rng))

  | _ ->
      assert false


let process fname lexbuf =
  let stack = Lexer.reset_to_program () in
  let () = lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = fname } in
  let supplier = I.lexer_lexbuf_to_supplier (Lexer.cut_token stack) lexbuf in
  I.loop_handle k_success k_fail supplier (Parser.Incremental.main lexbuf.Lexing.lex_curr_p)
