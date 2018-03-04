
exception Error of Range.t

module I = Parser.MenhirInterpreter


let k_success utast =
  utast


let k_fail chkpt =
  (* print_endline "k_fail";  (* for debug *) *)
  match chkpt with
  | I.HandlingError(penv) ->
      let (lposS, lposE) = I.positions penv in
      let cnumS = lposS.Lexing.pos_cnum - lposS.Lexing.pos_bol in
      let cnumE = lposE.Lexing.pos_cnum - lposE.Lexing.pos_bol in
      let rng = Range.make lposS.Lexing.pos_lnum cnumS cnumE in
      raise (Error(rng))

  | _ -> assert false


let process lexbuf =
  ErrorReporting.with_error_store (fun error_store ->
    ParserContext.with_context error_store (fun () ->
      (* print_endline "parserInterface.process";  (* for debug *) *)
      let stack = Lexer.reset_to_progexpr () in
      let supplier = I.lexer_lexbuf_to_supplier (Lexer.cut_token error_store stack) lexbuf in
        I.loop_handle k_success k_fail supplier (Parser.Incremental.main lexbuf.Lexing.lex_curr_p)
    )
  )
