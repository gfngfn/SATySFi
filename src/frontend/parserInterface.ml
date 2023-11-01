exception Error of Range.t

let process fname lexbuf =
  let stack = Lexer.reset_to_program () in
  let () = Sedlexing.set_filename lexbuf fname in
  let lexer () =
    let (ante_position, post_position) =
      Sedlexing.lexing_positions lexbuf
    in
    let token = Lexer.cut_token stack lexbuf in
    (token, ante_position, post_position)
  in
  let parser =
    MenhirLib.Convert.Simplified.traditional2revised Parser.main
  in
  try parser lexer with
  | Parser.Error -> (
    let rng = Lexer.get_pos lexbuf in
    raise (Error(rng))
  )