
let solve fpath_in =
  Main.solve ~fpath_in


let arg_in : string Cmdliner.Term.t =
  let open Cmdliner in
  Arg.(required (pos 0 (some file) None (info [])))


let command_solve =
  let open Cmdliner in
  let term : unit Term.t =
    Term.(const solve $ arg_in)
  in
  let info : Cmd.info =
    Cmd.info "solve"
  in
  Cmd.v info term


let () =
  let open Cmdliner in
  let term : unit Term.t =
    Term.(ret (const (`Error(true, "No subcommand specified."))))
  in
  let info : Cmd.info =
    Cmd.info ~version:Main.version "saphe"
  in
  let subcommands =
    [
      command_solve;
    ]
  in
  Stdlib.exit (Cmd.eval (Cmd.group ~default:term info subcommands))
