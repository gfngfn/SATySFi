open Ocamlbuild_plugin

let () =
  rule "menhir-compile-errors"
    ~prod:"%Messages.ml"
    ~deps:["%.messages"; "%.mly"]
    (fun env _ ->
      let menhir =
        if !Options.ocamlyacc = N then V"MENHIR"
        else !Options.ocamlyacc
      in
      let grammar = env "%.mly" in
      let messages = env "%.messages" in
      let output = env "%Messages.ml" in
      Cmd(S[
        menhir;
        T (tags_of_pathname grammar ++ "ocaml" ++ "menhir");
        P grammar;
        A "--compile-errors"; P messages;
        Sh ">"; Px output;
      ])
    )
