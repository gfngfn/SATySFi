open Core_kernel
open Main__

let () =
  let fn = Sys.argv.(1) in
  let trd3 (_, _, z) = z in
  In_channel.with_file fn
    ~f:(fun in_ch ->
        Lexing.from_channel in_ch
        |> ParserInterface.process fn
          )
  |> trd3
  |> [%derive.show: Types.untyped_abstract_tree]
  |> print_endline
