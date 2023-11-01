open Core
open Main__

let pp_exn fmt ex =
  Format.fprintf fmt "%s" @@ Exn.to_string ex

let p s =
  let try_parse l = Result.try_with (fun () -> ParserInterface.process "" l)
  in
  Lexing.from_string s
  |> try_parse
  |> Result.map ~f:snd
  |> [%derive.show: (Types.untyped_source_file, exn) result]
  |> print_endline

(* test template
open P

let%expect_test _ =
  p {|
module M = struct
   % string to be parsed
end
|}; [%expect]
   *)
