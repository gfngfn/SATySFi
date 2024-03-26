open Core
open MyUtil
open Main__

let () =
  let proj s = function
    | Error(e) ->
        Out_channel.fprintf stderr "%s: parse error: %s\n" s @@ Types.show_parse_error e;
        exit 1
    | Ok(utsrc) ->
        utsrc
  in
  Out_channel.print_endline ";;; generated automatically. DO NOT EDIT";
  Out_channel.print_endline ";;; To update this file, you should run `dune runtest; dune promote`.";
  let argv = Sys.get_argv () in
  argv
  |> Array.to_list
  |> List.tl
  |> Option.value ~default:[]
  |> List.iter ~f:begin fun fname ->
    Out_channel.printf "\n;; %s\n" fname;
    In_channel.with_file fname
      ~f:(fun in_ch ->
          Lexing.from_channel in_ch
          |> ParserInterface.process_common (make_abs_path (Filename.concat "/path/to" fname))
        )
    |> proj (argv.(0))
    |> [%derive.show: Types.untyped_source_file]
    |> print_endline
  end
