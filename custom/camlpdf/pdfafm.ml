(* Parse Adobe Font Metrics files *)
open Pdfutil

let print_lexeme = function
  | Genlex.Kwd s -> Printf.printf "KEYWORD: %s\n" s
  | Genlex.Ident s -> Printf.printf "IDENT: %s\n" s
  | Genlex.Int i -> Printf.printf "INT: %i\n" i
  | Genlex.Float f -> Printf.printf "FLOAT: %f\n" f
  | Genlex.String s -> Printf.printf "STRING: %s\n" s
  | Genlex.Char c -> Printf.printf "CHAR: %c\n" c

let read_char_metrics_lexer = Genlex.make_lexer ["C"; ";"; "WX"; "N"]

let read_char_metrics_line l =
  match
    Stream.npeek 8 (read_char_metrics_lexer (Stream.of_string l))
  with
  | Genlex.Kwd "C"::Genlex.Int charnum::Genlex.Kwd ";"::
    Genlex.Kwd "WX"::Genlex.Int width::Genlex.Kwd ";"::
    Genlex.Kwd "N"::(Genlex.Ident name | Genlex.Kwd name)::_ ->
      (name, (charnum, width))
  | x -> iter print_lexeme x; failwith "badline"

let lookup_charnum table name =
  match Hashtbl.find table name with (c', _) -> c'

let read_kern_line_lexer = Genlex.make_lexer ["KPX"]

let read_kern_line l =
  match
    Stream.npeek 4 (read_kern_line_lexer (Stream.of_string l))
  with
    | Genlex.Kwd "KPX"::Genlex.Ident n::Genlex.Ident n'::Genlex.Int i::_ ->
        n, n', i
    | x -> iter print_lexeme x; failwith "badline2"

let string_starts_with sub s =
  let sublength = String.length sub in
    if String.length s < sublength then false else
      let rec loop n =
        if n < 0 then true
        else s.[n] = sub.[n] && loop (n - 1)
      in
        loop (sublength - 1)

let get_tables lines =
  let char_metrics_lines =
    isolate
      (string_starts_with "StartCharMetrics")
      (string_starts_with "EndCharMetrics")
      lines
  and kern_lines =
    isolate
      (string_starts_with "StartKernPairs")
      (string_starts_with "EndKernPairs")
      lines
  and header_lines =
    map
      (fun s ->
         let a, b = cleavewhile (neq ' ') (explode s) in (implode a, implode b))
      (takewhile (notpred (string_starts_with "C ")) lines)
  in
    let remove_empty = lose (fun x -> String.length x < 5) in
    let charmetrics =
      map read_char_metrics_line (remove_empty char_metrics_lines)
    in
      let charmetrics_hash = hashtable_of_dictionary charmetrics in
      let kerns =
        map read_kern_line (remove_empty kern_lines)
      in
        header_lines,
        option_map
          (fun (_, (c, w)) -> if c > -1 then Some (c, w) else None)
          charmetrics,
        option_map
          (fun (n, n', kern) ->
             let p = lookup_charnum charmetrics_hash n
             and p' = lookup_charnum charmetrics_hash n' in
             if p > -1 && p' > -1 then Some (p, p', kern) else None)
          kerns,
        option_map
        (fun (name, (_, w)) -> Some (name, w))
          charmetrics

let read i =
  try
    let lines = Pdfio.read_lines i in
      get_tables lines
  with
    e -> failwith (Printexc.to_string e)
