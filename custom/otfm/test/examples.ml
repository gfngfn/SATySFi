(* This code is in the public domain *)

let otf_postscript_name bytes =
  let print_error e = Format.eprintf "@[%a@]@." Otfm.pp_error e in
  let print_name d =
    match Otfm.postscript_name d with
       | Error e -> print_error e
       | Ok (Some n) -> Format.printf "%s@." n
       | Ok None -> ()
  in
  match Otfm.decoder (`String bytes) with
  | Error e -> print_error e
  | Ok (SingleDecoder d) -> print_name d
  | Ok (TrueTypeCollection tes) ->
     List.iter
       (fun te ->
         match Otfm.decoder_of_ttc_element te with
         | Error e -> print_error e
         | Ok d -> print_name d)
       tes
