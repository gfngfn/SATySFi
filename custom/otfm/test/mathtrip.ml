open Result

let str = Format.sprintf

let string_of_file inf =
  try
    let ic = if inf = "-" then stdin else open_in_bin inf in
    let close ic = if inf <> "-" then close_in ic else () in
    let buf_size = 65536 in
    let b = Buffer.create buf_size in
    let s = Bytes.create buf_size in
    try
      while true do
        let c = input ic s 0 buf_size in
        if c = 0 then raise Exit else
        Buffer.add_substring b (Bytes.unsafe_to_string s) 0 c
      done;
      assert false
    with
    | Exit -> close ic; Ok (Buffer.contents b)
    | Failure _ -> close ic; Error (`Msg (str "%s: input file too large" inf))
    | Sys_error e -> close ic; (Error (`Msg e));
  with
  | Sys_error e -> (Error (`Msg e))


let ( >>= ) x f = match x with Ok(v) -> f v | Error(_) as e -> e
let return v = Ok(v)


let pp = Format.fprintf
let fmt = Format.std_formatter


let pp_cmap ppf d =
  let pp_map ppf u gid = pp ppf "@,(%a GID%d)" Otfm.pp_cp u gid in
  let pp_binding ppf () k (u0, u1) gid = match k with
  | `Glyph -> for u = u0 to u1 do pp_map ppf u gid done
  | `Glyph_range -> for i = 0 to (u1 - u0) do pp_map ppf (u0 + i) (gid + i)done
  in
  pp ppf "@,@[<v1>(cmap";
  match Otfm.cmap d (pp_binding ppf) () with
  | Error _ as e -> e
  | Ok ((pid, eid, fmt), _) ->
      pp ppf "@,@[<1>(source@ (platform-id %d)@ (encoding-id %d)\
              @ (format %d))@])@]" pid eid fmt;
      Ok ()


let pp_mvr fmt mvr =
  let (main, _) = mvr in
  pp fmt "%d" main


let main () =
  let filename =
    try Sys.argv.(1) with
    | Invalid_argument(_) -> begin print_endline "illegal argument"; exit 1 end
  in
  let src =
    match string_of_file filename with
    | Ok(src)       -> src
    | Error(`Msg e) -> begin print_endline e; exit 1 end
  in
  Otfm.decoder (`String(src)) >>= function
  | Otfm.SingleDecoder(d) ->
(*
      pp_cmap fmt d >>= fun () ->
*)
      Otfm.math d >>= fun math ->
      let mgi = math.Otfm.math_glyph_info in

      let mic = mgi.Otfm.math_italics_correction in
      pp fmt "@,@[<v1>(math-italics-correction";
      mic |> List.iter (fun (gid, mvr) -> pp fmt "@,@[(GID%d %a)@]" gid pp_mvr mvr);
      pp fmt ")@]";

      let mki = mgi.Otfm.math_kern_info in
      pp fmt "@,@[<v1>(math-kern-info";
      mki |> List.iter (fun (gid, mk) -> pp fmt "@,@[(GID%d)@]" gid);
      pp fmt ")@]";
      return ()

  | Otfm.TrueTypeCollection(_) ->
      Format.eprintf "TrueType Collection";
      return ()


let () =
  match main () with
  | Error(e) -> Format.eprintf "@[%a@]@." Otfm.pp_error e
  | Ok(())   -> ()
