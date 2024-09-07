
(* TODO: consider unifying this module with `InternalText` *)

let decode_utf8 (str_utf8 : string) : Uchar.t list =
  let decoder = Uutf.decoder ~encoding:`UTF_8 (`String(str_utf8)) in
  let rec loop (uchacc : Uchar.t Alist.t) =
    match Uutf.decode decoder with
    | `Await        -> assert false
    | `End          -> Alist.to_list uchacc
    | `Malformed(_) -> assert false
    | `Uchar(uch)   -> loop (Alist.extend uchacc uch)
  in
  loop Alist.empty


let encode_utf8 (uchs : Uchar.t list) : string =
  let buffer = Buffer.create (List.length uchs * 4) in
  let encoder = Uutf.encoder `UTF_8 (`Buffer(buffer)) in
  uchs |> List.iter (fun uch -> Uutf.encode encoder (`Uchar(uch)) |> ignore);
  Uutf.encode encoder `End |> ignore;
  Buffer.contents buffer
