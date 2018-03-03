
(* We use utf8 as the internal representation *)
type t = string

exception NotEncodableToUTF16BE of t


let of_utf8 (str_utf8 : string) : t =
  let buffer = Buffer.create (String.length str_utf8) in
  let decoder = Uutf.decoder ~encoding:`UTF_8 (`String str_utf8) in
  let encoder = Uutf.encoder `UTF_8 (`Buffer buffer) in
  let rec loop () =
    match Uutf.decode decoder with
    | `Await -> assert false (* Manual source! *)
    | `End -> ignore (Uutf.encode encoder `End)
    | `Malformed _ ->
        ignore (Uutf.encode encoder (`Uchar Uchar.rep));
        loop ()
    | `Uchar ch ->
        ignore (Uutf.encode encoder (`Uchar ch));
        loop ()
  in
  loop ();
  Buffer.contents buffer


let to_utf16be_hex (intext : t) =
  let buffer = Buffer.create (String.length intext * 4) in
  let decoder = Uutf.decoder ~encoding:`UTF_8 (`String intext) in
  let rec loop () =
    match Uutf.decode decoder with
    | `Await -> assert false (* Manual source! *)
    | `End -> ()
    | `Malformed _ -> assert false (* Valid utf-8! *)
    | `Uchar ch ->
        let cp = Uchar.to_int ch in
        if cp < 0x10000 then
          Printf.bprintf buffer "%04X" cp
        else
          Printf.bprintf buffer "%04X%04X"
            ((cp - 0x10000) / 0x0400 + 0xD800)
            ((cp - 0x10000) mod 0x0400 + 0xDC00);
        loop ()
  in
  loop ();
  Buffer.contents buffer


let to_uchar_list (intext : t) : Uchar.t list =
  let rev_chars = ref [] in
  let decoder = Uutf.decoder ~encoding:`UTF_8 (`String intext) in
  let rec loop () =
    match Uutf.decode decoder with
    | `Await -> assert false (* Manual source! *)
    | `End -> ()
    | `Malformed _ -> assert false (* Valid utf-8! *)
    | `Uchar ch ->
        rev_chars := ch :: !rev_chars;
        loop ()
  in
  loop ();
  List.rev !rev_chars



let to_utf8 (intext : t) : string = intext


let of_uchar (uch : Uchar.t) : t =
  let buffer = Buffer.create 4 in
  let encoder = Uutf.encoder `UTF_8 (`Buffer buffer) in
  ignore (Uutf.encode encoder (`Uchar uch));
  ignore (Uutf.encode encoder `End);
  Buffer.contents buffer


let of_uchar_list (uchlst : Uchar.t list) : t =
  let buffer = Buffer.create (List.length uchlst * 4) in
  let encoder = Uutf.encoder `UTF_8 (`Buffer buffer) in
  List.iter (fun uch ->
    ignore (Uutf.encode encoder (`Uchar uch))
  ) uchlst;
  ignore (Uutf.encode encoder `End);
  Buffer.contents buffer
