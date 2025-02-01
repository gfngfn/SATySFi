
(* -- We use utf8 as the internal representation -- *)
type t = string


let of_utf8 (str_utf8 : string) : t =
  let buffer = Buffer.create (String.length str_utf8) in
  let decoder = Uutf.decoder ~encoding:`UTF_8 (`String(str_utf8)) in
  let encoder = Uutf.encoder `UTF_8 (`Buffer(buffer)) in
  let rec loop () =
    match Uutf.decode decoder with
    | `Await ->
        assert false  (* -- Manual source! -- *)

    | `End ->
        Uutf.encode encoder `End |> ignore

    | `Malformed(_) ->
        Uutf.encode encoder (`Uchar(Uchar.rep)) |> ignore;
        loop ()

    | `Uchar(ch) ->
        Uutf.encode encoder (`Uchar(ch)) |> ignore;
        loop ()
  in
  loop ();
  Buffer.contents buffer


let of_utf16be (str_utf16be : string) : t =
  let buffer = Buffer.create (String.length str_utf16be) in
  let aux () _i = function
    | `Malformed(_) -> Uutf.Buffer.add_utf_8 buffer Uutf.u_rep
    | `Uchar(uch)   -> Uutf.Buffer.add_utf_8 buffer uch
  in
  Uutf.String.fold_utf_16be aux () str_utf16be;
  Buffer.contents buffer


let to_utf16be_hex (intext : t) =
  let buffer = Buffer.create (String.length intext * 4) in
  let decoder = Uutf.decoder ~encoding:`UTF_8 (`String(intext)) in
  let rec loop () =
    match Uutf.decode decoder with
    | `Await ->
        assert false  (* -- Manual source! -- *)

    | `End ->
        ()

    | `Malformed(_) ->
        assert false  (* -- Valid utf-8! -- *)

    | `Uchar(ch) ->
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


let to_utf16be (intext : t) =
  let buffer = Buffer.create (String.length intext * 4) in
  let decoder = Uutf.decoder ~encoding:`UTF_8 (`String(intext)) in
  let encoder = Uutf.encoder `UTF_16BE (`Buffer(buffer)) in
  let rec loop () =
    match Uutf.decode decoder with
    | `Await ->
        assert false  (* -- Manual source! -- *)

    | `End ->
        Uutf.encode encoder `End |> ignore

    | `Malformed(_) ->
        Uutf.encode encoder (`Uchar(Uchar.rep)) |> ignore;
        loop ()

    | `Uchar(ch) ->
        Uutf.encode encoder (`Uchar(ch)) |> ignore;
        loop ()
  in
  loop ();
  "\xFE\xFF" ^ Buffer.contents buffer
    (* -- add BOM -- *)


let to_uchar_list (intext : t) : Uchar.t list =
  let characc = ref Alist.empty in
  let decoder = Uutf.decoder ~encoding:`UTF_8 (`String(intext)) in
  let rec loop () =
    match Uutf.decode decoder with
    | `Await ->
        assert false  (* -- Manual source! -- *)

    | `End ->
        ()

    | `Malformed(_) ->
        assert false  (* -- Valid utf-8! -- *)

    | `Uchar(ch) ->
        characc := Alist.extend !characc ch;
        loop ()
  in
  loop ();
  Alist.to_list !characc



let to_utf8 (intext : t) : string = intext


let of_uchar (uch : Uchar.t) : t =
  let buffer = Buffer.create 4 in
  let encoder = Uutf.encoder `UTF_8 (`Buffer(buffer)) in
  Uutf.encode encoder (`Uchar(uch)) |> ignore;
  Uutf.encode encoder `End |> ignore;
  Buffer.contents buffer


let of_uchar_list (uchlst : Uchar.t list) : t =
  let buffer = Buffer.create (List.length uchlst * 4) in
  let encoder = Uutf.encoder `UTF_8 (`Buffer(buffer)) in
  List.iter (fun uch ->
    Uutf.encode encoder (`Uchar(uch)) |> ignore
  ) uchlst;
  Uutf.encode encoder `End |> ignore;
  Buffer.contents buffer
