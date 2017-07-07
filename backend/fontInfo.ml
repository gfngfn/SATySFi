
open Core.Result


type font_abbrev = string
type file_name = string

exception FontFormatBroken of Otfm.error
exception NoGlyph of Uchar.t
exception InvalidFontAbbrev of font_abbrev
exception FailToLoadFontFormatOwingToSize   of file_name
exception FailToLoadFontFormatOwingToSystem of string


module FontAbbrevHashTable = Hashtbl.Make(
  struct
    type t = font_abbrev
    let equal = (=)
    let hash = Hashtbl.hash
  end)

module FontFileNameHashTable = Hashtbl.Make(
  struct
    type t = file_name
    let equal = (=)
    let hash = Hashtbl.hash
  end)


let font_abbrev_hash_table : file_name FontAbbrevHashTable.t = FontAbbrevHashTable.create 32

let font_file_name_hash_table : Otfm.decoder FontFileNameHashTable.t = FontFileNameHashTable.create 32


let string_of_file (flnmin : file_name) : string =
  try
    let bufsize = 65536 in
    let buf : Buffer.t = Buffer.create bufsize in
    let byt : bytes = Bytes.create bufsize in
    let ic : in_channel = open_in_bin flnmin in
      try
        begin
          while true do
            let c = input ic byt 0 bufsize in
              if c = 0 then raise Exit else
                Buffer.add_substring buf (Bytes.unsafe_to_string byt) 0 c
          done ;
          assert false
        end
      with
      | Exit           -> begin close_in ic ; Buffer.contents buf end
      | Failure(_)     -> begin close_in ic ; raise (FailToLoadFontFormatOwingToSize(flnmin)) end
      | Sys_error(msg) -> begin close_in ic ; raise (FailToLoadFontFormatOwingToSystem(msg)) end
  with
  | Sys_error(msg) -> raise (FailToLoadFontFormatOwingToSystem(msg))


let get_decoder (fntabrv : font_abbrev) : Otfm.decoder =
  let flnmin = 
    try
      FontAbbrevHashTable.find font_abbrev_hash_table fntabrv
    with
    | Not_found -> raise (InvalidFontAbbrev(fntabrv))
  in
    try
      FontFileNameHashTable.find font_file_name_hash_table flnmin
    with
    | Not_found ->
        let s = string_of_file flnmin in
        let dcdr = Otfm.decoder (`String(s)) in
        begin
          FontFileNameHashTable.add font_file_name_hash_table flnmin dcdr ;
          dcdr
        end


let get_glyph_id (dcdr : Otfm.decoder) (uch : Uchar.t) : Otfm.glyph_id =
  let cp = Uchar.to_int uch in
  let cmapres =
    Otfm.cmap dcdr (fun accopt mapkd (u0, u1) gid ->
      match accopt with
      | Some(_) -> accopt
      | None    -> if u0 <= cp && cp <= u1 then Some(gid + (cp - u0)) else None
    ) None
  in
    match cmapres with
    | Error(e)                   -> raise (FontFormatBroken(e))
    | Ok(((_, _, _), None))      -> raise (NoGlyph(uch))
    | Ok(((_, _, _), Some(gid))) -> gid


let get_uchar_width (dcdr : Otfm.decoder) (uch : Uchar.t) =
  let gidkey = get_glyph_id dcdr uch in
  let hmtxres =
    Otfm.hmtx dcdr (fun accopt gid adv lsb ->
      match accopt with
      | Some(_) -> accopt
      | None    -> if gid = gidkey then Some(adv) else None
    ) None
  in
    match hmtxres with
    | Error(e)      -> raise (FontFormatBroken(e))
    | Ok(None)      -> assert false
    | Ok(Some(adv)) -> adv


let get_width_of_word (fntabrv : font_abbrev) (uword : Uchar.t list) =
  let dcdr = get_decoder fntabrv in
  let chwidlst = List.map (fun uch -> get_uchar_width dcdr uch) uword in
(* List.fold_left (+) 0 chwidlst *)
    chwidlst (* temporary *)


let () =
  FontAbbrevHashTable.add font_abbrev_hash_table "Hlv" "HelveticaBlack.ttf" ;
  let res = get_width_of_word "Hlv" (List.map Uchar.of_char ['O'; 'C'; 'a'; 'm'; 'l']) in
    res |> List.iter (fun adv -> print_endline (string_of_int adv))
