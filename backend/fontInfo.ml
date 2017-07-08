
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


let get_uchar_horz_metrics (dcdr : Otfm.decoder) (uch : Uchar.t) =
  let gidkey = get_glyph_id dcdr uch in
  let hmtxres =
    Otfm.hmtx dcdr (fun accopt gid adv lsb ->
      match accopt with
      | Some(_) -> accopt
      | None    -> if gid = gidkey then Some((adv, lsb)) else None
    ) None
  in
    match hmtxres with
    | Error(e)             -> raise (FontFormatBroken(e))
    | Ok(None)             -> assert false
    | Ok(Some((adv, lsb))) -> (adv, lsb)


let get_uchar_width (dcdr : Otfm.decoder) (uch : Uchar.t) =
  let (adv, _) = get_uchar_horz_metrics dcdr uch in adv


let get_width_of_word (fntabrv : font_abbrev) (uword : Uchar.t list) =
  let dcdr = get_decoder fntabrv in
  let chwidlst = List.map (fun uch -> get_uchar_width dcdr uch) uword in
(* List.fold_left (+) 0 chwidlst *)
    chwidlst (* temporary *)


let get_contour_list (dcdr : Otfm.decoder) (uch : Uchar.t) =
  let gid = get_glyph_id dcdr uch in
  let gloc =
    match Otfm.loca dcdr gid with
    | Error(e)    -> raise (FontFormatBroken(e))
    | Ok(None)    -> raise (NoGlyph(uch))
    | Ok(Some(l)) -> l
  in
    match Otfm.glyf dcdr gloc with
    | Error(e)                     -> raise (FontFormatBroken(e))
    | Ok((`Composite(_), _))       -> raise (NoGlyph(uch)) (* temporary; does not deal with composite glyphs *)
    | Ok((`Simple(cntrlst), bbox)) -> (cntrlst, bbox)


let svg_of_uchar ((xcur, ycur) : int * int) (dcdr : Otfm.decoder) (uch : Uchar.t) =
  let (cntrlst, (xmin, ymin, xmax, ymax)) = get_contour_list dcdr uch in
  let (adv, lsb) = get_uchar_horz_metrics dcdr uch in
  let ( ~$ ) = string_of_int in
  let dpoffset = 50 in
  let display_x x = x in
  let display_y y = 1000 - y in
  let path_string_of_contour cntr =
    let isfirst = ref true in
    let lst =
      cntr |> List.map (fun (oncurve, xpos, ypos) ->
        let prefix =
          if !isfirst then begin isfirst := false ; "M" end else "L"
        in
        let circ =
          "<circle cx=\"" ^ (~$ (display_x xpos)) ^ "\" cy=\"" ^ (~$ (display_y ypos)) ^ "\" r=\"5\" fill=\"" ^
            (if oncurve then "green" else "orange") ^ "\" />"
        in
          (prefix ^ (~$ (display_x xpos)) ^ "," ^ (~$ (display_y ypos)), circ)
      )
    in
    let strlst = List.map (fun (x, _) -> x) lst in
    let circlst = List.map (fun (_, y) -> y) lst in
      ("<path d=\"" ^ (String.concat " " strlst) ^ " Z\" fill=\"none\" stroke=\"red\" stroke-width=\"5\" />", String.concat "" circlst)
  in
  let newpos = (xcur + xmax - xmin, ycur + ymax - ymin) in
  let pclst = (cntrlst |> List.map path_string_of_contour) in
  let pathlst = List.map (fun (x, _) -> x) pclst in
  let circlst = List.map (fun (_, y) -> y) pclst in
  let lst =
    List.concat [
      [
        "<?xml version=\"1.0\" encoding=\"utf-8\"?>";
        "<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\" \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">";
        "<svg xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"http://www.w3.org/1999/xlink\" width=\"1000\" height=\"1100\" viewBox=\"" ^ (~$ (display_x (0 - dpoffset))) ^ " " ^ (~$ (display_y (ymax + dpoffset))) ^ " " ^ (~$ (adv + 2 * dpoffset)) ^ " " ^ (~$ (ymax - ymin + 2 * dpoffset)) ^ "\">";
        "<rect x=\"" ^ (~$ (display_x 0)) ^ "\" y=\"" ^ (~$ (display_y ymax)) ^ "\" width=\"" ^ (~$ adv) ^ "\" height=\"" ^ (~$ (ymax - ymin)) ^ "\" fill=\"gray\" stroke=\"purple\" stroke-width=\"5\" />";
        "<rect x=\"" ^ (~$ (display_x xmin)) ^ "\" y=\"" ^ (~$ (display_y ymax)) ^ "\" width=\"" ^ (~$ (xmax - xmin)) ^ "\" height=\"" ^ (~$ (ymax - ymin)) ^ "\" fill=\"none\" stroke=\"blue\" stroke-width=\"5\" />";
      ];
      pathlst;
      circlst;
      ["</svg>"];
    ]
  in
    (String.concat "" lst, newpos)


let () =
  FontAbbrevHashTable.add font_abbrev_hash_table "Hlv" "HelveticaBlack.ttf" ;
(*
  let testword = List.map Uchar.of_char ['O'; 'C'; 'a'; 'm'; 'l'] in
  let res = get_width_of_word "Hlv" testword in
    res |> List.iter (fun adv -> print_endline (string_of_int adv))
*)
  let dcdr = get_decoder "Hlv" in
  let (paths, _) = svg_of_uchar (100, 100) dcdr (Uchar.of_char 'R') in
    print_endline paths

