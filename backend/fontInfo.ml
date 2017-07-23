open Core.Result
open HorzBox


type font_abbrev = string
type file_name = string


exception FontFormatBroken                  of Otfm.error
exception NoGlyph                           of Uchar.t
exception InvalidFontAbbrev                 of font_abbrev
exception FailToLoadFontFormatOwingToSize   of file_name
exception FailToLoadFontFormatOwingToSystem of string

type tag = string


let string_of_file (flnmin : file_name) : string =
  try
    let bufsize = 65536 in  (* temporary; size of buffer for loading font format file *)
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


let get_decoder (src : file_name) () : Otfm.decoder =
  let s = string_of_file src in
  let dcdr = Otfm.decoder (`String(s)) in
    dcdr


module FontAbbrevHashTable
: sig
    val add : font_abbrev -> Pdftext.font * tag * file_name option -> unit
    val fold : (font_abbrev -> Pdftext.font * tag * Otfm.decoder option -> 'a -> 'a) -> 'a -> 'a
    val find_opt : font_abbrev -> (Pdftext.font * tag * Otfm.decoder option) option
  end
= struct

    module Ht = Hashtbl.Make(
      struct
        type t = font_abbrev
        let equal = (=)
        let hash = Hashtbl.hash
      end)

    let abbrev_to_definition_hash_table : (Pdftext.font * tag * Otfm.decoder option) Ht.t = Ht.create 32

    let add abbrev (font, tag, srcopt) =
      let dcdropt =
        match srcopt with
        | None      -> None
        | Some(src) -> Some(get_decoder src ())
      in
        Ht.add abbrev_to_definition_hash_table abbrev (font, tag, dcdropt)

    let fold f init =
      Ht.fold f abbrev_to_definition_hash_table init

    let find_opt (abbrev : font_abbrev) =
      try
        Some(Ht.find abbrev_to_definition_hash_table abbrev)
      with
      | Not_found -> None

  end


let get_tag (abbrev : font_abbrev) =
  match FontAbbrevHashTable.find_opt abbrev with
  | None              -> raise (InvalidFontAbbrev(abbrev))
  | Some((_, tag, _)) -> tag


let to_base85_pdf_bytes (dcdr : Otfm.decoder) : Pdfio.bytes =
  match Otfm.decoder_src dcdr with
  | `String(s) ->
      let s85 = Base85.encode s in
        Pdfio.bytes_of_string s85


let add_stream_of_decoder (pdf : Pdf.t) (dcdr : Otfm.decoder) () : int =
  let bt85 = to_base85_pdf_bytes dcdr in
  let len = Pdfio.bytes_size bt85 in
  let objstream =
    Pdf.Stream(ref (Pdf.Dictionary[
      ("/Length", Pdf.Integer(len));
      ("/Filter", Pdf.Name("/ASCII85Decode"));], Pdf.Got(bt85)))
  in
  let irstream = Pdf.addobj pdf objstream in
    irstream


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


let get_uchar_advance_width (dcdr : Otfm.decoder) (uch : Uchar.t) =
  try
    let (adv, _) = get_uchar_horz_metrics dcdr uch in adv
  with
  | NoGlyph(_) -> 0


let get_width_of_word (abbrev : font_abbrev) (fntsize : SkipLength.t) (word : string) : SkipLength.t =
  let uchar_list_of_string str =
    let rec aux acc i =
      if i < 0 then List.rev acc else
        aux ((Uchar.of_char (String.get str i)) :: acc) (i - 1)
    in
      aux [] ((String.length str) - 1)
  in
    match FontAbbrevHashTable.find_opt abbrev with
    | None               -> raise (InvalidFontAbbrev(abbrev))

    | Some((Pdftext.CIDKeyedFont(_, _, _), _, dcdropt))
    | Some((Pdftext.SimpleFont(_), _, dcdropt)) ->
        let uword = uchar_list_of_string word in
        let dcdr =
          match dcdropt with
          | None       -> assert false
          | Some(dcdr) -> dcdr
        in
        let chwidlst = List.map (fun uch -> get_uchar_advance_width dcdr uch) uword in
        let awtotal = List.fold_left (+) 0 chwidlst in
          (fntsize *% ((float_of_int awtotal) /. 1000.))

    | Some((Pdftext.StandardFont(stdfont, enc), _, _)) ->
        let awtotal = Pdfstandard14.textwidth true Pdftext.StandardEncoding stdfont word in
          (fntsize *% ((float_of_int awtotal) /. 1000.))


let get_truetype_widths_list (dcdr : Otfm.decoder) (firstchar : int) (lastchar : int) : int list =
  let rec range acc m n =
    if m > n then List.rev acc else
      range (m :: acc) (m + 1) n
  in
    (range [] firstchar lastchar) |> List.map (fun charcode ->
      get_uchar_advance_width dcdr (Uchar.of_int charcode)
    )


let make_dictionary (pdf : Pdf.t) (abbrev : font_abbrev) (fontdfn, tag, dcdropt) () =
  match fontdfn with
  | Pdftext.StandardFont(stdfont, _) ->
      Pdf.Dictionary[
        ("/Type", Pdf.Name("/Font"));
        ("/Subtype", Pdf.Name("/Type1"));
        ("/BaseFont", Pdf.Name("/" ^ (Pdftext.string_of_standard_font stdfont)));
      ]

  | Pdftext.SimpleFont(smplfont) ->
      let fontname = smplfont.Pdftext.basefont in
      begin
        match dcdropt with
        | None       -> assert false
        | Some(dcdr) ->
            match smplfont.Pdftext.fonttype with
            | Pdftext.Truetype ->
                let irstream = add_stream_of_decoder pdf dcdr () in
                  (* -- add to the PDF the stream in which the font file is embedded -- *)
                let objdescr =
                  Pdf.Dictionary[
                    ("/Type", Pdf.Name("/FontDescriptor"));
                    ("/FontName", Pdf.Name("/" ^ fontname));
                    ("/Flags", Pdf.Integer(4));  (* temporary; should be variable *)
                    ("/FontBBox", Pdf.Array[Pdf.Integer(0); Pdf.Integer(0); Pdf.Integer(0); Pdf.Integer(0)]);  (* temporary; should be variable *)
                    ("/ItalicAngle", Pdf.Integer(0));  (* temporary; should be variable *)
                    ("/Ascent", Pdf.Integer(0)); (* temporary; should be variable *)
                    ("/Descent", Pdf.Integer(0)); (* temporary; should be variable *)
                    ("/StemV", Pdf.Integer(0));  (* temporary; should be variable *)
                    ("/FontFile2", Pdf.Indirect(irstream));
                  ]
                in
                let irdescr = Pdf.addobj pdf objdescr in
                let firstchar = 0 in  (* temporary; should be variable *)
                let lastchar = 255 in  (* temporary; should be variable *)
                  Pdf.Dictionary[
                    ("/Type", Pdf.Name("/Font"));
                    ("/Subtype", Pdf.Name("/TrueType"));
                    ("/BaseFont", Pdf.Name("/" ^ smplfont.Pdftext.basefont));
                    ("/FirstChar", Pdf.Integer(firstchar));
                    ("/LastChar", Pdf.Integer(lastchar));
                    ("/Widths", Pdf.Array(List.map (fun x -> Pdf.Integer(x)) (get_truetype_widths_list dcdr firstchar lastchar)));
                    ("/FontDescriptor", Pdf.Indirect(irdescr));
                  ]

            | _ -> failwith "simple font other than TrueType; remains to be implemented."
      end

  | Pdftext.CIDKeyedFont(fontname, cidrecord, Pdftext.CMap(_)) -> failwith "CMap; remains to be implemented"
      
  | Pdftext.CIDKeyedFont(fontname (* -- name for the composite font -- *), cidrecord, Pdftext.Predefined(cmapnm)) ->
      begin
        match dcdropt with
        | None       -> assert false
        | Some(dcdr) ->
            let irstream = add_stream_of_decoder pdf dcdr () in
              (* -- add to the PDF the stream in which the font file is embedded -- *)
            let cidfontdescr = cidrecord.Pdftext.cid_fontdescriptor in
            let objdescr =
              Pdf.Dictionary[
                ("/Type", Pdf.Name("/FontDescriptor"));
                ("/FontName", Pdf.Name(cidrecord.Pdftext.cid_basefont));
                ("/Flags", Pdf.Integer(4));  (* temporary; should be variable *)
                ("/FontBBox", Pdf.Array[Pdf.Integer(0); Pdf.Integer(0); Pdf.Integer(0); Pdf.Integer(0)]);  (* temporary; should be variable *)
                ("/ItalicAngle", Pdf.Integer(0));  (* temporary; should be variable *)
                ("/Ascent", Pdf.Real(cidfontdescr.Pdftext.ascent));
                ("/Descent", Pdf.Real(cidfontdescr.Pdftext.descent));
                ("/StemV", Pdf.Integer(0));  (* temporary; should be variable *)
                ("/FontFile2", Pdf.Indirect(irstream));
              ]
            in
            let irdescr = Pdf.addobj pdf objdescr in
            let cidsysinfo = cidrecord.Pdftext.cid_system_info in
              Pdf.Dictionary[
                ("/Type", Pdf.Name("/Font"));
                ("/Subtype", Pdf.Name("/CIDFontType0")); (* temporary; should be variable *)
                ("/BaseFont", Pdf.Name(cidrecord.Pdftext.cid_basefont));
                ("/CIDSystemInfo", Pdf.Dictionary[
                  ("/Registry", Pdf.String(cidsysinfo.Pdftext.registry));
                  ("/Ordering", Pdf.String(cidsysinfo.Pdftext.ordering));
                  ("/Supplement", Pdf.Integer(cidsysinfo.Pdftext.supplement));
                ]);
                ("/FontDescriptor", Pdf.Indirect(irdescr));
              ]
      end


let get_font_dictionary (pdf : Pdf.t) () =
  [] |> FontAbbrevHashTable.fold (fun abbrev tuple acc ->
    let obj = make_dictionary pdf abbrev tuple () in
    let (_, tag, _) = tuple in
      (tag, obj) :: acc
  )


let initialize () =
  List.iter (fun (abbrev, tuple) -> FontAbbrevHashTable.add abbrev tuple) [
    ("Hlv",
     (Pdftext.SimpleFont({
       Pdftext.fonttype= Pdftext.Truetype;
       Pdftext.basefont= "Helvetica-Black";
       Pdftext.fontdescriptor= None;
       Pdftext.fontmetrics= None;
       Pdftext.encoding= Pdftext.StandardEncoding;
     }), "/F1", Some("./HelveticaBlack.ttf"))
    );
    ("TimesIt",
      (Pdftext.StandardFont(Pdftext.TimesItalic, Pdftext.StandardEncoding), "/F0", None)
    );
  ]




(* -- following are operations about handling glyphs -- *)


type contour_element =
  | OnCurve   of int * int
  | Quadratic of int * int * int * int

type contour = contour_element list


let get_contour_list (dcdr : Otfm.decoder) (uch : Uchar.t) : contour list * (int * int * int * int) =
  let gid = get_glyph_id dcdr uch in
  let gloc =
    match Otfm.loca dcdr gid with
    | Error(e)    -> raise (FontFormatBroken(e))
    | Ok(None)    -> raise (NoGlyph(uch))
    | Ok(Some(l)) -> l
  in
  let (precntrlst, bbox) =
    match Otfm.glyf dcdr gloc with
    | Error(e)               -> raise (FontFormatBroken(e))
    | Ok((`Composite(_), _)) -> raise (NoGlyph(uch)) (* temporary; does not deal with composite glyphs *)
    | Ok((`Simple(pcl), bb)) -> (pcl, bb)
  in
  let transform_contour (precntr : (bool * int * int) list) : contour =
    let (xfirst, yfirst) =
      match precntr with
      | (_, x, y) :: _ -> (x, y)
      | [] -> assert false
    in
    let rec aux acc lst =
    match lst with
    | []                                                  -> List.rev acc
    | (true, x, y) :: tail                                -> aux (OnCurve(x, y) :: acc) tail
    | (false, x1, y1) :: (true, x, y) :: tail             -> aux (Quadratic(x1, y1, x, y) :: acc) tail
    | (false, x1, y1) :: (((false, x2, y2) :: _) as tail) -> aux (Quadratic(x1, y1, (x1 + x2) / 2, (y1 + y2) / 2) :: acc) tail
    | (false, x1, y1) :: []                               -> List.rev (Quadratic(x1, y1, xfirst, yfirst) :: acc)
    in
      aux [] precntr
  in
    (List.map transform_contour precntrlst, bbox)


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
      cntr |> List.map (function
        | OnCurve(xto, yto) ->
            let prefix =
              if !isfirst then begin isfirst := false ; "M" end else "L"
            in
            let circ =
              "<circle cx=\"" ^ (~$ (display_x xto)) ^ "\" cy=\"" ^ (~$ (display_y yto)) ^ "\" r=\"5\" fill=\"green\" />"
            in
              (prefix ^ (~$ (display_x xto)) ^ "," ^ (~$ (display_y yto)), circ)
        | Quadratic(x1, y1, xto, yto) ->
            let circ =
              "<circle cx=\"" ^ (~$ (display_x x1)) ^ "\" cy=\"" ^ (~$ (display_y y1)) ^ "\" r=\"5\" fill=\"orange\" />"
                ^ "<circle cx=\"" ^ (~$ (display_x xto)) ^ "\" cy=\"" ^ (~$ (display_y yto)) ^ "\" r=\"5\" fill=\"green\" />"
            in
              ("Q" ^ (~$ (display_x x1)) ^ "," ^ (~$ (display_y y1)) ^ " " ^ (~$ (display_x xto)) ^ "," ^ (~$ (display_y yto)), circ)
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


(* for test *)
(*
let () =
  initialize () ;
(*
  let testword = List.map Uchar.of_char ['O'; 'C'; 'a'; 'm'; 'l'] in
  let res = get_width_of_word "Hlv" testword in
    res |> List.iter (fun adv -> print_endline (string_of_int adv))
*)
  let dcdr = get_decoder "Hlv" in
  let (paths, _) = svg_of_uchar (100, 100) dcdr (Uchar.of_char 'R') in
    print_endline paths
*)
