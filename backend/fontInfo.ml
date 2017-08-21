
(* for test *)
let print_for_debug msgln = ()


open Result
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


module KerningTable
: sig
    type t
    val create : int -> t
    val add : Otfm.glyph_id -> Otfm.glyph_id -> int -> t -> unit
    val find_opt : Otfm.glyph_id -> Otfm.glyph_id -> t -> int option
  end
= struct
    module Ht = Hashtbl.Make
      (struct
        type t = Otfm.glyph_id * Otfm.glyph_id
        let equal = (=)
        let hash = Hashtbl.hash
      end)

    type t = int Ht.t

    let create size =
      Ht.create size

    let add gid1 gid2 wid tbl =
      begin Ht.add tbl (gid1, gid2) wid ; end

    let find_opt gid1 gid2 tbl =
      try Some(Ht.find tbl (gid1, gid2)) with
      | Not_found -> None
  end


module FontAbbrevHashTable
: sig
    val add : font_abbrev -> Pdftext.font * file_name option -> unit
    val fold : (font_abbrev -> Pdftext.font * tag * (Otfm.decoder * KerningTable.t) option -> 'a -> 'a) -> 'a -> 'a
    val find_opt : font_abbrev -> (Pdftext.font * tag * (Otfm.decoder * KerningTable.t) option) option
  end
= struct

    module Ht = Hashtbl.Make(
      struct
        type t = font_abbrev
        let equal = (=)
        let hash = Hashtbl.hash
      end)

    let abbrev_to_definition_hash_table : (Pdftext.font * tag * (Otfm.decoder * KerningTable.t) option) Ht.t = Ht.create 32

    let current_tag_number = ref 0

    let generate_tag () =
        begin
          incr current_tag_number ;
          "/F" ^ (string_of_int !current_tag_number)
        end

    let add abbrev (font, srcopt) =
      let pairopt =
        match srcopt with
        | None      -> None
        | Some(src) ->
            let dcdr = get_decoder src () in
            let kerntbl = KerningTable.create 32 (* temporary; size of the hash table *) in
            let res =
              () |> Otfm.kern dcdr (fun () kinfo ->
                match kinfo with
                | {Otfm.kern_dir= `H; Otfm.kern_kind= `Kern; Otfm.kern_cross_stream= false} -> (`Fold, ())
                | _                                                                         -> (`Skip, ())
              ) (fun () gid1 gid2 wid ->
                kerntbl |> KerningTable.add gid1 gid2 wid
              )
            in
              match res with
              | Error(e) -> raise (FontFormatBroken(e))
              | Ok(())   -> Some((dcdr, kerntbl))
      in
      let tag = generate_tag () in
        Ht.add abbrev_to_definition_hash_table abbrev (font, tag, pairopt)

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


let get_uchar_raw_contour_list_and_bounding_box (dcdr : Otfm.decoder) (uch : Uchar.t)
    : ((bool * int * int) list) list * (int * int * int * int) =
  let gid = get_glyph_id dcdr uch in
  let gloc =
    match Otfm.loca dcdr gid with
    | Error(e)    -> raise (FontFormatBroken(e))
    | Ok(None)    -> raise (NoGlyph(uch))
    | Ok(Some(l)) -> l
  in
    match Otfm.glyf dcdr gloc with
    | Error(e)                        -> raise (FontFormatBroken(e))
    | Ok((`Composite(_), _))          -> raise (NoGlyph(uch))  (* temporary; does not deal with composite glyphs *)
    | Ok((`Simple(precntrlst), bbox)) -> (precntrlst, bbox)


let get_uchar_height_and_depth (dcdr : Otfm.decoder) (uch : Uchar.t) : int * int =
  try  (* temporary; for font formats that do not contain the `loca` table *)
    let (_, (_, ymin, _, ymax)) = get_uchar_raw_contour_list_and_bounding_box dcdr uch in
      (ymax, ymin)
  with FontFormatBroken(_) -> (500, -100)  (* temporary; for font formats that do not contain the `loca` table *)


let get_uchar_horz_metrics (dcdr : Otfm.decoder) (uch : Uchar.t) =
  let gidkey = get_glyph_id dcdr uch in
  let hmtxres =
    None |> Otfm.hmtx dcdr (fun accopt gid adv lsb ->
      match accopt with
      | Some(_) -> accopt
      | None    -> if gid = gidkey then Some((adv, lsb)) else None
    )
  in
    match hmtxres with
    | Error(e)             -> raise (FontFormatBroken(e))
    | Ok(None)             -> assert false
    | Ok(Some((adv, lsb))) -> (adv, lsb)


let get_uchar_advance_width (dcdr : Otfm.decoder) (uch : Uchar.t) : int =
  try
    let (adv, _) = get_uchar_horz_metrics dcdr uch in adv
  with
  | NoGlyph(_) -> 0


let get_uchar_metrics (dcdr : Otfm.decoder) (uch : Uchar.t) : int * int * int =
  let wid = get_uchar_advance_width dcdr uch in
  let (hgt, dpt) = get_uchar_height_and_depth dcdr uch in
    (wid, hgt, dpt)


let raw_length_to_skip_length (fontsize : SkipLength.t) (rawlen : int) =
  fontsize *% ((float_of_int rawlen) /. 1000.)


let get_metrics_of_word (abbrev : font_abbrev) (fontsize : SkipLength.t) (word : InternalText.t) : tj_string * skip_width * skip_height * skip_depth =
  let f_skip = raw_length_to_skip_length fontsize in
    match FontAbbrevHashTable.find_opt abbrev with
    | None               -> raise (InvalidFontAbbrev(abbrev))

    | Some((Pdftext.CIDKeyedFont(_, _, _), _, pairopt))
    | Some((Pdftext.SimpleFont(_), _, pairopt)) ->
        let uword = InternalText.to_uchar_list word in
        let (dcdr, kerntbl) =
          match pairopt with
          | None       -> assert false
          | Some(pair) -> pair
        in
        let (_, tjsacc, rawwid, rawhgt, rawdpt) =
          uword @|> (None, [], 0, 0, 0) @|> List.fold_left (fun (gidprevopt, tjsacc, wacc, hacc, dacc) uch ->
            let (w, h, d) = get_uchar_metrics dcdr uch in
            let gid = get_glyph_id dcdr uch in
            let (tjsaccnew, waccnew) =
              match gidprevopt with
              | None          -> (TJUchar(InternalText.of_uchar uch) :: tjsacc, wacc + w)
              | Some(gidprev) ->
                  match kerntbl |> KerningTable.find_opt gidprev gid with
                  | None        -> (TJUchar(InternalText.of_uchar uch) :: tjsacc, wacc + w)
                  | Some(wkern) -> (TJUchar(InternalText.of_uchar uch) :: TJKern(wkern) :: tjsacc, wacc + w + wkern)  (* -- kerning value is negative if two characters are supposed to be closer -- *)
            in
              (Some(gid), tjsaccnew, waccnew, max hacc h, min dacc d)
          )
        in
          (KernedText(List.rev tjsacc), f_skip rawwid, f_skip rawhgt, f_skip rawdpt)
            (* temporary; should reflect kerning pair information *)

    | Some((Pdftext.StandardFont(stdfont, enc), _, _)) ->
        let rawwid = Pdfstandard14.textwidth true Pdftext.StandardEncoding stdfont (InternalText.to_utf8 word) in
          (NoKernText(word), f_skip rawwid, fontsize *% 0.75, fontsize *% 0.25)  (* temporary; should get height and depth for standard 14 fonts *)


let get_truetype_widths_list (dcdr : Otfm.decoder) (firstchar : int) (lastchar : int) : int list =
  let rec range acc m n =
    if m > n then List.rev acc else
      range (m :: acc) (m + 1) n
  in
    (range [] firstchar lastchar) |> List.map (fun charcode ->
      get_uchar_advance_width dcdr (Uchar.of_int charcode)
    )


let make_dictionary (pdf : Pdf.t) (abbrev : font_abbrev) (fontdfn, tag, pairopt) () : Pdf.pdfobject =
  match fontdfn with
  | Pdftext.StandardFont(stdfont, _) ->
      Pdf.Dictionary[
        ("/Type"    , Pdf.Name("/Font"));
        ("/Subtype" , Pdf.Name("/Type1"));
        ("/BaseFont", Pdf.Name("/" ^ (Pdftext.string_of_standard_font stdfont)));
      ]

  | Pdftext.SimpleFont(smplfont) ->
      let fontname = smplfont.Pdftext.basefont in
      begin
        match pairopt with
        | None            -> assert false
        | Some((dcdr, _)) ->
            match smplfont.Pdftext.fonttype with
            | Pdftext.Truetype ->
                let irstream = add_stream_of_decoder pdf dcdr () in
                  (* -- add to the PDF the stream in which the font file is embedded -- *)
                let objdescr =
                  Pdf.Dictionary[
                    ("/Type"       , Pdf.Name("/FontDescriptor"));
                    ("/FontName"   , Pdf.Name("/" ^ fontname));
                    ("/Flags"      , Pdf.Integer(4));  (* temporary; should be variable *)
                    ("/FontBBox"   , Pdf.Array[Pdf.Integer(0); Pdf.Integer(0); Pdf.Integer(0); Pdf.Integer(0)]);  (* temporary; should be variable *)
                    ("/ItalicAngle", Pdf.Integer(0));  (* temporary; should be variable *)
                    ("/Ascent"     , Pdf.Integer(0)); (* temporary; should be variable *)
                    ("/Descent"    , Pdf.Integer(0)); (* temporary; should be variable *)
                    ("/StemV"      , Pdf.Integer(0));  (* temporary; should be variable *)
                    ("/FontFile2"  , Pdf.Indirect(irstream));
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
      
  | Pdftext.CIDKeyedFont(fontname (* -- name for the composite font -- *), cidrecord, Pdftext.Predefined(cmapencnm)) ->
      begin
        match pairopt with
        | None            -> assert false
        | Some((dcdr, _)) ->
            let irstream = add_stream_of_decoder pdf dcdr () in
              (* -- add to the PDF the stream in which the font file is embedded -- *)
            let cidfontdescr = cidrecord.Pdftext.cid_fontdescriptor in
            let objdescr =
              Pdf.Dictionary[
                ("/Type"       , Pdf.Name("/FontDescriptor"));
                ("/FontName"   , Pdf.Name("/" ^ cidrecord.Pdftext.cid_basefont));
                ("/Flags"      , Pdf.Integer(4));  (* temporary; should be variable *)
                ("/FontBBox"   , Pdf.Array[Pdf.Integer(0); Pdf.Integer(0); Pdf.Integer(0); Pdf.Integer(0)]);  (* temporary; should be variable *)
                ("/ItalicAngle", Pdf.Integer(0));  (* temporary; should be variable *)
                ("/Ascent"     , Pdf.Real(cidfontdescr.Pdftext.ascent));
                ("/Descent"    , Pdf.Real(cidfontdescr.Pdftext.descent));
                ("/StemV"      , Pdf.Real(cidfontdescr.Pdftext.stemv));
                ("/FontFile2", Pdf.Indirect(irstream));
              ]
            in
            let irdescr = Pdf.addobj pdf objdescr in
            let cidsysinfo = cidrecord.Pdftext.cid_system_info in
            let objdescend =
              Pdf.Dictionary[
                ("/Type", Pdf.Name("/Font"));
                ("/Subtype", Pdf.Name("/CIDFontType0")); (* temporary; should be variable *)
                ("/BaseFont", Pdf.Name("/" ^ cidrecord.Pdftext.cid_basefont));
                ("/CIDSystemInfo", Pdf.Dictionary[
                  ("/Registry", Pdf.String(cidsysinfo.Pdftext.registry));
                  ("/Ordering", Pdf.String(cidsysinfo.Pdftext.ordering));
                  ("/Supplement", Pdf.Integer(cidsysinfo.Pdftext.supplement));
                ]);
                ("/FontDescriptor", Pdf.Indirect(irdescr));
              ]
            in
            let irdescend = Pdf.addobj pdf objdescend in
              Pdf.Dictionary[
                ("/Type", Pdf.Name("/Font"));
                ("/Subtype", Pdf.Name("/Type0"));
                ("/Encoding", Pdf.Name("/" ^ cmapencnm));
                ("/BaseFont", Pdf.Name("/" ^ fontname));
                ("/DescendantFonts", Pdf.Array[Pdf.Indirect(irdescend)]);
              ]
      end


let get_font_dictionary (pdf : Pdf.t) () =
  print_for_debug "!!begin get_font_dictionary" ;  (* for debug *)
  let ret =  (* for debug *)
  [] |> FontAbbrevHashTable.fold (fun abbrev tuple acc ->
    let obj = make_dictionary pdf abbrev tuple () in
    let (_, tag, _) = tuple in
      (tag, obj) :: acc
  )
  in let () = print_for_debug "!!end get_font_dictionary" in ret  (* for debug *)


let initialize () =
  print_for_debug "!!begin initialize";  (* for debug *)
  List.iter (fun (abbrev, tuple) -> FontAbbrevHashTable.add abbrev tuple) [
    ("Hlv",
     (Pdftext.SimpleFont({
       Pdftext.fonttype= Pdftext.Truetype;
       Pdftext.basefont= "Helvetica-Black";
       Pdftext.fontdescriptor= None;
       Pdftext.fontmetrics= None;
       Pdftext.encoding= Pdftext.StandardEncoding;
     }), Some("./testfonts/HelveticaBlack.ttf"))
    );
    ("Arno",
     (Pdftext.SimpleFont({
       Pdftext.fonttype= Pdftext.Truetype;
       Pdftext.basefont= "ArnoPro-Regular";
       Pdftext.fontdescriptor= None;
       Pdftext.fontmetrics= None;
       Pdftext.encoding= Pdftext.StandardEncoding;
     }), Some("./testfonts/ArnoPro-Regular.otf"))
    );
    ("TimesIt",
     (Pdftext.StandardFont(Pdftext.TimesItalic, Pdftext.StandardEncoding), None)
    );

    ("KozMin",
     (Pdftext.CIDKeyedFont("KozMinComposite", {
       Pdftext.cid_system_info = {
         Pdftext.registry   = "Adobe";
         Pdftext.ordering   = "Japan1";
         Pdftext.supplement = 6;
       };
       Pdftext.cid_basefont = "KozMinPro-Medium";
       Pdftext.cid_fontdescriptor = {
         Pdftext.italicangle = 0.;
         Pdftext.ascent      = 1137.;
         Pdftext.descent     = -349.;
         Pdftext.leading     = 1500.;  (* temporary *)
         Pdftext.stemv       = 50.;    (* temporary *)
         Pdftext.avgwidth    = 1000.;
         Pdftext.maxwidth    = 1000.;
         Pdftext.fontfile    = None;  (* does not use Pdftext.fontfile field *)
       };
       Pdftext.cid_widths        = [];  (* temporary *)
       Pdftext.cid_default_width = 1000;
     }, Pdftext.Predefined("UniJIS-UTF16-H")), Some("./testfonts/KozMinPro-Medium.otf"))
    );

  ]
  ; print_for_debug "!!end initialize"  (* for debug *)




(* -- following are operations about handling glyphs -- *)


type contour_element =
  | OnCurve   of int * int
  | Quadratic of int * int * int * int

type contour = contour_element list


let get_contour_list (dcdr : Otfm.decoder) (uch : Uchar.t) : contour list * (int * int * int * int) =
  let (precntrlst, bbox) = get_uchar_raw_contour_list_and_bounding_box dcdr uch in

  let transform_contour (precntr : (bool * int * int) list) : contour =
    let (xfirst, yfirst) =
      match precntr with
      | (_, x, y) :: _ -> (x, y)
      | []             -> assert false
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
