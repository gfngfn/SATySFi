
(* for test *)
let print_for_debug msgln = ()


open Result
open HorzBox


type font_abbrev = string

exception InvalidFontAbbrev of font_abbrev

type tag = string

module GlyphIDTable
: sig
    type t
    val create : int -> t
    val add : Uchar.t -> FontFormat.glyph_id -> t -> unit
    val find_opt : Uchar.t -> t -> FontFormat.glyph_id option
  end
= struct
    module Ht = Hashtbl.Make
      (struct
        type t = Uchar.t
        let equal = (=)
        let hash = Hashtbl.hash
      end)

    type t = FontFormat.glyph_id Ht.t

    let create = Ht.create

    let add uch gid gidtbl = Ht.add gidtbl uch gid

    let find_opt uch gidtbl =
      try Some(Ht.find gidtbl uch) with
      | Not_found -> None
  end


type font_registration =
  | Type1Registration        of int * int
  | TrueTypeRegistration     of int * int
  | CIDFontType0Registration of string * FontFormat.cmap


module FontAbbrevHashTable
: sig
    val add : font_abbrev -> font_registration -> FontFormat.file_path -> unit
    val fold : (font_abbrev -> FontFormat.font * tag * Otfm.decoder * GlyphIDTable.t * FontFormat.KerningTable.t -> 'a -> 'a) -> 'a -> 'a
    val find_opt : font_abbrev -> (FontFormat.font * tag * Otfm.decoder * GlyphIDTable.t * FontFormat.KerningTable.t) option
  end
= struct

    module Ht = Hashtbl.Make(
      struct
        type t = font_abbrev
        let equal = (=)
        let hash = Hashtbl.hash
      end)

    let abbrev_to_definition_hash_table : (FontFormat.font * tag * Otfm.decoder * GlyphIDTable.t * FontFormat.KerningTable.t) Ht.t = Ht.create 32

    let current_tag_number = ref 0

    let generate_tag () =
        begin
          incr current_tag_number ;
          "/F" ^ (string_of_int !current_tag_number)
        end

    let add abbrev fontreg srcfile =
      let dcdr = FontFormat.get_decoder srcfile () in
      let kerntbl = FontFormat.get_kerning_table dcdr in
      let font =
        match fontreg with
        | Type1Registration(fc, lc) ->
            let ty1font = FontFormat.Type1.of_decoder dcdr fc lc in
              FontFormat.type1 ty1font
        | TrueTypeRegistration(fc, lc) ->
            let trtyfont = FontFormat.TrueType.of_decoder dcdr fc lc in
              FontFormat.true_type trtyfont
        | CIDFontType0Registration(fontname, cmap) ->
            let cidsysinfo = FontFormat.adobe_japan1 in
            let cidty0font = FontFormat.CIDFontType0.of_decoder dcdr cidsysinfo in
              FontFormat.cid_font_type_0 cidty0font fontname cmap
      in
      let tag = generate_tag () in
      let gidtbl = GlyphIDTable.create 256 in  (* temporary *)
        Ht.add abbrev_to_definition_hash_table abbrev (font, tag, dcdr, gidtbl, kerntbl)

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
  | None                    -> raise (InvalidFontAbbrev(abbrev))
  | Some((_, tag, _, _, _)) -> tag


let raw_length_to_skip_length (fontsize : SkipLength.t) (rawlen : int) =
  fontsize *% ((float_of_int rawlen) /. 1000.)


let get_glyph_id dcdr (gidtbl : GlyphIDTable.t) (uch : Uchar.t) : FontFormat.glyph_id option =
  match gidtbl |> GlyphIDTable.find_opt uch with
  | Some(gid) -> Some(gid)
  | None      ->
      match FontFormat.get_glyph_id dcdr uch with
      | None      -> None
      | Some(gid) -> begin gidtbl |> GlyphIDTable.add uch gid ; Some(gid) end


let get_metrics_of_word (abbrev : font_abbrev) (fontsize : SkipLength.t) (word : InternalText.t) : tj_string * skip_width * skip_height * skip_depth =
  let f_skip = raw_length_to_skip_length fontsize in
    match FontAbbrevHashTable.find_opt abbrev with
    | None                                -> raise (InvalidFontAbbrev(abbrev))
    | Some((_, _, dcdr, gidtbl, kerntbl)) ->
        let uword = InternalText.to_uchar_list word in
        let (_, tjsacc, rawwid, rawhgt, rawdpt) =
          uword @|> (None, [], 0, 0, 0) @|> List.fold_left (fun (gidprevopt, tjsacc, wacc, hacc, dacc) uch ->
            match get_glyph_id dcdr gidtbl uch with
            | None      -> (None, tjsacc, wacc, hacc, dacc)
                (* temporary; simply ignores character that is not assigned a glyph in the current font *)
            | Some(gid) ->
                let (w, h, d) = FontFormat.get_glyph_metrics dcdr gid in
                let (tjsaccnew, waccnew) =
                  match gidprevopt with
                  | None          -> (TJUchar(InternalText.of_uchar uch) :: tjsacc, wacc + w)
                  | Some(gidprev) ->
                      match kerntbl |> FontFormat.KerningTable.find_opt gidprev gid with
                      | None        -> (TJUchar(InternalText.of_uchar uch) :: tjsacc, wacc + w)
                      | Some(wkern) -> (TJUchar(InternalText.of_uchar uch) :: TJKern(wkern) :: tjsacc, wacc + w + wkern)
                          (* -- kerning value is negative if two characters are supposed to be closer -- *)
                in
                  (Some(gid), tjsaccnew, waccnew, max hacc h, min dacc d)
          )
        in
          (KernedText(List.rev tjsacc), f_skip rawwid, f_skip rawhgt, f_skip rawdpt)


let make_dictionary (pdf : Pdf.t) (abbrev : font_abbrev) (fontdfn, tag, dcdr, _, _) () : Pdf.pdfobject =
  match fontdfn with
  | FontFormat.Type1(ty1font)     -> FontFormat.Type1.to_pdfdict pdf ty1font dcdr
  | FontFormat.TrueType(trtyfont) -> FontFormat.TrueType.to_pdfdict pdf trtyfont dcdr
  | FontFormat.Type0(ty0font)     -> FontFormat.Type0.to_pdfdict pdf ty0font dcdr


let get_font_dictionary (pdf : Pdf.t) () =
  print_for_debug "!!begin get_font_dictionary" ;  (* for debug *)
  let ret =  (* for debug *)
  [] |> FontAbbrevHashTable.fold (fun abbrev tuple acc ->
    let obj = make_dictionary pdf abbrev tuple () in
    let (_, tag, _, _, _) = tuple in
      (tag, obj) :: acc
  )
  in let () = print_for_debug "!!end get_font_dictionary" in ret  (* for debug *)


let initialize () =
  print_for_debug "!!begin initialize";  (* for debug *)
  List.iter (fun (abbrev, fontreg, srcfile) -> FontAbbrevHashTable.add abbrev fontreg srcfile) [
    ("Hlv", TrueTypeRegistration(0, 255), "./testfonts/HelveticaBlack.ttf");
    ("Arno", Type1Registration(0, 255), "./testfonts/ArnoPro-Regular.otf");
    ("KozMin",
       CIDFontType0Registration(
(*
         Pdftext.ascent      = 1137.;
         Pdftext.descent     = -349.;
         Pdftext.leading     = 1500.;  (* temporary *)
         Pdftext.stemv       = 50.;    (* temporary *)
         Pdftext.avgwidth    = 1000.;
         Pdftext.maxwidth    = 1000.;
         Pdftext.fontfile    = None;  (* does not use Pdftext.fontfile field *)
*)
      "KozMin-Composite", FontFormat.PredefinedCMap("UniJIS-UTF16-H")), "./testfonts/KozMinPro-Medium.otf")

  ]
  ; print_for_debug "!!end initialize"  (* for debug *)




(* -- following are operations about handling glyphs -- *)

(*
type contour_element =
  | OnCurve   of int * int
  | Quadratic of int * int * int * int

type contour = contour_element list


let get_contour_list (dcdr : Otfm.decoder) (uch : Uchar.t) : contour list * (int * int * int * int) =
  let (precntrlst, bbox) = FontFormat.get_uchar_raw_contour_list_and_bounding_box dcdr uch in

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
  let (adv, lsb) = FontFormat.get_uchar_horz_metrics dcdr uch in
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

*)

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
