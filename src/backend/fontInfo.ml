
open Util
open Result
open HorzBox


exception InvalidFontAbbrev     of font_abbrev
exception InvalidMathFontAbbrev of math_font_abbrev

type tag = string

let get_latin1_width_list (dcdr : FontFormat.decoder) =

  let rec range acc firstchar lastchar =
    if firstchar > lastchar then List.rev acc else
      range (firstchar :: acc) (firstchar + 1) lastchar
  in

  let ucharlst = (range [] 0 255) |> List.map Uchar.of_int in
  let gidoptlst = ucharlst |> List.map (FontFormat.get_glyph_id dcdr) in
  let gidlst = gidoptlst |> Util.list_some in
  let widlst =
    gidlst |> List.map (fun gid ->
      let (w, _, _) = FontFormat.get_glyph_metrics dcdr gid in
        (gid, w)
    )
  in
    widlst


type font_registration =
  | Type1Registration          of int * int * encoding_in_pdf
  | TrueTypeRegistration       of int * int * encoding_in_pdf
  | CIDFontType0Registration   of string * FontFormat.cmap * encoding_in_pdf * FontFormat.cid_system_info * bool
      (* -- last boolean: true iff it should embed /W information -- *)
  | CIDFontType2OTRegistration of string * FontFormat.cmap * encoding_in_pdf * FontFormat.cid_system_info * bool
      (* -- last boolean: true iff it should embed /W information -- *)
  | CIDFontType2TTRegistration of string * FontFormat.cmap * encoding_in_pdf * FontFormat.cid_system_info * bool
      (* -- last boolean: true iff it should embed /W information -- *)


let get_font_and_encoding dcdr fontreg =
  match fontreg with
  | Type1Registration(fc, lc, enc) ->
      let ty1font = FontFormat.Type1.of_decoder dcdr fc lc in
        (FontFormat.type1 ty1font, enc)
  | TrueTypeRegistration(fc, lc, enc) ->
      let trtyfont = FontFormat.TrueType.of_decoder dcdr fc lc in
        (FontFormat.true_type trtyfont, enc)
  | CIDFontType0Registration(fontname, cmap, enc, cidsysinfo, embedW) ->
      let cidty0font = FontFormat.CIDFontType0.of_decoder dcdr cidsysinfo in
        (FontFormat.cid_font_type_0 cidty0font fontname cmap, enc)
  | CIDFontType2TTRegistration(fontname, cmap, enc, cidsysinfo, embedW) ->
      let cidty2font = FontFormat.CIDFontType2.of_decoder dcdr cidsysinfo true in
        (FontFormat.cid_font_type_2 cidty2font fontname cmap, enc)
  | CIDFontType2OTRegistration(fontname, cmap, enc, cidsysinfo, embedW) ->
      let cidty2font = FontFormat.CIDFontType2.of_decoder dcdr cidsysinfo false in
        (FontFormat.cid_font_type_2 cidty2font fontname cmap, enc)


type font_tuple = FontFormat.font * tag * FontFormat.decoder * encoding_in_pdf


module FontAbbrevHashTable
: sig
    val add : font_abbrev -> font_registration -> FontFormat.file_path -> unit
    val fold : (font_abbrev -> font_tuple -> 'a -> 'a) -> 'a -> 'a
    val find_opt : font_abbrev -> font_tuple option
  end
= struct

    module Ht = Hashtbl.Make
      (struct
        type t = font_abbrev
        let equal = (=)
        let hash = Hashtbl.hash
      end)

    let abbrev_to_definition_hash_table : font_tuple Ht.t = Ht.create 32

    let generate_tag =
      let current_tag_number = ref 0 in
      (fun () ->
        begin
          incr current_tag_number;
          "/F" ^ (string_of_int !current_tag_number)
        end)

    let add abbrev fontreg srcfile =
      let dcdr = FontFormat.get_decoder srcfile in
      let (font, enc) = get_font_and_encoding dcdr fontreg in
      let tag = generate_tag () in
        Ht.add abbrev_to_definition_hash_table abbrev (font, tag, dcdr, enc)

    let fold f init =
      Ht.fold f abbrev_to_definition_hash_table init

    let find_opt (abbrev : font_abbrev) =
      try Some(Ht.find abbrev_to_definition_hash_table abbrev) with
      | Not_found -> None

  end


let get_tag_and_encoding (abbrev : font_abbrev) =
  match FontAbbrevHashTable.find_opt abbrev with
  | None                   -> raise (InvalidFontAbbrev(abbrev))
  | Some((_, tag, _, enc)) -> (tag, enc)


let raw_length_to_skip_length (fontsize : length) (rawlen : int) =
  fontsize *% ((float_of_int rawlen) /. 1000.)
    (* temporary; should use UnitsPerEm? *)


let get_metrics_of_word (hsinfo : horz_string_info) (uchlst : Uchar.t list) : OutputText.t * length * length * length =
  let font_abbrev = hsinfo.font_abbrev in
  let f_skip = raw_length_to_skip_length hsinfo.font_size in
    match FontAbbrevHashTable.find_opt font_abbrev with
    | None                    -> raise (InvalidFontAbbrev(font_abbrev))
    | Some((_, _, dcdr, enc)) ->
          let init =
            OutputText.empty_hex_style
(*
            match enc with
            | Latin1                  -> OutputText.empty_literal_style
            | ( UTF16BE | IdentityH ) -> OutputText.empty_hex_style
*)
          in
          let gidoptlst = uchlst |> List.map (FontFormat.get_glyph_id dcdr) in
          let gidlst = Util.list_some gidoptlst in
          let gidligedlst = FontFormat.convert_to_ligatures dcdr gidlst in

          let (_, otxt, rawwid, rawhgt, rawdpt) =
            gidligedlst @|> (None, init, 0, 0, 0) @|> List.fold_left (fun (gidprevopt, otxtacc, wacc, hacc, dacc) gid ->
              let (w, h, d) = FontFormat.get_glyph_metrics dcdr gid in
              let ( @>> ) = OutputText.append_glyph_id in
              let ( @*> ) = OutputText.append_kern in
              let (tjsaccnew, waccnew) =
                match gidprevopt with
                | None          -> (otxtacc @>> gid, wacc + w)
                | Some(gidprev) ->
                    match FontFormat.find_kerning dcdr gidprev gid with
                    | None        -> (otxtacc @>> gid, wacc + w)
                    | Some(wkern) ->
                        let () = PrintForDebug.kernE (Printf.sprintf "Use KERN (%d, %d) = %d" (FontFormat.gid gidprev) (FontFormat.gid gid) wkern) in  (* for debug *)
                        ((otxtacc @*> wkern) @>> gid, wacc + w + wkern)
                        (* -- kerning value is negative if two characters are supposed to be closer -- *)
              in
                (Some(gid), tjsaccnew, waccnew, max hacc h, min dacc d)
            )
(*
            uword @|> (None, init, 0, 0, 0) @|> List.fold_left (fun (gidprevopt, otxtacc, wacc, hacc, dacc) uch ->
              match get_glyph_id dcdr gidtbl uch with
              | None      -> (None, otxtacc, wacc, hacc, dacc)
                  (* temporary; simply ignores character that is not assigned a glyph in the current font *)
              | Some(gid) ->
                  let (w, h, d) = get_glyph_metrics dcdr gmtbl gid in
                  let append_data (type a) (( @>> ) : OutputText.t -> a -> OutputText.t) (x : a) : OutputText.t * int =
                    let ( @*> ) = OutputText.append_kern in
                      match gidprevopt with
                      | None          -> (otxtacc @>> x, wacc + w)
                      | Some(gidprev) ->
                          match kerntbl |> FontFormat.KerningTable.find_opt gidprev gid with
                          | None        -> (otxtacc @>> x, wacc + w)
                          | Some(wkern) -> ((otxtacc @*> wkern) @>> x, wacc + w + wkern)
                              (* -- kerning value is negative if two characters are supposed to be closer -- *)
                  in
                  let (tjsaccnew, waccnew) =
                    match enc with
                    | ( Latin1 | UTF16BE ) -> append_data OutputText.append_uchar uch
                    | IdentityH            -> append_data OutputText.append_glyph_id gid
                  in
                    (Some(gid), tjsaccnew, waccnew, max hacc h, min dacc d)
            )
*)
        in
          let wid = f_skip rawwid in
          let hgtsub = f_skip rawhgt in
          let dptsub = f_skip rawdpt in
          let rising = hsinfo.rising in
            (otxt, wid, Length.max (hgtsub +% rising) Length.zero, Length.min (dptsub +% rising) Length.zero)


type math_font_tuple = FontFormat.font * tag * FontFormat.math_decoder * encoding_in_pdf

module MathFontAbbrevHashTable
: sig
    val add : math_font_abbrev -> font_registration -> FontFormat.file_path -> unit
    val fold : (math_font_abbrev -> math_font_tuple -> 'a -> 'a) -> 'a -> 'a
    val find_opt : math_font_abbrev -> math_font_tuple option
  end
= struct

    module Ht = Hashtbl.Make
      (struct
        type t = font_abbrev
        let equal = (=)
        let hash = Hashtbl.hash
      end)

    let abbrev_to_definition_hash_table : math_font_tuple Ht.t = Ht.create 32

    let generate_tag =
      let current_tag_number = ref 0 in
      (fun () ->
        begin
          incr current_tag_number;
          "/M" ^ (string_of_int !current_tag_number)
        end)

    let add mfabbrev fontreg srcfile =
      let md = FontFormat.get_math_decoder srcfile in
      let (font, enc) = get_font_and_encoding (FontFormat.math_base_font md) fontreg in
      let tag = generate_tag () in
        Ht.add abbrev_to_definition_hash_table mfabbrev (font, tag, md, enc)

    let fold f init =
      Ht.fold f abbrev_to_definition_hash_table init

    let find_opt (mfabbrev : math_font_abbrev) =
      try Some(Ht.find abbrev_to_definition_hash_table mfabbrev) with
      | Not_found -> None

  end


let get_math_font_size (scriptlev : int) (mathctx : math_context) (md : FontFormat.math_decoder) =
  let size = mathctx.math_context_font_size in
  let mc = FontFormat.get_math_constants md in
  match scriptlev with
  | 0              -> size
  | 1              -> size *% mc.FontFormat.script_scale_down
  | t  when t >= 2 -> size *% mc.FontFormat.script_script_scale_down
  | _              -> assert false


let get_math_string_info (scriptlev : int) (mathctx : math_context) : math_string_info =
  let mfabbrev = mathctx.math_context_font_abbrev in
  match MathFontAbbrevHashTable.find_opt mfabbrev with
  | None                -> raise (InvalidMathFontAbbrev(mfabbrev))
  | Some((_, _, md, _)) ->
      {
        math_font_abbrev = mfabbrev;
        math_font_size   = get_math_font_size scriptlev mathctx md;
      }


let get_math_tag mfabbrev =
  match MathFontAbbrevHashTable.find_opt mfabbrev with
  | None                 -> raise (InvalidMathFontAbbrev(mfabbrev))
  | Some((_, tag, _, _)) -> tag


let get_math_char_info (mathstrinfo : math_string_info) (uch : Uchar.t) : FontFormat.glyph_id * length * length * length * length * FontFormat.math_kern_info option =
  let f_skip = raw_length_to_skip_length mathstrinfo.math_font_size in
  let md = FontFormat.get_math_decoder "/usr/local/lib-satysfi/dist/fonts/euler.otf" in  (* temporary; should be variable according to 'mathinfo' *)
  let (gid, rawwid, rawhgt, rawdpt, rawmicopt, rawmkiopt) = FontFormat.get_math_glyph_info md uch in
  let mic =
    match rawmicopt with
    | None         -> Length.zero
    | Some(rawmic) -> f_skip rawmic
  in
    (gid, f_skip rawwid, f_skip rawhgt, f_skip rawdpt, mic, rawmkiopt)
  

let make_dictionary (pdf : Pdf.t) (fontdfn : FontFormat.font) (dcdr : FontFormat.decoder) : Pdf.pdfobject =
  match fontdfn with
  | FontFormat.Type1(ty1font)     -> FontFormat.Type1.to_pdfdict pdf ty1font dcdr
  | FontFormat.TrueType(trtyfont) -> FontFormat.TrueType.to_pdfdict pdf trtyfont dcdr
  | FontFormat.Type0(ty0font)     -> FontFormat.Type0.to_pdfdict pdf ty0font dcdr


let get_font_dictionary (pdf : Pdf.t) : Pdf.pdfobject =
  let keyval =
    [] |> FontAbbrevHashTable.fold (fun _ tuple acc ->
      let (fontdfn, tag, dcdr, _) = tuple in
      let obj = make_dictionary pdf fontdfn dcdr in
        (tag, obj) :: acc
    ) |> MathFontAbbrevHashTable.fold (fun _ mftuple acc ->
      let (fontdfn, tag, md, _) = mftuple in
      let obj = make_dictionary pdf fontdfn (FontFormat.math_base_font md) in
        (tag, obj) :: acc
    )
  in
    Pdf.Dictionary(keyval)


let initialize (satysfi_root_dir : string) =

  PrintForDebug.initfontE "!!ScriptDataMap";
  ScriptDataMap.set_from_file (Filename.concat satysfi_root_dir "dist/unidata/Scripts.txt");
  PrintForDebug.initfontE "!!LineBreakDataMap";
  LineBreakDataMap.set_from_file (Filename.concat satysfi_root_dir "dist/unidata/LineBreak.txt");

  let append_directory s = Filename.concat satysfi_root_dir (Filename.concat "dist/fonts" s) in

  PrintForDebug.initfontE "!!begin initialize";  (* for debug *)
  List.iter (fun (abbrev, fontreg, srcfile) -> FontAbbrevHashTable.add abbrev fontreg srcfile) [

    ("Hlv", CIDFontType2TTRegistration("Hlv-Composite", FontFormat.PredefinedCMap("Identity-H"), IdentityH, FontFormat.adobe_identity, true), append_directory "HelveticaBlack.ttf");


    ("Osaka", CIDFontType2TTRegistration("Osaka-Composite", FontFormat.PredefinedCMap("Identity-H"), IdentityH, FontFormat.adobe_identity, true), append_directory "Osaka.ttf");

    ("Arno", CIDFontType0Registration("Arno-Composite", FontFormat.PredefinedCMap("Identity-H"), IdentityH, FontFormat.adobe_identity, true), append_directory "ArnoPro-Regular.otf");
    ("ArnoIt", CIDFontType0Registration("ArnoIt-Composite", FontFormat.PredefinedCMap("Identity-H"), IdentityH, FontFormat.adobe_identity, true), append_directory "ArnoPro-Italic.otf");

    ("KozMin", CIDFontType0Registration("KozMin-Composite", FontFormat.PredefinedCMap("Identity-H"), IdentityH, FontFormat.adobe_japan1, true), append_directory "KozMinPro-Regular.otf");

  ];
  List.iter (fun (mfabbrev, fontreg, srcfile) -> MathFontAbbrevHashTable.add mfabbrev fontreg srcfile) [
    ("euler", CIDFontType0Registration("euler-Composite", FontFormat.PredefinedCMap("Identity-H"), IdentityH, FontFormat.adobe_identity, true), append_directory "euler.otf");
(*
    ("Asana", CIDFontType0Registration("euler-Composite", FontFormat.PredefinedCMap("Identity-H"), IdentityH, FontFormat.adobe_identity, true), append_directory "Asana-math.otf");
*)
  ]
  ; PrintForDebug.initfontE "!!end initialize"  (* for debug *)




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
