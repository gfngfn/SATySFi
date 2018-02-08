
open MyUtil
open LengthInterface
open HorzBox
open Types

exception InvalidFontAbbrev     of font_abbrev
exception InvalidMathFontAbbrev of math_font_abbrev
exception NotASingleFont        of font_abbrev * file_path
exception NotASingleMathFont    of font_abbrev * file_path

type tag = string


let get_latin1_width_list (dcdr : FontFormat.decoder) =

  let rec range acc firstchar lastchar =
    if firstchar > lastchar then List.rev acc else
      range (firstchar :: acc) (firstchar + 1) lastchar
  in

  let ucharlst = (range [] 0 255) |> List.map Uchar.of_int in
  let gidoptlst = ucharlst |> List.map (FontFormat.get_glyph_id dcdr) in
  let gidlst = gidoptlst |> list_some in
  let widlst =
    gidlst |> List.map (fun gid ->
      let (w, _, _) = FontFormat.get_glyph_metrics dcdr gid in
        (gid, w)
    )
  in
    widlst


let get_font dcdr fontreg fontname =
  let cmap = FontFormat.PredefinedCMap("Identity-H") in
  match fontreg with
  | FontFormat.CIDFontType0Registration(cidsysinfo, embedW) ->
      let cidty0font = FontFormat.CIDFontType0.of_decoder dcdr cidsysinfo in
        (FontFormat.cid_font_type_0 cidty0font fontname cmap)
  | FontFormat.CIDFontType2TTRegistration(cidsysinfo, embedW) ->
      let cidty2font = FontFormat.CIDFontType2.of_decoder dcdr cidsysinfo true in
        (FontFormat.cid_font_type_2 cidty2font fontname cmap)
  | FontFormat.CIDFontType2OTRegistration(cidsysinfo, embedW) ->
      let cidty2font = FontFormat.CIDFontType2.of_decoder dcdr cidsysinfo true (* temporary *) in
        (FontFormat.cid_font_type_2 cidty2font fontname cmap)


type font_definition = tag * FontFormat.font * FontFormat.decoder

type font_store =
  | Unused of file_path
  | Loaded of font_definition


module FontAbbrevHashTable
: sig
    val initialize : unit -> unit
    val add : font_abbrev -> file_path -> unit
    val fold : (font_abbrev -> font_definition -> 'a -> 'a) -> 'a -> 'a
    val find_opt : font_abbrev -> font_definition option
  end
= struct

    module Ht = Hashtbl.Make
      (struct
        type t = font_abbrev
        let equal = (=)
        let hash = Hashtbl.hash
      end)

    let abbrev_to_definition_hash_table : (font_store ref) Ht.t = Ht.create 32

    let current_tag_number = ref 0

    let initialize () =
      begin
        Ht.clear abbrev_to_definition_hash_table;
        current_tag_number := 0;
      end

    let generate_tag () =
      begin
        incr current_tag_number;
        "/F" ^ (string_of_int !current_tag_number)
      end

    let add abbrev srcpath =
      Ht.add abbrev_to_definition_hash_table abbrev (ref (Unused(srcpath)))

    let fold (f : font_abbrev -> tag * FontFormat.font * FontFormat.decoder -> 'a -> 'a) init =
      Ht.fold (fun abbrev storeref acc ->
        match !storeref with
        | Unused(_)   -> acc  (* -- ignores unused fonts -- *)
        | Loaded(dfn) -> f abbrev dfn acc
      ) abbrev_to_definition_hash_table init

    let find_opt (abbrev : font_abbrev) =
      match Ht.find_opt abbrev_to_definition_hash_table abbrev with
      | None ->
          None

      | Some(storeref) ->
          let store = !storeref in
          begin
            match store with
            | Unused(srcpath) ->
      (* -- if this is the first access to the font -- *)
                begin
                  match FontFormat.get_decoder_single srcpath with
                  | None ->
                      raise (NotASingleFont(abbrev, srcpath))

                  | Some((dcdr, fontreg)) ->
                      let font = get_font dcdr fontreg (abbrev ^ "-Composite") (* temporary *) in
                      let tag = generate_tag () in
                      let dfn = (tag, font, dcdr) in
                      let store = Loaded(dfn) in
                      storeref := store;
                      Some(dfn)
                end

            | Loaded(dfn) -> Some(dfn)
          end
  end


let get_font_tag (abbrev : font_abbrev) : tag =
  match FontAbbrevHashTable.find_opt abbrev with
  | None              -> raise (InvalidFontAbbrev(abbrev))
  | Some((tag, _, _)) -> tag


let raw_length_to_skip_length (fontsize : length) (FontFormat.PerMille(rawlen) : FontFormat.per_mille) =
  fontsize *% ((float_of_int rawlen) /. 1000.)


let ( @>> ) = OutputText.append_glyph_id
let ( @*> ) = OutputText.append_kern


let convert_gid_list (metricsf : FontFormat.glyph_id -> FontFormat.metrics) (dcdr : FontFormat.decoder) (gidlst : FontFormat.glyph_id list) : FontFormat.glyph_id list * OutputText.t * FontFormat.metrics =
  let gidligedlst = FontFormat.convert_to_ligatures dcdr gidlst in

  let (_, otxt, rawwid, rawhgt, rawdpt) =
    gidligedlst |> List.fold_left (fun (gidprevopt, otxtacc, wacc, hacc, dacc) gid ->
      let (FontFormat.PerMille(w), FontFormat.PerMille(h), FontFormat.PerMille(d)) = metricsf gid in
      let (tjsaccnew, waccnew) =
        match gidprevopt with
        | None ->
            (otxtacc @>> gid, wacc + w)

        | Some(gidprev) ->
            begin
              match FontFormat.find_kerning dcdr gidprev gid with
              | None ->
                  (otxtacc @>> gid, wacc + w)

              | Some(FontFormat.PerMille(wkern)) ->
(*
                  PrintForDebug.kernE (Printf.sprintf "Use KERN (%d, %d) = %d" (FontFormat.gid gidprev) (FontFormat.gid gid) wkern);  (* for debug *)
*)
                  ((otxtacc @*> wkern) @>> gid, wacc + w + wkern)
                    (* -- kerning value is negative if two characters are supposed to be closer -- *)
            end
      in
        (Some(gid), tjsaccnew, waccnew, max hacc h, min dacc d)
    ) (None, OutputText.empty_hex_style, 0, 0, 0)
  in
    (gidligedlst, otxt, (FontFormat.PerMille(rawwid), FontFormat.PerMille(rawhgt), FontFormat.PerMille(rawdpt)))


let get_metrics_of_word (hsinfo : horz_string_info) (uchlst : Uchar.t list) : OutputText.t * length * length * length =
  let font_abbrev = hsinfo.font_abbrev in
  let f_skip = raw_length_to_skip_length hsinfo.text_font_size in
    match FontAbbrevHashTable.find_opt font_abbrev with
    | None               -> raise (InvalidFontAbbrev(font_abbrev))
    | Some((_, _, dcdr)) ->
          let gidoptlst = uchlst |> List.map (FontFormat.get_glyph_id dcdr) in
          let gidlst = list_some gidoptlst in
            (* needs reconsideration; maybe should return GID 0 for code points which is not covered by the font *)
          let (gidligedlst, otxt, (rawwid, rawhgt, rawdpt)) = convert_gid_list (FontFormat.get_glyph_metrics dcdr) dcdr gidlst in
          let wid = f_skip rawwid in
          let hgtsub = f_skip rawhgt in
          let dptsub = f_skip rawdpt in
          let rising = hsinfo.rising in
            (otxt, wid, Length.max (hgtsub +% rising) Length.zero, Length.min (dptsub +% rising) Length.zero)


type math_font_definition = tag * FontFormat.font * FontFormat.math_decoder

type math_font_store =
  | UnusedMath of file_path
  | LoadedMath of math_font_definition


module MathFontAbbrevHashTable
: sig
    val initialize : unit -> unit
    val add : math_font_abbrev -> FontFormat.file_path -> unit
    val fold : (math_font_abbrev -> math_font_definition -> 'a -> 'a) -> 'a -> 'a
    val find_opt : math_font_abbrev -> math_font_definition option
  end
= struct

    module Ht = Hashtbl.Make
      (struct
        type t = font_abbrev
        let equal = (=)
        let hash = Hashtbl.hash
      end)

    let abbrev_to_definition_hash_table : (math_font_store ref) Ht.t = Ht.create 32

    let current_tag_number = ref 0

    let initialize () =
      begin
        Ht.clear abbrev_to_definition_hash_table;
        current_tag_number := 0;
      end

    let generate_tag () =
      begin
        incr current_tag_number;
        "/M" ^ (string_of_int !current_tag_number)
      end

    let add mfabbrev srcpath =
      Ht.add abbrev_to_definition_hash_table mfabbrev (ref (UnusedMath(srcpath)))

    let fold f init =
      Ht.fold (fun mfabbrev storeref acc ->
        match !storeref with
        | UnusedMath(_)     -> acc  (* -- ignores unused math fonts -- *)
        | LoadedMath(mfdfn) -> f mfabbrev mfdfn acc
      ) abbrev_to_definition_hash_table init

    let find_opt (mfabbrev : math_font_abbrev) =
      match Ht.find_opt abbrev_to_definition_hash_table mfabbrev with
      | None ->
          None

      | Some(storeref) ->
          begin
            match !storeref with
            | UnusedMath(srcpath) ->
              (* -- if this is the first access to the math font -- *)
                begin
                  match FontFormat.get_math_decoder srcpath with
                  | None ->
                      raise (NotASingleMathFont(mfabbrev, srcpath))

                  | Some((md, fontreg)) ->
                      let font = get_font (FontFormat.math_base_font md) fontreg (mfabbrev ^ "-Composite-Math") (* temporary *) in
                      let tag = generate_tag () in
                      let mfdfn = (tag, font, md) in
                      storeref := LoadedMath(mfdfn);
                      Some(mfdfn)
                end

            | LoadedMath(mfdfn) ->
                Some(mfdfn)
          end
  end


let find_math_decoder_exn mfabbrev =
  match MathFontAbbrevHashTable.find_opt mfabbrev with
  | None             -> raise (InvalidMathFontAbbrev(mfabbrev))
  | Some((_, _, md)) -> md


let actual_math_font_size mathctx =
  MathContext.actual_font_size mathctx find_math_decoder_exn


let get_math_string_info mathctx : math_string_info =
  {
    math_font_abbrev = MathContext.math_font_abbrev mathctx;
    math_font_size   = actual_math_font_size mathctx;
    math_color       = MathContext.color mathctx;
  }


let get_math_tag mfabbrev =
  match MathFontAbbrevHashTable.find_opt mfabbrev with
  | None              -> raise (InvalidMathFontAbbrev(mfabbrev))
  | Some((tag, _, _)) -> tag


let get_math_constants mathctx =
  let mfabbrev = MathContext.math_font_abbrev mathctx in
  let md = find_math_decoder_exn mfabbrev in
    FontFormat.get_math_constants md


type math_kern_scheme =
  | NoMathKern
  | DiscreteMathKern of FontFormat.math_kern
  | DenseMathKern    of (length -> length)


let no_math_kern = NoMathKern

let make_dense_math_kern kernf = DenseMathKern(kernf)

let make_discrete_math_kern mkern = DiscreteMathKern(mkern)


let get_axis_height (mfabbrev : math_font_abbrev) (fontsize : length) : length =
  match MathFontAbbrevHashTable.find_opt mfabbrev with
  | None ->
      raise (Invalid_argument(mfabbrev))

  | Some((_, _, md)) ->
      let ratio = FontFormat.get_axis_height_ratio md in
        fontsize *% ratio

(* --
   get_math_kern:
     returns kerning length
     (negative value stands for being closer to the previous glyph)
   -- *)
let get_math_kern (mathctx : math_context) (mkern : math_kern_scheme) (corrhgt : length) : length =
  let fontsize = actual_math_font_size mathctx in
  let mfabbrev = MathContext.math_font_abbrev mathctx in
    match MathFontAbbrevHashTable.find_opt mfabbrev with
    | None ->
        raise (InvalidMathFontAbbrev(mfabbrev))

    | Some((_, _, md)) ->
        begin
          match mkern with
          | NoMathKern              -> Length.zero
          | DiscreteMathKern(mkern) -> let ratiok = FontFormat.find_kern_ratio md mkern (corrhgt /% fontsize) in fontsize *% ratiok
          | DenseMathKern(kernf)    -> Length.negate (kernf corrhgt)
        end


let get_math_char_info (mathctx : math_context) (is_in_display : bool) (is_big : bool) (uchlst : Uchar.t list) : OutputText.t * length * length * length * length * FontFormat.math_kern_info option =
  let mfabbrev = MathContext.math_font_abbrev mathctx in
    match MathFontAbbrevHashTable.find_opt mfabbrev with
    | None ->
        raise (InvalidFontAbbrev(mfabbrev))

    | Some((_, _, md)) ->
        let gidlst =
          uchlst |> List.map (fun uch ->
            let gidraw = FontFormat.get_math_glyph_id md uch in
            let gidsub =
              if MathContext.is_in_base_level mathctx then
                gidraw
              else
                FontFormat.get_math_script_variant md gidraw
            in
              if is_in_display && is_big then
                match FontFormat.get_math_vertical_variants md gidsub with
                | [] -> gidsub

                | (gidvar, _) :: []
                | _ :: (gidvar, _) :: _
                    ->
(*
                    Format.printf "FontInfo> variant exists: %d ---> %d\n" (FontFormat.gid gidsub) (FontFormat.gid gidvar);  (* for debug *)
*)
                    gidvar
                      (* -- somewhat ad-hoc; uses the second smallest as the glyph for display style -- *)
              else
                gidsub
          )
        in
        let (gidligedlst, otxt, (rawwid, rawhgt, rawdpt)) =
          convert_gid_list (FontFormat.get_math_glyph_metrics md) (FontFormat.math_base_font md) gidlst
        in
        let (rawmicopt, rawmkiopt) =
          match List.rev gidligedlst with
          | gidlast :: _ -> FontFormat.get_math_correction_metrics md gidlast
          | []           -> (None, None)
        in
        let f_skip = raw_length_to_skip_length (actual_math_font_size mathctx) in
        let mic =
          match rawmicopt with
          | None         -> Length.zero
          | Some(rawmic) -> f_skip rawmic
        in
          (otxt, f_skip rawwid, f_skip rawhgt, f_skip rawdpt, mic, rawmkiopt)


let make_dictionary (pdf : Pdf.t) (fontdfn : FontFormat.font) (dcdr : FontFormat.decoder) : Pdf.pdfobject =
  match fontdfn with
(*
  | FontFormat.Type1(ty1font)     -> FontFormat.Type1.to_pdfdict pdf ty1font dcdr
  | FontFormat.TrueType(trtyfont) -> FontFormat.TrueType.to_pdfdict pdf trtyfont dcdr
*)
  | FontFormat.Type0(ty0font)     -> FontFormat.Type0.to_pdfdict pdf ty0font dcdr


let get_font_dictionary (pdf : Pdf.t) : Pdf.pdfobject =
  let keyval =
    [] |> FontAbbrevHashTable.fold (fun _ tuple acc ->
      let (tag, fontdfn, dcdr) = tuple in
      let obj = make_dictionary pdf fontdfn dcdr in
        (tag, obj) :: acc
    ) |> MathFontAbbrevHashTable.fold (fun _ mftuple acc ->
      let (tag, fontdfn, md) = mftuple in
      let obj = make_dictionary pdf fontdfn (FontFormat.math_base_font md) in
        (tag, obj) :: acc
    )
  in
    Pdf.Dictionary(keyval)


let initialize (satysfi_lib_root : string) =

  begin
    FontAbbrevHashTable.initialize ();
    MathFontAbbrevHashTable.initialize ();
(*
    PrintForDebug.initfontE "!!ScriptDataMap";
*)
    let filename_S   = Filename.concat satysfi_lib_root "dist/unidata/Scripts.txt" in
    let filename_EAW = Filename.concat satysfi_lib_root "dist/unidata/EastAsianWidth.txt" in
    ScriptDataMap.set_from_file filename_S filename_EAW;
(*
    PrintForDebug.initfontE "!!LineBreakDataMap";
*)
    LineBreakDataMap.set_from_file (Filename.concat satysfi_lib_root "dist/unidata/LineBreak.txt");
(*
    PrintForDebug.initfontE "!!begin initialize";  (* for debug *)
*)
    let font_hash = LoadFont.main satysfi_lib_root "fonts.satysfi-hash" in
    List.iter (fun (abbrev, srcpath) -> FontAbbrevHashTable.add abbrev srcpath) font_hash;

    let math_font_hash = LoadFont.main satysfi_lib_root "mathfonts.satysfi-hash" in
    List.iter (fun (mfabbrev, srcfile) -> MathFontAbbrevHashTable.add mfabbrev srcfile) math_font_hash;
(*
    PrintForDebug.initfontE "!!end initialize"  (* for debug *)
*)
  end


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
