
open MyUtil
open FontError
open LengthInterface
open HorzBox
open CharBasis


exception FontInfoError of font_error

type 'a ok = ('a, font_error) result

type tag = string

type key = FontKey.t

type math_key = FontKey.t

type font_definition = {
  font_tag : tag;
  font     : FontFormat.font;
  decoder  : FontFormat.decoder;
}


let raise_if_err = function
  | Ok(v)    -> v
  | Error(e) -> raise (FontInfoError(e))


let resolve_lib_file (relpath : lib_path) =
  Config.resolve_lib_file relpath
    |> Result.map_error (fun candidates -> CannotFindLibraryFileAsToFont(relpath, candidates))


module FontHashTable : sig
  val initialize : unit -> unit
  val add_single : abs_path -> key
  val add_ttc : abs_path -> int -> key
  val fold : (key -> font_definition -> 'a -> 'a) -> 'a -> 'a
  val find : key -> font_definition ok
end = struct

  type font_store =
    | UnusedSingle
    | UnusedTTC    of int
    | Loaded       of font_definition

  module Ht = Hashtbl.Make(FontKey)


  let key_to_definition_hash_table : (abs_path * font_store ref) Ht.t = Ht.create 32

  let current_tag_number = ref 0


  let initialize () =
    Ht.clear key_to_definition_hash_table;
    FontKey.initialize ();
    current_tag_number := 0


  let generate_tag () =
    incr current_tag_number;
    "/F" ^ (string_of_int !current_tag_number)


  (* TODO: check duplicate registration *)
  let add_single (abspath : abs_path) : key =
    let key = FontKey.generate () in
    let storeref = ref UnusedSingle in
    Ht.add key_to_definition_hash_table key (abspath, storeref);
    key


  (* TODO: check duplicate registration *)
  let add_ttc (abspath : abs_path) (index : int) : key =
    let key = FontKey.generate () in
    let storeref = ref (UnusedTTC(index)) in
    Ht.add key_to_definition_hash_table key (abspath, storeref);
    key


  let fold (f : key -> font_definition -> 'a -> 'a) (init : 'a) =
    Ht.fold (fun key (_, storeref) acc ->
      match !storeref with
      | UnusedSingle -> acc (* Ignores unused fonts *)
      | UnusedTTC(_) -> acc (* Ignores unused fonts *)
      | Loaded(dfn)  -> f key dfn acc
    ) key_to_definition_hash_table init


  let find (key : key) : font_definition ok =
    let open ResultMonad in
    match Ht.find_opt key_to_definition_hash_table key with
    | None ->
        assert false

    | Some((abspath, storeref)) ->
        begin
          match !storeref with
          | Loaded(dfn) ->
              return dfn

          | UnusedSingle ->
            (* If this is the first access to the single font: *)
              let* (dcdr, font) = FontFormat.get_decoder_single abspath in
              let tag = generate_tag () in
              let dfn = { font_tag = tag; font = font; decoder = dcdr; } in
              storeref := Loaded(dfn);
              return dfn

          | UnusedTTC(index) ->
            (* If this is the first access to the TrueType collection: *)
              let* (dcdr, font) = FontFormat.get_decoder_ttc abspath index in
              let tag = generate_tag () in
              let dfn = { font_tag = tag; font = font; decoder = dcdr; } in
              storeref := Loaded(dfn);
              return dfn
        end

end


let add_single (abspath : abs_path) : key =
  FontHashTable.add_single abspath


let add_ttc (abspath : abs_path) (index : int) : key =
  FontHashTable.add_ttc abspath index


let get_font_definition_exn (key : key) : font_definition =
  match FontHashTable.find key with
  | Ok(dfn)  -> dfn
  | Error(e) -> raise (FontInfoError(e))


let get_font_tag (key : key) : tag =
  let dfn = get_font_definition_exn key in
  dfn.font_tag


let raw_length_to_skip_length (fontsize : length) (FontFormat.PerMille(rawlen) : FontFormat.per_mille) =
  fontsize *% ((float_of_int rawlen) /. 1000.)


let ( @>> ) otxt (wpm, gsyn) = OutputText.append_glyph_synthesis otxt wpm gsyn
let ( @*> ) = OutputText.append_kern


let convert_gid_list (metricsf : FontFormat.glyph_id -> FontFormat.metrics) (dcdr : FontFormat.decoder) (gsynlst : FontFormat.glyph_synthesis list) : FontFormat.glyph_id list * OutputText.t * FontFormat.metrics =

  let (_, otxt, rawwid, rawhgt, rawdpt) =
    gsynlst |> List.fold_left (fun (gidprevopt, otxtacc, wacc, hacc, dacc) gsyn ->
      let (gid, _) = gsyn in
      let (wpm, FontFormat.PerMille(h), FontFormat.PerMille(d)) = metricsf gid in
      let FontFormat.PerMille(w) = wpm in
      let (tjsaccnew, waccnew) =
        match gidprevopt with
        | None ->
            (otxtacc @>> (wpm, gsyn), wacc + w)

        | Some(gidprev) ->
            begin
              match FontFormat.find_kerning dcdr gidprev gid with
              | None ->
                  (otxtacc @>> (wpm, gsyn), wacc + w)

              | Some(FontFormat.PerMille(wkern)) ->
                  ((otxtacc @*> wkern) @>> (wpm, gsyn), wacc + w + wkern)
                    (* Kerning values are negative if two characters are supposed to be closer *)
            end
      in
        (Some(gid), tjsaccnew, waccnew, max hacc h, min dacc d)
    ) (None, OutputText.empty_hex_style, 0, 0, 0)
  in
    (gsynlst |> List.map (fun (gid, _) -> gid) (* temporary *), otxt, (FontFormat.PerMille(rawwid), FontFormat.PerMille(rawhgt), FontFormat.PerMille(rawdpt)))


let get_glyph_id (dcdr : FontFormat.decoder) (uch : Uchar.t) =
  match raise_if_err @@ FontFormat.get_glyph_id dcdr uch with
  | None ->
      Logging.warn_no_glyph (FontFormat.postscript_name dcdr) uch;
      FontFormat.notdef

  | Some(gid) ->
      gid


let get_metrics_of_word (hsinfo : horz_string_info) (uchseglst : uchar_segment list) : OutputText.t * length * length * length =
  let fontkey = hsinfo.font_key in
  let f_skip = raw_length_to_skip_length hsinfo.text_font_size in
  let dfn = get_font_definition_exn fontkey in
  let dcdr = dfn.decoder in
  let gseglst =
    uchseglst |> List.map (fun (ubase, umarks) ->
      let gbase = get_glyph_id dcdr ubase in
      let gmarks = List.map (get_glyph_id dcdr) umarks in
      (gbase, gmarks)
    )
  in
  let gsynlst = raise_if_err @@ FontFormat.convert_to_ligatures dcdr gseglst in
  let (_, otxt, (rawwid, rawhgt, rawdpt)) =
    convert_gid_list (fun gid -> raise_if_err @@ FontFormat.get_glyph_metrics dcdr gid) dcdr gsynlst
  in
  let wid = f_skip rawwid in
  let hgtsub = f_skip rawhgt in
  let dptsub = f_skip rawdpt in
  let rising = hsinfo.rising in
  (otxt, wid, Length.max (hgtsub +% rising) Length.zero, Length.min (dptsub +% rising) Length.zero)


type math_font_definition = {
  math_font_tag : tag;
  math_font     : FontFormat.font;
  math_decoder  : FontFormat.math_decoder;
}


module MathFontHashTable : sig
  val initialize : unit -> unit
  val add_single : abs_path -> math_key
  val add_ttc : abs_path -> int -> math_key
  val fold : (math_key -> math_font_definition -> 'a -> 'a) -> 'a -> 'a
  val find : math_key -> math_font_definition ok
end = struct

  type math_font_store =
    | UnusedMathSingle
    | UnusedMathTTC    of int
    | LoadedMath       of math_font_definition

  module Ht = Hashtbl.Make(FontKey)

  let key_to_definition_hash_table : (abs_path * math_font_store ref) Ht.t = Ht.create 32

  let current_tag_number = ref 0


  let initialize () =
    Ht.clear key_to_definition_hash_table;
    current_tag_number := 0


  let generate_tag () =
    incr current_tag_number;
    "/M" ^ (string_of_int !current_tag_number)


  let add_single (abspath : abs_path) : math_key =
    let mathkey = FontKey.generate () in
    let storeref = ref UnusedMathSingle in
    Ht.add key_to_definition_hash_table mathkey (abspath, storeref);
    mathkey


  let add_ttc (abspath : abs_path) (index : int) : math_key =
    let mathkey = FontKey.generate () in
    let storeref = ref (UnusedMathTTC(index)) in
    Ht.add key_to_definition_hash_table mathkey (abspath, storeref);
    mathkey


  let fold f init =
    Ht.fold (fun mathkey (_, storeref) acc ->
      match !storeref with
      | UnusedMathSingle  -> acc  (* Ignores unused math fonts *)
      | UnusedMathTTC(_)  -> acc  (* Ignores unused math fonts *)
      | LoadedMath(mfdfn) -> f mathkey mfdfn acc
    ) key_to_definition_hash_table init


  let find (mathkey : math_key) : math_font_definition ok =
    let open ResultMonad in
    match Ht.find_opt key_to_definition_hash_table mathkey with
    | None ->
        assert false

    | Some((abspath, storeref)) ->
        begin
          match !storeref with
          | UnusedMathSingle ->
            (* If this is the first access to the single math font: *)
              let* (md, font) = FontFormat.get_math_decoder_single abspath in
              let tag = generate_tag () in
              let mfdfn = { math_font_tag = tag; math_font = font; math_decoder = md; } in
              storeref := LoadedMath(mfdfn);
              return mfdfn

          | UnusedMathTTC(index) ->
            (* If this is the first access to the collection math font: *)
              let* (md, font) = FontFormat.get_math_decoder_ttc abspath index in
              let tag = generate_tag () in
              let mfdfn = { math_font_tag = tag; math_font = font; math_decoder = md; } in
              storeref := LoadedMath(mfdfn);
              return mfdfn

          | LoadedMath(mfdfn) ->
              return mfdfn
        end

end


let add_math_single (abspath : abs_path) : math_key =
  MathFontHashTable.add_single abspath


let add_math_ttc (abspath : abs_path) (index : int) : math_key =
  MathFontHashTable.add_ttc abspath index


let find_math_font_definition_exn (mathkey : math_key) =
  match MathFontHashTable.find mathkey with
  | Ok(mfdfn) -> mfdfn
  | Error(e)  -> raise (FontInfoError(e))


let find_math_decoder_exn (mathkey : math_key) =
  let mfdfn = find_math_font_definition_exn mathkey in
  mfdfn.math_decoder


let get_math_tag (mathkey : math_key) =
  let mfdfn = find_math_font_definition_exn mathkey in
  mfdfn.math_font_tag


let get_math_constants (mathkey : math_key) : FontFormat.math_constants =
  let md = find_math_decoder_exn mathkey in
  FontFormat.get_math_constants md


let get_math_kern_ratio (mathkey : math_key) (mkern : FontFormat.math_kern) (r : float) : float =
  let mfdfn = find_math_font_definition_exn mathkey in
  let md = mfdfn.math_decoder in
  FontFormat.find_kern_ratio md mkern r


let get_math_char_info (mathkey : math_key) ~(is_in_base_level : bool) ~(is_in_display : bool) ~(is_big : bool) ~(font_size : length) (uchlst : Uchar.t list) : OutputText.t * length * length * length * length * FontFormat.math_kern_info option =
  let mfdfn = find_math_font_definition_exn mathkey in
  let md = mfdfn.math_decoder in
  let gidlst =
    uchlst |> List.map (fun uch ->
      let gidraw =
        match raise_if_err @@ FontFormat.get_math_glyph_id md uch with
        | None ->
            let fontname = FontFormat.postscript_name (FontFormat.math_base_font md) in
            Logging.warn_no_math_glyph fontname uch;
            FontFormat.notdef

        | Some(gid) ->
            gid
      in
      let gidsub =
        if is_in_base_level then
          gidraw
        else
          raise_if_err @@ FontFormat.get_math_script_variant md gidraw
      in
      let gid =
        if is_in_display && is_big then
          match raise_if_err @@ FontFormat.get_math_vertical_variants md gidsub with
          | [] ->
              gidsub

          | (gidvar, _) :: []
          | _ :: (gidvar, _) :: _ ->
              gidvar
                (* Somewhat ad-hoc; uses the second smallest as the glyph for display style *)
        else
          gidsub
      in
      (gid, [])  (* temporary; empty marks *)
    )
  in
  let (gidligedlst, otxt, (rawwid, rawhgt, rawdpt)) =
    convert_gid_list
      (fun gid -> raise_if_err @@ FontFormat.get_math_glyph_metrics md gid)
      (FontFormat.math_base_font md)
      gidlst
  in
  let (rawmicopt, rawmkiopt) =
    match List.rev gidligedlst with
    | gidlast :: _ -> FontFormat.get_math_correction_metrics md gidlast
    | []           -> (None, None)
  in
  let f_skip = raw_length_to_skip_length font_size in
  let mic =
    match rawmicopt with
    | None         -> Length.zero
    | Some(rawmic) -> f_skip rawmic
  in
  (otxt, f_skip rawwid, f_skip rawhgt, f_skip rawdpt, mic, rawmkiopt)


let get_font_dictionary (pdf : Pdf.t) : Pdf.pdfobject =
  let keyval =
    [] |> FontHashTable.fold (fun _ dfn acc ->
      let tag = dfn.font_tag in
      let font = dfn.font in
      let dcdr = dfn.decoder in
      let obj = raise_if_err @@ FontFormat.make_dictionary pdf font dcdr in
      (tag, obj) :: acc
    ) |> MathFontHashTable.fold (fun _ mfdfn acc ->
      let tag = mfdfn.math_font_tag in
      let font = mfdfn.math_font in
      let md = mfdfn.math_decoder in
      let obj = raise_if_err @@ FontFormat.make_dictionary pdf font (FontFormat.math_base_font md) in
      (tag, obj) :: acc
    )
  in
  Pdf.Dictionary(keyval)


let initialize () =
  let res =
    let open ResultMonad in
    FontHashTable.initialize ();
    MathFontHashTable.initialize ();
    let* abspath_S   = resolve_lib_file (make_lib_path "unidata/Scripts.txt") in
    let* abspath_EAW = resolve_lib_file (make_lib_path "unidata/EastAsianWidth.txt") in
    ScriptDataMap.set_from_file abspath_S abspath_EAW;
    let* abspath_LB = resolve_lib_file (make_lib_path "unidata/LineBreak.txt") in
    LineBreakDataMap.set_from_file abspath_LB;
    return ()
  in
  match res with
  | Ok(())   -> ()
  | Error(e) -> raise (FontInfoError(e))
