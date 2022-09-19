
open MyUtil
open LengthInterface
open HorzBox
open CharBasis
open Types


exception InvalidFontAbbrev     of font_abbrev
exception InvalidMathFontAbbrev of math_font_abbrev
exception NotASingleFont        of font_abbrev * abs_path
exception NotATTCElement        of font_abbrev * abs_path * int
exception NotASingleMathFont    of math_font_abbrev * abs_path
exception NotATTCMathFont       of math_font_abbrev * abs_path * int

type tag = string

type font_definition = {
  font_tag : tag;
  font     : FontFormat.font;
  decoder  : FontFormat.decoder;
}


module FontAbbrevHashTable : sig
  val initialize : unit -> unit
  val add_single : font_abbrev -> lib_path -> unit
  val add_ttc : font_abbrev -> lib_path -> int -> unit
  val fold : (font_abbrev -> font_definition -> 'a -> 'a) -> 'a -> 'a
  val find : font_abbrev -> font_definition
end = struct

    type font_store =
      | UnusedSingle
      | UnusedTTC    of int
      | Loaded       of font_definition

    module Ht = Hashtbl.Make
      (struct
        type t = font_abbrev
        let equal = (=)
        let hash = Hashtbl.hash
      end)


    let abbrev_to_definition_hash_table : (lib_path * font_store ref) Ht.t = Ht.create 32

    let current_tag_number = ref 0


    let initialize () =
      Ht.clear abbrev_to_definition_hash_table;
      current_tag_number := 0


    let generate_tag () =
      incr current_tag_number;
      "/F" ^ (string_of_int !current_tag_number)


    let add_single abbrev relpath =
      match abbrev |> Ht.find_opt abbrev_to_definition_hash_table with
      | Some((relpath, _)) ->
          Logging.warn_duplicate_font_hash abbrev relpath

      | None ->
          let storeref = ref UnusedSingle in
          Ht.add abbrev_to_definition_hash_table abbrev (relpath, storeref)


    let add_ttc abbrev relpath i =
      if abbrev |> Ht.mem abbrev_to_definition_hash_table then
        Logging.warn_duplicate_font_hash abbrev relpath
      else
        let storeref = ref (UnusedTTC(i)) in
        Ht.add abbrev_to_definition_hash_table abbrev (relpath, storeref)


    let fold (f : font_abbrev -> font_definition -> 'a -> 'a) init =
      Ht.fold (fun abbrev (_, storeref) acc ->
        match !storeref with
        | UnusedSingle -> acc (* Ignores unused fonts *)
        | UnusedTTC(_) -> acc (* Ignores unused fonts *)
        | Loaded(dfn)  -> f abbrev dfn acc
      ) abbrev_to_definition_hash_table init


    let find (abbrev : font_abbrev) : font_definition =
      match Ht.find_opt abbrev_to_definition_hash_table abbrev with
      | None ->
          raise (InvalidFontAbbrev(abbrev))

      | Some((relpath, storeref)) ->
          begin
            match !storeref with
            | Loaded(dfn) ->
                dfn

            | UnusedSingle ->
              (* -- if this is the first access to the single font -- *)
                let abspath = Config.resolve_lib_file_exn relpath in
                begin
                  match FontFormat.get_decoder_single (abbrev ^ "-Composite") (* temporary *) abspath with
                  | None ->
                    (* -- if the font file is a TrueTypeCollection -- *)
                      raise (NotASingleFont(abbrev, abspath))

                  | Some((dcdr, font)) ->
                      let tag = generate_tag () in
                      let dfn = { font_tag = tag; font = font; decoder = dcdr; } in
                      storeref := Loaded(dfn);
                      dfn
                end

            | UnusedTTC(i) ->
              (* -- if this is the first access to the TrueTypeCollection -- *)
                let srcpath = Config.resolve_lib_file_exn relpath in
                begin
                  match FontFormat.get_decoder_ttc (abbrev ^ "-Composite") (* temporary *) srcpath i with
                  | None ->
                      raise (NotATTCElement(abbrev, srcpath, i))

                  | Some((dcdr, font)) ->
                      let tag = generate_tag () in
                      let dfn = { font_tag = tag; font = font; decoder = dcdr; } in
                      storeref := Loaded(dfn);
                      dfn
                end
          end

  end


let get_font_tag (abbrev : font_abbrev) : tag =
  let dfn = FontAbbrevHashTable.find abbrev in
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


let get_glyph_id font_abbrev dcdr uch =
  match FontFormat.get_glyph_id dcdr uch with
  | None ->
      Logging.warn_no_glyph font_abbrev uch;
      FontFormat.notdef

  | Some(gid) ->
      gid


let get_metrics_of_word (hsinfo : horz_string_info) (uchseglst : uchar_segment list) : OutputText.t * length * length * length =
  let font_abbrev = hsinfo.font_abbrev in
  let f_skip = raw_length_to_skip_length hsinfo.text_font_size in
  let dfn = FontAbbrevHashTable.find font_abbrev in
  let dcdr = dfn.decoder in
  let gseglst =
    uchseglst |> List.map (fun (ubase, umarks) ->
      let gbase = get_glyph_id font_abbrev dcdr ubase in
      let gmarks = List.map (get_glyph_id font_abbrev dcdr) umarks in
      (gbase, gmarks)
    )
  in
  let gsynlst = FontFormat.convert_to_ligatures dcdr gseglst in
  let (_, otxt, (rawwid, rawhgt, rawdpt)) = convert_gid_list (FontFormat.get_glyph_metrics dcdr) dcdr gsynlst in
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


module MathFontAbbrevHashTable
: sig
    val initialize : unit -> unit
    val add_single : math_font_abbrev -> lib_path -> unit
    val add_ttc : math_font_abbrev -> lib_path -> int -> unit
    val fold : (math_font_abbrev -> math_font_definition -> 'a -> 'a) -> 'a -> 'a
    val find : math_font_abbrev -> math_font_definition
  end
= struct

    type math_font_store =
      | UnusedMathSingle
      | UnusedMathTTC    of int
      | LoadedMath       of math_font_definition

    module Ht = Hashtbl.Make
      (struct
        type t = font_abbrev
        let equal = (=)
        let hash = Hashtbl.hash
      end)

    let abbrev_to_definition_hash_table : (lib_path * math_font_store ref) Ht.t = Ht.create 32

    let current_tag_number = ref 0


    let initialize () =
      Ht.clear abbrev_to_definition_hash_table;
      current_tag_number := 0


    let generate_tag () =
      incr current_tag_number;
      "/M" ^ (string_of_int !current_tag_number)


    let add_single mfabbrev relpath =
      match mfabbrev |> Ht.find_opt abbrev_to_definition_hash_table with
      | Some((relpath, _)) ->
          Logging.warn_duplicate_math_font_hash mfabbrev relpath

      | None ->
          let storeref = ref UnusedMathSingle in
          Ht.add abbrev_to_definition_hash_table mfabbrev (relpath, storeref)


    let add_ttc mfabbrev relpath i =
      if mfabbrev |> Ht.mem abbrev_to_definition_hash_table then
        Logging.warn_duplicate_font_hash mfabbrev relpath
      else
        let storeref = ref (UnusedMathTTC(i)) in
        Ht.add abbrev_to_definition_hash_table mfabbrev (relpath, storeref)


    let fold f init =
      Ht.fold (fun mfabbrev (_, storeref) acc ->
        match !storeref with
        | UnusedMathSingle  -> acc  (* -- ignores unused math fonts -- *)
        | UnusedMathTTC(_)  -> acc  (* -- ignores unused math fonts -- *)
        | LoadedMath(mfdfn) -> f mfabbrev mfdfn acc
      ) abbrev_to_definition_hash_table init


    let find (mfabbrev : math_font_abbrev) : math_font_definition =
      match Ht.find_opt abbrev_to_definition_hash_table mfabbrev with
      | None ->
          raise (InvalidMathFontAbbrev(mfabbrev))

      | Some((relpath, storeref)) ->
          begin
            match !storeref with
            | UnusedMathSingle ->
              (* -- if this is the first access to the single math font -- *)
                let srcpath = Config.resolve_lib_file_exn relpath in
                begin
                  match FontFormat.get_math_decoder_single (mfabbrev ^ "-Composite-Math") (* temporary *) srcpath with
                  | None ->
                    (* -- if the font file does not have a MATH table or is a TrueType Collection -- *)
                      raise (NotASingleMathFont(mfabbrev, srcpath))

                  | Some((md, font)) ->
                      let tag = generate_tag () in
                      let mfdfn = { math_font_tag = tag; math_font = font; math_decoder = md; } in
                      storeref := LoadedMath(mfdfn);
                      mfdfn
                end

            | UnusedMathTTC(i) ->
            (* -- if this is the first access to the collection math font -- *)
                let srcpath = Config.resolve_lib_file_exn relpath in
                begin
                  match FontFormat.get_math_decoder_ttc (mfabbrev ^ "-Composite-Math") (* temporary *) srcpath i with
                  | None ->
                    (* -- if the font does not have a MATH table or is a single font file -- *)
                      raise (NotATTCMathFont(mfabbrev, srcpath, i))

                  | Some((md, font)) ->
                      let tag = generate_tag () in
                      let mfdfn = { math_font_tag = tag; math_font = font; math_decoder = md; } in
                      storeref := LoadedMath(mfdfn);
                      mfdfn
                end

            | LoadedMath(mfdfn) ->
                mfdfn
          end

  end


let find_math_decoder_exn mfabbrev =
  let mfdfn = MathFontAbbrevHashTable.find mfabbrev in
  mfdfn.math_decoder


let get_math_string_info mathctx : math_string_info =
  {
    info_math_font_abbrev = MathContext.math_font_abbrev mathctx;
    info_math_font_size   = MathContext.font_size mathctx;
    info_math_color       = MathContext.color mathctx;
  }


let get_math_tag mfabbrev =
  let mfdfn = MathFontAbbrevHashTable.find mfabbrev in
  mfdfn.math_font_tag


let get_math_constants mathctx =
  let mfabbrev = MathContext.math_font_abbrev mathctx in
  let md = find_math_decoder_exn mfabbrev in
  FontFormat.get_math_constants md


type math_kern_scheme =
  | NoMathKern
  | DiscreteMathKern of FontFormat.math_kern
  | DenseMathKern    of math_kern_func


let no_math_kern = NoMathKern

let make_dense_math_kern kernf = DenseMathKern(kernf)

let make_discrete_math_kern mkern = DiscreteMathKern(mkern)


(* Returns kerning length (negative value stands for being closer to the previous glyph) *)
let get_math_kern (mathctx : math_context) (mkern : math_kern_scheme) (corrhgt : length) : length =
  let fontsize = MathContext.font_size mathctx in
  let mfabbrev = MathContext.math_font_abbrev mathctx in
  let mfdfn = MathFontAbbrevHashTable.find mfabbrev in
  let md = mfdfn.math_decoder in
  match mkern with
  | NoMathKern ->
      Length.zero

  | DiscreteMathKern(mkern) ->
      let ratiok = FontFormat.find_kern_ratio md mkern (corrhgt /% fontsize) in
      fontsize *% ratiok

  | DenseMathKern(kernf) ->
      Length.negate (kernf corrhgt)


let get_math_char_info (mathctx : math_context) (is_in_display : bool) (is_big : bool) (uchlst : Uchar.t list) : OutputText.t * length * length * length * length * FontFormat.math_kern_info option =
  let mfabbrev = MathContext.math_font_abbrev mathctx in
  let mfdfn = MathFontAbbrevHashTable.find mfabbrev in
  let md = mfdfn.math_decoder in
  let gidlst =
    uchlst |> List.map (fun uch ->
      let gidraw =
        match FontFormat.get_math_glyph_id md uch with
        | None ->
            Logging.warn_no_math_glyph mfabbrev uch;
            FontFormat.notdef

        | Some(gid) ->
            gid
      in
      let gidsub =
        if MathContext.is_in_base_level mathctx then
          gidraw
        else
          FontFormat.get_math_script_variant md gidraw
      in
      let gid =
        if is_in_display && is_big then
          match FontFormat.get_math_vertical_variants md gidsub with
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
    convert_gid_list (FontFormat.get_math_glyph_metrics md) (FontFormat.math_base_font md) gidlst
  in
  let (rawmicopt, rawmkiopt) =
    match List.rev gidligedlst with
    | gidlast :: _ -> FontFormat.get_math_correction_metrics md gidlast
    | []           -> (None, None)
  in
  let f_skip = raw_length_to_skip_length (MathContext.font_size mathctx) in
  let mic =
    match rawmicopt with
    | None         -> Length.zero
    | Some(rawmic) -> f_skip rawmic
  in
  (otxt, f_skip rawwid, f_skip rawhgt, f_skip rawdpt, mic, rawmkiopt)


let get_font_dictionary (pdf : Pdf.t) : Pdf.pdfobject =
  let keyval =
    [] |> FontAbbrevHashTable.fold (fun _ dfn acc ->
      let tag = dfn.font_tag in
      let font = dfn.font in
      let dcdr = dfn.decoder in
      let obj = FontFormat.make_dictionary pdf font dcdr in
      (tag, obj) :: acc
    ) |> MathFontAbbrevHashTable.fold (fun _ mfdfn acc ->
      let tag = mfdfn.math_font_tag in
      let font = mfdfn.math_font in
      let md = mfdfn.math_decoder in
      let obj = FontFormat.make_dictionary pdf font (FontFormat.math_base_font md) in
      (tag, obj) :: acc
    )
  in
  Pdf.Dictionary(keyval)


let initialize () =
  FontAbbrevHashTable.initialize ();
  MathFontAbbrevHashTable.initialize ();
  let abspath_S   = Config.resolve_lib_file_exn (make_lib_path "dist/unidata/Scripts.txt") in
  let abspath_EAW = Config.resolve_lib_file_exn (make_lib_path "dist/unidata/EastAsianWidth.txt") in
  ScriptDataMap.set_from_file abspath_S abspath_EAW;
  LineBreakDataMap.set_from_file (Config.resolve_lib_file_exn (make_lib_path "dist/unidata/LineBreak.txt"));
  let font_hash_local =
    match Config.resolve_lib_file_opt (make_lib_path "local/hash/fonts.satysfi-hash") with
    | None          -> []
    | Some(abspath) -> LoadFont.main abspath
  in
  let font_hash_dist = LoadFont.main (Config.resolve_lib_file_exn (make_lib_path "dist/hash/fonts.satysfi-hash")) in
  let font_hash = List.append font_hash_local font_hash_dist in
  if OptionState.does_show_fonts () then Logging.show_fonts font_hash;
  font_hash |> List.iter (fun (abbrev, data) ->
    match data with
    | FontAccess.Single(relpath)        -> FontAbbrevHashTable.add_single abbrev relpath
    | FontAccess.Collection(relpath, i) -> FontAbbrevHashTable.add_ttc abbrev relpath i
  );
  let math_font_hash_local =
    match Config.resolve_lib_file_opt (make_lib_path "local/hash/mathfonts.satysfi-hash") with
    | None          -> []
    | Some(abspath) -> LoadFont.main abspath
  in
  let math_font_hash_dist = LoadFont.main (Config.resolve_lib_file_exn (make_lib_path "dist/hash/mathfonts.satysfi-hash")) in
  let math_font_hash = List.append math_font_hash_local math_font_hash_dist in
  if OptionState.does_show_fonts () then Logging.show_math_fonts math_font_hash;
  math_font_hash |> List.iter (fun (mfabbrev, data) ->
    match data with
    | FontAccess.Single(srcpath)        -> MathFontAbbrevHashTable.add_single mfabbrev srcpath
    | FontAccess.Collection(srcpath, i) -> MathFontAbbrevHashTable.add_ttc mfabbrev srcpath i
  );
