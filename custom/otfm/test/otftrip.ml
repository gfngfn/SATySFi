(*---------------------------------------------------------------------------
   Copyright (c) 2013 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Result

let pp = Format.fprintf
let str = Format.sprintf

let exec = Filename.basename Sys.executable_name
let log msg = Format.eprintf ("%s: " ^^ msg ^^ "@.") exec
let log_err inf e =
  Format.eprintf "@[<2>%s:%s:@ %a@]@." exec inf Otfm.pp_error e

let string_of_file inf =
  try
    let ic = if inf = "-" then stdin else open_in_bin inf in
    let close ic = if inf <> "-" then close_in ic else () in
    let buf_size = 65536 in
    let b = Buffer.create buf_size in
    let s = Bytes.create buf_size in
    try
      while true do
        let c = input ic s 0 buf_size in
        if c = 0 then raise Exit else
        Buffer.add_substring b (Bytes.unsafe_to_string s) 0 c
      done;
      assert false
    with
    | Exit -> close ic; Ok (Buffer.contents b)
    | Failure _ -> close ic; Error (`Msg (str "%s: input file too large" inf))
    | Sys_error e -> close ic; (Error (`Msg e));
  with
  | Sys_error e -> (Error (`Msg e))

(* Table pretty printers *)

let pp_cmap ppf d =
  let pp_map ppf u gid = pp ppf "@,(%a %d)" Otfm.pp_cp u gid in
  let pp_binding ppf () k (u0, u1) gid =
    match k with
    | `Glyph       -> for u = u0 to u1 do pp_map ppf u gid done
    | `Glyph_range -> for i = 0 to (u1 - u0) do pp_map ppf (u0 + i) (gid + i)done
  in
  pp ppf "@,@[<v1>(cmap";
  match Otfm.cmap d with
  | Error(_) as e -> e
  | Ok(subtbllst) ->
      subtbllst |> List.fold_left (fun _ subtbl ->
        let (pid, eid, fmt) = Otfm.cmap_subtable_ids subtbl in
        pp ppf "@,@[<v1>@[<1>(cmap-source@ (platform-id %d)@ (encoding-id %d)\
              @ (format %d)" pid eid fmt;
        let res = Otfm.cmap_subtable subtbl (pp_binding ppf) () in
        pp ppf ")@]@])@]";
        res
      ) (Ok())


let pp_glyf ppf has_glyf d =
  if not has_glyf then Ok () else
  let pp_bbox ppf (minx, miny, maxx, maxy) =
    pp ppf "@[(bbox@ %d@ %d@ %d@ %d)@]" minx miny maxx maxy
  in
  let pp_contour ppf pts =
    let rec pp_pts ppf = function
    | [] -> pp ppf ")@]"
    | (b, x, y) :: pts -> pp ppf "@,@[<1>(pt@ %b %d %d)@]" b x y; pp_pts ppf pts
    in
    pp ppf "@,@[<v1>(contour%a" pp_pts pts
  in
  let pp_simple ppf gid cs bbox =
    pp ppf "@,@[<v1>(glyph-simple %d %a" gid pp_bbox bbox;
    List.iter (pp_contour ppf) cs;
    pp ppf ")@]"
  in
  let pp_matrix ppf m = match m with
  | None -> ()
  | Some (a, b, c, d) -> pp ppf "@ @[<1>(ltr %g %g %g %g)@]" a b c d
  in
  let pp_component ppf (gid, (dx, dy), m) =
    pp ppf "@,@[(component %d @[(move@ %d %d)@]%a)@]" gid dx dy pp_matrix m
  in
  let pp_composite ppf gid cs bbox =
    pp ppf "@,@[<v1>(glyph-composite %d %a" gid pp_bbox bbox;
    List.iter (pp_component ppf) cs;
    pp ppf ")@]"
  in
  match Otfm.glyph_count d with
  | Error _ as e -> e
  | Ok gc ->
      pp ppf "@,@[<v1>(glyf";
      let rec loop gid =
        if gid >= gc then (pp ppf "@]"; Ok ()) else
        match Otfm.loca d gid with
        | Error _ as e -> e
        | Ok None -> pp ppf "@,@[(glyph-no-outline %d)@]" gid; loop (gid + 1)
        | Ok (Some gloc) ->
            match Otfm.glyf d gloc with
            | Error _ as e -> e
            | Ok (`Simple cs, bb) -> pp_simple ppf gid cs bb; loop (gid + 1)
            | Ok (`Composite cs, bb) ->
                pp_composite ppf gid cs bb; loop (gid + 1)
      in
      loop 0

let pp_loc_format ppf = function
  | Otfm.ShortLocFormat -> pp ppf "SHORT"
  | Otfm.LongLocFormat  -> pp ppf "LONG"

let pp_head ppf d =
  pp ppf "@,@[<v1>(head";
  match Otfm.head d with
  | Error _ as e -> e
  | Ok h ->
      pp ppf "@,(font-revision 0x%08LX)" (Otfm.WideInt.to_int64 h.Otfm.head_font_revision);
      pp ppf "@,(flags 0x%04X)" h.Otfm.head_flags;
      pp ppf "@,(units-per-em %d)" h.Otfm.head_units_per_em;
      pp ppf "@,(created %a)" Otfm.WideInt.pp h.Otfm.head_created;
      pp ppf "@,(modified %a)" Otfm.WideInt.pp h.Otfm.head_modified;
      pp ppf "@,(xmin %d)" h.Otfm.head_xmin;
      pp ppf "@,(ymin %d)" h.Otfm.head_ymin;
      pp ppf "@,(xmax %d)" h.Otfm.head_xmax;
      pp ppf "@,(ymax %d)" h.Otfm.head_ymax;
      pp ppf "@,(mac-style 0x%04X)" h.Otfm.head_mac_style;
      pp ppf "@,(lowest_rec_ppem %d)" h.Otfm.head_lowest_rec_ppem;
      pp ppf "@,(index_to_loc_format %a)" pp_loc_format h.Otfm.head_index_to_loc_format;
      pp ppf ")@]";
      Ok ()

let pp_hhea ppf d = match Otfm.hhea d with
| Error _ as e -> e
| Ok h ->
    pp ppf "@,@[<v1>(hhea";
    pp ppf "@,(ascender %d)" h.Otfm.hhea_ascender;
    pp ppf "@,(descender %d)" h.Otfm.hhea_descender;
    pp ppf "@,(line-gap %d)" h.Otfm.hhea_line_gap;
    pp ppf "@,(advance-width-max %d)" h.Otfm.hhea_advance_width_max;
    pp ppf "@,(min-left-side-bearing %d)" h.Otfm.hhea_min_left_side_bearing;
    pp ppf "@,(min-right-side-bearing %d)" h.Otfm.hhea_min_right_side_bearing;
    pp ppf "@,(xmax-extent %d)" h.Otfm.hhea_xmax_extent;
    pp ppf "@,(caret-slope-rise %d)" h.Otfm.hhea_caret_slope_rise;
    pp ppf "@,(caret-slope-run %d)" h.Otfm.hhea_caret_slope_run;
    pp ppf "@,(caret-offset %d)" h.Otfm.hhea_caret_offset;
    pp ppf ")@]";
    Ok ()

let pp_hmtx ppf d =
  let pp_hm ppf () id adv lsb = pp ppf "@,(%d (adv %d) (lsb %d))" id adv lsb in
  pp ppf "@,@[<v1>(hmtx";
  match Otfm.hmtx d (pp_hm ppf) () with
  | Error _ as e -> e
  | Ok () -> pp ppf ")@]"; Ok ()

let pp_name ppf d =
  let pp_n ppf () id lang string = pp ppf "@,(%d %s \"%s\")" id lang string in
  pp ppf "@,@[<v1>(name";
  match Otfm.name d (pp_n ppf) () with
  | Error _ as e -> e
  | Ok () -> pp ppf ")@]"; Ok ()

let pp_os2 ppf d =
  let pp_opt pp_v ppf = function None -> pp ppf "None" | Some v -> pp_v ppf v in
  let pp_oint = pp_opt Format.pp_print_int in
  match Otfm.os2 d with
  | Error _ as e -> e
  | Ok o ->
      pp ppf "@,@[<v1>(os2";
      pp ppf "@,(x-avg-char-width %d)" o.Otfm.os2_x_avg_char_width;
      pp ppf "@,(us-weight-class %d)" o.Otfm.os2_us_weight_class;
      pp ppf "@,(us-width-class %d)" o.Otfm.os2_us_width_class;
      pp ppf "@,(fs-type %X)" o.Otfm.os2_fs_type;
      pp ppf "@,(y-subscript-x-size %d)" o.Otfm.os2_y_subscript_x_size;
      pp ppf "@,(y-subscript-y-size %d)" o.Otfm.os2_y_subscript_y_size;
      pp ppf "@,(y-subscript-x-offset %d)" o.Otfm.os2_y_subscript_x_offset;
      pp ppf "@,(y-subscript-y-offset %d)" o.Otfm.os2_y_subscript_y_offset;
      pp ppf "@,(y-superscript-x-size %d)" o.Otfm.os2_y_superscript_x_size;
      pp ppf "@,(y-superscript-y-size %d)" o.Otfm.os2_y_superscript_y_size;
      pp ppf "@,(y-superscript-x-offset %d)" o.Otfm.os2_y_superscript_x_offset;
      pp ppf "@,(y-superscript-y-offset %d)" o.Otfm.os2_y_superscript_y_offset;
      pp ppf "@,(y-strikeout-size %d)" o.Otfm.os2_y_strikeout_size;
      pp ppf "@,(y-strikeout-position %d)" o.Otfm.os2_y_strikeout_position;
      pp ppf "@,(family-class %d)" o.Otfm.os2_family_class;
      pp ppf "@,(panose \"%s\")" (String.escaped o.Otfm.os2_panose);
      pp ppf "@,(ul-unicode-range1 %LX)" (Otfm.WideInt.to_int64 o.Otfm.os2_ul_unicode_range1);
      pp ppf "@,(ul-unicode-range2 %LX)" (Otfm.WideInt.to_int64 o.Otfm.os2_ul_unicode_range2);
      pp ppf "@,(ul-unicode-range3 %LX)" (Otfm.WideInt.to_int64 o.Otfm.os2_ul_unicode_range3);
      pp ppf "@,(ul-unicode-range4 %LX)" (Otfm.WideInt.to_int64 o.Otfm.os2_ul_unicode_range4);
      pp ppf "@,(ach-vend-id %a)"
        Otfm.Tag.pp (Otfm.Tag.of_wide_int o.Otfm.os2_ach_vend_id);
      pp ppf "@,(fs-selection %X)" o.Otfm.os2_fs_selection;
      pp ppf "@,(us-first-char-index %d)" o.Otfm.os2_us_first_char_index;
      pp ppf "@,(us-last-char-index %d)" o.Otfm.os2_us_last_char_index;
      pp ppf "@,(s-typo-ascender %d)" o.Otfm.os2_s_typo_ascender;
      pp ppf "@,(s-type-descender %d)" o.Otfm.os2_s_type_descender;
      pp ppf "@,(s-typo-linegap %d)" o.Otfm.os2_s_typo_linegap;
      pp ppf "@,(us-win-ascent %d)" o.Otfm.os2_us_win_ascent;
      pp ppf "@,(us-win-descent %d)" o.Otfm.os2_us_win_descent;
      pp ppf "@,(ul-code-page-range-1 %a)" (pp_opt Otfm.WideInt.pp) o.Otfm.os2_ul_code_page_range_1;
      pp ppf "@,(ul-code-page-range-2 %a)" (pp_opt Otfm.WideInt.pp) o.Otfm.os2_ul_code_page_range_2;
      pp ppf "@,(s-x-height %a)" pp_oint o.Otfm.os2_s_x_height;
      pp ppf "@,(s-cap-height %a)" pp_oint o.Otfm.os2_s_cap_height;
      pp ppf "@,(us-default-char %a)" pp_oint o.Otfm.os2_us_default_char;
      pp ppf "@,(us-break-char %a)" pp_oint o.Otfm.os2_us_break_char;
      pp ppf "@,(us-max-context %a)" pp_oint o.Otfm.os2_us_max_context;
      pp ppf ")@]";
      Ok ()

let pp_kern ppf has_kern d =
  if not has_kern then Ok () else
  let dir = function `H -> "H" | `V -> "V" in
  let kind = function `Kern -> "kerning" | `Min -> "minimal" in
  let pp_kinfo ppf first i =
    if not first then pp ppf ")@]";
    pp ppf "@,@[<v1>((dir %s)@,(kind %s)@,(cross-stream %b)"
      (dir i.Otfm.kern_dir) (kind i.Otfm.kern_kind)
      (i.Otfm.kern_cross_stream);
    `Fold, false
  in
  let pp_pair ppf first l r v = pp ppf "@,(%d %d %d)" l r v; first in
  pp ppf "@,@[<v1>(kern";
  match Otfm.kern d (pp_kinfo ppf) (pp_pair ppf) true with
  | Error _ as e -> e
  | Ok _ -> pp ppf ")@])@]"; Ok ()

let pp_tables ppf inf ts d =
  let err = ref false in
  let ( >>= ) x f = match x with
  | Ok () -> f ()
  | Error e -> log_err inf e; err := true; f ()
  in
  pp_name ppf d >>= fun () ->
  pp_head ppf d >>= fun () ->
  pp_hhea ppf d >>= fun () ->
  pp_os2  ppf d >>= fun () ->
  pp_cmap ppf d >>= fun () ->
  pp_hmtx ppf d >>= fun () ->
  pp_glyf ppf (List.mem Otfm.Tag.glyf ts) d >>= fun () ->
  pp_kern ppf (List.mem Otfm.Tag.kern ts) d >>= fun () ->
  if !err then (Error `Reported) else Ok ()

(* Commands *)

let pp_single_font ppf inf d =
    let ( >>= ) x f = match x with
    | Ok v -> f v
    | Error e -> Error (e :> [ Otfm.error | `Reported | `Msg of string])
    in
      Otfm.flavour d >>= fun f ->
      let fs =
        match f with
        | Otfm.TTF_OT   -> "TTF-OpenType"
        | Otfm.TTF_true -> "TTF-TrueType"
        | Otfm.CFF      -> "CFF"
      in
      pp ppf "@,@[<1>(flavor %s)@]" fs;
      Otfm.postscript_name d >>= fun name ->
      let oname = match name with None -> "<none>" | Some n -> n in
      pp ppf "@,@[<1>(postscript-name %s)@]" oname;
      Otfm.glyph_count d >>= fun glyph_count ->
      pp ppf "@,@[<1>(glyph-count %d)@]" glyph_count;
      Otfm.table_list d >>= fun ts ->
      pp ppf "@,@[<1>(tables ";
      List.iter (fun t -> pp ppf "@ %a" Otfm.Tag.pp t) ts;
      pp ppf ")@]";
      pp_tables ppf inf ts d


let pp_file ppf inf =
  match string_of_file inf with
  | Error _ as e -> e
  | Ok s ->
    let ( >>= ) x f = match x with
    | Ok v -> f v
    | Error e -> Error (e :> [ Otfm.error | `Reported | `Msg of string])
    in
    Otfm.decoder (`String s) >>= function
    | Otfm.TrueTypeCollection(ttc) ->
        pp ppf "@[<v1>(@[<1>(file %S)@]" inf;
        pp ppf "@,@[<1>(TRUETYPE-COLLECTION %d)@]" (List.length ttc);
        ttc |> List.fold_left (fun res ttcelem ->
          res >>= fun i ->
          Otfm.decoder_of_ttc_element ttcelem >>= fun d ->
          let name =
            match Otfm.postscript_name d with
            | Error(_)       -> "<error>"
            | Ok(None)       -> "<none>"
            | Ok(Some(name)) -> name
          in
          pp ppf "@,@[<v1>(@[<1>(ttc-element %d \"%s\")@]" i name;
          pp_single_font ppf inf d >>= fun () ->
          pp ppf ")@]";
          Ok(i + 1)
        ) (Ok(0)) >>= fun _ ->
        pp ppf ")@]@.";
        Ok ()
    | Otfm.SingleDecoder(d) ->
        pp ppf "@[<v1>(@[<1>(file %S)@]" inf;
        pp ppf "@,@[<1>(SINGLE-FONT)@]";
        pp_single_font ppf inf d >>= fun () ->
        pp ppf ")@]@.";
        Ok ()

let dec_file inf = match string_of_file inf with
| Error _ as e -> e
| Ok s ->
    let err = ref false in
    let ( >>= ) x f = match x with
    | Ok _    -> f ()
    | Error e -> log_err inf e; err := true; f ()
    in
    let kern_nop () _ = `Fold, () in
    let nop4 _ _ _ _ = () in
    match Otfm.decoder (`String s) with
    | Error e -> log_err inf e; err := true; Error (`Msg("dec_file"))
    | Ok(Otfm.TrueTypeCollection(_)) ->
        Ok ()
    | Ok(Otfm.SingleDecoder(d)) ->
        Otfm.flavour d      >>= fun () ->
        Otfm.table_list d   >>= fun () ->
        Otfm.cmap d         >>= fun () ->
        Otfm.head d         >>= fun () ->
        Otfm.hhea d         >>= fun () ->
        Otfm.hmtx d nop4 () >>= fun () ->
        Otfm.name d nop4 () >>= fun () ->
        Otfm.os2  d         >>= fun () ->
        Otfm.kern d kern_nop nop4 () >>= fun () ->
        if !err then (Error `Reported) else Ok ()

let ps_file inf = match string_of_file inf with
| Error _ as e -> e
| Ok s ->
    match Otfm.decoder (`String s) with
    | Ok(Otfm.SingleDecoder(d)) ->
        begin
          match Otfm.postscript_name d with
          | Error e -> Error (e :> [ Otfm.error | `Reported | `Msg of string])
          | Ok None -> Printf.printf "%s: <none>\n" inf; Ok ()
          | Ok (Some n) -> Printf.printf "%s: %s\n" inf n; Ok ()
        end

    | _ -> Printf.eprintf "error, or TTC file\n"; Ok ()

(* otftrip *)

let main () =
  let usage = Printf.sprintf
    "Usage: %s [OPTION]... [OTFFILE]...\n\
     Print human readable OpenType file font information on stdout.\n\
    Options:" exec
  in
  let cmd = ref `Pp in
  let set_cmd v () = cmd := v in
  let files = ref [] in
  let add_file f = files := f :: !files in
  let options = [
    "-d", Arg.Unit (set_cmd `Dec), "Decode only";
    "-p", Arg.Unit (set_cmd `Ps), "Only output postscript name";
  ]
  in
  Arg.parse (Arg.align options) add_file usage;
  let files = match List.rev ! files with [] -> ["-"] | fs -> fs in
  let cmd =
    match !cmd with
    | `Pp  -> pp_file Format.std_formatter (* (Format.formatter_of_out_channel (open_out "/dev/null")) *)
    | `Dec -> dec_file
    | `Ps  -> ps_file
  in
  let fold_cmd cmd err fn = match cmd fn with
  | Error `Reported -> true
  | Error (`Msg e) -> log "%s" e; true
  | Error (#Otfm.error as e) -> log_err fn e; true
  | Ok () -> err
  in
  let err = List.fold_left (fold_cmd cmd) false files in
  if err then exit 1 else exit 0

let () = main ()

(*---------------------------------------------------------------------------
   Copyright (c) 2013 Daniel C. Bünzli

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
