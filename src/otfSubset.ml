
open OtfUtils


let make d gidlst =

(* -- generates the subset of the glyph table -- *)
  gidlst |> List.fold_left (fun res gid ->
    res >>= fun rgacc ->
    Otfm.get_raw_glyph d gid >>= fun rg ->
    return (rg :: rgacc)
  ) (return []) >>= fun rgacc ->
  let rglst = List.rev rgacc in
  Otfm.Encode.truetype_outline_tables rglst >>= fun info ->
  let rawtbl_hmtx = info.Otfm.Encode.hmtx in
  let rawtbl_glyf = info.Otfm.Encode.glyf in
  let rawtbl_loca = info.Otfm.Encode.loca in

(* -- updates the 'maxp' table -- *)
  Otfm.maxp d >>= fun maxp ->
  let maxpnew =
    { maxp with
      Otfm.maxp_num_glyphs = info.Otfm.Encode.number_of_glyphs;
    }
  in
  Otfm.Encode.maxp maxpnew >>= fun rawtbl_maxp ->

(* -- updates the 'head' table -- *)
  Otfm.head d >>= fun head ->
  let headnew =
    { head with
      Otfm.head_xmin = info.Otfm.Encode.xmin;
      Otfm.head_ymin = info.Otfm.Encode.ymin;
      Otfm.head_xmax = info.Otfm.Encode.xmax;
      Otfm.head_ymax = info.Otfm.Encode.ymax;
      Otfm.head_index_to_loc_format = info.Otfm.Encode.index_to_loc_format;
    }
  in
  Otfm.Encode.head headnew >>= fun rawtbl_head ->

(* -- updates the 'hhea' table -- *)
  Otfm.hhea d >>= fun hhea ->
  let hheanew =
    { hhea with
      Otfm.hhea_advance_width_max = info.Otfm.Encode.advance_width_max;
      Otfm.hhea_min_left_side_bearing = info.Otfm.Encode.min_left_side_bearing;
      Otfm.hhea_min_right_side_bearing = info.Otfm.Encode.min_right_side_bearing;
      Otfm.hhea_xmax_extent = info.Otfm.Encode.x_max_extent;
    }
  in
  Otfm.Encode.hhea info.Otfm.Encode.number_of_h_metrics hheanew >>= fun rawtbl_hhea ->

(* -- 'cmap' table -- *)
  Otfm.Encode.empty_cmap () >>= fun rawtbl_cmap ->

  Otfm.Encode.make_font_file [
    rawtbl_cmap;
    rawtbl_head;
    rawtbl_hhea;
    rawtbl_maxp;
    rawtbl_hmtx;
    rawtbl_loca;
    rawtbl_glyf;
  ] >>= fun data ->
  return data
