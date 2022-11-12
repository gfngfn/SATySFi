
open MyUtil
open FontError

type 'a ok = ('a, font_error) result

type glyph_id

val notdef : glyph_id

type glyph_segment = glyph_id * glyph_id list

type per_mille =
  | PerMille of int

type mark_info =
  | Mark of glyph_id * per_mille * (per_mille * per_mille)

type glyph_synthesis = glyph_id * mark_info list

type metrics = per_mille * per_mille * per_mille

val hex_of_glyph_id : glyph_id -> string

type decoder

val postscript_name : decoder -> string

type 'a resource =
  | Data           of 'a
  | EmbeddedStream of int

type cmap =
  | PredefinedCMap of string
(*
  | CMapFile       of (string resource) ref  (* TODO *)
*)

type font

val make_dictionary : Pdf.t -> font -> decoder -> Pdf.pdfobject ok

val get_decoder_single : abs_path -> (decoder * font) ok

val get_decoder_ttc : abs_path -> int -> (decoder * font) ok

val get_glyph_metrics : decoder -> glyph_id -> metrics ok

val get_glyph_id : decoder -> Uchar.t -> (glyph_id option) ok

val convert_to_ligatures : decoder -> glyph_segment list -> (glyph_synthesis list) ok

val find_kerning : decoder -> glyph_id -> glyph_id -> per_mille option

type math_kern

type math_kern_info = {
  kernTR : math_kern;
  kernTL : math_kern;
  kernBR : math_kern;
  kernBL : math_kern;
}

type math_decoder

val get_math_decoder_single : abs_path -> (math_decoder * font) ok

val get_math_decoder_ttc : abs_path -> int -> (math_decoder * font) ok

val math_base_font : math_decoder -> decoder

val get_math_glyph_id : math_decoder -> Uchar.t -> (glyph_id option) ok

val get_math_script_variant : math_decoder -> glyph_id -> (glyph_id) ok

val get_math_glyph_metrics : math_decoder -> glyph_id -> metrics ok

val get_math_correction_metrics : math_decoder -> glyph_id -> per_mille option * math_kern_info option

val get_math_vertical_variants : math_decoder -> glyph_id -> ((glyph_id * float) list) ok

val get_math_horizontal_variants : math_decoder -> glyph_id -> ((glyph_id * float) list) ok

type math_constants = {
(* General: *)
  axis_height                   : float;
(* Sub/superscripts: *)
  superscript_bottom_min        : float;
  superscript_shift_up          : float;
  superscript_baseline_drop_max : float;
  subscript_top_max             : float;
  subscript_shift_down          : float;
  subscript_baseline_drop_min   : float;
  script_scale_down             : float;
  script_script_scale_down      : float;
  space_after_script            : float;
  sub_superscript_gap_min       : float;
(* Fractions: *)
  fraction_rule_thickness       : float;
  fraction_numer_d_shift_up     : float;
  fraction_numer_d_gap_min      : float;
  fraction_denom_d_shift_down   : float;
  fraction_denom_d_gap_min      : float;
(* Radicals: *)
  radical_extra_ascender        : float;
  radical_rule_thickness        : float;
  radical_d_vertical_gap        : float;
(* Limits: *)
  upper_limit_gap_min           : float;
  upper_limit_baseline_rise_min : float;
  lower_limit_gap_min           : float;
  lower_limit_baseline_drop_min : float;
}

val get_axis_height_ratio : math_decoder -> float

val get_math_constants : math_decoder -> math_constants

val find_kern_ratio : math_decoder -> math_kern -> float -> float
