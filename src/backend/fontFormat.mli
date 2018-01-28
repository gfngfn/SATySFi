
type file_path = string

type glyph_id

type per_mille =
  | PerMille of int

type metrics = per_mille * per_mille * per_mille

(*
val gid : glyph_id -> int  (* for debug *)
*)

val hex_of_glyph_id : glyph_id -> string

type decoder

exception FailToLoadFontOwingToSize   of file_path
exception FailToLoadFontOwingToSystem of file_path * string
exception BrokenFont                  of file_path * string
exception CannotFindUnicodeCmap       of file_path

type cid_system_info

type font_registration =
  | CIDFontType0Registration   of cid_system_info * bool
      (* -- last boolean: true iff it should embed /W information -- *)
  | CIDFontType2OTRegistration of cid_system_info * bool
      (* -- last boolean: true iff it should embed /W information -- *)
  | CIDFontType2TTRegistration of cid_system_info * bool
      (* -- last boolean: true iff it should embed /W information -- *)

val get_decoder_single : file_path -> (decoder * font_registration) option

type 'a resource =
  | Data           of 'a
  | EmbeddedStream of int

type cmap =
  | PredefinedCMap of string
  | CMapFile       of (string resource) ref  (* temporary;*)
(*
module Type1 : sig
  type font
  val of_decoder : decoder -> int -> int -> font
  val to_pdfdict : Pdf.t -> font -> decoder -> Pdf.pdfobject
end

module TrueType : sig
  type font
  val of_decoder : decoder -> int -> int -> font
  val to_pdfdict : Pdf.t -> font -> decoder -> Pdf.pdfobject
end
*)
module Type0 : sig
  type font
  val to_pdfdict : Pdf.t -> font -> decoder -> Pdf.pdfobject
end

module CIDFontType0 : sig
  type font
  val of_decoder : decoder -> cid_system_info -> font
end

module CIDFontType2 : sig
  type font
  val of_decoder : decoder -> cid_system_info -> bool -> font
end

type cid_font =
  | CIDFontType0 of CIDFontType0.font
  | CIDFontType2 of CIDFontType2.font

type font =
(*
  | Type1    of Type1.font
  | TrueType of TrueType.font
*)
  | Type0    of Type0.font

(*
val type1 : Type1.font -> font
val true_type : TrueType.font -> font
*)
val cid_font_type_0 : CIDFontType0.font -> string -> cmap -> font
val cid_font_type_2 : CIDFontType2.font -> string -> cmap -> font

val get_glyph_metrics : decoder -> glyph_id -> metrics
val get_glyph_id : decoder -> Uchar.t -> glyph_id option

val adobe_japan1 : cid_system_info
val adobe_identity : cid_system_info

val convert_to_ligatures : decoder -> glyph_id list -> glyph_id list

val find_kerning : decoder -> glyph_id -> glyph_id -> per_mille option

type math_kern

type math_kern_info =
  {
    kernTR : math_kern;
    kernTL : math_kern;
    kernBR : math_kern;
    kernBL : math_kern;
  }

type math_decoder

val get_math_decoder : file_path -> (math_decoder * font_registration) option

val math_base_font : math_decoder -> decoder

val get_math_glyph_id : math_decoder -> Uchar.t -> glyph_id

val get_math_script_variant : math_decoder -> glyph_id -> glyph_id

val get_math_glyph_metrics : math_decoder -> glyph_id -> metrics

val get_math_correction_metrics : math_decoder -> glyph_id -> per_mille option * math_kern_info option

val get_math_vertical_variants : math_decoder -> glyph_id -> (glyph_id * float) list

val get_math_horizontal_variants : math_decoder -> glyph_id -> (glyph_id * float) list

type math_constants =
  {
  (* -- general -- *)
    axis_height                   : float;
  (* -- sub/superscripts -- *)
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
  (* -- fractions -- *)
    fraction_rule_thickness       : float;
    fraction_numer_d_shift_up     : float;
    fraction_numer_d_gap_min      : float;
    fraction_denom_d_shift_down   : float;
    fraction_denom_d_gap_min      : float;
  (* -- radicals -- *)
    radical_extra_ascender        : float;
    radical_rule_thickness        : float;
    radical_d_vertical_gap        : float;
  (* -- limits -- *)
    upper_limit_gap_min           : float;
    upper_limit_baseline_rise_min : float;
    lower_limit_gap_min           : float;
    lower_limit_baseline_drop_min : float;
  }

val get_axis_height_ratio : math_decoder -> float

val get_math_constants : math_decoder -> math_constants

val find_kern_ratio : math_decoder -> math_kern -> float -> float
