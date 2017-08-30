
type file_path = string

type glyph_id

exception FailToLoadFontFormatOwingToSize   of file_path
exception FailToLoadFontFormatOwingToSystem of string
exception FontFormatBroken                  of Otfm.error
exception NoGlyphID                         of glyph_id

val get_decoder : file_path -> unit -> Otfm.decoder

module KerningTable : sig
  type t
  val create : int -> t
  val add : glyph_id -> glyph_id -> int -> t -> unit
  val find_opt : glyph_id -> glyph_id -> t -> int option
end

val get_kerning_table : Otfm.decoder -> KerningTable.t

type 'a resource =
  | Data           of 'a
  | EmbeddedStream of int

type cmap =
  | PredefinedCMap of string
  | CMapFile       of (string resource) ref  (* temporary;*)

type cid_system_info

module Type1 : sig
  type font
  val of_decoder : Otfm.decoder -> int -> int -> font
  val to_pdfdict : Pdf.t -> font -> Otfm.decoder -> Pdf.pdfobject
end

module TrueType : sig
  type font
  val of_decoder : Otfm.decoder -> int -> int -> font
  val to_pdfdict : Pdf.t -> font -> Otfm.decoder -> Pdf.pdfobject
end

module Type0 : sig
  type font
  val to_pdfdict : Pdf.t -> font -> Otfm.decoder -> Pdf.pdfobject
end

module CIDFontType0 : sig
  type font
  val of_decoder : Otfm.decoder -> cid_system_info -> font
end

module CIDFontType2 : sig
  type font
end

type cid_font =
  | CIDFontType0 of CIDFontType0.font
  | CIDFontType2 of CIDFontType2.font

type font =
  | Type1    of Type1.font
(*  | Type1C *)
(*  | MMType1 *)
(*  | Type3 *)
  | TrueType of TrueType.font
  | Type0    of Type0.font

val adobe_japan1 : cid_system_info

val type1 : Type1.font -> font
val true_type : TrueType.font -> font
val cid_font_type_0 : CIDFontType0.font -> string -> cmap -> font

val get_glyph_metrics : Otfm.decoder -> glyph_id -> int * int * int
val get_glyph_id : Otfm.decoder -> Uchar.t -> glyph_id option
