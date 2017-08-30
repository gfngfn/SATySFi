open HorzBox

exception InvalidFontAbbrev of font_abbrev

val initialize : unit -> unit

val get_metrics_of_word : font_abbrev -> SkipLength.t -> InternalText.t -> OutputText.t * skip_width * skip_height * skip_depth

val get_tag_and_encoding : font_abbrev -> string * encoding_in_pdf

val get_font_dictionary : Pdf.t -> unit -> (string * Pdf.pdfobject) list
