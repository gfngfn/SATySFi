open HorzBox

exception InvalidFontAbbrev of font_abbrev

val initialize : unit -> unit

val get_metrics_of_word : font_abbrev -> SkipLength.t -> InternalText.t -> tj_string * skip_width * skip_height * skip_depth

val get_tag : font_abbrev -> string

val get_font_dictionary : Pdf.t -> unit -> (string * Pdf.pdfobject) list
