(** Parsing fonts and extracting text from content streams and PDF strings *)

(** {2 Data Types } *)

type type3_glpyhs =
  {fontbbox : float * float * float * float;
   fontmatrix : Pdftransform.transform_matrix;
   charprocs : (string * Pdf.pdfobject) list;
   type3_resources : Pdf.pdfobject}

type simple_fonttype =
  | Type1
  | MMType1
  | Type3 of type3_glpyhs
  | Truetype

type fontmetrics = float array

type fontfile =
  | FontFile of int
  | FontFile2 of int
  | FontFile3 of int

type fontdescriptor = {
    italicangle : float;
    ascent      : float;
    descent     : float;
    leading     : float;
    avgwidth    : float;
    maxwidth    : float;
    stemv       : float;
    fontfile    : fontfile option;
  }

type differences = (string * int) list

type encoding =
  | ImplicitInFontFile
  | StandardEncoding
  | MacRomanEncoding
  | WinAnsiEncoding
  | MacExpertEncoding
  | CustomEncoding of encoding * differences
  | FillUndefinedWithStandard of encoding

type simple_font =
  {fonttype : simple_fonttype;
   basefont : string;
   fontmetrics : fontmetrics option;
   fontdescriptor : fontdescriptor option;
   encoding : encoding}

type standard_font =
  | TimesRoman
  | TimesBold
  | TimesItalic
  | TimesBoldItalic
  | Helvetica
  | HelveticaBold
  | HelveticaOblique
  | HelveticaBoldOblique
  | Courier
  | CourierBold
  | CourierOblique
  | CourierBoldOblique
  | Symbol
  | ZapfDingbats

type cid_system_info =
  {registry : string;
   ordering : string;
   supplement : int}

type composite_CIDfont =
  {cid_system_info : cid_system_info;
   cid_basefont : string;
   cid_fontdescriptor : fontdescriptor;
   cid_widths : (int * float) list;
   cid_default_width : int}
  
type cmap_encoding =
  | Predefined of string
  | CMap of int (* indirect reference to CMap stream *)

type font =
  | StandardFont of standard_font * encoding
  | SimpleFont of simple_font
  | CIDKeyedFont of string * composite_CIDfont * cmap_encoding

(** {2 String representations of fonts } *)

(** Returns a string such as "Times-Bold" for Pdftext.TimesBold etc. *)
val string_of_standard_font : standard_font -> string

(** Parses a string such as "/Times-Bold" or "/TimesNewRoman,Bold" to Pdftext.TimesRomanBold etc. *)
val standard_font_of_name : string -> standard_font option

(** A debug string for the whole font datatype. *)
val string_of_font : font -> string

(** {2 Reading a Font} *)

(** Read a font from a given document and object *)
val read_font : Pdf.t -> Pdf.pdfobject -> font

(** {2 Writing a Font} *)

(** Write a font to a given document, returning the object number for the main
font dictionary *)
val write_font : Pdf.t -> font -> int

(** {2 Utility functions} *)

(** A list of unicode codepoints for a UTF8 string *)
val codepoints_of_utf8 : string -> int list

(** A UTF8 string for a list of unicode codepoints *)
val utf8_of_codepoints : int list -> string

(** {2 Text from strings outside page content} *)

(** Take a pdf string (which will be either pdfdocencoding or UTF16BE) and
return a string representing the same unicode codepoints in UTF8 *)
val utf8_of_pdfdocstring : string -> string

(** Take a UTF8 string and convert to pdfdocencoding (if no unicode-only
characters are used) or UTF16BE (if they are)) *)
val pdfdocstring_of_utf8 : string -> string

(** Build a pdf string in pdfdocencoding (if no unicode-only characters are
used) or UTF16BE (if they are) *)
val pdfdocstring_of_codepoints : int list -> string

(** Produce a list of unicode codepoints from a pdfdocencoding or UTF16BE pdf
document string *)
val codepoints_of_pdfdocstring : string -> int list

(** {2 Text from strings inside page content} *)

(** The type of text extractors. *)
type text_extractor

(** Build a text extractor from a document and font object *)
val text_extractor_of_font : Pdf.t -> Pdf.pdfobject -> text_extractor

(** Return a list of unicode points from a given extractor and string (for
example from a [Pdfpages.Op_Tj] or [Op_TJ] operator). *)
val codepoints_of_text : text_extractor -> string -> int list

(** Return a list of glyph names from a given extractor and string *)
val glyphnames_of_text : text_extractor -> string -> string list

(** {2 Building text for strings inside page content} *)

(** Return the character code for a given unicode codepoint, if it exists in
this encoding. This is only really suitable for simple stuff like standard 14
fonts, or editing text in existing fonts. *)
val charcode_extractor_of_encoding : encoding -> (int -> int option)

val table_of_encoding : encoding -> (int, string) Hashtbl.t

val reverse_table_of_encoding : encoding -> (string, int) Hashtbl.t

