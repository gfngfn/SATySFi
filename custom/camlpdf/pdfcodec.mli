(** Encoding and Decoding PDF Streams *)

(**
{b Currently supported:}
- Decoders: ASCIIHexDecode, ASCII85Decode, FlateDecode,
LZWDecode, RunLengthDecode.
- Encoders: ASCIIHexDecode, ASCII85Decode, FlateDecode, RunLengthDecode.
- Predictors: PNG (all), TIFF (8-bit only).

*)

(** {2 Types} *)

(** Supported encodings. *)
type encoding =
  | ASCIIHex
  | ASCII85
  | RunLength
  | Flate

(** Predictors. *)
type predictor =
    TIFF2
  | PNGNone
  | PNGSub
  | PNGUp
  | PNGAverage
  | PNGPaeth
  | PNGOptimum

(** There was bad data. *)
exception Couldn'tDecodeStream of string

(** CamlPDF doesn't support this encoding or its predictor. *)
exception DecodeNotSupported of string


(** {2 Encoding} *)

(** Encode a PDF stream with an encoding. The only predictor supported is PNGUp. *)
val encode_pdfstream : Pdf.t -> encoding -> ?predictor:predictor -> ?predictor_columns:int -> Pdf.pdfobject -> unit

(** {2 Decoding} *)

(** Given a document and stream, decode. The pdf document is updated
with the decoded stream. May raise either of the exceptions above. *)
val decode_pdfstream : Pdf.t -> Pdf.pdfobject -> unit

(** Given a document and stream decode just one stage. May raise either of the
exceptions above. *)
val decode_pdfstream_onestage : Pdf.t -> Pdf.pdfobject -> unit

(** Given a document and stream decode until there's an unknown decoder. May
raise [Couldn'tDecodeStream]. *)
val decode_pdfstream_until_unknown : Pdf.t -> Pdf.pdfobject -> unit

(** Given a [Pdfio.input] with pointer at the first byte and an inline image
stream dictionary, decode the first decoder and its predictor. Return the data,
or [None] if this decoder isn't supported but the data pointer has been left in
the right place. The exceptions above can both be raised, in the case of bad
data or a completely unknown encoding. *)
val decode_from_input : Pdfio.input -> Pdf.pdfobject -> Pdfio.bytes option

(**/**)

(* Inter-module recursion. *)
val string_of_pdf : (Pdf.pdfobject -> string) ref

val flate_level : int ref

val encode_flate : Pdfio.bytes -> Pdfio.bytes



