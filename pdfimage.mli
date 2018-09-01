(** Extract Images. *)

(* FIXME: Also CMYK etc... *)
type pixel_layout =
  | BPP1 (* Black and white *)
  | BPP8 (* Greyscale *)
  | BPP24 (* Colour *)
  | BPP48 (* 48 bit colour *)

type t =
  | JPEG of Pdfio.bytes * float list option
  | JPEG2000 of Pdfio.bytes * float list option
  | JBIG2 of Pdfio.bytes * float list option
  | Raw of int * int * pixel_layout * Pdfio.bytes

(** Given a pdf document, resources dictionary and a stream representing an
image, return a triple : width, height, and a stream of (width * height * 3)
bytes RGBRGB etc. In all instances, if JPEG or JPEG2000 or JBIG2 is the compression
 method, data is returned still encoded. *)
val get_image_24bpp :
  Pdf.t -> Pdf.pdfobject -> Pdf.pdfobject -> t

(** Similarly, but if it's to be Raw, use the smallest pixel layout required to
represent the data. Not implemented yet. *)
val get_image :
  Pdf.t -> Pdf.pdfobject -> Pdf.pdfobject -> t

(** Get the uncompressed image contents in 24bpp, decoding JPEG, JPEG2000 and JBIG2 as
required. Not implemented yet. *)
val get_image_raw_24bpp :
  Pdf.t -> Pdf.pdfobject -> Pdf.pdfobject -> t

(** Get the uncompressed image contents, decoding JPEG, JPEG2000 and JBIG2 as
required. Not implemented yet. *)
val get_image_raw :
  Pdf.t -> Pdf.pdfobject -> Pdf.pdfobject -> t

(** Return a function which, when given an x and y coordinate, returns the pixel
byte values prior to any decoding, i.e in the raw input image data before /Decode,
/Index lookups and so on. Returns array of components as bytes. *)
val get_image_unprocessed_pixel : Pdf.t -> Pdf.pdfobject -> Pdf.pdfobject
-> (int -> int array) option

