
type file_path = string
type mime_type = string

type file_specification =
  | External of file_path
(*
  | Embedded of file_path
*)

type media_clip_data = {
  data      : file_specification;
  mime_type : mime_type;
}

type media_clip =
  | MediaClipData of media_clip_data
(*
  | MediaClipSection of
*)

type media_rendition = {
  media_clip : media_clip;
(*
  media_play_parameter   :
  media_screen_parameter :
*)
}

type t =
  | Media of media_rendition

val pdfobject_of_rendition : Pdf.t -> t -> Pdf.pdfobject
