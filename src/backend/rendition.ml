
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


let indirect_dictionary pdf keyvals =
  let pdfdict = Pdf.Dictionary(keyvals) in
  let ir = Pdf.addobj pdf pdfdict in
  Pdf.Indirect(ir)


let pdfobject_of_data pdf = function
  | External(src) ->
      let srchex = InternalText.to_utf16be_hex (InternalText.of_utf8 src) in
      indirect_dictionary pdf [
        ("/Type", Pdf.Name("/Filespec"));
        ("/F"   , Pdf.String(src));
        ("/UF"  , Pdf.StringHex(srchex));
      ]
        (* -- file specification dictionary [Table 3.41] -- *)
(*
  | Embedded(src) ->
*)

let pdfobject_of_media_clip pdf = function
  | MediaClipData(clipdata) ->
      let pdfobjfilespec = pdfobject_of_data pdf clipdata.data in
      indirect_dictionary pdf [
        ("/Type", Pdf.Name("/MediaClip"));
        ("/S"   , Pdf.Name("/MCD"));
        ("/D"   , pdfobjfilespec);
        ("/CT"  , Pdf.String(clipdata.mime_type));
      ]


let pdfobject_of_rendition pdf = function
  | Media(medrend) ->
      let pdfobjclip = pdfobject_of_media_clip pdf medrend.media_clip in
      indirect_dictionary pdf [
        ("/Type", Pdf.Name("/Rendition"));
        ("/S"   , Pdf.Name("/MR"));
        ("/C"   , pdfobjclip);
      ]
