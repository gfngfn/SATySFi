open MyUtil

type data =
  { raw_bytes : bytes
  ; bits_per_components : int
  }

let cannot_load_image msg abs_path = raise @@ ImageHashTable.CannotLoadImage (msg, abs_path)

let make_xobject (pdfmain : Pdf.t) (colorspace : Pdf.pdfobject) widdots hgtdots abs_path =
  let img =
    try Images.load (get_abs_path_string abs_path) []
    with
    | Failure message -> cannot_load_image message abs_path
  in
  let {raw_bytes; bits_per_components} =
    match img with
    | Rgb24 img ->
      { raw_bytes = Rgb24.dump img
      ; bits_per_components = 8
      }
    | Index8 img ->
      { raw_bytes = Rgb24.dump (Index8.to_rgb24 img)
      ; bits_per_components = 8
      }
    | Index16 img ->
      { raw_bytes = Rgb24.dump (Index16.to_rgb24 img)
      ; bits_per_components = 8
      }
    | Rgba32 _ ->
      cannot_load_image "images with alpha channel are not supported" abs_path
    | Cmyk32 _ ->
      cannot_load_image "cmyk images are not supported" abs_path
  in
  let iobytes =
    raw_bytes
    |> Pdfio.bytes_of_caml_bytes
    |> Pdfcodec.encode_flate
  in
  let len = Pdfio.bytes_size iobytes in
  let stream = Pdf.Got(iobytes) in
  let pdfdict =
    Pdf.Dictionary[
      ("/Type"            , Pdf.Name("/XObject"));
      ("/Subtype"         , Pdf.Name("/Image"));
      ("/Width"           , Pdf.Integer(widdots));
      ("/Height"          , Pdf.Integer(hgtdots));
      ("/ColorSpace"      , colorspace);
      ("/Filter"          , Pdf.Name("/FlateDecode"));
      ("/Length"          , Pdf.Integer(len));
      ("/BitsPerComponent", Pdf.Integer(bits_per_components));
    ]
  in
  let ir = Pdf.addobj pdfmain (Pdf.Stream(ref (pdfdict, stream))) in
  Pdf.Indirect(ir)
