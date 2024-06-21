open MyUtil

type data =
  { raw_bytes : bytes
  ; bits_per_components : int
  ; alpha_bytes : bytes option
  }

let cannot_load_image msg abs_path = raise @@ ImageHashTable.CannotLoadImage (msg, abs_path)

let make_smask pdfmain widdots hgtdots alpha_bytes =
  let iobytes =
    alpha_bytes
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
      ("/ColorSpace"      , Pdf.Name("/DeviceGray"));
      ("/Filter"          , Pdf.Name("/FlateDecode"));
      ("/Length"          , Pdf.Integer(len));
      ("/BitsPerComponent", Pdf.Integer(8));
    ]
  in
  let ir = Pdf.addobj pdfmain (Pdf.Stream(ref (pdfdict, stream))) in
  Pdf.Indirect(ir)

let make_xobject (pdfmain : Pdf.t) (colorspace : Pdf.pdfobject) widdots hgtdots abs_path =
  let img =
    try Images.load (get_abs_path_string abs_path) []
    with
    | Failure message -> cannot_load_image message abs_path
  in
  let {raw_bytes; bits_per_components; alpha_bytes} =
    match img with
    | Rgb24 img ->
      { raw_bytes = Rgb24.dump img
      ; bits_per_components = 8
      ; alpha_bytes = None
      }
    | Index8 img ->
      { raw_bytes = Rgb24.dump (Index8.to_rgb24 img)
      ; bits_per_components = 8
      ; alpha_bytes = None
      }
    | Index16 img ->
      { raw_bytes = Rgb24.dump (Index16.to_rgb24 img)
      ; bits_per_components = 8
      ; alpha_bytes = None
      }
    | Rgba32 img ->
      let extract_alpha_channel xs =
        Bytes.to_seqi xs
        |> Seq.filter_map (fun (i, x) -> if i mod 4 = 3 then Some x else None)
        |> Bytes.of_seq
      in
      { raw_bytes = Rgb24.dump (Rgb24.of_rgba32 img)
      ; bits_per_components = 8
      ; alpha_bytes = Some (extract_alpha_channel (Rgba32.dump img))
      }
    | Cmyk32 _ ->
      cannot_load_image "cmyk images are not supported" abs_path
  in
  let smask = Option.map (make_smask pdfmain widdots hgtdots) alpha_bytes in
  let iobytes =
    raw_bytes
    |> Pdfio.bytes_of_caml_bytes
    |> Pdfcodec.encode_flate
  in
  let len = Pdfio.bytes_size iobytes in
  let stream = Pdf.Got(iobytes) in
  let pdfdict =
    Pdf.Dictionary ([
      ("/Type"            , Pdf.Name("/XObject"));
      ("/Subtype"         , Pdf.Name("/Image"));
      ("/Width"           , Pdf.Integer(widdots));
      ("/Height"          , Pdf.Integer(hgtdots));
      ("/ColorSpace"      , colorspace);
      ("/Filter"          , Pdf.Name("/FlateDecode"));
      ("/Length"          , Pdf.Integer(len));
      ("/BitsPerComponent", Pdf.Integer(bits_per_components));
    ] @ Option.to_list (Option.map (fun smask -> ("/SMask", smask)) smask))
  in
  let ir = Pdf.addobj pdfmain (Pdf.Stream(ref (pdfdict, stream))) in
  Pdf.Indirect(ir)
