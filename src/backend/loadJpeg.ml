

type file_path = string

(*
exception TooLargeImage         of file_path
exception CannotLoadImage       of file_path * string


let string_of_file (srcpath : file_path) =
    let ic = open_in_bin srcpath in
    let bufsize = 65536 in
    let stepsize = 65536 in
    let buf = Buffer.create bufsize in
    let bytes = Bytes.create stepsize in
    let flag = ref true in
    try
      while !flag do
        let c = input ic bytes 0 bufsize in
        if c = 0 then
          flag := false
        else
          Buffer.add_subbytes buf bytes 0 c
      done;
      close_in ic;
      Buffer.contents buf
    with
    | Failure(_)        -> close_in ic; raise (TooLargeImage(srcpath))
    | Sys_error(errmsg) -> close_in ic; raise (CannotLoadImage(srcpath, errmsg))
*)

let make_xobject (pdfmain : Pdf.t) (colorspace : Pdf.pdfobject) (widdots : int) (hgtdots : int) (srcpath : file_path) =
(*
  let s = string_of_file srcpath in
*)
  let ic = open_in_bin srcpath in
    (* -- may emit 'Sys_error(_)' -- *)
  let iobytes = Pdfio.bytes_of_input_channel ic in
  let len = Pdfio.bytes_size iobytes in
  let stream = Pdf.Got(iobytes) in
  let pdfdict =
    Pdf.Dictionary[
      ("/Type"            , Pdf.Name("/XObject"));
      ("/Subtype"         , Pdf.Name("/Image"));
      ("/Width"           , Pdf.Integer(widdots));
      ("/Height"          , Pdf.Integer(hgtdots));
      ("/ColorSpace"      , colorspace);
      ("/Filter"          , Pdf.Name("/DCTDecode"));
      ("/Length"          , Pdf.Integer(len));
      ("/BitsPerComponent", Pdf.Integer(8));
        (* -- bits per component is 8 for /DCTDecode -- *)
    ]
  in
  let ir = Pdf.addobj pdfmain (Pdf.Stream(ref (pdfdict, stream))) in
    Pdf.Indirect(ir)
