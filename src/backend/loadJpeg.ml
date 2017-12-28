
open MyUtil


type file_path = string

exception UnsupportedColorModel of Images.colormodel
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

let make_xobject (pdfmain : Pdf.t) (colormodel : Images.colormodel) (widdots : int) (hgtdots : int) (srcpath : file_path) =
(*
  let s = string_of_file srcpath in
*)
  let ic = open_in_bin srcpath in
    (* -- may emit 'Sys_error(_)' -- *)
  let stream = Pdf.Got(Pdfio.bytes_of_input_channel ic) in
  let colorspace =
    match colormodel with
    | Images.Gray  -> Pdf.Name("/DeviceGray")
    | Images.RGB   -> Pdf.Name("/DeviceRGB")
    | _            -> raise (UnsupportedColorModel(colormodel))
  in
  let pdfdict =
    Pdf.Dictionary[
      ("/Type"            , Pdf.Name("/XObject"));
      ("/Subtype"         , Pdf.Name("/Image"));
      ("/Width"           , Pdf.Integer(widdots));
      ("/Height"          , Pdf.Integer(hgtdots));
      ("/ColorSpace"      , colorspace);
      ("/BitsPerComponent", Pdf.Integer(8));
        (* -- bits per component is 8 for /DCDDecode -- *)
    ]
  in
  let ir = Pdf.addobj pdfmain (Pdf.Stream(ref (pdfdict, stream))) in
    Pdf.Indirect(ir)
