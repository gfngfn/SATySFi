
open MyUtil

type tag = string

type bbox = float * float * float * float

type key = int

type value_main =
  | PDFImage   of Pdf.t * Pdfpage.t
  | OtherImage of Images.format * Pdf.pdfobject * int * int * abs_path
  | StubImage  of abs_path

type value = tag * bbox * value_main

exception CannotLoadPdf          of string * abs_path * int
exception CannotLoadImage        of string * abs_path
exception ImageOfWrongFileType   of abs_path
exception UnsupportedColorModel  of Images.colormodel * abs_path


let main_hash_table : (key, value) Hashtbl.t = Hashtbl.create 32

let current_id_ref : int ref = ref 0


let initialize () =
  begin
    Images.add_methods Images.Jpeg
      Images.({
        check_header  = Jpeg.check_header;
        load          = Some(Jpeg.load);
        save          = Some(Jpeg.save);
        load_sequence = None;
        save_sequence = None;
      });
    current_id_ref := 0;
    Hashtbl.clear main_hash_table;
  end


let generate_tag () : key * tag =
  let n = !current_id_ref in
  begin
    incr current_id_ref;
    (n, "/I" ^ (string_of_int n))
  end


let add_pdf (abspath : abs_path) (pageno : int) =
  let pdfext =
    try Pdfread.pdf_of_file None None (get_abs_path_string abspath) with
    | Pdf.PDFError(msg) -> raise (CannotLoadPdf(msg, abspath, pageno))
  in
    if pageno < 1 then
      raise (CannotLoadPdf("Page number should be greater than 0", abspath, pageno))
    else
      match LoadPdf.get_page pdfext (pageno - 1) with
      | None               -> raise (CannotLoadPdf("Invalid page number", abspath, pageno))
      | Some((bbox, page)) ->
          let (key, tag) = generate_tag () in
          begin
            Hashtbl.add main_hash_table key (tag, bbox, PDFImage(pdfext, page));
            key
          end


let add_image (abspath : abs_path) =
  let (imgfmt, imgheader) =
    try Images.file_format (get_abs_path_string abspath) with
    | Images.Wrong_file_type -> raise (ImageOfWrongFileType(abspath))
    | Sys_error(msg)         -> raise (CannotLoadImage(msg, abspath))
  in
  let infolst = imgheader.Images.header_infos in
  let widdots = imgheader.Images.header_width in
  let hgtdots = imgheader.Images.header_height in
(*
  Format.printf "ImageHashTable> length of info = %d width = %d, height = %d\n" (List.length infolst) widdots hgtdots;
*)
  let dpi =
    match Images.dpi infolst with
    | Some(dpi) -> dpi
    | None      -> 72.  (* -- default dots per inch -- *)
  in
  let colormodel =
    match
      infolst |> List.fold_left (fun opt info ->
        match opt with
        | Some(_) -> opt
        | None ->
            match info with
            | Images.Info_ColorModel(colormodel) -> Some(colormodel)
            | _                                  -> opt
      ) None
    with
    | None             -> Images.RGB  (* when no color model is specified; doubtful implementation *)
    | Some(colormodel) -> colormodel
  in
  let colorspace =
    match colormodel with
    | Images.Gray  -> Pdf.Name("/DeviceGray")
    | Images.RGB   -> Pdf.Name("/DeviceRGB")
    | Images.YCbCr -> Pdf.Name("/DeviceRGB")
    | Images.CMYK  -> Logging.warn_cmyk_image abspath; Pdf.Name("/DeviceCMYK")
    | _            -> raise (UnsupportedColorModel(colormodel, abspath))
  in
  let pdf_points_of_inches inch = 72. *. inch in
  let wid = pdf_points_of_inches ((float_of_int widdots) /. dpi) in
  let hgt = pdf_points_of_inches ((float_of_int hgtdots) /. dpi) in
  let bbox = (0., 0., wid, hgt) in
  let (key, tag) = generate_tag () in
  begin
    Hashtbl.add main_hash_table key (tag, bbox, OtherImage(imgfmt, colorspace, widdots, hgtdots, abspath));
    key
  end


let add_stub (abspath : abs_path) =
  let bbox = (0., 0., 100., 100.) in
  let (key, tag) = generate_tag () in
  begin
    Hashtbl.add main_hash_table key (tag, bbox, StubImage(abspath));
    key
  end


let find (key : key) : value =
  match Hashtbl.find_opt main_hash_table key with
  | None        -> assert false
  | Some(value) -> value


let fold (type a) (f : key -> value -> a -> a) (init : a) : a =
  Hashtbl.fold f main_hash_table init
