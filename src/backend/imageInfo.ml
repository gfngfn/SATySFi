
type file_path = string

type tag = string

type xobject = Pdf.pdfobject

type bbox = float * float * float * float

type value_main =
  | PDFImage   of Pdf.t * Pdfpage.t
  | OtherImage of Images.format * file_path

type value = tag * bbox * value_main

exception CannotLoadPdf   of file_path * int
exception CannotLoadImage of file_path

module ImageHashTable
: sig
    type key
    val initialize : unit -> unit
    val add_pdf : file_path -> int -> key
    val find : key -> value
    val fold : (key -> value -> 'a -> 'a) -> 'a -> 'a
  end
= struct

    type key = int

    let main_hash_table : (key, value) Hashtbl.t = Hashtbl.create 32

    let current_id_ref : int ref = ref 0

    let initialize () =
      begin
        current_id_ref := 0;
        Hashtbl.clear main_hash_table;
      end

    let generate_tag () : key * tag =
      let n = !current_id_ref in
      begin
        incr current_id_ref;
        (n, "/I" ^ (string_of_int n))
      end

    let add_pdf (srcpath : file_path) (pageno : int) =
      let pdfext =
        try Pdfread.pdf_of_file None None srcpath with
        | Pdf.PDFError(_) -> raise (CannotLoadPdf(srcpath, pageno))
      in
        match LoadPdf.get_page pdfext (pageno - 1) with
        | None               -> raise (CannotLoadPdf(srcpath, pageno))
        | Some((bbox, page)) ->
            let (key, tag) = generate_tag () in
            begin
              Hashtbl.add main_hash_table key (tag, bbox, PDFImage(pdfext, page));
              key
            end

    let add_image (srcpath : file_path) =
      let (imgfmt, imgheader) =
        try Images.file_format srcpath with
        | Images.Wrong_file_type -> raise (CannotLoadImage(srcpath))
      in
      let infolst = imgheader.Images.header_infos in
      let rawwid = imgheader.Images.header_width in
      let rawhgt = imgheader.Images.header_height in
      let dpi =
        match Images.dpi infolst with
        | Some(dpi) -> dpi
        | None      -> 72.  (* -- default dots per inch -- *)
      in
      let pdf_points_of_inches inch = 72. *. inch in
      let wid = pdf_points_of_inches ((float_of_int rawwid) /. dpi) in
      let hgt = pdf_points_of_inches ((float_of_int rawhgt) /. dpi) in
      let bbox = (0., 0., wid, hgt) in
      let (key, tag) = generate_tag () in
      begin
        Hashtbl.add main_hash_table key (tag, bbox, OtherImage(imgfmt, srcpath));
        key
      end

    let find (key : key) : value =
      match Hashtbl.find_opt main_hash_table key with
      | None        -> assert false
      | Some(value) -> value

    let fold (type a) (f : key -> value -> a -> a) (init : a) : a =
      Hashtbl.fold f main_hash_table init
  end


type key = ImageHashTable.key


let initialize () =
  ImageHashTable.initialize ()


let add_pdf srcpath pageno =
  ImageHashTable.add_pdf srcpath pageno


let get_xobject_dictionary pdfmain : Pdf.pdfobject =
  let keyval =
    [] |> ImageHashTable.fold (fun _ (tag, bbox, imgvalue) acc ->
      match imgvalue with
      | PDFImage(pdfext, page) ->
          let irxobj = LoadPdf.make_xobject pdfmain pdfext page in
            (tag, irxobj) :: acc

      | OtherImage(imgfmt, srcpath) ->
          acc  (* temporary *)
    ) |> List.rev
  in
    Pdf.Dictionary(keyval)


let get_bounding_box key =
  let (_,bbox, _) = ImageHashTable.find key in bbox


let get_tag key =
  let (tag, _, _) = ImageHashTable.find key in tag
