
open LengthInterface


type file_path = string

type key = ImageHashTable.key

type bbox = float * float * float * float


let initialize () =
  ImageHashTable.initialize ()


let add_pdf srcpath pageno =
  ImageHashTable.add_pdf srcpath pageno


let add_image srcpath =
  ImageHashTable.add_image srcpath


let get_xobject_dictionary pdfmain : Pdf.pdfobject =
  let keyval =
    [] |> ImageHashTable.fold (fun _ (tag, bbox, imgvalue) acc ->
      match imgvalue with
      | ImageHashTable.PDFImage(pdfext, page) ->
          let irxobj = LoadPdf.make_xobject pdfmain pdfext page in
            (tag, irxobj) :: acc

      | ImageHashTable.OtherImage(imgfmt, colorspace, widdots, hgtdots, srcpath) ->
          begin
            match imgfmt with
            | Images.Jpeg ->
                let irxobj = LoadJpeg.make_xobject pdfmain colorspace widdots hgtdots srcpath in
                (tag, irxobj) :: acc

            | _ -> acc  (* temporary *)
          end
    ) |> List.rev
  in
    Pdf.Dictionary(keyval)


let get_height_from_width key wid =
  let (_,bbox, _) = ImageHashTable.find key in
  let (xmin, ymin, xmax, ymax) = bbox in
    wid *% ((ymax -. ymin) /. (xmax -. xmin))


let get_ratio key wid hgt =
  let (_,bbox, _) = ImageHashTable.find key in
  let (xmin, ymin, xmax, ymax) = bbox in
  let xratio = wid /% (Length.of_pdf_point (xmax -. xmin)) in
  let yratio = hgt /% (Length.of_pdf_point (ymax -. ymin)) in
    (xratio, yratio)


let get_tag key =
  let (tag, _, _) = ImageHashTable.find key in tag


let get_color_space key =
  let (_, _, valuemain) = ImageHashTable.find key in
  match valuemain with
  | ImageHashTable.PDFImage(_, _)                     -> None
  | ImageHashTable.OtherImage(_, colorspace, _, _, _) -> Some(colorspace)
