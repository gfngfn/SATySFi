
open MyUtil
open LengthInterface

type key = ImageHashTable.key

type bbox = float * float * float * float


let initialize () =
  ImageHashTable.initialize ()


let add_pdf abspath pageno =
  ImageHashTable.add_pdf abspath pageno


let add_image abspath =
  ImageHashTable.add_image abspath


let get_xobject_dictionary pdfmain : Pdf.pdfobject =
  let keyval =
    [] |> ImageHashTable.fold (fun _ (tag, _bbox, imgvalue) acc ->
      match imgvalue with
      | ImageHashTable.PDFImage(pdfext, page) ->
          let irxobj = LoadPdf.make_xobject pdfmain pdfext page in
            (tag, irxobj) :: acc

      | ImageHashTable.OtherImage(imgfmt, colorspace, widdots, hgtdots, abspath) ->
          begin
            match imgfmt with
            | Images.Jpeg ->
                let irxobj = LoadJpeg.make_xobject pdfmain colorspace widdots hgtdots (get_abs_path_string abspath) in
                (tag, irxobj) :: acc

            | Images.Png ->
                let irxobj = LoadRawImage.make_xobject pdfmain colorspace widdots hgtdots abspath in
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
  let (_,bbox, valuemain) = ImageHashTable.find key in
    match valuemain with
    | ImageHashTable.PDFImage(_, _) ->
        let (xmin, ymin, xmax, ymax) = bbox in
        let xratio = wid /% (Length.of_pdf_point (xmax -. xmin)) in
        let yratio = hgt /% (Length.of_pdf_point (ymax -. ymin)) in
          (xratio, yratio)

    | ImageHashTable.OtherImage(_, _, _, _, _) ->
        let xratio = Length.to_pdf_point wid in
        let yratio = Length.to_pdf_point hgt in
          (xratio, yratio)


let get_tag key =
  let (tag, _, _) = ImageHashTable.find key in tag


let get_color_space key =
  let (_, _, valuemain) = ImageHashTable.find key in
  match valuemain with
  | ImageHashTable.PDFImage(_, _)                     -> None
  | ImageHashTable.OtherImage(_, colorspace, _, _, _) -> Some(colorspace)
