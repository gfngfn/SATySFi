
type bbox = float * float * float * float

exception PdfBroken


let collect_direct_objects (pdf : Pdf.t) (pdfdict : Pdf.pdfobject) : Pdf.pdfobject =
  match pdfdict with
  | Pdf.Dictionary(keyval) -> 
      let keyvalnew =
        keyval |> List.map (fun pair ->
          let (key, value) = pair in
            match value with
            | Pdf.Indirect(ir) ->
                begin
                  try
                    let direct = Pdf.lookup_obj pdf ir in
                      (key, direct)
                  with
                  | Not_found -> raise PdfBroken
                end

            | _ -> pair
        )
      in
        Pdf.Dictionary(keyvalnew)

  | _ -> raise PdfBroken


let xobject_of_page (pdf : Pdf.t) (page : Pdfpage.t) : bbox * Pdf.pdfobject =
  let content = page.Pdfpage.content in
  let pdfdict_resources = collect_direct_objects pdf page.Pdfpage.resources in
  let (bbox, pdfarr_rect) =
    let mediabox = page.Pdfpage.mediabox in
    match mediabox with
    | Pdf.Array[pdfxmin; pdfymin; pdfxmax; pdfymax] ->
        begin
          try
            let xmin = Pdf.getnum pdfxmin in
            let ymin = Pdf.getnum pdfymin in
            let xmax = Pdf.getnum pdfxmax in
            let ymax = Pdf.getnum pdfymax in
              ((xmin, ymin, xmax, ymax), mediabox)
          with
          | Pdf.PDFError(_) -> raise PdfBroken
        end

    | _ -> raise PdfBroken
  in
    (* -- uses only /MediaBox entry -- *)
  let entriesadded =
    [
      ("/Type"     , Pdf.Name("/XObject"));
      ("/Subtype"  , Pdf.Name("/Form"));
      ("/FormType" , Pdf.Integer(1));
      ("/BBox"     , pdfarr_rect);
      ("/Resources", pdfdict_resources);
    ]
  in
  let ops : Pdfops.t list = Pdfops.parse_operators pdf pdfdict_resources content in
  let retobj : Pdf.pdfobject = Pdfops.stream_of_ops ops in
      (* -- 'Pdfops.stream_of_ops ops' returns a stream
            with a dictionary containing only a /Length entry -- *)
    match retobj with
    | Pdf.Stream({contents = (Pdf.Dictionary(entrieslen), stream)} as streamref) ->
        begin
          streamref := (Pdf.Dictionary(List.append entriesadded entrieslen), stream);
          (bbox, retobj)
        end

    | _ -> assert false


let make_xobject (pdf : Pdf.t) (pageno : int) : (bbox * Pdf.pdfobject) option =
  let pagelst = Pdfpage.pages_of_pagetree pdf in
    match List.nth_opt pagelst pageno with
    | None ->
        None

    | Some(page) ->
        let (bbox, pdfstream) = xobject_of_page pdf page in
        Some((bbox, pdfstream))
