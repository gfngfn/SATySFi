
exception PdfBroken


let xobject_of_page (pdf : Pdf.t) (page : Pdfpage.t) : Pdf.pdfobject =
  let content = page.Pdfpage.content in
  let pdfdict_resources =
    match page.Pdfpage.resources with
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
  in
  let pdfarr_rect = page.Pdfpage.mediabox in
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
          retobj
        end

    | _ -> assert false


let make_xobject (pdf : Pdf.t) (pageno : int) : Pdf.pdfobject option =
  let pagelst = Pdfpage.pages_of_pagetree pdf in
    match List.nth_opt pagelst pageno with
    | None ->
        None

    | Some(page) ->
        let pdfstream = xobject_of_page pdf page in
        Some(pdfstream)
