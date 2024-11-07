
type bbox = float * float * float * float

exception PdfBroken


module IRSet = MutableSet.Make
  (struct
    type t = int
    let equal = (=)
    let hash = Hashtbl.hash
  end)


let collect_direct_objects (pdfmain : Pdf.t) (pdfext : Pdf.t) (pdfobj : Pdf.pdfobject) : Pdf.pdfobject =

  let irset = IRSet.create 32 in

  let rec aux pdfobj =
    match pdfobj with
    | Pdf.Dictionary(keyval) -> 
        let keyvalnew =
          keyval |> List.map (fun (key, pdfobjsub) -> (key, aux pdfobjsub))
        in
          Pdf.Dictionary(keyvalnew)

    | Pdf.Indirect(ir) ->
        begin
          IRSet.add irset ir;
          try
            let pdfobjdirect = aux (Pdf.lookup_obj pdfext ir) in
            let irnew = Pdf.addobj pdfmain pdfobjdirect in
              Pdf.Indirect(irnew)
          with
          | Not_found -> raise PdfBroken
        end

    | Pdf.Array(lst) ->
        Pdf.Array(lst |> List.map aux)

    | Pdf.Stream({contents = (pdfdict, stream)} as streamref) ->
        begin
          match pdfdict with
          | Pdf.Dictionary(keyval) ->
              let keyvalnew =
                keyval |> List.map (fun (key, pdfobjsub) -> (key, aux pdfobjsub))
              in
              begin
                streamref := (Pdf.Dictionary(keyvalnew), stream);
                pdfobj
              end

          | _ -> raise PdfBroken
        end

    | _ -> pdfobj
  in
    aux pdfobj


let xobject_of_page (pdfmain : Pdf.t) (pdfext : Pdf.t) (pageext : Pdfpage.t) : Pdf.pdfobject =
  let content = pageext.Pdfpage.content in
  let pdfdict_resources = collect_direct_objects pdfmain pdfext pageext.Pdfpage.resources in
  let pdfarr_rect = pageext.Pdfpage.mediabox in
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
  let ops : Pdfops.t list = Pdfops.parse_operators pdfext pdfdict_resources content in
  let retobj : Pdf.pdfobject = Pdfops.stream_of_ops ops in
      (* -- 'Pdfops.stream_of_ops ops' returns a stream
            with a dictionary containing only a /Length entry -- *)
    match retobj with
    | Pdf.Stream({contents = (Pdf.Dictionary(entrieslen), stream)} as streamref) ->
        begin
          streamref := (Pdf.Dictionary(List.append entriesadded entrieslen), stream);
          let ir = Pdf.addobj pdfmain retobj in
          Pdf.Indirect(ir)
        end

    | _ -> assert false


let make_xobject (pdfmain : Pdf.t) (pdfext : Pdf.t) (pageext : Pdfpage.t) : Pdf.pdfobject =
  let pdfstream = xobject_of_page pdfmain pdfext pageext in
  pdfstream


let get_page (pdfext : Pdf.t) (pageno : int) : (bbox * Pdfpage.t) option =
  let pagelst = Pdfpage.pages_of_pagetree pdfext in
    match List.nth_opt pagelst pageno with
    | None ->
        None

    | Some(page) ->
        let bbox =
          match page.Pdfpage.mediabox with
          | Pdf.Array[pdfxmin; pdfymin; pdfxmax; pdfymax] ->
              begin
                try
                  let xmin = Pdf.getnum pdfxmin in
                  let ymin = Pdf.getnum pdfymin in
                  let xmax = Pdf.getnum pdfxmax in
                  let ymax = Pdf.getnum pdfymax in
                    (xmin, ymin, xmax, ymax)
                with
                | Pdf.PDFError(_) -> raise PdfBroken
              end

          | _ -> raise PdfBroken
        in
          Some((bbox, page))
