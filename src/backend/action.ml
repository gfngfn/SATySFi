
type t =
  | Goto      of Pdfdest.t
  | GotoName  of string
  | Uri       of string
  | Rendition of Rendition.t


let pdfobject_of_action pdf = function
  | Goto(dest) ->
      let destobj = Pdfdest.pdfobject_of_destination dest in
      Pdf.Dictionary[
        ("/Type", Pdf.Name("/Action"));
        ("/S"   , Pdf.Name("/GoTo"));
        ("/D"   , destobj);
      ]

  | GotoName(destname) ->
      Pdf.Dictionary[
        ("/Type", Pdf.Name("/Action"));
        ("/S"   , Pdf.Name("/GoTo"));
        ("/D"   , Pdf.Name("/" ^ destname));
      ]

  | Uri(uri) ->
      Pdf.Dictionary[
        ("/Type", Pdf.Name("/Action"));
        ("/S"   , Pdf.Name("/URI"));
        ("/URI" , Pdf.String(uri));
      ]

  | Rendition(r) ->
      let pdfobj = Rendition.pdfobject_of_rendition pdf r in
      Pdf.Dictionary[
        ("/Type", Pdf.Name("/Action"));
        ("/S"   , Pdf.Name("/Rendition"));
        ("/R"   , pdfobj);
      ]
