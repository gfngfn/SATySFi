
type link =
  | Goto      of Pdfdest.t
  | GotoName  of string
  | Uri       of string

type screen =
  | Rendition of int * Rendition.t


(* -- 'pdfobject_of_action': returns an action dictionary [Section 8.5, Table 8.43] --*)
let pdfobject_of_link_action = function
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


let pdfobject_of_screen_action pdf irannot = function
  | Rendition(opnum, r) ->
      let pdfobj = Rendition.pdfobject_of_rendition pdf r in
      Pdf.Dictionary[
        ("/Type", Pdf.Name("/Action"));
        ("/S"   , Pdf.Name("/Rendition"));
        ("/R"   , pdfobj);
        ("/AN"  , Pdf.Indirect(irannot));
        ("/OP"  , Pdf.Integer(opnum));
      ]
        (* -- Rendition action [Table 8.43, Table 8.64] -- *)
