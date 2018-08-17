
type t =
  | Goto of Pdfdest.t
  | Uri  of string


let pdfobject_of_action = function
  | Goto(dest) ->
    let destobj = Pdfdest.pdfobject_of_destination dest in
      Pdf.Dictionary ["/Type", Pdf.Name "/Action"; "/S", Pdf.Name "/GoTo"; "/D", destobj]
  | Uri(uri) ->
    Pdf.Dictionary ["/Type", Pdf.Name "/Action"; "/S", Pdf.Name "/URI"; "/URI", Pdf.String uri]


