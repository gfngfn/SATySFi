
type t =
  | Goto of Pdfdest.t
  | Uri  of string


let pdfobject_of_action = function
  | Goto(dest) ->
    let destobj = Pdfdest.pdfobject_of_destination dest in
      Pdf.Dictionary ["/S", Pdf.String "GoTo"; "/D", destobj]
  | Uri(uri) ->
    Pdf.Dictionary ["/S", Pdf.String "URI"; "/URI", Pdf.String uri]


