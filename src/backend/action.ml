
type t =
  | Goto of Pdfdest.t
  | GotoName of string
  | Uri  of string


let pdfobject_of_action = function
  | Goto(dest) ->
    let destobj = Pdfdest.pdfobject_of_destination dest in
      Pdf.Dictionary ["/Type", Pdf.Name "/Action"; "/S", Pdf.Name "/GoTo"; "/D", destobj]
  | GotoName(destname) ->
      Pdf.Dictionary ["/Type", Pdf.Name "/Action"; "/S", Pdf.Name "/GoTo"; "/D", Pdf.Name ("/" ^ destname)]
  | Uri(uri) ->
    Pdf.Dictionary ["/Type", Pdf.Name "/Action"; "/S", Pdf.Name "/URI"; "/URI", Pdf.String uri]


