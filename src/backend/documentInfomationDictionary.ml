type document_infomation_dictionary = {
  title : string option;
  subject : string option;
  author : string option;
  keywords : string list;
}

let registered_document_infomation_dictionary : document_infomation_dictionary ref =
  ref {
    title = None;
    subject = None;
    author = None;
    keywords = [];
  }


let register dictionary =
  registered_document_infomation_dictionary := dictionary


let add_to_pdf pdf =
  let document_infomation_dictionary = !registered_document_infomation_dictionary in
  let title =
    match document_infomation_dictionary.title with
    | None -> []
    | Some(title) -> [("/Title", Pdf.String title)]
  in
  let subject =
    match document_infomation_dictionary.subject with
    | None -> []
    | Some(subject) -> [("/Subject", Pdf.String subject)]
  in
  let author =
    match document_infomation_dictionary.author with
    | None -> []
    | Some(author) -> [("/Author", Pdf.String author)]
  in
  let keywords =
    let lst = document_infomation_dictionary.keywords in
    let rec sub lst str =
      match lst with
      | [] -> str
      | [s] -> str^s
      | s::xs -> sub xs (str^s^" ")
    in
    [("/Keywords", Pdf.String (sub lst ""))]
  in
  let creator =
    [
      ("/Creator", Pdf.String "SATySFi");
      ("/Producer", Pdf.String "SATySFi")
    ]
  in
  let infomation =
    ("/Info", Pdf.Dictionary (title @ subject @ author @ keywords @ creator))
  in
  let dict =
    match pdf.Pdf.trailerdict with
    | Dictionary(lst) -> Pdf.Dictionary(infomation::lst)
    | d -> d
  in
  {pdf with Pdf.trailerdict = dict}

