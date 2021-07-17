type document_information_dictionary = {
  title : string option;
  subject : string option;
  author : string option;
  keywords : string list;
}

val add_to_pdf : Pdf.t -> Pdf.t

val register : document_information_dictionary -> unit
