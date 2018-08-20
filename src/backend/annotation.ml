
open Length
open LengthInterface


type t =
  | Link of Action.t * length * length * length * length * length


let annot_acc = ref []


let register_annotation annot =
  annot_acc := annot :: !annot_acc


let of_annotation = function
  | Link(act, x, y, wid, hgt, dpt) ->
    let rect = (to_pdf_point x, to_pdf_point (y -% dpt), to_pdf_point (x +% wid), to_pdf_point (y +% hgt)) in
    let link = Pdfannot.make ~border:(Pdfannot.make_border 0.5) ~rectangle:rect Pdfannot.Link in
      { link with Pdfannot.annotrest =
            Pdf.Dictionary ["/A", (Action.pdfobject_of_action act)] }


let add_annotations pdf page =
  let page = List.fold_left (fun page annot ->
    Pdfannot.add_annotation pdf page (of_annotation annot)) page !annot_acc in
  let () = annot_acc := [] in
    page



