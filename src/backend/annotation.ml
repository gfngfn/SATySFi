
open Length
open LengthInterface
open GraphicBase


type t =
  | Link of Action.t


let annot_acc = ref []


let register annot rect border coloropt =
  annot_acc := (annot, rect, border, coloropt) :: !annot_acc


let of_annotation = function
  | (Link(act), (x, y, wid, hgt, dpt), border, coloropt) ->
    let rect = (to_pdf_point x, to_pdf_point (y -% dpt), to_pdf_point (x +% wid), to_pdf_point (y +% hgt)) in
    let color =
      match coloropt with
      | Some(DeviceRGB(r, g, b)) -> Some((int_of_float r, int_of_float g, int_of_float b))
      | _                        -> None
    in
    let link = Pdfannot.make ~border:(Pdfannot.make_border (to_pdf_point border)) ~rectangle:rect Pdfannot.Link in
      { link with
          Pdfannot.annotrest =
            Pdf.Dictionary ["/A", (Action.pdfobject_of_action act)];
          Pdfannot.colour = color }


let add_to_pdf pdf page =
  let page = List.fold_left (fun page annotinfo ->
    Pdfannot.add_annotation pdf page (of_annotation annotinfo)) page !annot_acc in
  let () = annot_acc := [] in
    page

