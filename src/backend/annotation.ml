
open Length
open LengthInterface
open GraphicBase


type t =
  | Link of Action.t


let annot_acc = ref Alist.empty


let register annot rect borderopt =
  if State.during_page_break () then
    annot_acc := Alist.extend !annot_acc (annot, rect, borderopt)
  else
    raise State.NotDuringPageBreak


let of_annotation (Link(act), ((x, y), wid, hgt, dpt), borderopt) =
  let rect = (to_pdf_point x, to_pdf_point (y -% dpt), to_pdf_point (x +% wid), to_pdf_point (y +% hgt)) in
  let (border, coloropt) =
    match borderopt with
    | Some(l, color) ->
        begin
          match color with
          | DeviceRGB(r, g, b) ->
              (l, Some((int_of_float r, int_of_float g, int_of_float b)))

          | _ ->
              Format.printf "! [Warning] border color other than RGB; ignored\n";
                (* temporary; should warn in a more sophisticated manner *)
              (Length.zero, None)
        end

    | _ ->
        (Length.zero, None)
  in
  let link =
    Pdfannot.make
      ~border:(Pdfannot.make_border (to_pdf_point border))
      ~rectangle:rect
      Pdfannot.Link
  in
  let pdfobj_annotrest =
    Pdf.Dictionary[("/A", Action.pdfobject_of_action act)]
  in
  { link with
    Pdfannot.annotrest = pdfobj_annotrest;
    Pdfannot.colour    = coloropt;
  }


let add_to_pdf pdf page =
  let page =
    !annot_acc |> Alist.to_list |> List.fold_left (fun page annotinfo ->
      Pdfannot.add_annotation pdf page (of_annotation annotinfo)
    ) page
  in
  annot_acc := Alist.empty;
  page
