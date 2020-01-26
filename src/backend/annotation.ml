
open Length
open LengthInterface
open GraphicBase


type text = {
  contents : string;
  is_open  : bool;
  kind     : string;
}

type t =
  | Link of Pdfaction.t
  | Text of text


let annot_acc = ref Alist.empty


let register annot rect borderopt =
  if State.during_page_break () then
    annot_acc := Alist.extend !annot_acc (annot, rect, borderopt)
  else
    raise State.NotDuringPageBreak


let of_annotation (annot, ((x, y), wid, hgt, dpt), borderopt) =
  let rect = (to_pdf_point x, to_pdf_point (y -% dpt), to_pdf_point (x +% wid), to_pdf_point (y +% hgt)) in
  let (border, coloropt) =
    match borderopt with
    | Some(l, color) ->
        begin
          match color with
          | DeviceGray(gray)       -> (l, Some(Pdfannot.DeviceGray(gray)))
          | DeviceRGB(r, g, b)     -> (l, Some(Pdfannot.DeviceRGB(r, g, b)))
          | DeviceCMYK(c, m, y, k) -> (l, Some(Pdfannot.DeviceCMYK(c, m, y, k)))
        end

    | _ ->
        (Length.zero, None)
  in
  match annot with
  | Link(act) ->
      let link =
        Pdfannot.make
          ~border:(Pdfannot.make_border (to_pdf_point border))
          ~rectangle:rect
          Pdfannot.Link
      in
      let pdfobj_annotrest =
        Pdf.Dictionary[("/A", Pdfaction.pdfobject_of_action act)]
      in
      { link with
        Pdfannot.annotrest = pdfobj_annotrest;
        Pdfannot.colour    = coloropt;
      }

  | Text(text) ->
      let annot_text =
        Pdfannot.make
          ~border:(Pdfannot.make_border (to_pdf_point border))
          ~rectangle:rect
          ~content:text.contents
          Pdfannot.Text
      in
      Pdfannot.{ annot_text with
        colour = coloropt;
        annotrest = Pdf.Dictionary[
          ("/CA"  , Pdf.Real(0.5));
            (* -- opacity in closed state [PDF 1.7, Table 8.21] -- *)
          ("/Open", Pdf.Boolean(text.is_open));
            (* -- whether it is open by default [PDF 1.7, Table 8.23] -- *)
          ("/Name", Pdf.String(text.kind));
            (* -- kind of text annotation (e.g. "Note") [PDF 1.7, Table 8.23] -- *)
        ];
      }

let add_to_pdf pdf page =
  let page =
    !annot_acc |> Alist.to_list |> List.fold_left (fun page annotinfo ->
      Pdfannot.add_annotation pdf page (of_annotation annotinfo)
    ) page
  in
  annot_acc := Alist.empty;
  page
