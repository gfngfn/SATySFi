
open Length
open LengthInterface
open GraphicBase


let ( ~% ) = Length.to_pdf_point


type t =
  | Link of Action.t
  | Screen of Action.t


let annot_acc = ref Alist.empty


let register annot rect border coloropt =
  annot_acc := Alist.extend !annot_acc (annot, rect, border, coloropt)


(* 'make_appearance_dictionary': returns an appearance stream  *)
let make_appearance_dictionary pdf wid hgt dptnonneg =
  let widpt = ~% wid in
  let hgtpt = ~% (hgt +% dptnonneg) in
  let entriesadded =
    [
      ("/Type"     , Pdf.Name("/XObject"));
      ("/Subtype"  , Pdf.Name("/Form"));
      ("/FormType" , Pdf.Integer(1));
      ("/BBox"     , Pdf.Array[Pdf.Real(0.); Pdf.Real(0.); Pdf.Real(widpt); Pdf.Real(hgtpt)]);
      ("/Resources", Pdf.Dictionary[]);
    ]
  in
  let ops =
    (* -- appearance for the standby state -- *)
    [
      Pdfops.Op_q;
      Pdfops.Op_CS("/DeviceGray");
      Pdfops.Op_g(0.);
      Pdfops.Op_G(0.25);
      Pdfops.Op_w(2.);
      Pdfops.Op_re(0., 0., widpt, hgtpt);
      Pdfops.Op_S;
      Pdfops.Op_m(widpt *. 0.25, hgtpt *. 0.75);
      Pdfops.Op_l(widpt *. 0.75, hgtpt *. 0.5);
      Pdfops.Op_l(widpt *. 0.25, hgtpt *. 0.25);
      Pdfops.Op_h;
      Pdfops.Op_f;
      Pdfops.Op_Q;
    ]  (* temporary; should be able to be specified by user *)
  in
  let retobj : Pdf.pdfobject = Pdfops.stream_of_ops ops in
      (* -- 'Pdfops.stream_of_ops ops' returns a stream
            with a dictionary containing only a /Length entry -- *)
    match retobj with
    | Pdf.Stream({contents = (Pdf.Dictionary(entrieslen), stream)} as streamref) ->
        streamref := (Pdf.Dictionary(List.append entriesadded entrieslen), stream);
        let ir = Pdf.addobj pdf retobj in
        Pdf.Indirect(ir)

    | _ ->
        assert false


let of_annotation pdf (annot, ((x, y), wid, hgt, dptnonneg), border, coloropt) =
  let rect = (~% x, ~% (y -% dptnonneg), ~% (x +% wid), ~% (y +% hgt)) in
  let color =
    match coloropt with
    | Some(DeviceRGB(r, g, b)) -> Some((int_of_float r, int_of_float g, int_of_float b))
    | _                        -> None
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
        Pdf.Dictionary[("/A", Action.pdfobject_of_action pdf act)]
      in
      { link with
        Pdfannot.annotrest = pdfobj_annotrest;
        Pdfannot.colour    = color;
      }

  | Screen(act) ->
      let screen =
        Pdfannot.make
          ~border:(Pdfannot.make_border (to_pdf_point border))
          ~rectangle:rect
          Pdfannot.Screen
      in
      let pdfobj_annotrest =
        Pdf.Dictionary[
          ("/A", Action.pdfobject_of_action pdf act);
          ("/AP", make_appearance_dictionary pdf wid hgt dptnonneg);
        ]
      in
      { screen with
        Pdfannot.annotrest = pdfobj_annotrest;
        Pdfannot.colour    = color;
      }


let add_to_pdf pdf page =
  let page =
    !annot_acc |> Alist.to_list |> List.fold_left (fun page annotinfo ->
      Pdfannot.add_annotation pdf page (of_annotation pdf annotinfo)
    ) page
  in
  annot_acc := Alist.empty;
  page
