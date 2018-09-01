(* Make a PDF suitable for draft printing by replacing its images by crossed
boxes. Usage: pdfdraft input.pdf output.pdf *)
open Pdfutil

(* Predicate on an xobject: true if an image xobject. *)
let isimage pdf (_, xobj) =
  Pdf.lookup_direct pdf "/Subtype" xobj = Some (Pdf.Name "/Image")

(* Given a set of resources for a page, and the name of a resource, determine if
that name refers to an image xobject. *)
let xobject_isimage pdf resources name =
  match resources with
  | Pdf.Dictionary _ ->
      begin match Pdf.lookup_direct pdf "/XObject" resources with
      | Some xobjects ->
          isimage pdf ("", Pdf.lookup_fail "xobject not there" pdf name xobjects)
      | _ -> false
      end
  | _ -> failwith "bad resources"

(* Remove any image xobjects from a set of resources. *)
let remove_image_xobjects pdf resources =
  match resources with
  | Pdf.Dictionary res ->
      begin match Pdf.lookup_direct pdf "/XObject" resources with
      | Some (Pdf.Dictionary xobjects) ->
          Pdf.Dictionary
            (replace "/XObject" (Pdf.Dictionary (lose (isimage pdf) xobjects)) res)
      | _ -> resources
      end
  | _ -> failwith "bad resources"

(* The subsitute for an image. *)
let substitute =
  rev
    [Pdfops.Op_q;
     Pdfops.Op_w 0.;
     Pdfops.Op_G 0.;
     Pdfops.Op_re (0., 0., 1., 1.);
     Pdfops.Op_m (0., 0.);
     Pdfops.Op_l (1., 1.);
     Pdfops.Op_m (0., 1.);
     Pdfops.Op_l (1., 0.);
     Pdfops.Op_S;
     Pdfops.Op_Q]

(* Remove references to images from a graphics stream. *)
let rec remove_images_stream pdf resources prev = function
  | [] -> rev prev
  | (Pdfops.Op_Do name) as h::t ->
      if xobject_isimage pdf resources name
        then remove_images_stream pdf resources (substitute @ prev) t
        else remove_images_stream pdf resources (h::prev) t
  | Pdfops.InlineImage _::t ->
      remove_images_stream pdf resources (substitute @ prev) t
  | h::t ->
      remove_images_stream pdf resources (h::prev) t

(* Remove images from a page. *)
let remove_images_page pdf page =
  let content' =
    remove_images_stream pdf page.Pdfpage.resources []
       (Pdfops.parse_operators pdf page.Pdfpage.resources page.Pdfpage.content)
  in
    {page with
      Pdfpage.content =
        (let stream = Pdfops.stream_of_ops content' in
          Pdfcodec.encode_pdfstream pdf Pdfcodec.Flate stream;
          [stream]);
      Pdfpage.resources =
        remove_image_xobjects pdf page.Pdfpage.resources}

(* Remove images from all pages in a document. *)
let remove_images pdf =
  let pages = Pdfpage.pages_of_pagetree pdf in
    let pages' = map (remove_images_page pdf) pages in
      let pdf, pagetree_num = Pdfpage.add_pagetree pages' pdf in
        let pdf = Pdfpage.add_root pagetree_num [] pdf in
          Pdf.remove_unreferenced pdf;
          pdf

(* Read command line arguments and call [remove_images] *)
let _ =
  match Array.to_list Sys.argv with
  | [_; in_file; out_file] ->
      begin try
        let ch = open_in_bin in_file in
          let pdf = Pdfread.pdf_of_channel None None ch in
            Pdfwrite.pdf_to_file (remove_images pdf) out_file;
            close_in ch
      with
        err ->
          Printf.printf "Failed to produce output.\n%s\n\n" (Printexc.to_string err);
          exit 1
      end
  | _ ->
      print_string "Syntax: pdfdraft <input> <output>\n\n"; exit 1

