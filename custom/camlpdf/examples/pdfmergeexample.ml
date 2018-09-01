(* pdfmergeexample a.pdf b.pdf c.pdf appends b.pdf to a.pdf and writes to
c.pdf. This is written from scratch, rather than relying on the PdfMerge *)
open Pdfutil

(* We read all the files, read their pages and concatenate them, dealing with
clashing object numbers. We then build a new page tree, and build the output PDF
document, with a new root and trailer dictionary. We then remove any unreferenced
objects, and write to file. *)
let merge_pdfs pdfs out_name =
  let pdfs = Pdf.renumber_pdfs pdfs
  and minor' = fold_left max 0 (map (fun p -> p.Pdf.minor) pdfs) in
    let pages = flatten (map Pdfpage.pages_of_pagetree pdfs)
    and pdf = ref (Pdf.empty ()) in
      iter (Pdf.objiter (fun k v -> ignore (Pdf.addobj_given_num !pdf (k, v)))) pdfs;
      let pdf, pagetree_num = Pdfpage.add_pagetree pages !pdf in
        let pdf = Pdfpage.add_root pagetree_num [] pdf in
          let pdf = {pdf with Pdf.major = 1; Pdf.minor = minor'} in
            Pdf.remove_unreferenced pdf;
            pdf

(* Read command line arguments, read files, call merge_pdfs, write result. *)
let _ =
  let in_names, out_name =
    match rev (tl (Array.to_list Sys.argv)) with
    | h::t::t' -> rev (t::t'), h
    | _ -> print_string "Syntax: pdfmerge <inputs> <output>\n\n"; exit 1
  in
    try
      let pdfs = map (Pdfread.pdf_of_file None None) in_names in
        let result = merge_pdfs pdfs out_name in
          Pdfwrite.pdf_to_file result out_name
    with
      err ->
        Printf.printf "Failed to merge files.\n%s\n\n" (Printexc.to_string err);
        exit 1

