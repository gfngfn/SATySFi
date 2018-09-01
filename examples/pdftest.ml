(* pdftest in.pdf out.pdf reads, lexes, parses a document in.pdf and its
graphics streams, then writes it to out.pdf.  *)
open Pdfutil

let _ =
  let in_name, out_name =
    match tl (Array.to_list Sys.argv) with
    | [i; o] -> (i, o)
    | _ -> print_string "Syntax: pdftest <input> <output>\n\n"; exit 1
  in
    try
      let pdf = Pdfread.pdf_of_file None None in_name in
        if Pdfcrypt.is_encrypted pdf then failwith "File is encrypted.";
        let pages = Pdfpage.pages_of_pagetree pdf in
          let pages' = 
            map 
              (fun page ->
                let ops = Pdfops.parse_operators pdf page.Pdfpage.resources page.Pdfpage.content in
                  {page with Pdfpage.content = [Pdfops.stream_of_ops ops]})
              pages
          in
            let pdf = Pdfpage.change_pages true pdf pages' in
              Pdf.remove_unreferenced pdf;
              Pdfwrite.pdf_to_file pdf out_name
    with
      err ->
        Printf.printf "Test failed:\n%s\n\n" (Printexc.to_string err);
        exit 1

