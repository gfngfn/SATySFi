(* pdfdecomp a.pdf b.pdf decompresses all streams in a.pdf writing the result
to b.pdf. *)
match Array.to_list Sys.argv with
| [_; in_file; out_file] ->
    begin try
      let pdf = Pdfread.pdf_of_file None None in_file in
        Pdf.iter_stream (Pdfcodec.decode_pdfstream_until_unknown pdf) pdf;
        Pdfwrite.pdf_to_file pdf out_file
    with
      err ->
        Printf.printf "Failed to decompress file.\n%s\n\n" (Printexc.to_string err);
        exit 1
    end
| _ ->
    print_string "Syntax: pdfdecomp <input> <output>\n\n"; exit 1
