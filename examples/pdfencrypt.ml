(* Encrypt a PDF file. Syntax is pdfencrypt in.pdf out.pdf ownerpassword. The
user password is blank. The permissions applied are NoEdit and NoPrint *)
open Pdfutil

(* Read command line arguments and call [decrypt_pdf] *)
let _ =
  let pw, in_file, out_file =
    match Array.to_list Sys.argv with
    | [_; in_file; out_file; pw] -> pw, in_file, out_file
    | _ ->
      print_string "Syntax: pdfencrypt <input> <output> <owner-password>\n\n"; exit 1
  in
    begin try
      let pdf = Pdfread.pdf_of_file None None in_file
      and encryption =
        {Pdfwrite.encryption_method = Pdfwrite.AES128bit false;
         Pdfwrite.owner_password = pw;
         Pdfwrite.user_password = "";
         Pdfwrite.permissions = [Pdfcrypt.NoEdit; Pdfcrypt.NoPrint]}
      in
        Pdfwrite.pdf_to_file_options false (Some encryption) false pdf out_file
    with
      err ->
        Printf.printf "Failed to produce output.\n%s\n\n" (Printexc.to_string err);
        exit 1
    end

