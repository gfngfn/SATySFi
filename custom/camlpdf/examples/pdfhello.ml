(* We build a font dictionary for one of the 14 standard PostScript fonts,
(which are supported by all PDF readers), make a graphics stream using the
Pdfops module, build a PDF document in memory and then write it to hello.pdf *)

let font =
  Pdf.Dictionary
    [("/Type", Pdf.Name "/Font");
     ("/Subtype", Pdf.Name "/Type1");
     ("/BaseFont", Pdf.Name "/Times-Italic")]
and ops =
  [Pdfops.Op_cm (Pdftransform.matrix_of_transform [Pdftransform.Translate (50., 770.)]);
   Pdfops.Op_BT;
   Pdfops.Op_Tf ("/F0", 36.);
   Pdfops.Op_Tj "Hello, World!";
   Pdfops.Op_ET]
in
  let page =
    {(Pdfpage.blankpage Pdfpaper.a4) with
        Pdfpage.content = [Pdfops.stream_of_ops ops];
        Pdfpage.resources = Pdf.Dictionary [("/Font", Pdf.Dictionary [("/F0", font)])]}
  in
    let pdf, pageroot = Pdfpage.add_pagetree [page] (Pdf.empty ()) in
      let pdf = Pdfpage.add_root pageroot [] pdf in
        Pdfwrite.pdf_to_file pdf "hello.pdf"

