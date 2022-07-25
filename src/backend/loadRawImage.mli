(**
   A Generic image loader for PDF.
*)

open MyUtil

(**
   [make_xobject pdfmain colorspace widdots hgtdots abs_path] creates a PDF XObject Image
   for the file located at [abspath] with ColorSpace = [colorspace], Width = [widdots], and Height = [hgtdots],
   and then add it to the PDF document [pdfmain].
   [colorspace] is can be acquired by {!val:ImageHashTable.find}.
   [widdots] and [hgtdots] are the image width and height in dots.

   This function raises {!val:ImageHashTable.CannotLoadImage} if the images format is not supported
   or the image cannot be loaded due to any other reason.
*)
val make_xobject : Pdf.t -> Pdf.pdfobject -> int -> int -> abs_path -> Pdf.pdfobject
