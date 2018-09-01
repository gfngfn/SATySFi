(* Convert an Type 0 composite font containing CFF / Type 1 / Truetype
descendant font into a Type 3 font. The resultant font is for internal use only
and is not a valid PDF font. Only deals with Identity-H at the moment. A full
treatment of composite fonts will require more work. *)
open Pdfutil

let widths_of_cidwidths l default =
  let arr = Array.make 256 (float_of_int default) in
    iter (fun (x, f) -> if x >= 0 && x <= 255 then arr.(x) <- f) l;
    arr

let to_type3 pdf = function
    Pdftext.CIDKeyedFont (basefont, cidfont, Pdftext.Predefined "/Identity-H") ->
      let font =
        Pdftext.SimpleFont
          {Pdftext.fonttype = Pdftext.Type1;
           Pdftext.basefont = basefont;
           Pdftext.fontmetrics =
             Some (widths_of_cidwidths cidfont.Pdftext.cid_widths cidfont.Pdftext.cid_default_width);
           Pdftext.fontdescriptor = Some cidfont.Pdftext.cid_fontdescriptor;
           Pdftext.encoding = Pdftext.ImplicitInFontFile}
      in
        Pdfcff.to_type3 pdf font
  | _ -> raise (Pdf.PDFError "Pdftype0.to_type3 : not supported")

