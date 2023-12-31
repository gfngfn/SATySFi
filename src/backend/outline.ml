

let registered_outline : (int * string * string * bool) list ref = ref []


let make_entry (level, text, key, isopen) =
  let destname = NamedDest.get key in
    {
      Pdfmarks.level  = level;
      Pdfmarks.text   = InternalText.to_utf16be (InternalText.of_utf8 text);
      Pdfmarks.dest   = Pdfdest.NullDestination;
      Pdfmarks.action = Pdfaction.GotoName(destname);
      Pdfmarks.isopen = isopen;
    }


let add_to_pdf pdf =
  Pdfmarks.add_bookmarks (List.map make_entry !registered_outline) pdf


let register ol =
  registered_outline := ol
