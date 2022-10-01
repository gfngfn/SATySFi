
open Length


type key = string

type name = string


module NameHashTable = Hashtbl.Make
  (struct
    type t = key
    let equal = String.equal
    let hash = Hashtbl.hash
  end)


let name_id = ref 0
let named_dest_acc = ref Alist.empty
let pending_dest_acc = ref Alist.empty
let name_hash_table : name NameHashTable.t = NameHashTable.create 64


let initialize () =
  name_id := 0;
  NameHashTable.clear name_hash_table


let name_from_hash_table key =
  match NameHashTable.find_opt name_hash_table key with
  | Some(name) ->
      name

  | None ->
      let name = ("nameddest" ^ (string_of_int !name_id)) in
      incr name_id;
      NameHashTable.add name_hash_table key name;
      name


let register key loc =
  if State.during_page_break () then
    let name = name_from_hash_table key in
    pending_dest_acc := Alist.extend !pending_dest_acc (name, loc)
  else
    raise State.NotDuringPageBreak


let get key =
  name_from_hash_table key


let notify_pagebreak pageno =
  let acc =
    !pending_dest_acc |> Alist.to_list |> List.fold_left (fun acc (nm, loc) ->
      Alist.extend acc (nm, loc, pageno)
    ) !named_dest_acc
  in
  pending_dest_acc := Alist.empty;
  named_dest_acc := acc


let add_to_pdf pdf =
  let keyval =
    !named_dest_acc |> Alist.to_list |> List.map (fun (nm, (x, y), pageno) ->
      match Pdfpage.page_object_number pdf pageno with
      | Some(n) ->
          ("/" ^ nm,
            Pdfdest.pdfobject_of_destination
              (Pdfdest.XYZ(PageObject(n),
                Some(to_pdf_point x),
                Some(to_pdf_point y), None)))

      | None ->
          assert false
    )
  in
  let named_dest_dict = Pdf.Dictionary(keyval) in
    match Pdf.lookup_direct pdf "/Root" pdf.Pdf.trailerdict with
    | None ->
        raise (Pdf.PDFError("Bad PDF: no root"))

    | Some(catalog) ->
        let catalog =
          Pdf.add_dict_entry catalog "/Dests"
            (Pdf.Indirect (Pdf.addobj pdf named_dest_dict))
        in
        let newcatnum = Pdf.addobj pdf catalog in
        { pdf with
          Pdf.root = newcatnum;
          Pdf.trailerdict = Pdf.add_dict_entry
            pdf.Pdf.trailerdict "/Root" (Pdf.Indirect newcatnum) }
