
open LengthInterface
open Length

let name_id = ref 0

let named_dest_acc = ref []

let pending_dest_acc = ref []

let name_hash_table : (string, string) Hashtbl.t = Hashtbl.create 64


let name_from_hashtbl key =
  match Hashtbl.find_opt name_hash_table key with
  | Some(name) -> name
  | None ->
      let name = ("nameddest:" ^ (string_of_int !name_id)) in
      incr name_id;
      Hashtbl.add name_hash_table key name;
      name


let register_location key loc =
  let name = name_from_hashtbl key in
  pending_dest_acc := (name, loc) :: !pending_dest_acc


let notify_new_page pageno =
  let lst = List.fold_left (fun acc (nm, loc) -> (nm, loc, pageno) :: acc)
                        !named_dest_acc !pending_dest_acc
  in
    pending_dest_acc := [];
    named_dest_acc := lst


let add_locations pdf =
  let named_dest_dict = Pdf.Dictionary
      (List.map (fun (nm, (x, y), pageno) ->
          match Pdfpage.page_object_number pdf pageno with
          | Some(n) ->
            ("/" ^ nm,
              Pdfdest.pdfobject_of_destination
                (Pdfdest.XYZ(PageObject(n),
                  Some(to_pdf_point x),
                  Some(to_pdf_point y), None)))
          | None -> assert false
    ) !named_dest_acc)
  in
    match Pdf.lookup_direct pdf "/Root" pdf.Pdf.trailerdict with
    | None -> raise (Pdf.PDFError "Bad PDF: no root")
    | Some(catalog) ->
        let catalog = Pdf.add_dict_entry catalog "/Dests"
            (Pdf.Indirect (Pdf.addobj pdf named_dest_dict))
        in
          let newcatnum = Pdf.addobj pdf catalog in
            { pdf with 
              Pdf.root = newcatnum;
              Pdf.trailerdict = Pdf.add_dict_entry
                pdf.Pdf.trailerdict "/Root" (Pdf.Indirect newcatnum) }


let get_location key =
  name_from_hashtbl key
