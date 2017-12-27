
open HorzBox

exception CannotLoadPdf of file_path * int

type tag = string

type xobject = Pdf.pdfobject

module ImageHashTable
: sig
    type key
    val initialize : unit -> unit
    val add_pdf : file_path -> int -> key
    val find : key -> tag * xobject
    val fold : (key -> tag * xobject -> 'a -> 'a) -> 'a -> 'a
  end
= struct

    type key = int

    let main_hash_table : (key, tag * xobject) Hashtbl.t = Hashtbl.create 32

    let current_id_ref : int ref = ref 0

    let initialize () =
      begin
        current_id_ref := 0;
        Hashtbl.clear main_hash_table;
      end

    let generate_tag () : key * tag =
      let n = !current_id_ref in
      begin
        incr current_id_ref;
        (n, "/I" ^ (string_of_int n))
      end

    let add_pdf (srcpath : file_path) (pageno : int) =
      let pdf = Pdfread.pdf_of_file None None srcpath in
      match LoadPdf.make_xobject pdf pageno with
      | None       -> raise (CannotLoadPdf(srcpath, pageno))
      | Some(xobj) ->
          let (key, tag) = generate_tag () in
          begin
            Hashtbl.add main_hash_table key (tag, xobj);
            key
          end

    let find (key : key) : tag * xobject =
      match Hashtbl.find_opt main_hash_table key with
      | None        -> assert false
      | Some(value) -> value

    let fold (type a) (f : key -> tag * xobject -> a -> a) (init : a) : a =
      Hashtbl.fold f main_hash_table init
  end


let initialize () =
  ImageHashTable.initialize ()


let get_xobject_dictionary () : Pdf.pdfobject =
  let keyval =
    [] |> ImageHashTable.fold (fun _ (tag, xobj) acc ->
      (tag, xobj) :: acc
    ) |> List.rev
  in
    Pdf.Dictionary(keyval)
