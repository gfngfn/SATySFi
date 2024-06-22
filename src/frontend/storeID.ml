
type t = int


let current_id = ref 0

(*
let min_id_in_document = ref 0
*)

let equal = (=)


let compare = Stdlib.compare


let hash = Hashtbl.hash


let initialize () =
  current_id := 0


let fresh () =
  let stid = !current_id in
  begin
    incr current_id;
    stid
  end

(*
let set () =
  min_id_in_document := !current_id


let reset () =
  current_id := !min_id_in_document


let should_be_omitted stid =
  stid >= !min_id_in_document
*)


let show_direct stid =
  "<SID:" ^ (string_of_int stid) ^ ">"


let pp fmt _ = Format.fprintf fmt "<store-id>"
