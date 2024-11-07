
type t = {
  number : int;
  name   : string;
}
[@@deriving show]


let current_id =
  ref 0


let initialize () =
  current_id := 0


let fresh (tynm : string) : t =
  incr current_id;
  { number = !current_id; name = tynm }


let extract_name (id : t) : string =
  id.name


let compare (id1 : t) (id2 : t) =
  Int.compare id2.number id1.number


let equal (id1 : t) (id2 : t) =
  id1.number = id2.number


let hash (id : t) =
  id.number


let show_direct (id : t) : string =
  Printf.sprintf "%s/%d" id.name id.number
