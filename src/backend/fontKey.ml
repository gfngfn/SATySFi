
type t = int
[@@deriving show]

let current_key_number = ref 0


let initialize () =
  current_key_number := 0


let generate () =
  incr current_key_number;
  !current_key_number


let equal =
  Int.equal


let hash =
  Hashtbl.hash
