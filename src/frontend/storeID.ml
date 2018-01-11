
type t = int


let current_id = ref 0


let initialize () =
  current_id := 0


let fresh () =
  let stid = !current_id in
  begin
    incr current_id;
    stid
  end


let pp fmt _ = Format.fprintf fmt "<store-id>"
