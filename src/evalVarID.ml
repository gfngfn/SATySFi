
type t = int * string


let current_id = ref 0


let initialize () =
  current_id := 0


let fresh varnm =
  begin
    incr current_id;
    (!current_id, varnm)
  end


let equal (i1, _) (i2, _) =
  (i1 = i2)


let show_direct (i, varnm) =
  "<" ^ (string_of_int i) ^ "|" ^ varnm ^ ">"


let pp fmt evid =
  Format.fprintf fmt "%s" (show_direct evid)
