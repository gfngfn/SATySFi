
type t = {
  number : int;
  name   : string;
  range  : Range.t;
}


let current_id = ref 0


let initialize () =
  current_id := 0


let fresh (rng, varnm) =
  begin
    incr current_id;
    { number = !current_id; name = varnm; range = rng; }
  end


let equal evid1 evid2 =
  (evid1.number = evid2.number)


let compare evid1 evid2 =
  Stdlib.compare evid1.number evid2.number


let show_direct evid =
  "<" ^ (string_of_int evid.number) ^ "|" ^ evid.name ^ "|" ^ (Range.to_string evid.range) ^ ">"


let pp fmt evid =
  Format.fprintf fmt "%s" (show_direct evid)


let get_varnm (evid : t) = evid.name


let get_range (evid : t) = evid.range
