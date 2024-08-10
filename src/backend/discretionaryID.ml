
type t =
  | Beginning
  | Middle of int
  | Final


let current_id = ref 0


let beginning = Beginning


let final = Final


let initialize () =
(*
  print_endline "DiscretionaryID> INITIALIZE";  (* for debug  *)
*)
  begin current_id := 0; end


let fresh () =
  incr current_id;
  Middle(!current_id)


let equal did1 did2 =
  match (did1, did2) with
  | (Beginning, Beginning)   -> true
  | (Middle(i1), Middle(i2)) -> i1 = i2
  | (Final, Final)           -> true
  | _                        -> false


let show did =
  match did with
  | Beginning -> "<beginning>"
  | Final     -> "<final>"
  | Middle(i) -> "<" ^ (string_of_int i) ^ ">"


let hash = Hashtbl.hash


let compare = Stdlib.compare
