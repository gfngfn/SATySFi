
type t = {
  number : int;
  mutable level : Level.t;
}
[@@deriving show]


let current_number = ref 0


let initialize () =
  current_number := 0


let equal (frid1 : t) (frid2 : t) =
  (frid1.number = frid2.number)


let fresh (lev : Level.t) : t =
  incr current_number;
  { level = lev; number = !current_number; }


let get_level (frid : t) : Level.t =
  frid.level


let set_level (frid : t) (lev : Level.t) : unit =
  frid.level <- lev


let show_direct (frid : t) : string =
  Printf.sprintf "$%d[%s]" frid.number (Level.show frid.level)
