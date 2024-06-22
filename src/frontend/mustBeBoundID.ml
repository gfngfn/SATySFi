
type t = {
  main  : BoundID.t;
  level : Level.t;
}
[@@deriving show]


let fresh lev =
  let bid = BoundID.fresh () in
  { main = bid; level = lev }


let equal mbbid1 mbbid2 =
  BoundID.equal mbbid1.main mbbid2.main


let get_level mbbid =
  mbbid.level


let to_bound_id mbbid =
  mbbid.main
