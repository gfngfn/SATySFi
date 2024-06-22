
type t = {
  main  : BoundRowID.t;
  level : Level.t;
}
[@@deriving show]


let fresh lev labset =
  let brid = BoundRowID.fresh labset in
  { main = brid; level = lev }


let equal mbbid1 mbbid2 =
  BoundRowID.equal mbbid1.main mbbid2.main


let get_level mbbid =
  mbbid.level


let to_bound_id mbbid =
  mbbid.main
