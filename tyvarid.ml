
type quantifiability = Quantifiable | Unquantifiable
type t = int * quantifiability


let tvidmax : int ref = ref 0

let initialize () = ( tvidmax := 0 )

let fresh () =
  let res = !tvidmax in ( tvidmax := !tvidmax + 1 ; (res, Quantifiable) )

let fresh_unquantifiable () =
  let res = !tvidmax in ( tvidmax := !tvidmax + 1 ; (res, Unquantifiable) )


let same (tvid1 : t) (tvid2 : t) =
  let (tvn1, q1) = tvid1 in
  let (tvn2, q2) = tvid2 in
    (tvn1 = tvn2)

let less_than (tvid1 : t) (tvid2 : t) =
  let (tvn1, q1) = tvid1 in
  let (tvn2, q2) = tvid2 in
    (tvn1 < tvn2)

let show_direct (tvid : t) =
  let (tvn, q) = tvid in
    match q with
    | Quantifiable   -> string_of_int tvn
    | Unquantifiable -> "_" ^ (string_of_int tvn)

let of_int_for_quantifier (var_id : int) = (-16 - var_id, Quantifiable)

let make_unquantifiable_if_needed ((tvid1, tvid2) : t * t) =
  match (tvid1, tvid2) with
  | ((tvn1, Quantifiable), (tvn2, Quantifiable)) -> (tvid1, tvid2)
  | ((tvn1, _), (tvn2, _))                       -> ((tvn1, Unquantifiable), (tvn2, Unquantifiable))
