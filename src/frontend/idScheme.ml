
module type S = sig
  type t
  val show : int -> t -> string
end

module Make(X : S) = struct

  type t = {
    number     : int;
    supplement : X.t
  }


  let current_id = ref 0


  let initialize () =
    current_id := 0


  let generate supp =
    incr current_id;
    { number = !current_id; supplement = supp }


  let get_supplement id =
    id.supplement


  let compare id1 id2 =
    Int.compare id1.number id2.number


  let equal id1 id2 =
    id1.number = id2.number


  let hash id =
    id.number


  let show id =
    X.show id.number id.supplement

end
