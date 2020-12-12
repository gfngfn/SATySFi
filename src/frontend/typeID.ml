
module Scheme(M : sig val suffix : string end) : sig
  type t
  [@@deriving show]
  val initialize : unit -> unit
  val fresh : string -> t
  val extract_name : t -> string
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val show_direct : t -> string
end = struct

  type t = {
    number : int;
    name   : string;
  }
  [@@deriving show]


  let current_id =
    ref 0


  let initialize () =
    current_id := 0


  let fresh (tynm : string) : t =
    incr current_id;
    { number = !current_id; name = tynm }


  let extract_name (id : t) : string =
    id.name


  let compare (id1 : t) (id2 : t) =
    id2.number - id1.number


  let equal (id1 : t) (id2 : t) =
    id1.number = id2.number


  let show_direct (id : t) : string =
    Printf.sprintf "%s/%d%s" id.name id.number M.suffix

end

module Variant = Scheme(struct let suffix = "!" end)

module Synonym = Scheme(struct let suffix = "$" end)

module Opaque = Scheme(struct let suffix = "@" end)

type t =
  | Variant of Variant.t
  | Synonym of Synonym.t
  | Opaque  of Opaque.t
[@@deriving show]
