
module Variant : sig
  type t
  [@@deriving show]

  val initialize : unit -> unit

  val fresh : string -> t

  val extract_name : t -> string

  val equal : t -> t -> bool

  val show_direct : t -> string
end

module Synonym : sig
  type t
  [@@deriving show]

  val initialize : unit -> unit

  val fresh : string -> t

  val extract_name : t -> string

  val equal : t -> t -> bool

  val show_direct : t -> string
end

type t =
  | Variant of Variant.t
  | Synonym of Synonym.t
