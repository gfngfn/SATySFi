
module type VertexType =
  sig
    type t
    val compare : t -> t -> int
  end


module type S =
  sig
    type state = Remained | Touched | Done
    type vertex
    type 'a t
    exception UndefinedSourceVertex
    exception UndefinedDestinationVertex
    val create : int -> 'a t
    val add_vertex : 'a t -> vertex -> 'a -> unit
    val find_vertex : 'a t -> vertex -> 'a
    val add_edge : 'a t -> vertex -> vertex -> unit
    val find_cycle : 'a t -> (vertex list) option
  end


module Make(Vertex : VertexType) : S with type vertex = Vertex.t
