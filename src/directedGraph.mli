
module type VertexType =
  sig
    type t
    val compare : t -> t -> int
  end


module type S =
  sig
    type vertex
    type 'a t
    exception UndefinedSourceVertex
    exception UndefinedDestinationVertex
    val create : int -> 'a t
    val add_vertex : 'a t -> vertex -> 'a -> unit
    val find_vertex : 'a t -> vertex -> 'a
    val iter_vertex : (vertex -> unit) -> 'a t -> unit
    val mem_vertex : vertex -> 'a t -> bool
    val add_edge : 'a t -> vertex -> vertex -> unit
    val find_cycle : 'a t -> (vertex list) option
    val backward_bfs : ('a -> unit) -> 'a t -> unit
  end


module Make (Vertex : VertexType) : S with type vertex = Vertex.t
