
module type VertexType =
  sig
    type t
    val compare : t -> t -> int
    val show : t -> string (* for debug *)
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
    val iter_vertex : (vertex -> 'a -> unit) -> 'a t -> unit
    val fold_vertex : (vertex -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val mem_vertex : vertex -> 'a t -> bool
    val add_edge : 'a t -> vertex -> vertex -> unit
    val find_cycle : 'a t -> (vertex list) option
    val backward_bfs_fold : ('b -> vertex -> 'a -> 'b) -> 'b -> 'a t -> 'b
    val backward_bfs : (vertex -> 'a -> unit) -> 'a t -> unit
    val get_vertex : 'a t -> vertex -> 'a
  end


module Make (Vertex : VertexType) : S with type vertex = Vertex.t
