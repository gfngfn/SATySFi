
open Types


module type ElementType = sig
  type t

  val compare : t -> t -> int
end


module Make (Element : ElementType) = struct

  module ElementMap = Map.Make(Element)

  module GraphImpl = Graph.Persistent.Digraph.Abstract(Element)

  module ComponentImpl = Graph.Components.Make(GraphImpl)

  module TopologicalImpl = Graph.Topological.Make(GraphImpl)

  module ReachabilityImpl = Graph.Fixpoint.Make(GraphImpl)(struct
    type vertex = GraphImpl.E.vertex
    type edge = GraphImpl.E.t
    type g = GraphImpl.t

    type data = bool
      (* Stands for whether the vertex is reachable. *)

    let direction = Graph.Fixpoint.Forward

    let equal = Bool.equal

    let join = ( || )

    let analyze _edge is_reachable = is_reachable
      (* Directed edges transmit the reachability from its origin to its end. *)
  end)

  type element = Element.t

  module Vertex = GraphImpl.V

  module VertexSet = Set.Make(Vertex)

  type 'a t = {
    labels : ('a * Vertex.t) ElementMap.t;
    main   : GraphImpl.t;
  }


  let empty =
    {
      labels = ElementMap.empty;
      main   = GraphImpl.empty;
    }


  let add_vertex (elem : element) (data : 'a) (graph : 'a t) : ('a t * Vertex.t, 'a * Vertex.t) result =
    match graph.labels |> ElementMap.find_opt elem with
    | Some(pair) ->
        Error(pair)

    | None ->
      let vertex = Vertex.create elem in
      let graph =
        {
          labels = graph.labels |> ElementMap.add elem (data, vertex);
          main   = GraphImpl.add_vertex graph.main vertex
        }
      in
      Ok((graph, vertex))


  let get_vertex (elem : element) (graph : 'a t) : Vertex.t option =
    graph.labels |> ElementMap.find_opt elem |> Option.map (fun (_data, vertex) -> vertex)


  let add_edge ~from:(vertex1 : Vertex.t) ~to_:(vertex2 : Vertex.t) (graph : 'a t) : 'a t =
    { graph with main = GraphImpl.add_edge graph.main vertex1 vertex2 }


  let extract_vertex_info (graph : 'a t) (vertex : Vertex.t) : element * 'a =
    let elem = GraphImpl.V.label vertex in
    match graph.labels |> ElementMap.find_opt elem with
    | None            -> assert false
    | Some((data, _)) -> (elem, data)


  let find_loop (g : GraphImpl.t) =
    GraphImpl.fold_vertex (fun v acc ->
      match acc with
      | Some(_) -> acc
      | None    -> if GraphImpl.mem_edge g v v then Some(v) else None
    ) g None


  let topological_sort (graph : 'a t) : ((element * 'a) list, (element * 'a) cycle) result =
    match find_loop graph.main with
    | Some(v) ->
        Error(Loop(extract_vertex_info graph v))

    | None ->
        let sccs = ComponentImpl.scc_list graph.main in
        begin
          match
            sccs |> List.find_map (fun scc ->
              match scc with
              | [] ->
                  assert false

              | [_] ->
                  None

              | v1 :: v2 :: vrest ->
                  let vs = TupleList.make v1 v2 vrest in
                  Some(Cycle(vs |> TupleList.map (extract_vertex_info graph)))
            )
          with
          | Some(cycle) ->
              Error(cycle)

          | None ->
              let acc =
                TopologicalImpl.fold (fun v acc ->
                  let info = extract_vertex_info graph v in
                  Alist.extend acc info
                ) graph.main Alist.empty
              in
              Ok(Alist.to_list_rev acc)
        end


  let reachability_closure (graph : 'a t) (vertices_origin : VertexSet.t) : VertexSet.t =
    let g = graph.main in
    let is_reachable = ReachabilityImpl.analyze (fun v -> vertices_origin |> VertexSet.mem v) g in
    GraphImpl.fold_vertex (fun v vertices ->
      if is_reachable v then
        vertices |> VertexSet.add v
      else
        vertices
    ) g VertexSet.empty

end
