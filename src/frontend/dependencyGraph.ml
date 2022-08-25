
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

  type element = Element.t

  module Vertex = GraphImpl.V

  type 'a t = {
    labels : ('a * Vertex.t) ElementMap.t;
    main   : GraphImpl.t;
  }


  let empty =
    {
      labels = ElementMap.empty;
      main   = GraphImpl.empty;
    }


  let add_vertex (elem : element) (data : 'a) (graph : 'a t) : ('a t * Vertex.t) option =
    if graph.labels |> ElementMap.mem elem then
      None
    else
      let vertex = GraphImpl.V.create elem in
      let graph =
        {
          labels = graph.labels |> ElementMap.add elem (data, vertex);
          main   = GraphImpl.add_vertex graph.main vertex
        }
      in
      Some((graph, vertex))


  let get_vertex (elem : element) (graph : 'a t) : Vertex.t option =
    graph.labels |> ElementMap.find_opt elem |> Option.map (fun (_data, vertex) -> vertex)


  let add_edge ~from:(vertex1 : Vertex.t) ~to_:(vertex2 : Vertex.t) (graph : 'a t) : 'a t =
    { graph with main = GraphImpl.add_edge graph.main vertex1 vertex2 }


  let extract_vertex_info (graph : 'a t) (vertex : GraphImpl.V.t) : element * 'a =
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

end
