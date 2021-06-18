
open Types

module IDMap = Map.Make(String)

module GraphImpl = Graph.Persistent.Digraph.Abstract(String)

module ComponentImpl = Graph.Components.Make(GraphImpl)

module TopologicalImpl = Graph.Topological.Make(GraphImpl)

type data = {
  position : Range.t;
}

type t = {
  labels : (data * GraphImpl.V.t) IDMap.t;
  main   : GraphImpl.t;
}


let empty : t =
  {
    labels = IDMap.empty;
    main   = GraphImpl.empty;
  }


let add_vertex (tynm : type_name) (data : data) (graph : t) : t =
  let vertex = GraphImpl.V.create tynm in
  {
    labels = graph.labels |> IDMap.add tynm (data, vertex);
    main   = GraphImpl.add_vertex graph.main vertex;
  }


let get_vertex_token (map : (data * GraphImpl.V.t) IDMap.t) (tynm : type_name) : GraphImpl.V.t =
  match map |> IDMap.find_opt tynm with
  | None            -> assert false
  | Some(_, vertex) -> vertex


let add_edge (tynm1 : type_name) (tynm2 : type_name) (graph : t) : t =
  let map = graph.labels in
  let vertex1 = get_vertex_token map tynm1 in
  let vertex2 = get_vertex_token map tynm2 in
  { graph with main = GraphImpl.add_edge graph.main vertex1 vertex2 }


let extract_vertex_info (graph : t) (v : GraphImpl.V.t) : type_name * data =
  let tynm = GraphImpl.V.label v in
  match graph.labels |> IDMap.find_opt tynm with
  | None            -> assert false
  | Some((data, _)) -> (tynm, data)


let find_loop g =
  GraphImpl.fold_vertex (fun v acc ->
    match acc with
    | Some(_) -> acc
    | None    -> if GraphImpl.mem_edge g v v then Some(v) else None
  ) g None


let topological_sort (graph : t) : ((type_name * data) list, (type_name * data) cycle) result =
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
            Ok(Alist.to_list acc)
      end
