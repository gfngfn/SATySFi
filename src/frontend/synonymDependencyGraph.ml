
open Types

module IDMap = Map.Make(String)

module GraphImpl = Graph.Persistent.Digraph.Abstract(String)

module ComponentImpl = Graph.Components.Make(GraphImpl)

module TopologicalImpl = Graph.Topological.Make(GraphImpl)

type data = {
  position        : Range.t;
  type_variables  : (type_variable_name ranged) list;
  definition_body : manual_type;
}

type vertex = GraphImpl.V.t

type t = {
  labels : (data * vertex) IDMap.t;
  main   : GraphImpl.t;
}


let empty : t =
  {
    labels = IDMap.empty;
    main   = GraphImpl.empty;
  }


let add_vertex (tynm : type_name) (data : data) (graph : t) : t * vertex =
  let vertex = GraphImpl.V.create tynm in
  let graph =
    {
      labels = graph.labels |> IDMap.add tynm (data, vertex);
      main   = GraphImpl.add_vertex graph.main vertex;
    }
  in
  (graph, vertex)


let get_vertex (tynm : type_name) (graph : t) : vertex option =
  graph.labels |> IDMap.find_opt tynm |> Option.map (fun (_, vertex) -> vertex)


let add_edge ~from:(vertex1 : vertex) ~to_:(vertex2 : vertex) (graph : t) : t =
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
            Ok(Alist.to_list acc) (* TODO: test this; maybe `Alist.to_list_rev` is appropriate *)
      end
