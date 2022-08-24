
open MyUtil
open Types

module AbsPath = struct
  type t = abs_path

  let compare ap1 ap2 = String.compare (get_abs_path_string ap1) (get_abs_path_string ap2)

  let show ap = Filename.basename (get_abs_path_string ap)
end

module AbsPathMap = Map.Make(AbsPath)

module GraphImpl = Graph.Persistent.Digraph.Abstract(AbsPath)

module ComponentImpl = Graph.Components.Make(GraphImpl)

module TopologicalImpl = Graph.Topological.Make(GraphImpl)

type vertex = GraphImpl.V.t

type t = {
  labels : (file_info * vertex) AbsPathMap.t;
  main   : GraphImpl.t;
}

let empty =
  {
    labels = AbsPathMap.empty;
    main   = GraphImpl.empty;
  }


let add_vertex (abspath : abs_path) (data : file_info) (graph : t) : t * vertex =
  let vertex = GraphImpl.V.create abspath in
  let graph =
    {
      labels = graph.labels |> AbsPathMap.add abspath (data, vertex);
      main   = GraphImpl.add_vertex graph.main vertex
    }
  in
  (graph, vertex)


let get_vertex (abspath : abs_path) (graph : t) : vertex option =
  graph.labels |> AbsPathMap.find_opt abspath |> Option.map (fun (_data, vertex) -> vertex)


let add_edge ~from:(vertex1 : vertex) ~to_:(vertex2 : vertex) (graph : t) : t =
  { graph with main = GraphImpl.add_edge graph.main vertex1 vertex2 }


let extract_vertex_info (graph : t) (vertex : GraphImpl.V.t) : abs_path * file_info =
  let abspath = GraphImpl.V.label vertex in
  match graph.labels |> AbsPathMap.find_opt abspath with
  | None            -> assert false
  | Some((data, _)) -> (abspath, data)


let find_loop g =
  GraphImpl.fold_vertex (fun v acc ->
    match acc with
    | Some(_) -> acc
    | None    -> if GraphImpl.mem_edge g v v then Some(v) else None
  ) g None


let topological_sort (graph : t) : ((abs_path * file_info) list, (abs_path * file_info) cycle) result =
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
