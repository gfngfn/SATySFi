
open Types

module IDMap = Map.Make(TypeID.Synonym)

module GraphImpl = Graph.Persistent.Digraph.Abstract(TypeID.Synonym)

module TraverseImpl = Graph.Traverse.Dfs(GraphImpl)

type t = {
  labels : (type_name ranged * GraphImpl.V.t) IDMap.t;
  main   : GraphImpl.t;
}


let empty : t =
  {
    labels = IDMap.empty;
    main   = GraphImpl.empty;
  }


let add_vertex (sid : TypeID.Synonym.t) (tyident : type_name ranged) (graph : t) : t =
  let vertex = GraphImpl.V.create sid in
  {
    labels = graph.labels |> IDMap.add sid (tyident, vertex);
    main   = GraphImpl.add_vertex graph.main vertex;
  }


let get_vertex_token map (sid : TypeID.Synonym.t) : GraphImpl.V.t =
  match map |> IDMap.find_opt sid with
  | None            -> assert false
  | Some(_, vertex) -> vertex


let add_edge (sid1 : TypeID.Synonym.t) (sid2 : TypeID.Synonym.t) (graph : t) : t =
  let map = graph.labels in
  let vertex1 = get_vertex_token map sid1 in
  let vertex2 = get_vertex_token map sid2 in
  { graph with main = GraphImpl.add_edge graph.main vertex1 vertex2 }


let has_cycle (graph : t) : bool =
(* --
   TODO: make this function return one cycle for proof
   Since ocamlgraph does not provide APIs for such a use case,
   it requires inspecting how `Graph.Traverse.Dfs(_).has_cycle` is implemented.
   -- *)
  TraverseImpl.has_cycle graph.main
