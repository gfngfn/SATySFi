
module DependencyGraph = Main__DependencyGraph


module DG = DependencyGraph.Make(Int)


(** Creates a graph of structure [v1 --> v2 --> v3]. *)
let create_graph1 () =
  let graph = DG.empty in
  let (graph, vertex1) = graph |> DG.add_vertex 1 "one" in
  let (graph, vertex2) = graph |> DG.add_vertex 2 "two" in
  let (graph, vertex3) = graph |> DG.add_vertex 3 "three" in
  let graph = graph |> DG.add_edge ~from:vertex1 ~to_:vertex2 in
  let graph = graph |> DG.add_edge ~from:vertex2 ~to_:vertex3 in
  graph


let tests () =
  let graph1 = create_graph1 () in
  let result1 = DG.topological_sort graph1 in
  match result1 with
  | Error(_cycle) ->
      Alcotest.fail "cannot sort graph1"

  | Ok(got1) ->
      let expected1 = [ (3, "three"); (2, "two"); (1, "one") ] in
      Alcotest.(check (list (pair int string))) "graph1" expected1 got1
