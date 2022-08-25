
open Main__MyUtil


module DependencyGraph = Main__DependencyGraph


module DG = DependencyGraph.Make(Int)


let map_error f = function
  | Ok(v)    -> Ok(v)
  | Error(e) -> Error(f e)


let check_result f = function
  | Ok(v)    -> f v
  | Error(s) -> Alcotest.fail s


let pp_int_and_string ppf (n, s) =
  Format.fprintf ppf "(%d, %s)" n s


(** Creates a graph of structure [v1 --> v2 --> v3]. *)
let create_graph1 () =
  let open ResultMonad in
  let graph = DG.empty in
  graph |> DG.add_vertex 1 "one" >>= fun (graph, vertex1) ->
  graph |> DG.add_vertex 2 "two" >>= fun (graph, vertex2) ->
  graph |> DG.add_vertex 3 "three" >>= fun (graph, vertex3) ->
  let graph = graph |> DG.add_edge ~from:vertex1 ~to_:vertex2 in
  let graph = graph |> DG.add_edge ~from:vertex2 ~to_:vertex3 in
  return graph


let tests () =
  let expected1 = [ (3, "three"); (2, "two"); (1, "one") ] in
  begin
    let open ResultMonad in
    create_graph1 () |> map_error (fun _ -> "cannot construct graph1") >>= fun graph1 ->
    DG.topological_sort graph1 |> map_error (fun _ -> "cannot sort graph1")
  end |> check_result (fun got1 ->
    Alcotest.(check (list (pair int string))) "graph1" expected1 got1
  )
