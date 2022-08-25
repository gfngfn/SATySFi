
open Main__MyUtil


module DependencyGraph = Main__DependencyGraph


module DG = DependencyGraph.Make(Int)


let map_error f = function
  | Ok(v)    -> Ok(v)
  | Error(e) -> Error(f e)


let check_result f = function
  | Ok(v)    -> f v
  | Error(s) -> Alcotest.fail s


let expect_pattern msg pp predicate got =
  if predicate got then
    Alcotest.(check pass) msg () ()
  else
    Alcotest.failf "not of expected pattern; %a" pp got


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


(** Creates a graph of structure [v1 --> v2 --> v4] and [v1 --> v3 --> v4]. *)
let create_graph2 () =
  let open ResultMonad in
  let graph = DG.empty in
  graph |> DG.add_vertex 1 "one" >>= fun (graph, vertex1) ->
  graph |> DG.add_vertex 2 "two" >>= fun (graph, vertex2) ->
  graph |> DG.add_vertex 3 "three" >>= fun (graph, vertex3) ->
  graph |> DG.add_vertex 4 "four" >>= fun (graph, vertex4) ->
  let graph = graph |> DG.add_edge ~from:vertex1 ~to_:vertex2 in
  let graph = graph |> DG.add_edge ~from:vertex1 ~to_:vertex3 in
  let graph = graph |> DG.add_edge ~from:vertex2 ~to_:vertex4 in
  let graph = graph |> DG.add_edge ~from:vertex3 ~to_:vertex4 in
  return graph


let test1 () =
  begin
    let open ResultMonad in
    create_graph1 () |> map_error (fun _ -> "cannot construct graph1") >>= fun graph1 ->
    DG.topological_sort graph1 |> map_error (fun _ -> "cannot sort graph1")
  end |> check_result (fun got1 ->
    let expected1 = [ (3, "three"); (2, "two"); (1, "one") ] in
    Alcotest.(check (list (pair int string))) "graph1" expected1 got1
  )


let test2 () =
  begin
    let open ResultMonad in
    create_graph2 () |> map_error (fun _ -> "cannot construct graph2") >>= fun graph2 ->
    DG.topological_sort graph2 |> map_error (fun _ -> "cannot sort graph2")
  end |> check_result (fun got2 ->
    let pp = Format.pp_print_list pp_int_and_string in
    got2 |> expect_pattern "graph2" pp (function
    | [ (4, "four"); _; _; (1, "one") ] -> true
    | _                                 -> false
    )
  )


let test_cases = Alcotest.[
  test_case "IntDependencyGraph 1" `Quick test1;
  test_case "IntDependencyGraph 2" `Quick test2;
]
