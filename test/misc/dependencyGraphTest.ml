
open Main__MyUtil
open Main__Types
module TupleList = Main__TupleList


module DependencyGraph = Main__DependencyGraph


module DG = DependencyGraph.Make(Int)


let map_error f = function
  | Ok(v)    -> Ok(v)
  | Error(e) -> Error(f e)


let continue_if_ok msg k = function
  | Ok(v)    -> k v
  | Error(_) -> Alcotest.fail msg


let continue_if_error msg k = function
  | Ok(_)    -> Alcotest.fail msg
  | Error(e) -> k e


let expect_pattern msg pp predicate got =
  if predicate got then
    Alcotest.(check pass) msg () ()
  else
    Alcotest.failf "%s: not of expected pattern; %a" msg pp got


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


(** Creates a graph of structure [v1 --> v2 --> v3 --> v1] and [v3 --> v4], which has a cycle. *)
let create_graph3 () =
  let open ResultMonad in
  let graph = DG.empty in
  graph |> DG.add_vertex 1 "one" >>= fun (graph, vertex1) ->
  graph |> DG.add_vertex 2 "two" >>= fun (graph, vertex2) ->
  graph |> DG.add_vertex 3 "three" >>= fun (graph, vertex3) ->
  graph |> DG.add_vertex 4 "four" >>= fun (graph, vertex4) ->
  let graph = graph |> DG.add_edge ~from:vertex1 ~to_:vertex2 in
  let graph = graph |> DG.add_edge ~from:vertex2 ~to_:vertex3 in
  let graph = graph |> DG.add_edge ~from:vertex3 ~to_:vertex1 in
  let graph = graph |> DG.add_edge ~from:vertex3 ~to_:vertex4 in
  return graph


(** Creates a graph of structure [v1 --> v2 --> v2], which has a loop. *)
let create_graph4 () =
  let open ResultMonad in
  let graph = DG.empty in
  graph |> DG.add_vertex 1 "one" >>= fun (graph, vertex1) ->
  graph |> DG.add_vertex 2 "two" >>= fun (graph, vertex2) ->
  let graph = graph |> DG.add_edge ~from:vertex1 ~to_:vertex2 in
  let graph = graph |> DG.add_edge ~from:vertex2 ~to_:vertex2 in
  return graph


let test1 () =
  create_graph1 () |> continue_if_ok "cannot construct graph1" (fun graph1 ->
    DG.topological_sort graph1 |> continue_if_ok "cannot sort graph1" (fun got1 ->
      let expected1 = [ (3, "three"); (2, "two"); (1, "one") ] in
      Alcotest.(check (list (pair int string))) "should be [v3, v2, v1]" expected1 got1
    )
  )


let test2 () =
  create_graph2 () |> continue_if_ok "cannot construct graph2" (fun graph2 ->
    DG.topological_sort graph2 |> continue_if_ok "cannot sort graph2" (fun got2 ->
      let pp = Format.pp_print_list pp_int_and_string in
      got2 |> expect_pattern "v4 must be first, and v1 must be the last" pp (function
      | [ (4, "four"); _; _; (1, "one") ] -> true
      | _                                 -> false
      )
    )
  )


let test3 () =
  create_graph3 () |> continue_if_ok "cannot construct graph3" (fun graph3 ->
    DG.topological_sort graph3 |> continue_if_error "cannot find cycle" (fun cycle ->
      let pp = pp_cycle pp_int_and_string in
      cycle |> expect_pattern "is the cycle of v1, v2, and v3" pp (function
      | Cycle(xs) ->
          let sorted = xs |> TupleList.to_list |> List.sort (fun (n1, _) (n2, _) -> Int.compare n1 n2) in
          begin
            match sorted with
            | [ (1, "one"); (2, "two"); (3, "three") ] -> true
            | _                                        -> false
          end

      | _ ->
          false
      )
    )
  )


let test4 () =
  create_graph4 () |> continue_if_ok "cannot construct graph4" (fun graph4 ->
    DG.topological_sort graph4 |> continue_if_error "cannot find cycle" (fun cycle ->
      let pp = pp_cycle pp_int_and_string in
      cycle |> expect_pattern "is the loop of v2" pp (function
      | Loop((2, "two")) -> true
      | _                -> false
      )
    )
  )


let test_cases =
  Alcotest.[
    test_case "IntDependencyGraph 1" `Quick test1;
    test_case "IntDependencyGraph 2" `Quick test2;
    test_case "IntDependencyGraph 3" `Quick test3;
    test_case "IntDependencyGraph 4" `Quick test4;
  ]
