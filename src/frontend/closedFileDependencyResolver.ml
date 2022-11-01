
open MyUtil
open Types
open ConfigError

type 'a ok = ('a, config_error) result


module SourceModuleDependencyGraph = DependencyGraph.Make(String)


let main (utlibs : (abs_path * untyped_library_file) list) : ((abs_path * untyped_library_file) list) ok =
  let open ResultMonad in

  (* Add vertices: *)
  let* (graph, entryacc) =
    utlibs |> foldM (fun (graph, entryacc) (abspath, utlib) ->
      let (_, ((_, modnm), _, _)) = utlib in
      let* (graph, vertex) =
        match graph |> SourceModuleDependencyGraph.add_vertex modnm (abspath, utlib) with
        | Error(_) -> err @@ FileModuleNameConflict(modnm, abspath)
        | Ok(pair) -> return pair
      in
      return (graph, Alist.extend entryacc (utlib, vertex))
    ) (SourceModuleDependencyGraph.empty, Alist.empty)
  in

  (* Add edges: *)
  let* graph =
    entryacc |> Alist.to_list |> foldM (fun graph (utlib, vertex) ->
      let (header, _) = utlib in
      header |> foldM (fun graph headerelem ->
        match headerelem with
        | HeaderUse((rng, modnm_sub)) ->
            begin
              match graph |> SourceModuleDependencyGraph.get_vertex modnm_sub with
              | None ->
                  err @@ FileModuleNotFound(rng, modnm_sub)

              | Some(vertex_sub) ->
                  let graph = graph |> SourceModuleDependencyGraph.add_edge ~from:vertex ~to_:vertex_sub in
                  return graph
            end

        | HeaderUsePackage(_) ->
            return graph

        | HeaderUseOf(_, _) ->
            assert false

      ) graph
    ) graph
  in

  (* Solve dependency: *)
  let* sorted =
    SourceModuleDependencyGraph.topological_sort graph
      |> Result.map_error (fun cycle ->
        let cycle = cycle |> map_cycle (fun (_modnm, pair) -> pair) in
        CyclicFileDependency(cycle)
      )
  in
  return (sorted |> List.map (fun (_modnm, pair) -> pair))
