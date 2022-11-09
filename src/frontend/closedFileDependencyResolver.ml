
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
      let (_attrs, _header, ((_, modnm), _, _)) = utlib in
      let* (graph, vertex) =
        match graph |> SourceModuleDependencyGraph.add_vertex modnm (abspath, utlib) with
        | Error(((abspath_prev, _utlib_prev), _vertex_prev)) ->
            err @@ FileModuleNameConflict(modnm, abspath_prev, abspath)

        | Ok(pair) ->
            return pair
      in
      return (graph, Alist.extend entryacc (utlib, vertex))
    ) (SourceModuleDependencyGraph.empty, Alist.empty)
  in

  (* Add edges: *)
  let* graph =
    entryacc |> Alist.to_list |> foldM (fun graph (utlib, vertex) ->
      let (_attrs, header, _) = utlib in
      header |> foldM (fun graph headerelem ->
        match headerelem with
        | HeaderUse{ module_name = (rng, modnm_sub); _ } ->
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

        | HeaderUseOf{ module_name = modident; _ } ->
            err @@ CannotUseHeaderUseOf(modident)

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
