
open MyUtil
open Types

type error =
  | FileModuleNotFound   of Range.t * module_name
  | CyclicFileDependency of (abs_path * untyped_library_file) cycle
[@@deriving show { with_path = false }]

type 'a ok = ('a, error) result


let main (utlibs : (abs_path * untyped_library_file) list) : ((abs_path * untyped_library_file) list) ok =
  let open ResultMonad in

  (* Add vertices: *)
  let (graph, modnm_to_path, entryacc) =
    utlibs |> List.fold_left (fun (graph, modnm_to_path, entryacc) (abspath, utlib) ->
      let (_, ((_, modnm), _, _)) = utlib in
      let (graph, vertex) =
        match graph |> FileDependencyGraph.add_vertex abspath utlib with
        | Error(_) -> assert false
        | Ok(pair) -> pair
      in
      let entry = (modnm, utlib, vertex) in
      (graph, modnm_to_path |> ModuleNameMap.add modnm abspath, Alist.extend entryacc entry)
    ) (FileDependencyGraph.empty, ModuleNameMap.empty, Alist.empty)
  in

  (* Add edges: *)
  let* graph =
    entryacc |> Alist.to_list |> foldM (fun graph (modnm, utlib, vertex) ->
      let (header, _) = utlib in
      header |> foldM (fun graph headerelem ->
        match headerelem with
        | HeaderUse((rng, modnm_sub)) ->
            begin
              match modnm_to_path |> ModuleNameMap.find_opt modnm_sub with
              | None ->
                  err @@ FileModuleNotFound(rng, modnm_sub)

              | Some(abspath_sub) ->
                  begin
                    match graph |> FileDependencyGraph.get_vertex abspath_sub with
                    | None ->
                        assert false

                    | Some(vertex_sub) ->
                        Printf.printf "****SRC DEP: %s ---> %s\n" modnm modnm_sub; (* TODO: remove this *)
                        let graph = graph |> FileDependencyGraph.add_edge ~from:vertex ~to_:vertex_sub in
                        return graph
                  end
            end

        | HeaderUsePackage(_) ->
            return graph

        | HeaderUseOf(_, _) ->
            assert false

      ) graph
    ) graph
  in

  FileDependencyGraph.topological_sort graph |> Result.map_error (fun cycle -> CyclicFileDependency(cycle))
