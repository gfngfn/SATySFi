
open MyUtil
open Types
open ConfigError


type 'a ok = ('a, config_error) result


module LockDependencyGraph = DependencyGraph.Make(String)


let main ~(extensions : string list) (lock_config : LockConfig.t) : ((lock_name * untyped_package) list) ok =
  let open ResultMonad in

  let locks = lock_config.LockConfig.locked_packages in

  (* Add vertices: *)
  let* (graph, entryacc) =
    locks |> foldM (fun (graph, entryacc) lock ->
      let lock_name = lock.lock_name in
      let absdir_package = lock.lock_directory in
      let* package = PackageReader.main ~extensions absdir_package in
      let* (graph, vertex) =
        graph |> LockDependencyGraph.add_vertex lock_name package
          |> Result.map_error (fun _ -> LockNameConflict(lock_name))
      in
      return (graph, Alist.extend entryacc (lock, vertex))
    ) (LockDependencyGraph.empty, Alist.empty)
  in

  (* Add edges: *)
  let* graph =
    entryacc |> Alist.to_list |> foldM (fun graph (lock, vertex) ->
      lock.lock_dependencies |> foldM (fun graph lock_name_dep ->
        begin
          match graph |> LockDependencyGraph.get_vertex lock_name_dep with
          | None ->
              err @@ DependencyOnUnknownLock{
                depending = lock.lock_name;
                depended  = lock_name_dep;
              }

          | Some(vertex_dep) ->
              let graph = graph |> LockDependencyGraph.add_edge ~from:vertex ~to_:vertex_dep in
              return graph
        end
      ) graph
    ) graph
  in

  LockDependencyGraph.topological_sort graph |> Result.map_error (fun cycle -> CyclicLockDependency(cycle))
