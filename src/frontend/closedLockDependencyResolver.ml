
open MyUtil
open PackageSystemBase
open Types
open ConfigError


type 'a ok = ('a, config_error) result


module LockDependencyGraph = DependencyGraph.Make(String)


let main (display_config : Logging.config) ~(use_test_only_lock : bool) ~library_root:(absdir_lib_root : abs_path) ~(extensions : string list) (lock_config : LockConfig.t) : ((lock_name * (PackageConfig.t * untyped_package)) list) ok =
  let open ResultMonad in

  let locks = lock_config.LockConfig.locked_packages in

  (* Add vertices: *)
  let* (graph, entryacc) =
    locks |> foldM (fun (graph, entryacc) (lock : LockConfig.locked_package) ->
      let LockConfig.{ lock_name; lock_contents; lock_dependencies; test_only_lock; _ } = lock in
      if test_only_lock && not use_test_only_lock then
      (* Skips test-only locks when using sources only: *)
        return (graph, entryacc)
      else
        let absdir_package =
          match lock_contents with
          | RegisteredLock{ registry_hash_value; package_name; version = locked_version } ->
              let libdir = Constant.lock_directory Lock.{ registry_hash_value; package_name; locked_version } in
              make_abs_path (Filename.concat (get_abs_path_string absdir_lib_root) (get_lib_path_string libdir))
        in
        let* package_with_config =
          PackageReader.main display_config ~use_test_files:use_test_only_lock ~extensions absdir_package
        in
        let* (graph, vertex) =
          graph |> LockDependencyGraph.add_vertex lock_name package_with_config
            |> Result.map_error (fun _ -> LockNameConflict(lock_name))
        in
        let lock_info =
          {
            lock_name;
            lock_directory = absdir_package;
            lock_dependencies;
          }
        in
        return (graph, Alist.extend entryacc (lock_info, vertex))
    ) (LockDependencyGraph.empty, Alist.empty)
  in

  (* Add edges: *)
  let* graph =
    entryacc |> Alist.to_list |> foldM (fun graph (lock_info, vertex) ->
      lock_info.lock_dependencies |> foldM (fun graph lock_name_dep ->
        begin
          match graph |> LockDependencyGraph.get_vertex lock_name_dep with
          | None ->
              err @@ DependencyOnUnknownLock{
                depending = lock_info.lock_name;
                depended  = lock_name_dep;
              }

          | Some(vertex_dep) ->
              let graph = graph |> LockDependencyGraph.add_edge ~from:vertex ~to_:vertex_dep in
              return graph
        end
      ) graph
    ) graph
  in

  LockDependencyGraph.topological_sort graph
    |> Result.map_error (fun cycle ->
      CyclicLockDependency(cycle |> map_cycle (fun (lock_name, (_config, package)) -> (lock_name, package)))
    )
