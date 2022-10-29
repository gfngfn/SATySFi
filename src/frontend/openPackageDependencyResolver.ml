(*
open MyUtil
open Types
open ConfigError


type 'a ok = ('a, config_error) result

module PackageDependencyGraph = DependencyGraph.Make(String)

type graph = package_info PackageDependencyGraph.t

type vertex = PackageDependencyGraph.Vertex.t


let rec add_package (extensions : string list) (graph : graph) ~prev:(vertex_prev_opt : vertex option) (main_module_name : module_name) : graph ok =
  let open ResultMonad in
  match graph |> PackageDependencyGraph.get_vertex main_module_name with
  | Some(vertex) ->
    (* If `main_module_name` has already been read: *)
      let graph =
        match vertex_prev_opt with
        | None ->
            graph

        | Some(vertex_prev) ->
            graph |> PackageDependencyGraph.add_edge ~from:vertex_prev ~to_:vertex
      in
      return graph

  | None ->
      let* absdir =
        Config.resolve_package_directory main_module_name
          |> Result.map_error (fun cands -> PackageDirectoryNotFound(cands))
      in
      let* package = PackageReader.main ~extensions absdir in
      if String.equal package.main_module_name main_module_name then
        let (graph, vertex) =
          match graph |> PackageDependencyGraph.add_vertex main_module_name package with
          | Error(_) -> assert false
          | Ok(pair) -> pair
        in
        let graph =
          match vertex_prev_opt with
          | None              -> graph
          | Some(vertex_prev) -> graph |> PackageDependencyGraph.add_edge ~from:vertex_prev ~to_:vertex
        in
        package.dependencies |> foldM (fun graph main_module_name_dep ->
          add_package extensions graph ~prev:(Some(vertex)) main_module_name_dep
        ) graph
      else
        err @@ MainModuleNameMismatch{
          expected = main_module_name;
          got      = package.main_module_name;
        }


let main ~(extensions : string list) (package_name_set_init : PackageNameSet.t) : (package_info list) ok =
  let open ResultMonad in
  let main_module_names_init = package_name_set_init |> PackageNameSet.elements in
  let* graph =
    main_module_names_init |> foldM (fun graph main_module_name ->
      add_package extensions graph ~prev:None main_module_name
    ) PackageDependencyGraph.empty
  in
  let* pairs =
    PackageDependencyGraph.topological_sort graph
      |> Result.map_error (fun cycle -> CyclicPackageDependency(cycle))
  in
  return (pairs |> List.map (fun (_, package) -> package))
*)
