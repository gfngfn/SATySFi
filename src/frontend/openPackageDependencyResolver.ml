
open MyUtil
open Types

type error =
  | MainModuleNameMismatch of {
      expected : module_name;
      got      : module_name;
    }
  | DependencyNotFound of {
      depending : module_name;
      depended  : module_name;
    }
  | PackageDirectoryNotFound of string list
  | PackageReadingError of PackageReader.error
  | CyclicPackageDependency of (module_name * package_info) cycle
[@@deriving show { with_path = false }]

type 'a ok = ('a, error) result


module PackageDependencyGraph = DependencyGraph.Make(String)


let main (package_name_set : PackageNameSet.t) : (package_info list) ok =
  let open ResultMonad in
  let main_module_names = package_name_set |> PackageNameSet.elements in

  (* Add vertices: *)
  let* (graph, entryacc) =
    main_module_names |> foldM (fun (graph, entryacc) main_module_name ->
      let* absdir =
        Config.resolve_package_directory main_module_name
          |> Result.map_error (fun cands -> PackageDirectoryNotFound(cands))
      in
      let* package =
        PackageReader.main absdir
          |> Result.map_error (fun e -> PackageReadingError(e))
      in
      if String.equal package.main_module_name main_module_name then
        match graph |> PackageDependencyGraph.add_vertex main_module_name package with
        | Error(_) ->
            assert false

        | Ok((graph, vertex)) ->
            let entry = (package, vertex) in
            return (graph, Alist.extend entryacc entry)
      else
        err @@ MainModuleNameMismatch{
          expected = main_module_name;
          got      = package.main_module_name;
        }
    ) (PackageDependencyGraph.empty, Alist.empty)
  in

  let* graph =
    entryacc |> Alist.to_list |> foldM (fun graph (package, vertex) ->
      let main_module_names_dep = package.dependencies in
      main_module_names_dep |> foldM (fun graph main_module_name_dep ->
        match graph |> PackageDependencyGraph.get_vertex main_module_name_dep with
        | None ->
            err @@ DependencyNotFound{
              depending = package.main_module_name;
              depended  = main_module_name_dep;
            }

        | Some(vertex_dep) ->
            return (graph |> PackageDependencyGraph.add_edge ~from:vertex ~to_:vertex_dep)
      ) graph
    ) graph
  in

  PackageDependencyGraph.topological_sort graph
    |> Result.map (fun pairs -> pairs |> List.map (fun (_, package) -> package))
    |> Result.map_error (fun cycle -> CyclicPackageDependency(cycle))
