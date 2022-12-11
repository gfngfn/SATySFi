
open PackageSystemBase


module SolverInput = struct

  module Role = struct

    type t =
      | LocalRole of {
          requires : package_dependency list;
          context  : package_context;
        }
      | Role of {
          package_name        : package_name;
          registry_local_name : registry_local_name;
          compatibility       : string;
          context             : package_context;
        }


    let pp ppf (role : t) =
      match role with
      | Role{ package_name; _ } -> Format.fprintf ppf "%s" package_name
      | LocalRole(_)            -> Format.fprintf ppf "local"


    let compare (role1 : t) (role2 : t) =
      match (role1, role2) with
      | (LocalRole(_), LocalRole(_)) -> 0
      | (LocalRole(_), _)            -> 1
      | (_, LocalRole(_))            -> -1

      | (
          Role{ package_name = name1; compatibility = c1; _ },
          Role{ package_name = name2; compatibility = c2; _ }
        ) ->
          begin
            match String.compare name1 name2 with
            | 0       -> String.compare c1 c2
            | nonzero -> nonzero
          end

  end


  (* Unused *)
  type command = unit

  (* Unused *)
  type command_name = string

  type restriction = SemanticVersion.requirement

  type dependency =
    | Dependency of {
        role                : Role.t;
        version_requirement : SemanticVersion.requirement;
      }

  type dep_info = {
    dep_role              : Role.t;
    dep_importance        : [ `Essential | `Recommended | `Restricts ];
    dep_required_commands : command_name list;
  }

  type requirements = {
    role    : Role.t;
    command : command_name option;
  }

  type impl =
    | DummyImpl
    | LocalImpl of {
        dependencies : dependency list;
      }
    | Impl of {
        package_name : package_name;
        version      : SemanticVersion.t;
        source       : implementation_source;
        dependencies : dependency list;
      }

  type role_information = {
    replacement : Role.t option;
    impls       : impl list;
  }

  (* Unused *)
  type machine_group = string

  type conflict_class = string

  type rejection = unit (* TODO: define this *)


  let pp_impl (ppf : Format.formatter) (impl : impl) =
    match impl with
    | DummyImpl ->
        Format.fprintf ppf "dummy"

    | LocalImpl(_) ->
        Format.fprintf ppf "local"

    | Impl{ package_name; version; _ } ->
        Format.fprintf ppf "%s %s" package_name (SemanticVersion.to_string version)


  let pp_impl_long (ppf : Format.formatter) (impl : impl) =
    pp_impl ppf impl (* TODO: show dependencies *)


  (* Unused *)
  let pp_command (_ppf : Format.formatter) (_cmd : command) =
    ()


  let pp_version (ppf : Format.formatter) (impl : impl) =
    match impl with
    | DummyImpl          -> Format.fprintf ppf "dummy"
    | LocalImpl(_)       -> Format.fprintf ppf "local"
    | Impl{ version; _ } -> Format.fprintf ppf "%s" (SemanticVersion.to_string version)


  (* Unused *)
  let get_command (_impl : impl) (_cmdnm : command_name) =
    None


  let dep_info (dep : dependency) : dep_info =
    let Dependency{ role; _ } = dep in
    { dep_role = role; dep_importance = `Essential; dep_required_commands = [] }


  let requires (_role : Role.t) (impl : impl) : dependency list * command_name list =
    match impl with
    | DummyImpl                 -> ([], [])
    | LocalImpl{ dependencies } -> (dependencies, [])
    | Impl{ dependencies; _ }   -> (dependencies, [])


  (* Unused *)
  let command_requires (_role : Role.t) (_cmd : command) =
    ([], [])


  let make_internal_dependency_from_registry (registry_local_name : registry_local_name) (context : package_context) (requires : package_dependency_in_registry list) : dependency list =
    requires |> List.map (function
    | PackageDependencyInRegistry{ package_name; version_requirement } ->
        let compatibility =
          match version_requirement with
          | SemanticVersion.CompatibleWith(semver) ->
              SemanticVersion.get_compatibility_unit semver
        in
        Dependency{ role = Role{ package_name; registry_local_name; compatibility; context }; version_requirement }
    )


  let make_internal_dependency (context : package_context) (requires : package_dependency list) : dependency list =
    requires |> List.map (function
    | PackageDependency{ package_name; registry_local_name; version_requirement } ->
        let compatibility =
          match version_requirement with
          | SemanticVersion.CompatibleWith(semver) ->
              SemanticVersion.get_compatibility_unit semver
        in
        Dependency{ role = Role{ package_name; registry_local_name; compatibility; context }; version_requirement }
    )


  let implementations (role : Role.t) : role_information =
    match role with
    | Role{ package_name; registry_local_name; compatibility; context } ->
        let impl_records =
          context.registries
            |> RegistryLocalNameMap.find_opt registry_local_name
            |> Option.value ~default:PackageNameMap.empty
            |> PackageNameMap.find_opt package_name |> Option.value ~default:[]
        in
        let impls =
          impl_records |> List.filter_map (fun impl_record ->
            let ImplRecord{ version; source; language_requirement; dependencies } = impl_record in
            if Constant.current_language_version |> SemanticVersion.fulfill language_requirement then
              if String.equal (SemanticVersion.get_compatibility_unit version) compatibility then
                let dependencies = make_internal_dependency_from_registry registry_local_name context dependencies in
                Some(Impl{ package_name; version; source; dependencies })
              else
                None
            else
              None
          )
        in
        { replacement = None; impls }

    | LocalRole{ requires; context } ->
        let dependencies = make_internal_dependency context requires in
        let impls = [ LocalImpl{ dependencies } ] in
        { replacement = None; impls }


  let restrictions (dep : dependency) : restriction list =
    let Dependency{ version_requirement; _ } = dep in
    [ version_requirement ]


  let meets_restriction (impl : impl) (restr : restriction) : bool =
    match impl with
    | DummyImpl ->
        false

    | LocalImpl(_) ->
        true

    | Impl{ version = semver_provided; _} ->
        begin
          match restr with
          | CompatibleWith(semver_required) ->
              SemanticVersion.is_compatible ~old:semver_required ~new_:semver_provided
        end


  (* Unused *)
  let machine_group (_impl : impl) : machine_group option =
    None


  let conflict_class (impl : impl) : conflict_class list =
    match impl with
    | DummyImpl | LocalImpl(_) ->
        [ "*" ]

    | Impl{ package_name; version; _ } ->
        let compat = SemanticVersion.get_compatibility_unit version in
        [ Printf.sprintf "%s/%s" package_name compat ]


  let rejects (_role : Role.t) : (impl * rejection) list * string list =
    ([], []) (* TODO: define `rejection` and implement this *)


  let compare_version (impl1 : impl) (impl2 : impl) : int =
    match (impl1, impl2) with
    | (DummyImpl, DummyImpl) -> 0
    | (DummyImpl, _)         -> 1
    | (_, DummyImpl)         -> -1

    | (LocalImpl(_), LocalImpl(_)) -> 0
    | (LocalImpl(_), _)            -> 1
    | (_, LocalImpl(_))            -> -1

    | (Impl{ version = semver1; _ }, Impl{ version = semver2; _ }) ->
        SemanticVersion.compare semver1 semver2


  let user_restrictions (_role : Role.t) : restriction option =
    None


  let format_machine (_impl : impl) : string =
    ""


  let string_of_restriction (restr : restriction) : string =
    match restr with
    | CompatibleWith(semver) -> SemanticVersion.to_string semver


  let describe_problem (_impl : impl) (_rej : rejection) : string =
    "" (* TODO: define `rejection` and implement this *)


  let dummy_impl : impl =
    DummyImpl

end


module InternalSolver = Zeroinstall_solver.Make(SolverInput)

module LockDependencyGraph = DependencyGraph.Make(Lock)

module VertexSet = LockDependencyGraph.VertexSet


let solve (context : package_context) (dependencies_with_flags : (dependency_flag * package_dependency) list) : (package_solution list) option =
  let (explicit_source_dependencies, dependency_acc) =
    dependencies_with_flags |> List.fold_left (fun (explicit_source_dependencies, dependency_acc) (flag, dep) ->
      match dep with
      | PackageDependency{ package_name; _ } ->
          let explicit_source_dependencies =
            match flag with
            | SourceDependency   -> explicit_source_dependencies |> PackageNameSet.add package_name
            | TestOnlyDependency -> explicit_source_dependencies
          in
          (explicit_source_dependencies, Alist.extend dependency_acc dep)
    ) (PackageNameSet.empty, Alist.empty)
  in
  let requires = Alist.to_list dependency_acc in
  let output_opt =
    InternalSolver.do_solve ~closest_match:false {
      role    = LocalRole{ requires; context };
      command = None;
    }
  in
  output_opt |> Option.map (fun output ->
    let open InternalSolver in

    (* Adds vertices to the graph: *)
    let rolemap = output |> Output.to_map in
    let (quad_acc, graph, explicit_vertices, lock_to_vertex_map) =
      Output.RoleMap.fold (fun _role impl acc ->
        let impl = Output.unwrap impl in
        match impl with
        | DummyImpl | LocalImpl(_) ->
            acc

        | Impl{ package_name; version = locked_version; source; dependencies } ->
            let lock = Lock.{ package_name; locked_version } in
            let (quad_acc, graph, explicit_vertices, lock_to_vertex_map) = acc in
            let (graph, vertex) =
              match graph |> LockDependencyGraph.add_vertex lock () with
              | Error(_) -> assert false
              | Ok(pair) -> pair
            in
            let quad_acc = Alist.extend quad_acc (lock, source, dependencies, vertex) in
            let explicit_vertices =
              if explicit_source_dependencies |> PackageNameSet.mem package_name then
                explicit_vertices |> VertexSet.add vertex
              else
                explicit_vertices
            in
            let lock_to_vertex_map = lock_to_vertex_map |> LockMap.add lock vertex in
            (quad_acc, graph, explicit_vertices, lock_to_vertex_map)

      ) rolemap (Alist.empty, LockDependencyGraph.empty, VertexSet.empty, LockMap.empty)
    in

    (* Add edges to the graph: *)
    let (solmap, graph) =
      quad_acc |> Alist.to_list |> List.fold_left (fun acc quint ->
        let open SolverInput in
        let (solmap, graph) = acc in
        let (lock, source, dependencies, vertex) = quint in
        let (locked_dependency_acc, graph) =
          dependencies |> List.fold_left (fun (locked_dependency_acc, graph) dep ->
            let Dependency{ role = role_dep; _ } = dep in
            match role_dep with
            | Role{ package_name = package_name_dep; _ } ->
                let lock_dep =
                  match rolemap |> Output.RoleMap.find_opt role_dep |> Option.map Output.unwrap with
                  | None | Some(DummyImpl) | Some(LocalImpl(_)) ->
                      assert false

                  | Some(Impl{ version = version_dep; _ }) ->
                      Lock.{
                        package_name   = package_name_dep;
                        locked_version = version_dep;
                      }
                in
                let locked_dependency_acc = Alist.extend locked_dependency_acc lock_dep in
                let vertex_dep =
                  match lock_to_vertex_map |> LockMap.find_opt lock_dep with
                  | None    -> assert false
                  | Some(v) -> v
                in
                let graph = graph |> LockDependencyGraph.add_edge ~from:vertex ~to_:vertex_dep in
                (locked_dependency_acc, graph)

            | LocalRole(_) ->
                (locked_dependency_acc, graph)

          ) (Alist.empty, graph)
        in
        let locked_dependencies = Alist.to_list locked_dependency_acc in
        let solmap = solmap |> LockMap.add lock (source, locked_dependencies) in
        (solmap, graph)

      ) (LockMap.empty, graph)
    in

    (* Computes the set of source dependencies: *)
    let resulting_source_dependencies =
      LockDependencyGraph.reachability_closure graph explicit_vertices
    in

    let solution_acc =
      LockMap.fold (fun lock (locked_source, locked_dependencies) solution_acc ->
        let vertex =
          match lock_to_vertex_map |> LockMap.find_opt lock with
          | None    -> assert false
          | Some(v) -> v
        in
        let used_in_test_only = not (resulting_source_dependencies |> VertexSet.mem vertex) in
        Alist.extend solution_acc {
          lock;
          locked_source;
          locked_dependencies;
          used_in_test_only;
        }
      ) solmap Alist.empty
    in
    Alist.to_list solution_acc
  )
