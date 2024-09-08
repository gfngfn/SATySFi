
open PackageSystemBase
open MyUtil


module SolverInput = struct

  module Role = struct

    type t =
      | TargetRole of {
          requires : package_dependency list;
          context  : package_context;
        }
      | LocalFixedRole of {
          absolute_path : abs_path;
          context       : package_context;
        }
      | RegisteredRole of {
          registered_package_id : RegisteredPackageId.t;
          compatibility         : string;
          context               : package_context;
        }


    let pp ppf (role : t) =
      match role with
      | TargetRole(_) ->
          Format.fprintf ppf "target"

      | LocalFixedRole{ absolute_path; _ } ->
          Format.fprintf ppf "local '%s'" (AbsPath.to_string absolute_path)

      | RegisteredRole{ registered_package_id; _ } ->
          let RegisteredPackageId.{ package_name; _ } = registered_package_id in
          Format.fprintf ppf "%s" package_name


    let compare (role1 : t) (role2 : t) =
      match (role1, role2) with
      | (TargetRole(_), TargetRole(_)) ->
          0

      | (TargetRole(_), _) ->
          1

      | (_, TargetRole(_)) ->
          -1

      | ( LocalFixedRole{ absolute_path = abspath1; _ },
          LocalFixedRole{ absolute_path = abspath2; _ }) ->
          AbsPath.compare abspath1 abspath2

      | (LocalFixedRole(_), _) ->
          1

      | (_, LocalFixedRole(_)) ->
          -1

      | ( RegisteredRole{ registered_package_id = regpkgid1; compatibility = c1; _ },
          RegisteredRole{ registered_package_id = regpkgid2; compatibility = c2; _ }) ->
          begin
            match RegisteredPackageId.compare regpkgid1 regpkgid2 with
            | 0       -> String.compare c1 c2
            | nonzero -> nonzero
          end

  end


  (* Unused *)
  type command = unit

  (* Unused *)
  type command_name = string

  type restriction =
    | VersionRequirement of SemanticVersion.requirement
    | AsIs

  type dependency =
    | Dependency of {
        role        : Role.t;
        used_as     : string;
        restriction : restriction;
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
    | TargetImpl of {
        dependencies : dependency list;
      }
    | LocalFixedImpl of {
        absolute_path : abs_path;
        dependencies  : dependency list;
      }
    | Impl of {
        package_name        : package_name;
        package_version     : SemanticVersion.t;
        registry_hash_value : registry_hash_value;
        source              : implementation_source;
        dependencies        : dependency list;
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

    | TargetImpl(_) ->
        Format.fprintf ppf "target"

    | LocalFixedImpl{ absolute_path; _ } ->
        Format.fprintf ppf "local impl '%s'" (AbsPath.to_string absolute_path)

    | Impl{ package_name; package_version; _ } ->
        Format.fprintf ppf "%s %s" package_name (SemanticVersion.to_string package_version)


  let pp_impl_long (ppf : Format.formatter) (impl : impl) =
    pp_impl ppf impl (* TODO: show dependencies *)


  (* Unused *)
  let pp_command (_ppf : Format.formatter) (_cmd : command) =
    ()


  let pp_version (ppf : Format.formatter) (impl : impl) =
    match impl with
    | DummyImpl                  -> Format.fprintf ppf "dummy"
    | TargetImpl(_)              -> Format.fprintf ppf "target"
    | LocalFixedImpl(_)          -> Format.fprintf ppf "as-is"
    | Impl{ package_version; _ } -> Format.fprintf ppf "%s" (SemanticVersion.to_string package_version)


  (* Unused *)
  let get_command (_impl : impl) (_cmdnm : command_name) =
    None


  let dep_info (dep : dependency) : dep_info =
    let Dependency{ role; _ } = dep in
    { dep_role = role; dep_importance = `Essential; dep_required_commands = [] }


  let requires (_role : Role.t) (impl : impl) : dependency list * command_name list =
    match impl with
    | DummyImpl                        -> ([], [])
    | TargetImpl{ dependencies }       -> (dependencies, [])
    | LocalFixedImpl{ dependencies; _} -> (dependencies, [])
    | Impl{ dependencies; _ }          -> (dependencies, [])


  (* Unused *)
  let command_requires (_role : Role.t) (_cmd : command) =
    ([], [])


  let make_internal_dependency_from_registry (self_registry_hash_value : string) (context : package_context) (requires : package_dependency_in_registry list) : dependency list =
    requires |> List.map (function
    | PackageDependencyInRegistry{
        used_as;
        external_registry_hash_value;
        package_name;
        version_requirement;
      } ->
        let compatibility =
          match version_requirement with
          | SemanticVersion.CompatibleWith(semver) ->
              SemanticVersion.get_compatibility_unit semver
        in
        let registry_hash_value =
          match external_registry_hash_value with
          | Some(r) -> r
          | None    -> self_registry_hash_value
        in
        let role =
          Role.RegisteredRole{
            registered_package_id = RegisteredPackageId.{ registry_hash_value; package_name };
            compatibility;
            context;
          }
        in
        Dependency{ role; used_as; restriction = VersionRequirement(version_requirement) }
    )


  let make_internal_dependency (context : package_context) (requires : package_dependency list) : dependency list =
    requires |> List.map (fun dep ->
      let PackageDependency{ used_as; spec } = dep in
      match spec with
      | RegisteredDependency{ registered_package_id; version_requirement } ->
          let compatibility =
            match version_requirement with
            | SemanticVersion.CompatibleWith(semver) ->
                SemanticVersion.get_compatibility_unit semver
          in
          let role = Role.RegisteredRole{ registered_package_id; compatibility; context } in
          Dependency{ role; used_as; restriction = VersionRequirement(version_requirement) }

      | LocalFixedDependency{ absolute_path } ->
          let role = Role.LocalFixedRole{ absolute_path; context } in
          Dependency{ role; used_as; restriction = AsIs }
    )


  let implementations (role : Role.t) : role_information =
    match role with
    | RegisteredRole{ registered_package_id; compatibility; context } ->
        let RegisteredPackageId.{ package_name; registry_hash_value } = registered_package_id in
        let impl_records =
          context.registered_package_impls
            |> RegisteredPackageIdMap.find_opt registered_package_id
            |> Option.value ~default:[]
        in
        let impls =
          impl_records |> List.filter_map (fun impl_record ->
            let ImplRecord{ package_version; source; language_requirement; dependencies; _ } = impl_record in
            if context.language_version |> SemanticVersion.fulfill language_requirement then
              if String.equal (SemanticVersion.get_compatibility_unit package_version) compatibility then
                let dependencies =
                  make_internal_dependency_from_registry registry_hash_value context dependencies
                in
                Some(Impl{ package_name; package_version; registry_hash_value; source; dependencies })
              else
                None
            else
              None
          )
        in
        { replacement = None; impls }

    | LocalFixedRole{ absolute_path; context } ->
        let requires =
          match context.local_fixed_dependencies |> LocalFixedPackageIdMap.find_opt absolute_path with
          | None           -> assert false
          | Some(requires) -> requires
        in
        let dependencies = make_internal_dependency context requires in
        let impl = LocalFixedImpl{ absolute_path; dependencies } in
        { replacement = None; impls = [ impl ] }

    | TargetRole{ requires; context } ->
        let dependencies = make_internal_dependency context requires in
        let impl = TargetImpl{ dependencies } in
        { replacement = None; impls = [ impl ] }


  let restrictions (dep : dependency) : restriction list =
    let Dependency{ restriction; _ } = dep in
    [ restriction ]


  let meets_restriction (impl : impl) (restr : restriction) : bool =
    match impl with
    | DummyImpl ->
        false

    | TargetImpl(_) ->
        true

    | LocalFixedImpl(_) ->
        begin
          match restr with
          | AsIs -> true
          | _    -> false
        end

    | Impl{ package_version = semver_provided; _} ->
        begin
          match restr with
          | VersionRequirement(SemanticVersion.CompatibleWith(semver_required)) ->
              SemanticVersion.is_compatible ~old:semver_required ~new_:semver_provided

          | AsIs ->
              false
        end


  (* Unused *)
  let machine_group (_impl : impl) : machine_group option =
    None


  let conflict_class (impl : impl) : conflict_class list =
    match impl with
    | DummyImpl ->
        [ "dummy" ]

    | TargetImpl(_) ->
        [ "target" ]

    | LocalFixedImpl{ absolute_path; _ } ->
        [ Printf.sprintf "local/%s" (AbsPath.to_string absolute_path) ]

    | Impl{ package_name; package_version; _ } ->
        let compat = SemanticVersion.get_compatibility_unit package_version in
        [ Printf.sprintf "registered/%s/%s" package_name compat ]


  let rejects (_role : Role.t) : (impl * rejection) list * string list =
    ([], []) (* TODO: define `rejection` and implement this *)


  let compare_version (impl1 : impl) (impl2 : impl) : int =
    match (impl1, impl2) with
    | (DummyImpl, DummyImpl) -> 0
    | (DummyImpl, _)         -> 1
    | (_, DummyImpl)         -> -1

    | (TargetImpl(_), TargetImpl(_)) -> 0
    | (TargetImpl(_), _)             -> 1
    | (_, TargetImpl(_))             -> -1

    | (LocalFixedImpl(_), LocalFixedImpl(_)) -> 0
    | (LocalFixedImpl(_), _)                 -> 1
    | (_, LocalFixedImpl(_))                 -> -1

    | (Impl{ package_version = semver1; _ }, Impl{ package_version = semver2; _ }) ->
        SemanticVersion.compare semver1 semver2


  let user_restrictions (_role : Role.t) : restriction option =
    None


  let format_machine (_impl : impl) : string =
    ""


  let string_of_restriction (restr : restriction) : string =
    match restr with
    | VersionRequirement(SemanticVersion.CompatibleWith(semver)) ->
        Printf.sprintf "^%s" (SemanticVersion.to_string semver)

    | AsIs ->
        "as-is"


  let describe_problem (_impl : impl) (_rej : rejection) : string =
    "" (* TODO: define `rejection` and implement this *)


  let dummy_impl : impl =
    DummyImpl

end


module InternalSolver = Zeroinstall_solver.Make(SolverInput)

module LockDependencyGraph = DependencyGraph.Make(Lock)

module VertexSet = LockDependencyGraph.VertexSet

module VertexMap = LockDependencyGraph.VertexMap


let solve (context : package_context) (dependencies_with_flags : (dependency_flag * package_dependency) list) : (package_solution list) option =
  let (explicit_source_dependencies, explicit_test_dependencies, dependency_acc) =
    dependencies_with_flags |> List.fold_left (fun acc (flag, dep) ->
      let (explicit_source_dependencies, explicit_test_dependencies, dependency_acc) = acc in
      match dep with
      | PackageDependency{ spec; used_as } ->
          let package_id =
            match spec with
            | RegisteredDependency{ registered_package_id; _ } ->
                PackageId.Registered(registered_package_id)

            | LocalFixedDependency{ absolute_path } ->
                PackageId.LocalFixed{ absolute_path }
          in
          let (explicit_source_dependencies, explicit_test_dependencies) =
            match flag with
            | SourceDependency ->
                (explicit_source_dependencies |> PackageIdMap.add package_id used_as, explicit_test_dependencies)

            | TestOnlyDependency ->
                (explicit_source_dependencies, explicit_test_dependencies |> PackageIdMap.add package_id used_as)
          in
          (explicit_source_dependencies, explicit_test_dependencies, Alist.extend dependency_acc dep)
    ) (PackageIdMap.empty, PackageIdMap.empty, Alist.empty)
  in
  let requires = Alist.to_list dependency_acc in
  let target_role = SolverInput.Role.TargetRole{ requires; context } in
  let output_opt =
    InternalSolver.do_solve ~closest_match:false {
      role    = target_role;
      command = None;
    }
  in
  output_opt |> Option.map (fun output ->
    let open InternalSolver in

    (* Adds vertices to the graph: *)
    let rolemap = output |> Output.to_map in
    let (quad_acc, graph, explicit_vertex_to_used_as, explicit_test_vertex_to_used_as, lock_to_vertex_map) =
      Output.RoleMap.fold (fun _role impl acc ->
        let impl = Output.unwrap impl in
        match impl with
        | DummyImpl | TargetImpl(_) ->
            acc

        | LocalFixedImpl{ absolute_path; dependencies } ->
            let package_id = PackageId.LocalFixed{ absolute_path } in
            let source = NoSource in (* TODO: reconsider this *)
            let lock = Lock.LocalFixed{ absolute_path } in
            let (quad_acc, graph, explicit_vertex_to_used_as, explicit_test_vertex_to_used_as, lock_to_vertex_map) = acc in
            let (graph, vertex) =
              match graph |> LockDependencyGraph.add_vertex lock () with
              | Error(_) -> assert false
              | Ok(pair) -> pair
            in
            let quad_acc = Alist.extend quad_acc (lock, source, dependencies, vertex) in
            let explicit_vertex_to_used_as =
              match explicit_source_dependencies |> PackageIdMap.find_opt package_id with
              | Some(used_as) -> explicit_vertex_to_used_as |> VertexMap.add vertex used_as
              | None          -> explicit_vertex_to_used_as
            in
            let explicit_test_vertex_to_used_as =
              match explicit_test_dependencies |> PackageIdMap.find_opt package_id with
              | Some(used_as) -> explicit_test_vertex_to_used_as |> VertexMap.add vertex used_as
              | None          -> explicit_test_vertex_to_used_as
            in
            let lock_to_vertex_map = lock_to_vertex_map |> LockMap.add lock vertex in
            (quad_acc, graph, explicit_vertex_to_used_as, explicit_test_vertex_to_used_as, lock_to_vertex_map)

        | Impl{ package_name; package_version = locked_version; registry_hash_value; source; dependencies } ->
            let registered_package_id = RegisteredPackageId.{ registry_hash_value; package_name } in
            let package_id = PackageId.Registered(registered_package_id) in
            let reglock = RegisteredLock.{ registered_package_id; locked_version } in
            let lock = Lock.Registered(reglock) in
            let (quad_acc, graph, explicit_vertex_to_used_as, explicit_test_vertex_to_used_as, lock_to_vertex_map) = acc in
            let (graph, vertex) =
              match graph |> LockDependencyGraph.add_vertex lock () with
              | Error(_) -> assert false
              | Ok(pair) -> pair
            in
            let quad_acc = Alist.extend quad_acc (lock, source, dependencies, vertex) in
            let explicit_vertex_to_used_as =
              match explicit_source_dependencies |> PackageIdMap.find_opt package_id with
              | Some(used_as) -> explicit_vertex_to_used_as |> VertexMap.add vertex used_as
              | None          -> explicit_vertex_to_used_as
            in
            let explicit_test_vertex_to_used_as =
              match explicit_test_dependencies |> PackageIdMap.find_opt package_id with
              | Some(used_as) -> explicit_test_vertex_to_used_as |> VertexMap.add vertex used_as
              | None          -> explicit_test_vertex_to_used_as
            in
            let lock_to_vertex_map = lock_to_vertex_map |> LockMap.add lock vertex in
            (quad_acc, graph, explicit_vertex_to_used_as, explicit_test_vertex_to_used_as, lock_to_vertex_map)

      ) rolemap (Alist.empty, LockDependencyGraph.empty, VertexMap.empty, VertexMap.empty, LockMap.empty)
    in

    (* Add edges to the graph: *)
    let (solmap, graph) =
      quad_acc |> Alist.to_list |> List.fold_left (fun acc quint ->
        let open SolverInput in
        let (solmap, graph) = acc in
        let (lock, source, dependencies, vertex) = quint in
        let (locked_dependency_acc, graph) =
          dependencies |> List.fold_left (fun (locked_dependency_acc, graph) dep ->
            let Dependency{ role = role_dep; used_as; _ } = dep in
            match role_dep with
            | TargetRole(_) ->
                (locked_dependency_acc, graph)

            | LocalFixedRole(_) ->
                let lock_dep =
                  match rolemap |> Output.RoleMap.find_opt role_dep |> Option.map Output.unwrap with
                  | None | Some(DummyImpl) | Some(TargetImpl(_)) | Some(Impl(_)) ->
                      assert false

                  | Some(LocalFixedImpl{ absolute_path; _ }) ->
                      Lock.LocalFixed{ absolute_path }
                in
                let locked_dependency =
                  {
                    depended_lock      = lock_dep;
                    dependency_used_as = used_as;
                  }
                in
                let locked_dependency_acc = Alist.extend locked_dependency_acc locked_dependency in
                (locked_dependency_acc, graph)
                  (* TODO: reconsider this *)

            | RegisteredRole{ registered_package_id = registered_package_id_dep; _ } ->
                let lock_dep =
                  match rolemap |> Output.RoleMap.find_opt role_dep |> Option.map Output.unwrap with
                  | None | Some(DummyImpl) | Some(TargetImpl(_)) | Some(LocalFixedImpl(_)) ->
                      assert false

                  | Some(Impl{ package_version = version_dep; _ }) ->
                      Lock.Registered{
                        registered_package_id = registered_package_id_dep;
                        locked_version        = version_dep;
                      }
                in
                let locked_dependency =
                  {
                    depended_lock      = lock_dep;
                    dependency_used_as = used_as;
                  }
                in
                let locked_dependency_acc = Alist.extend locked_dependency_acc locked_dependency in
                let vertex_dep =
                  match lock_to_vertex_map |> LockMap.find_opt lock_dep with
                  | None    -> assert false
                  | Some(v) -> v
                in
                let graph = graph |> LockDependencyGraph.add_edge ~from:vertex ~to_:vertex_dep in
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
      let explicit_vertices =
        LockDependencyGraph.map_domain explicit_vertex_to_used_as
      in
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
        let explicitly_depended =
          explicit_vertex_to_used_as |> VertexMap.find_opt vertex
        in
        let explicitly_test_depended =
          explicit_test_vertex_to_used_as |> VertexMap.find_opt vertex
        in
        Alist.extend solution_acc {
          lock;
          locked_source;
          locked_dependencies;
          used_in_test_only;
          explicitly_depended;
          explicitly_test_depended;
        }
      ) solmap Alist.empty
    in
    Alist.to_list solution_acc
  )
