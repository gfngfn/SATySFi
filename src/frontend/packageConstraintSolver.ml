
open PackageSystemBase


module SolverInput = struct

  module Role = struct

    type t =
      | LocalRole of {
          requires : package_dependency list;
          context  : package_context;
        }
      | Role of {
          package_name : package_name;
          context      : package_context;
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

      | (Role{ package_name = name1; _ }, Role{ package_name = name2; _ }) ->
          String.compare name1 name2

  end


  (* Unused *)
  type command = unit

  (* Unused *)
  type command_name = string

  type restriction = package_restriction

  type dependency =
    | Dependency of {
        role         : Role.t;
        restrictions : package_restriction list;
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


  let make_internal_dependency (context : package_context) (requires : package_dependency list) : dependency list =
    requires |> List.map (function
    | PackageDependency{ package_name; restrictions } ->
        Dependency{ role = Role{ package_name; context }; restrictions }
    )


  let implementations (role : Role.t) : role_information =
    match role with
    | Role{ package_name; context } ->
        let impl_records =
          context.registry_contents |> PackageNameMap.find_opt package_name |> Option.value ~default:[]
        in
        let impls =
          impl_records |> List.map (fun impl_record ->
            let version = impl_record.version in
            let dependencies = make_internal_dependency context impl_record.requires in
            Impl{ package_name; version; dependencies }
          )
        in
        { replacement = None; impls }

    | LocalRole{ requires; context } ->
        let dependencies = make_internal_dependency context requires in
        let impls = [ LocalImpl{ dependencies } ] in
        { replacement = None; impls }


  let restrictions (dep : dependency) : restriction list =
    let Dependency{ restrictions; _ } = dep in
    restrictions


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

    | Impl{ package_name; _ } ->
        [ package_name ] (* TODO: take major versions into account *)


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


let get_flag (package_name : package_name) (flagmap : dependency_flag PackageNameMap.t) : dependency_flag =
  match flagmap |> PackageNameMap.find_opt package_name with
  | Some(flag) -> flag
  | None       -> SourceDependency (* Indirect dependencies *)


let solve (context : package_context) (dependencies_with_flags : (dependency_flag * package_dependency) list) : (package_solution list) option =
  let (flagmap, dependency_acc) =
    dependencies_with_flags |> List.fold_left (fun (flagmap, dependency_acc) (flag, dep) ->
      match dep with
      | PackageDependency{ package_name; _ } ->
          (flagmap |> PackageNameMap.add package_name flag, Alist.extend dependency_acc dep)
    ) (PackageNameMap.empty, Alist.empty)
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
    let rolemap = output |> Output.to_map in
    let (solmap, packages_depended_by_source) =
      Output.RoleMap.fold (fun _role impl acc ->
        let open SolverInput in
        let (solmap, packages_depended_by_source) = acc in
        let impl = Output.unwrap impl in
        match impl with
        | DummyImpl | LocalImpl(_) ->
            acc

        | Impl{ package_name; version = locked_version; dependencies; _ } ->
            let flag = flagmap |> get_flag package_name in
            let (locked_dependency_acc, packages_depended_by_source) =
              dependencies |> List.fold_left (fun (locked_dependency_acc, packages_depended_by_source) dep ->
                let Dependency{ role = role_dep; _ } = dep in
                match role_dep with
                | Role{ package_name = package_name_dep; _ } ->
                    let locked_dependency_acc =
                      match rolemap |> Output.RoleMap.find_opt role_dep |> Option.map Output.unwrap with
                      | None | Some(DummyImpl) | Some(LocalImpl(_)) ->
                          locked_dependency_acc

                      | Some(Impl{ version = version_dep; _ }) ->
                          Alist.extend locked_dependency_acc (package_name_dep, version_dep)
                    in
                    let packages_depended_by_source =
                      match flag with
                      | SourceDependency   -> packages_depended_by_source |> PackageNameSet.add package_name_dep
                      | TestOnlyDependency -> packages_depended_by_source
                    in
                    (locked_dependency_acc, packages_depended_by_source)

                | LocalRole(_) ->
                    (locked_dependency_acc, packages_depended_by_source)

              ) (Alist.empty, packages_depended_by_source)
            in
            let locked_dependencies = Alist.to_list locked_dependency_acc in
            let solmap = solmap |> PackageNameMap.add package_name (flag, locked_version, locked_dependencies) in
            (solmap, packages_depended_by_source)

      ) rolemap (PackageNameMap.empty, PackageNameSet.empty)
    in
    let solution_acc =
      PackageNameMap.fold (fun package_name (flag, locked_version, locked_dependencies) solution_acc ->
        let used_in_test_only =
          match (flag, packages_depended_by_source |> PackageNameSet.mem package_name) with
          | (SourceDependency, _) | (_, true) -> false
          | (TestOnlyDependency, false)       -> true
        in
        Alist.extend solution_acc {
          package_name;
          locked_version;
          locked_dependencies;
          used_in_test_only;
        }
      ) solmap Alist.empty
    in
    Alist.to_list solution_acc
  )
