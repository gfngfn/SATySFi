
open PackageSystemBase


module SolverInput = struct

  module Role = struct

    type t = package_name


    let pp ppf s =
      Format.fprintf ppf "%s" s


    let compare =
      String.compare

  end


  (* Unused *)
  type command = unit

  (* Unused *)
  type command_name = string

  type restriction = package_restriction

  type dependency = package_dependency

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
    | Impl of {
        role         : Role.t;
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

    | Impl{ role; version; _ } ->
        Format.fprintf ppf "%s %s" role (SemanticVersion.to_string version)


  let pp_impl_long (ppf : Format.formatter) (impl : impl) =
    pp_impl ppf impl (* TODO: show dependencies *)


  (* Unused *)
  let pp_command (_ppf : Format.formatter) (_cmd : command) =
    ()


  let pp_version (ppf : Format.formatter) (impl : impl) =
    match impl with
    | DummyImpl          -> Format.fprintf ppf "dummy"
    | Impl{ version; _ } -> Format.fprintf ppf "%s" (SemanticVersion.to_string version)


  (* Unused *)
  let get_command (_impl : impl) (_cmdnm : command_name) =
    None


  let dep_info (dep : dependency) : dep_info =
    let Dependency{ role; _ } = dep in
    { dep_role = role; dep_importance = `Essential; dep_required_commands = [] }


  let requires (_role : Role.t) (impl : impl) : dependency list * command_name list =
    match impl with
    | DummyImpl               -> ([], [])
    | Impl{ dependencies; _ } -> (dependencies, [])


  (* Unused *)
  let command_requires (_role : Role.t) (_cmd : command) =
    ([], [])


  let implementations (role : Role.t) : role_information =
    let impl_records = PackageRegistry.find role in
    let impls =
          impl_records |> List.map (fun impl_record ->
            Impl{
              role         = role;
              version      = impl_record.PackageRegistry.version;
              dependencies = impl_record.PackageRegistry.requires;
            }
          )
    in
    { replacement = None; impls }


  let restrictions (dep : dependency) : restriction list =
    let Dependency{ restrictions; _ } = dep in
    restrictions


  let meets_restriction (impl : impl) (restr : restriction) : bool =
    match impl with
    | DummyImpl ->
        false

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
    | DummyImpl       -> []
    | Impl{ role; _ } -> [ role ] (* TODO: take major versions into account *)


  let rejects (_role : Role.t) : (impl * rejection) list * string list =
    ([], []) (* TODO: define `rejection` and implement this *)


  let compare_version (impl1 : impl) (impl2 : impl) : int =
    match (impl1, impl2) with
    | (DummyImpl, DummyImpl) -> 0
    | (DummyImpl, _)         -> 1
    | (_, DummyImpl)         -> -1

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


type package_solution = {
  package_name   : package_name;
  locked_version : SemanticVersion.t;
  dependencies   : (package_name * SemanticVersion.t) list;
}


let solve (package_name : package_name) : (package_solution list) option =
  let output_opt =
    InternalSolver.do_solve ~closest_match:false {
      role    = package_name;
      command = None;
    }
  in
  output_opt |> Option.map (fun output ->
    let open InternalSolver in
    let rolemap = output |> Output.to_map in
    let acc =
      Output.RoleMap.fold (fun role impl acc ->
        let open SolverInput in
        match Output.unwrap impl with
        | DummyImpl ->
            acc

        | Impl{ version; dependencies; _ } ->
            let dependencies_with_version =
              dependencies |> List.map (fun dep ->
                let Dependency{ role = role_dep; _ } = dep in
                match rolemap |> Output.RoleMap.find_opt role_dep |> Option.map Output.unwrap with
                | None | Some(DummyImpl) ->
                    assert false

                | Some(Impl{ version = version_dep; _ }) ->
                    (role_dep, version_dep)
              )
            in
            Alist.extend acc {
              package_name   = role;
              locked_version = version;
              dependencies   = dependencies_with_version;
            }

      ) rolemap Alist.empty
    in
    Alist.to_list acc
  )
