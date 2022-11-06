
type package_name = string

type semver = string


let is_backward_compatible ~new_:(_ : semver) ~old:(_ : semver) : bool =
  failwith "TODO: is_backward_compatible"


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

  type restriction =
    | CompatibleWith of semver

  type dependency =
    | Dependency of {
        role         : Role.t;
        restrictions : restriction list;
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
    | Impl of {
        role         : Role.t;
        version      : semver;
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


  let pp_impl (_ppf : Format.formatter) (_impl : impl) =
    failwith "TODO: SolverInput.pp_impl"


  let pp_impl_long (_ppf : Format.formatter) (_impl : impl) =
    failwith "TODO: SolverInput.pp_impl_long"


  (* Unused *)
  let pp_command (_ppf : Format.formatter) (_cmd : command) =
    ()


  let pp_version (_ppf : Format.formatter) (_impl : impl) =
    failwith "TODO: SolverInput.pp_version"


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


  let implementations (_role : Role.t) : role_information =
    failwith "TODO: SolverInput.implementations; must access registry"


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
              is_backward_compatible ~new_:semver_provided ~old:semver_required
        end


  (* Unused *)
  let machine_group (_impl : impl) : machine_group option =
    None


  let conflict_class (impl : impl) : conflict_class list =
    match impl with
    | DummyImpl       -> []
    | Impl{ role; _ } -> [ role ] (* TODO: take major versions into account *)


  let rejects (_role : Role.t) : (impl * rejection) list * string list =
    failwith "TODO: SolverInput.rejects"


  let compare_version (_impl1 : impl) (_impl2 : impl) : int =
    failwith "TODO: SolverInput.compare_version"


  let user_restrictions (_role : Role.t) : restriction option =
    None


  let format_machine (_impl : impl) : string =
    ""


  let string_of_restriction (_restr : restriction) : string =
    failwith "TODO: SolverInput.string_of_restriction"


  let describe_problem (_impl : impl) (_rej : rejection) : string =
    failwith "TODO: SolverInput.describe_problem"


  let dummy_impl : impl =
    DummyImpl

end


module Impl = Zeroinstall_solver.Make(SolverInput)


let solve (package_name : package_name) =
  Impl.do_solve {
    role    = package_name;
    command = None;
  }
