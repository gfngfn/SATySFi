
open Types
open PackageSystemBase


type error =
  | LabelNotFound of {
      record_range : Range.t;
      label        : label;
    }
  | DuplicateLabel of {
      record_range : Range.t;
      label        : label;
    }
  | NoConfigArgument         of Range.t
  | DuplicateConfigAttribute of Range.t * Range.t
  | NotAVersionRequirement   of Range.t * string
  | NotAPackageDependency    of Range.t
  | NotARegistry             of Range.t
  | NotARegistryRemote       of Range.t
  | NotAListLiteral          of Range.t
  | NotAStringLiteral        of Range.t
  | DuplicateRegistryLocalName of {
      list_range          : Range.t;
      registry_local_name : registry_local_name;
    }

type 'a ok = ('a, error) result

type t = {
  registry_specs : registry_remote RegistryLocalNameMap.t;
  dependencies   : package_dependency list;
}


let get_unique ~(record_range : Range.t) (label : string) (record : (label ranged * untyped_abstract_tree) list) =
  let open ResultMonad in
  match
    record |> List.filter_map
      (fun ((_, label0), utast0) -> if String.equal label0 label then Some(utast0) else None)
  with
  | []        -> err @@ LabelNotFound{ record_range; label }
  | [ utast ] -> return utast
  | _ :: _    -> err @@ DuplicateLabel{ record_range; label }


let expect_list : untyped_abstract_tree -> (untyped_abstract_tree list) ok =
  let open ResultMonad in
  let rec aux acc = function
    | (_, UTEndOfList) ->
        return @@ Alist.to_list acc

    | (_, UTListCons(utast_elem, utast_tail)) ->
        aux (Alist.extend acc utast_elem) utast_tail

    | (rng, _) ->
        err @@ NotAListLiteral(rng)
  in
  aux Alist.empty


let expect_string : untyped_abstract_tree -> string ok =
  let open ResultMonad in
  function
  | (_, UTStringConstant(s)) -> return s
  | (rng, _)                 -> err @@ NotAStringLiteral(rng)


let expect_version_requirement (utast : untyped_abstract_tree) : SemanticVersion.requirement ok =
  let open ResultMonad in
  let* s = expect_string utast in
  match SemanticVersion.parse_requirement s with
  | None ->
      let (rng, _) = utast in
      err @@ NotAVersionRequirement(rng, s)

  | Some(verreq) ->
      return verreq


let decode_registry_remote (utast : untyped_abstract_tree) : registry_remote ok =
  let open ResultMonad in
  match utast with
  | (_, UTConstructor([], "Git", (record_range, UTRecord(r)))) ->
      let* url =
        let* utast_url = r |> get_unique ~record_range "url" in
        expect_string utast_url
      in
      let* branch =
        let* utast_branch = r |> get_unique ~record_range "branch" in
        expect_string utast_branch
      in
      return @@ GitRegistry{ url; branch }

  | (rng, _) ->
      err @@ NotARegistryRemote(rng)


let decode_registry (utast : untyped_abstract_tree) : (registry_local_name * registry_remote) ok =
  let open ResultMonad in
  match utast with
  | (record_range, UTRecord(r)) ->
      let* registry_local_name =
        let* utast_reg = r |> get_unique ~record_range "name" in
        expect_string utast_reg
      in
      let* registry_remote =
        let* utast_remote = r |> get_unique ~record_range "remote" in
        decode_registry_remote utast_remote
      in
      return (registry_local_name, registry_remote)

  | (rng, _) ->
      err @@ NotARegistry(rng)


let decode_dependency (utast : untyped_abstract_tree) : package_dependency ok =
  let open ResultMonad in
  match utast with
  | (record_range, UTRecord(r)) ->
      let* package_name =
        let* utast_pkg = r |> get_unique ~record_range "name" in
        expect_string utast_pkg
      in
      let* registry_local_name =
        let* utast_reg = r |> get_unique ~record_range "registry" in
        expect_string utast_reg
      in
      let* version_requirement =
        let* utast_verreq = r |> get_unique ~record_range "requirement" in
        expect_version_requirement utast_verreq
      in
      return @@ PackageDependency{
        package_name;
        registry_local_name;
        version_requirement;
      }

  | (rng, _) ->
      err @@ NotAPackageDependency(rng)


let decode_config (utast : untyped_abstract_tree) : t ok =
  let open ResultMonad in
  match utast with
  | (record_range, UTRecord(r)) ->
      let* utast_registries = r |> get_unique ~record_range "registries" in
      let (list_range, _) = utast_registries in
      let* registries =
        let* utasts_registries = expect_list utast_registries in
        utasts_registries |> mapM decode_registry
      in
      let* dependencies =
        let* utast_dependencies = r |> get_unique ~record_range "dependencies" in
        let* utasts_dependencies = expect_list utast_dependencies in
        utasts_dependencies |> mapM decode_dependency
      in
      let* registry_specs =
        registries |> foldM (fun map (registry_local_name, registry_remote) ->
          if map |> RegistryLocalNameMap.mem registry_local_name then
            err @@ DuplicateRegistryLocalName{ list_range; registry_local_name }
          else
            return (map |> RegistryLocalNameMap.add registry_local_name registry_remote)
        ) RegistryLocalNameMap.empty
      in
      return { registry_specs; dependencies }

  | (rng, _) ->
      err @@ NotAPackageDependency(rng)


let make (attrs : untyped_attribute list) : t ok =
  let open ResultMonad in
  let dependencies_attrs =
    attrs |> List.filter_map (function
    | (rng, UTAttribute(attrnm, utast)) ->
        if String.equal "config" attrnm then
          Some((rng, utast))
        else
          None
    )
  in
  match dependencies_attrs with
  | [] ->
      return { registry_specs = RegistryLocalNameMap.empty; dependencies = [] }

  | [ (_, Some(utast)) ] ->
      decode_config utast

  | [ (rng, None) ] ->
      err @@ NoConfigArgument(rng)

  | (rng1, _) :: (rng2, _) :: _ ->
      err @@ DuplicateConfigAttribute(rng1, rng2)
