
open Types
open PackageSystemBase


type error =
  | NotUnique
  | NoDependencyList               of Range.t
  | MoreThanOneDependencyAttribute of Range.t * Range.t
  | NotAVersionRequirement         of Range.t * string
  | NotAPackageDependency          of Range.t
  | NotARegistry                   of Range.t
  | NotARegistryRemote             of Range.t
  | NotAListLiteral                of Range.t
  | NotAStringLiteral              of Range.t

type 'a ok = ('a, error) result

type t = {
  registries   : (registry_local_name * registry_remote) list;
  dependencies : package_dependency list;
}


let filter (label_required : string) =
  List.filter_map
    (fun ((_, label), utast) -> if String.equal label label_required then Some(utast) else None)


(* TODO: refine errors *)
let expect_unique =
  let open ResultMonad in
  function
  | []        -> err NotUnique
  | [ utast ] -> return utast
  | _ :: _    -> err NotUnique


let expect_list =
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


let expect_string =
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


let decode_registry (utast : untyped_abstract_tree) : (registry_local_name * registry_remote) ok =
  let open ResultMonad in
  match utast with
  | (_, UTRecord(r)) ->
      let* registry_local_name =
        let* utast_reg = r |> filter "name" |> expect_unique in
        expect_string utast_reg
      in
      let* registry_remote =
        let* utast_remote = r |> filter "remote" |> expect_unique in
        match utast_remote with
        | (_, UTConstructor([], "Git", (_, UTRecord(r)))) ->
            let* url =
              let* utast_url = r |> filter "url" |> expect_unique in
              expect_string utast_url
            in
            let* branch =
              let* utast_branch = r |> filter "branch" |> expect_unique in
              expect_string utast_branch
            in
            return @@ GitRegistry{ url; branch }

        | (rng, _) ->
            err @@ NotARegistryRemote(rng)
      in
      return (registry_local_name, registry_remote)

  | (rng, _) ->
      err @@ NotARegistry(rng)


let decode_dependency (utast : untyped_abstract_tree) : package_dependency ok =
  let open ResultMonad in
  match utast with
  | (_, UTRecord(r)) ->
      let* package_name =
        let* utast_pkg = r |> filter "name" |> expect_unique in
        expect_string utast_pkg
      in
      let* registry_local_name =
        let* utast_reg = r |> filter "registry" |> expect_unique in
        expect_string utast_reg
      in
      let* version_requirement =
        let* utast_verreq = r |> filter "requirement" |> expect_unique in
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
  | (_, UTRecord(r)) ->
      let* registries =
        let* utast_registries = r |> filter "registries" |> expect_unique in
        let* utasts_registries = expect_list utast_registries in
        utasts_registries |> mapM decode_registry
      in
      let* dependencies =
        let* utast_dependencies = r |> filter "dependencies" |> expect_unique in
        let* utasts_dependencies = expect_list utast_dependencies in
        utasts_dependencies |> mapM decode_dependency
      in
      return { registries; dependencies }

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
      return { registries = []; dependencies = [] }

  | [ (_, Some(utast)) ] ->
      decode_config utast

  | [ (rng, None) ] ->
      err @@ NoDependencyList(rng)

  | (rng1, _) :: (rng2, _) :: _ ->
      err @@ MoreThanOneDependencyAttribute(rng1, rng2)
