
open Types
open PackageSystemBase


type error =
  | MoreThanOneDependencyAttribute of Range.t * Range.t
  | NotASemanticVersion            of Range.t * string
  | NotAPackageDependency          of Range.t
  | NotAListLiteral                of Range.t

type 'a ok = ('a, error) result

type t = {
  dependencies : package_dependency list;
}


let decode_package_dependency (utast : untyped_abstract_tree) : package_dependency ok =
  let open ResultMonad in
  match utast with
  | (rng, UTTuple(utasts)) ->
      begin
        match TupleList.to_list utasts with
        | [ (_, UTStringConstant(package_name)); (rng_version, UTStringConstant(s_version)) ] ->
            begin
              match SemanticVersion.parse s_version with
              | Some(semver) ->
                  return @@ PackageDependency{
                    package_name;
                    restrictions = [ CompatibleWith(semver) ];
                  }

              | None ->
                  err @@ NotASemanticVersion(rng_version, s_version)
            end

        | _ ->
            err @@ NotAPackageDependency(rng)
      end

  | (rng, _) ->
      err @@ NotAPackageDependency(rng)


let decode_dependencies (utast : untyped_abstract_tree) : (package_dependency list) ok =
  let open ResultMonad in
  let rec aux depacc (utast : untyped_abstract_tree) =
    match utast with
    | (_, UTEndOfList) ->
        return @@ Alist.to_list depacc

    | (_, UTListCons(utast_elem, utast_tail)) ->
        let* dep = decode_package_dependency utast_elem in
        aux (Alist.extend depacc dep) utast_tail

    | (rng, _) ->
        err @@ NotAListLiteral(rng)
  in
  aux Alist.empty utast


let make (attrs : untyped_attribute list) : t ok =
  let open ResultMonad in
  let dependencies_attrs =
    attrs |> List.filter_map (function
    | (rng, UTAttribute(attrnm, utast)) ->
        if String.equal "dependencies" attrnm then
          Some((rng, utast))
        else
          None
    )
  in
  match dependencies_attrs with
  | [] ->
      return { dependencies = [] }

  | [ (_, utast) ] ->
      let* dependencies = decode_dependencies utast in
      return { dependencies }

  | (rng1, _) :: (rng2, _) :: _ ->
      err @@ MoreThanOneDependencyAttribute(rng1, rng2)
