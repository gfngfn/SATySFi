
open ConfigError
open PackageSystemBase


module ConfigDecoder = YamlDecoder.Make(YamlError)


let language_version_checker : unit ConfigDecoder.t =
  let open ConfigDecoder in
  string >>= function
  | "^0.1.0" -> succeed ()
  | language -> failure (fun _yctx -> UnexpectedLanguage(language))


let package_name_decoder : package_name ConfigDecoder.t =
  let open ConfigDecoder in
  string >>= fun package_name ->
  let chars = Core.String.to_list_rev package_name in
  if
    chars |> List.for_all (fun char ->
      Char.equal char '-' || Core.Char.is_digit char || Core.Char.is_lowercase char
    )
  then
    succeed package_name
  else
    failure (fun yctx -> InvalidPackageName(yctx, package_name))


let version_decoder : SemanticVersion.t ConfigDecoder.t =
  let open ConfigDecoder in
  string >>= fun s_version ->
  match SemanticVersion.parse s_version with
  | None ->
      failure (fun yctx -> NotASemanticVersion(yctx, s_version))

  | Some(semver) ->
      succeed semver


let requirement_decoder : SemanticVersion.requirement ConfigDecoder.t =
  let open ConfigDecoder in
  string >>= fun s_version_requirement ->
  match SemanticVersion.parse_requirement s_version_requirement with
  | None         -> failure (fun context -> NotASemanticVersion(context, s_version_requirement))
  | Some(verreq) -> succeed verreq


let dependency_decoder : package_dependency ConfigDecoder.t =
  let open ConfigDecoder in
  get "name" string >>= fun package_name ->
  get "requirement" requirement_decoder >>= fun version_requirement ->
  succeed @@ PackageDependency{
    package_name;
    version_requirement;
  }
