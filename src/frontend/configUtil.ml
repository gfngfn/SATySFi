
open ConfigError
open PackageSystemBase


module ConfigDecoder = YamlDecoder.Make(YamlError)


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
