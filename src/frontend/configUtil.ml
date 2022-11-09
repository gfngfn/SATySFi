
open ConfigError
open PackageSystemBase


module ConfigDecoder = YamlDecoder.Make(YamlError)


let requirement_decoder : package_restriction ConfigDecoder.t =
  let open ConfigDecoder in
  string >>= fun s_version ->
  match SemanticVersion.parse s_version with
  | None         -> failure (fun context -> NotASemanticVersion(context, s_version))
  | Some(semver) -> succeed @@ CompatibleWith(semver)


let dependency_decoder : package_dependency ConfigDecoder.t =
  let open ConfigDecoder in
  get "name" string >>= fun package_name ->
  get "requirements" (list requirement_decoder) >>= fun restrictions ->
  succeed @@ PackageDependency{
    package_name;
    restrictions;
  }
