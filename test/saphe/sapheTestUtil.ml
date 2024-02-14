
let make_version (s_version : string) : SemanticVersion.t =
  match SemanticVersion.parse s_version with
  | Some(semver) -> semver
  | None         -> assert false
