
let current_language_version =
  match SemanticVersion.parse "0.1.0-alpha.1" with
  | Some(semver) -> semver
  | None         -> assert false
