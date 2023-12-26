
open MyUtil


let current_language_version =
  match SemanticVersion.parse "0.1.0" with
  | Some(semver) -> semver
  | None         -> assert false


(* TODO: remove this *)
let library_root_config_file =
  make_lib_path "satysfi-library-root.yaml"
