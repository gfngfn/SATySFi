
open MyUtil


type path_display_setting =
  | FullPath
  | RelativeToCwd of abs_path

type verbosity =
  | Verbose
  | NormalVerbosity
  | Quiet

type logging_spec = {
  path_display_setting : path_display_setting;
  verbosity            : verbosity;
}


let show_path (spec : logging_spec) (abspath : abs_path) : string =
  match spec.path_display_setting with
  | FullPath                   -> AbsPath.to_string abspath
  | RelativeToCwd(abspath_cwd) -> AbsPath.to_relative_string_if_descendant ~from:abspath_cwd abspath


let is_verbose (spec : logging_spec) =
  match spec.verbosity with
  | Verbose -> true
  | _       -> false


let is_not_quiet (spec : logging_spec) =
  match spec.verbosity with
  | Quiet -> false
  | _     -> true
