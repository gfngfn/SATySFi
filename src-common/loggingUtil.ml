
open MyUtil


type path_display_setting =
  | FullPath
  | RelativeToCwd of abs_path


let display_path (setting : path_display_setting) (abspath : abs_path) =
  match setting with
  | FullPath                   -> AbsPath.to_string abspath
  | RelativeToCwd(abspath_cwd) -> AbsPath.make_relative ~from:abspath_cwd abspath


type verbosity =
  | Verbose
  | NormalVerbosity
  | Quiet


let is_verbose = function
  | Verbose -> true
  | _       -> false


let is_not_quiet = function
  | Quiet -> false
  | _     -> true


type logging_spec = {
  path_display_setting : path_display_setting;
  verbosity            : verbosity;
}
