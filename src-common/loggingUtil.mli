
open MyUtil

type path_display_setting =
  | FullPath
  | RelativeToCwd of abs_path

val display_path : path_display_setting -> abs_path -> string

type verbosity =
  | Verbose
  | NormalVerbosity
  | Quiet

val is_verbose : verbosity -> bool

val is_not_quiet : verbosity -> bool

type logging_spec = {
  path_display_setting : path_display_setting;
  verbosity            : verbosity;
}
