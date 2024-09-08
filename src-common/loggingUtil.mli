
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

val show_path : logging_spec -> abs_path -> string

val is_verbose : logging_spec -> bool

val is_not_quiet : logging_spec -> bool
