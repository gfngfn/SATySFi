
open MyUtil

val is_uppercased_identifier : string -> bool

val is_lowercased_identifier : string -> bool

val parse_long_command : prefix:string -> string -> (string list * string) option

val parse_long_identifier : string -> (string list * string) option

type path_display_setting =
  | FullPath
  | RelativeToCwd of abs_path

val display_path : path_display_setting -> abs_path -> string

val is_verbose : Verbosity.t -> bool

val is_not_quiet : Verbosity.t -> bool
