
open MyUtil

type font_abbrev = string

type data =
  | Single     of lib_path
  | Collection of lib_path * int

val main : abs_path -> (font_abbrev * data) list
