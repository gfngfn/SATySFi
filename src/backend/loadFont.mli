
type file_path = string
type dir_path = string
type font_abbrev = string

type data =
  | Single     of file_path
  | Collection of file_path * int

val main : file_path -> (font_abbrev * data) list
