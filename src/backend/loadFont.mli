
open MyUtil

type font_abbrev = string

type data = FontAccess.t

val main : abs_path -> (font_abbrev * data) list
