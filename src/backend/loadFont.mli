
type file_path = string
type dir_path = string
type font_abbrev = string

exception InvalidYOJSON                   of file_path * string
exception FontHashOtherThanDictionary     of file_path
exception FontHashElementOtherThanVariant of file_path * font_abbrev * string
exception MultipleDesignation             of file_path * font_abbrev * string
exception UnexpectedYOJSONKey             of file_path * font_abbrev * string
exception UnexpectedYOJSONValue           of file_path * font_abbrev * string * string
exception MissingRequiredYOJSONKey        of file_path * font_abbrev * string

type data =
  | Single     of file_path
  | Collection of file_path * int

val main : file_path -> (font_abbrev * data) list
