
exception InvalidYOJSON             of string
exception UnexpectedFontHashTop     of string
exception UnexpectedFontHashElement of string
exception MultipleDesignation       of string
exception UnexpectedYOJSONKey       of string
exception UnexpectedYOJSONValue     of string * string
exception MissingRequiredYOJSONKey  of string

val main : string -> string -> (string * string) list
