
type file_path = string

exception MultipleDesignation of file_path * string
exception InvalidYOJSON       of file_path * string
exception MissingRequiredKey  of file_path * string
exception InvalidValueForKey  of file_path * string

val main : string -> DecodeMD.command_record * string list
