
open MyUtil

exception MultipleCodeNameDesignation of Range.t * string
exception NotAnInlineCommand          of Range.t * string
exception NotABlockCommand            of Range.t * string

val main : abs_path -> DecodeMD.command_record * string list
