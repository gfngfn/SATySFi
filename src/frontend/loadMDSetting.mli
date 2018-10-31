
exception MultipleCodeNameDesignation of Range.t * string
exception NotAnInlineCommand          of Range.t * string
exception NotABlockCommand            of Range.t * string

val main : string -> DecodeMD.command_record * string list
