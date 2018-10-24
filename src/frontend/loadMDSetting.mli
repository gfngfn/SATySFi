
exception MultipleCodeNameDesignation of Range.t * string
exception NotACommandName             of Range.t * string

val main : string -> DecodeMD.command_record * string list
