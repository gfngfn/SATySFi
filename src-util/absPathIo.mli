
val open_in_abs : AbsPath.t -> in_channel

val readdir : AbsPath.t -> (string list, string) result

val read_file : AbsPath.t -> (string, string) result

val write_file : AbsPath.t -> string -> (unit, string) result

val is_directory : AbsPath.t -> bool

val file_exists : AbsPath.t -> bool
