
val getcwd : unit -> AbsPath.t

val get_home_directory : unit -> AbsPath.t option * string

val open_in : AbsPath.t -> (in_channel -> 'a) -> 'a

val readdir : AbsPath.t -> (string list, string) result

val read_file : AbsPath.t -> (string, string) result

val write_file : AbsPath.t -> string -> (unit, string) result

val is_directory : AbsPath.t -> bool

val file_exists : AbsPath.t -> bool