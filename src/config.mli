
open MyUtil

exception PackageNotFound     of string * abs_path list
exception LibraryFileNotFound of lib_path * abs_path list
exception LibraryFilesNotFound of lib_path list * abs_path list

val initialize : string list -> unit

val resolve_lib_file_opt : lib_path -> abs_path option

val resolve_lib_file_exn : lib_path -> abs_path

val resolve_lib_file_from_candidates_exn : lib_path list -> abs_path

val resolve_package_exn : string -> string list -> abs_path
