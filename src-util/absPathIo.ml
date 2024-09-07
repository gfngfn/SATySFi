
let open_in (abspath : AbsPath.t) (k : in_channel -> 'a) : 'a =
  let inc = Stdlib.open_in (AbsPath.to_string abspath) in
  let ret = k inc in
  Stdlib.close_in inc;
  ret


let readdir (absdir : AbsPath.t) : (string list, string) result =
  let open ResultMonad in
  try
    return (Sys.readdir (AbsPath.to_string absdir) |> Array.to_list)
  with
  | Sys_error(s) ->
      err s


let read_file (abspath : AbsPath.t) : (string, string) result =
  let open ResultMonad in
  try
    return @@ Core.In_channel.read_all (AbsPath.to_string abspath)
  with
  | Sys_error(s) ->
      err s


let write_file (abspath : AbsPath.t) (data : string) : (unit, string) result =
  let open ResultMonad in
  try
    Core.Out_channel.write_all (AbsPath.to_string abspath) ~data;
    return ()
  with
  | Sys_error(s) ->
      err s


let is_directory (abspath : AbsPath.t) : bool =
  try
    Sys.is_directory (AbsPath.to_string abspath)
  with
  | Sys_error(_) ->
      false
        (* Exceptions are raised when `dirname abspath` is not a directory. *)


let file_exists (abspath : AbsPath.t) : bool =
  Sys.file_exists (AbsPath.to_string abspath)
