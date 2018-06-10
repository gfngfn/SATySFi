
exception DistFileNotFound of string * string list


let satysfi_root_dirs : (string list) ref = ref []


let initialize root_dirs =
  satysfi_root_dirs := root_dirs


let resolve_dist_path filename =
  let dirlst = !satysfi_root_dirs in
  let rec go = function
    | [] ->
        raise (DistFileNotFound(filename, dirlst))

    | d :: ds ->
        let fn = Filename.concat d filename in
          if Sys.file_exists fn then fn else go ds
  in
    go dirlst
