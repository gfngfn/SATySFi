
exception DistFileNotFound of string

let satysfi_root_dirs : string list ref = ref []


let initialize root_dirs =
  satysfi_root_dirs := root_dirs


let resolve_dist_path filename =
  let rec go = function
    | [] ->
        raise (DistFileNotFound(filename))

    | d :: ds ->
        let fn = Filename.concat d filename in
          if Sys.file_exists fn then fn else go ds
  in
    go (!satysfi_root_dirs)
