
open MyUtil


type deps_entry =
  | EntryAbsPath of abs_path
  | EntryLibPath of lib_path


let main_list = ref []


let reset () =
    main_list := []


let find entry =
    let rec impl lst =
        match lst with
        | [] -> false
        | head::tail ->
            let res = match head with
            | EntryAbsPath(pat1) ->
                match entry with
                | EntryAbsPath(pat2) ->
                        String.compare
                            (get_abs_path_string pat1) (get_abs_path_string pat2)
                            == 0
                | _ -> false

            | EntryLibPath(pat1) ->
                match entry with
                | EntryLibPath(pat2) ->
                        String.compare
                            (get_lib_path_string pat1) (get_lib_path_string pat2)
                            == 0
                | _ -> false
            in
            if res then
                true
            else
                impl tail
    in
    impl !main_list

let register entry =
    if find entry then
        ()
    else
        main_list := entry :: !main_list

let register_abs_path abspath =
    let entry = EntryAbsPath(abspath) in
    register entry


let register_lib_path libpath =
    let entry = EntryLibPath(libpath) in
    register entry


let list_abspath () =
    !main_list |> List.fold_left (fun acc entry -> (
        match entry with
        | EntryAbsPath(abspath) -> abspath::acc
        | _ -> acc
    )) []


