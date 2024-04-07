let version_regex = Str.regexp {|^version: ?"\([0-9]+\.[0-9]+\.[0-9]+\)"$|}

let () =
  let version = ref "" in
  let opam_file_path = Sys.argv.(1) in
  let channel_in = open_in opam_file_path in
  let search_version () =
    let cond = ref true in
    while !cond do
      let line = input_line channel_in in
      try
        let _ = Str.search_forward version_regex line 0 in
        version := Str.matched_group 1 line;
        cond := false
      with Not_found ->
        ()
    done
  in
  search_version ();
  close_in channel_in;
  print_endline ("let version = \"" ^ !version ^ "\"")
