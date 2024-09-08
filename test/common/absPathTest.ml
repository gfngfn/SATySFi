
let of_string_exn_test () =
  List.iteri (fun i (input, expected) ->
    let got = AbsPath.to_components (AbsPath.of_string_exn input) in
    Alcotest.(check (list string)) (Printf.sprintf "of_string_exn (%d)" i) expected got
  ) [
    ("/foo/bar/baz.txt", ["foo"; "bar"; "baz.txt"]);
    ("/foo//bar/baz.txt", ["foo"; "bar"; "baz.txt"]);
    ("/foo/./bar/baz.txt", ["foo"; "bar"; "baz.txt"]);
    ("/foo/../bar/baz.txt", ["bar"; "baz.txt"]);
  ]


let to_relative_string_test () =
  List.iteri (fun i (s_from, s_target, expected) ->
    let from = AbsPath.of_string_exn s_from in
    let target = AbsPath.of_string_exn s_target in
    let got = AbsPath.to_relative_string ~from target in
    Alcotest.(check string) (Printf.sprintf "to_relative_string (%d)" i) expected got
  ) [
    ("/foo/bar",          "/foo/bar/baz.txt", "baz.txt");
    ("/foo/bar/qux",      "/foo/bar/baz.txt", "../baz.txt");
    ("/foo/bar/qux/quux", "/foo/bar/baz.txt", "../../baz.txt");
    ("/foo/bar",          "/foo/bar",         ".");
    ("/foo/bar",          "/foo/bar.txt",     "../bar.txt");
  ]


let to_relative_string_if_descendant_test () =
  List.iteri (fun i (s_from, s_target, expected) ->
    let from = AbsPath.of_string_exn s_from in
    let target = AbsPath.of_string_exn s_target in
    let got = AbsPath.to_relative_string_if_descendant ~from target in
    Alcotest.(check string) (Printf.sprintf "to_relative_string_if_descendant (%d)" i) expected got
  ) [
    ("/foo/bar",          "/foo/bar/baz.txt", "baz.txt");
    ("/foo/bar/qux",      "/foo/bar/baz.txt", "/foo/bar/baz.txt");
    ("/foo/bar/qux/quux", "/foo/bar/baz.txt", "/foo/bar/baz.txt");
    ("/foo/bar",          "/foo/bar",         ".");
    ("/foo/bar",          "/foo/bar.txt",     "/foo/bar.txt");
  ]


let test_cases =
  Alcotest.[
    test_case "of_string_exn" `Quick of_string_exn_test;
    test_case "to_relative_string" `Quick to_relative_string_test;
    test_case "to_relative_string_if_descendant" `Quick to_relative_string_if_descendant_test;
  ]
