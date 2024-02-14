
let of_string_exn_test () =
  List.iter (fun (input, expected) ->
    let got = AbsPath.to_components (AbsPath.of_string_exn input) in
    Alcotest.(check (list string)) "of_string_exn" expected got
  ) [
    ("/foo/bar/baz.txt", ["foo"; "bar"; "baz.txt"]);
    ("/foo//bar/baz.txt", ["foo"; "bar"; "baz.txt"]);
    ("/foo/./bar/baz.txt", ["foo"; "bar"; "baz.txt"]);
    ("/foo/../bar/baz.txt", ["bar"; "baz.txt"]);
  ]


let test_cases =
  Alcotest.[
    test_case "of_string_exn" `Quick of_string_exn_test;
  ]
